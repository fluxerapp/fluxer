// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {UserOwnsGuildsError} from '@fluxer/errors/src/domains/guild/UserOwnsGuildsError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms} from 'itty-time';

interface UserAccountLifecycleServiceDeps {
	apiContext: ApiContext;
	userAccountRepository: UserRepository;
	guildRepository: GuildRepository;
	emailService: IEmailService;
	userUpdateDeps: PartialUserChangePropagationDeps;
	kvDeletionQueue: KVAccountDeletionQueueService;
}

export class UserAccountLifecycleService {
	constructor(private readonly deps: UserAccountLifecycleServiceDeps) {}

	async selfDisable(userId: UserID): Promise<void> {
		const user = await this.deps.userAccountRepository.findUniqueAssert(userId);
		const updatedUser = await this.deps.userAccountRepository.updateFlags(
			userId,
			(flags) => flags | UserFlags.DISABLED,
		);
		await revokeAllAuthSessions(this.deps.apiContext.services, userId);
		if (updatedUser) {
			await propagateUserUpdate(this.deps.userUpdateDeps, user, updatedUser);
		}
	}

	async selfDelete(userId: UserID): Promise<void> {
		const user = await this.deps.userAccountRepository.findUniqueAssert(userId);
		const ownedGuildIds = await this.deps.guildRepository.listOwnedGuildIds(userId);
		if (ownedGuildIds.length > 0) {
			throw new UserOwnsGuildsError();
		}
		const gracePeriodMs = Config.deletionGracePeriodHours * ms('1 hour');
		const pendingDeletionAt = new Date(Date.now() + gracePeriodMs);
		const updatedUser = await this.deps.userAccountRepository.updateDeletionSchedule(user, {
			flags: user.flags | UserFlags.SELF_DELETED,
			pending_deletion_at: pendingDeletionAt,
			deletion_reason_code: DeletionReasons.USER_REQUESTED,
			deletion_scheduled_by: userId,
			deletion_scheduled_at: new Date(),
		});
		await reschedulePendingDeletion({
			userId,
			currentPendingDeletionAt: user.pendingDeletionAt,
			nextPendingDeletionAt: pendingDeletionAt,
			deletionReasonCode: DeletionReasons.USER_REQUESTED,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		await revokeAllAuthSessions(this.deps.apiContext.services, userId);
		if (user.email) {
			await this.deps.emailService.send(
				'self_deletion_scheduled',
				user.email,
				{deletionDate: pendingDeletionAt},
				user.locale,
			);
		}
		if (updatedUser) {
			await propagateUserUpdate(this.deps.userUpdateDeps, user, updatedUser);
		}
	}
}
