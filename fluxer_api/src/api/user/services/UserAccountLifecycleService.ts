// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthSession from '@app/api/auth/AuthSession';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import type {UserAccountUpdatePropagator} from '@app/api/user/services/UserAccountUpdatePropagator';
import {hasPartialUserFieldsChanged} from '@app/api/user/UserMappers';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {UserOwnsGuildsError} from '@fluxer/errors/src/domains/guild/UserOwnsGuildsError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms} from 'itty-time';

interface UserAccountLifecycleServiceDeps {
	apiContext: ApiContext;
	userAccountRepository: IUserAccountRepository;
	guildRepository: IGuildRepositoryAggregate;
	emailService: IEmailService;
	updatePropagator: UserAccountUpdatePropagator;
	kvDeletionQueue: KVAccountDeletionQueueService;
}

export class UserAccountLifecycleService {
	constructor(private readonly deps: UserAccountLifecycleServiceDeps) {}

	async selfDisable(userId: UserID): Promise<void> {
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updatedUser = await this.deps.userAccountRepository.updateFlags(
			userId,
			(flags) => flags | UserFlags.DISABLED,
		);
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		if (updatedUser) {
			await this.deps.updatePropagator.dispatchUserUpdate(updatedUser);
			if (hasPartialUserFieldsChanged(user, updatedUser)) {
				await this.deps.updatePropagator.propagatePartialUserChange(updatedUser);
			}
		}
	}

	async selfDelete(userId: UserID): Promise<void> {
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
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
			userRepository: this.deps.userAccountRepository,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		if (user.email) {
			await this.deps.emailService.sendSelfDeletionScheduledEmail(
				user.email,
				user.username,
				pendingDeletionAt,
				user.locale,
			);
		}
		if (updatedUser) {
			await this.deps.updatePropagator.dispatchUserUpdate(updatedUser);
			if (hasPartialUserFieldsChanged(user, updatedUser)) {
				await this.deps.updatePropagator.propagatePartialUserChange(updatedUser);
			}
		}
	}
}
