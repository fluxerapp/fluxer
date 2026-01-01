/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {AuthService} from '~/auth/AuthService';
import type {UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {DeletionReasons, UserFlags} from '~/Constants';
import {UnknownUserError, UserOwnsGuildsError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import {hasPartialUserFieldsChanged} from '~/user/UserMappers';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {UserAccountUpdatePropagator} from './UserAccountUpdatePropagator';

interface UserAccountLifecycleServiceDeps {
	userAccountRepository: IUserAccountRepository;
	guildRepository: IGuildRepository;
	authService: AuthService;
	emailService: IEmailService;
	updatePropagator: UserAccountUpdatePropagator;
	redisDeletionQueue: RedisAccountDeletionQueueService;
}

export class UserAccountLifecycleService {
	constructor(private readonly deps: UserAccountLifecycleServiceDeps) {}

	async selfDisable(userId: UserID): Promise<void> {
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const ownedGuildIds = await this.deps.guildRepository.listOwnedGuildIds(userId);
		if (ownedGuildIds.length > 0) {
			throw new UserOwnsGuildsError();
		}

		const updatedUser = await this.deps.userAccountRepository.patchUpsert(userId, {
			flags: user.flags | UserFlags.DISABLED,
		});

		await this.deps.authService.terminateAllUserSessions(userId);

		await this.deps.updatePropagator.dispatchUserUpdate(updatedUser!);
		if (hasPartialUserFieldsChanged(user, updatedUser!)) {
			await this.deps.updatePropagator.invalidateUserCache(userId);
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

		const gracePeriodMs = Config.deletionGracePeriodHours * 60 * 60 * 1000;
		const pendingDeletionAt = new Date(Date.now() + gracePeriodMs);

		const updatedUser = await this.deps.userAccountRepository.patchUpsert(userId, {
			flags: user.flags | UserFlags.SELF_DELETED,
			pending_deletion_at: pendingDeletionAt,
		});

		await this.deps.userAccountRepository.addPendingDeletion(userId, pendingDeletionAt, DeletionReasons.USER_REQUESTED);

		await this.deps.redisDeletionQueue.scheduleDeletion(userId, pendingDeletionAt, DeletionReasons.USER_REQUESTED);

		if (user.email) {
			await this.deps.emailService.sendSelfDeletionScheduledEmail(
				user.email,
				user.username,
				pendingDeletionAt,
				user.locale,
			);
		}

		await this.deps.authService.terminateAllUserSessions(userId);

		await this.deps.updatePropagator.dispatchUserUpdate(updatedUser!);
		if (hasPartialUserFieldsChanged(user, updatedUser!)) {
			await this.deps.updatePropagator.invalidateUserCache(userId);
		}
	}
}
