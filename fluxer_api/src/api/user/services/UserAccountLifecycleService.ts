// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthSession from '@app/api/auth/AuthSession';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import type {UserAccountUpdatePropagator} from '@app/api/user/services/UserAccountUpdatePropagator';
import {getEffectiveSuspiciousFlags} from '@app/api/user/UserHelpers';
import {hasPartialUserFieldsChanged} from '@app/api/user/UserMappers';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {
	DEFERRABLE_PHONE_FLAGS,
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	NEVER_DEFERRABLE_PHONE_FLAGS,
	PHONE_GATE_PROMOTED_FROM_DEFERRAL,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import {UserOwnsGuildsError} from '@fluxer/errors/src/domains/guild/UserOwnsGuildsError';
import {PhoneGateEscapeUnavailableError} from '@fluxer/errors/src/domains/user/PhoneGateEscapeUnavailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms} from 'itty-time';

const WRITE_RESTORED_DEFERRAL_ATTEMPTS = 3;

function holdsPromotedDeferral(user: User): boolean {
	const flagBits = user.suspiciousActivityFlags ?? 0;
	return (
		!user.hasVerifiedPhone &&
		(flagBits & PHONE_GATE_PROMOTED_FROM_DEFERRAL) !== 0 &&
		(flagBits & DEFERRED_PHONE_ON_COMMUNITY_JOIN) === 0 &&
		(flagBits & DEFERRABLE_PHONE_FLAGS) !== 0 &&
		(flagBits & NEVER_DEFERRABLE_PHONE_FLAGS) === 0 &&
		getEffectiveSuspiciousFlags(user) !== 0
	);
}

function restorePhoneGateDeferral(flagBits: number): number {
	return (flagBits | DEFERRED_PHONE_ON_COMMUNITY_JOIN) & ~PHONE_GATE_PROMOTED_FROM_DEFERRAL;
}

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
		const updatedUser = await this.deps.userAccountRepository.patchUpsert(
			userId,
			{
				flags: user.flags | UserFlags.DISABLED,
			},
			user.toRow(),
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

	async previewPhoneGateEscape(userId: UserID): Promise<boolean> {
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return holdsPromotedDeferral(user);
	}

	async executePhoneGateEscape(userId: UserID): Promise<User> {
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const context = {
			userId: userId.toString(),
			accountAgeMs: Date.now() - snowflakeToDate(BigInt(user.id)).getTime(),
		};
		if (!holdsPromotedDeferral(user)) {
			Logger.info(context, 'deferred_phone_gate.escape_refused');
			throw new PhoneGateEscapeUnavailableError();
		}
		const updated = await this.writeRestoredDeferral(userId);
		try {
			await this.deps.updatePropagator.dispatchUserUpdate(updated);
		} catch (error) {
			Logger.warn({...context, error}, 'deferred_phone_gate.escape_dispatch_failed');
		}
		Logger.info(
			{...context, flagsBefore: user.suspiciousActivityFlags ?? 0, flagsAfter: updated.suspiciousActivityFlags ?? 0},
			'deferred_phone_gate.escaped',
		);
		return updated;
	}

	private async writeRestoredDeferral(userId: UserID): Promise<User> {
		for (let attempt = 0; attempt < WRITE_RESTORED_DEFERRAL_ATTEMPTS; attempt++) {
			const current = await this.deps.userAccountRepository.findUnique(userId);
			if (!current) {
				throw new UnknownUserError();
			}
			if (((current.suspiciousActivityFlags ?? 0) & DEFERRED_PHONE_ON_COMMUNITY_JOIN) !== 0) {
				return current;
			}
			try {
				return await this.deps.userAccountRepository.patchUpsert(
					current.id,
					{suspicious_activity_flags: restorePhoneGateDeferral(current.suspiciousActivityFlags ?? 0)},
					current.toRow(),
				);
			} catch (error) {
				if (attempt === WRITE_RESTORED_DEFERRAL_ATTEMPTS - 1) {
					throw error;
				}
			}
		}
		throw new UnknownUserError();
	}
}
