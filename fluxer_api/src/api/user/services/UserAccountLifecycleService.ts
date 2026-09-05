// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {DEFERRABLE_PHONE_FLAGS, DEFERRED_PHONE_ON_COMMUNITY_JOIN, UserFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import {UserOwnsGuildsError} from '@fluxer/errors/src/domains/guild/UserOwnsGuildsError';
import {PhoneGateEscapeUnavailableError} from '@fluxer/errors/src/domains/user/PhoneGateEscapeUnavailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import * as AuthSession from '../../auth/AuthSession';
import type {UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../../guild/services/GuildService';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import {Logger} from '../../Logger';
import type {Guild} from '../../models/Guild';
import type {User} from '../../models/User';
import {
	canEscapePhoneGate,
	type DeferredPhoneGateStatus,
	getDeferredPhoneGateConfig,
	guildTriggersPhoneGate,
	PHONE_GATE_ESCAPE_MAX_GUILDS,
	restorePhoneGateDeferral,
} from '../../risk/DeferredPhoneGate';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import {getEffectiveSuspiciousFlags} from '../UserHelpers';
import {hasPartialUserFieldsChanged} from '../UserMappers';
import {reschedulePendingDeletion} from './PendingDeletionCoordinator';
import type {UserAccountUpdatePropagator} from './UserAccountUpdatePropagator';

const WRITE_RESTORED_DEFERRAL_ATTEMPTS = 3;

interface UserAccountLifecycleServiceDeps {
	apiContext: ApiContext;
	userAccountRepository: IUserAccountRepository;
	guildRepository: IGuildRepositoryAggregate;
	guildService: GuildService;
	emailService: IEmailService;
	updatePropagator: UserAccountUpdatePropagator;
	kvDeletionQueue: KVAccountDeletionQueueService;
}

interface PhoneGateEscapePlan {
	available: boolean;
	leavable: Array<Guild>;
	owned: Array<Guild>;
	gateStatus: DeferredPhoneGateStatus;
	threshold: number;
	refusedReason: 'ineligible' | 'not_locked' | null;
}

interface PhoneGateEscapePreview {
	available: boolean;
	guilds: Array<Guild>;
	ownedGuilds: Array<Guild>;
}

interface PhoneGateEscapeResult {
	user: User;
	remainingGuildCount: number;
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
				await this.deps.updatePropagator.updateUserCache(updatedUser);
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
		const updatedUser = await this.deps.userAccountRepository.patchUpsert(
			userId,
			{
				flags: user.flags | UserFlags.SELF_DELETED,
				pending_deletion_at: pendingDeletionAt,
				deletion_reason_code: DeletionReasons.USER_REQUESTED,
			},
			user.toRow(),
		);
		await reschedulePendingDeletion({
			userId,
			currentPendingDeletionAt: user.pendingDeletionAt,
			nextPendingDeletionAt: pendingDeletionAt,
			deletionReasonCode: DeletionReasons.USER_REQUESTED,
			userRepository: this.deps.userAccountRepository,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		if (user.email) {
			await this.deps.emailService.sendSelfDeletionScheduledEmail(
				user.email,
				user.username,
				pendingDeletionAt,
				user.locale,
			);
		}
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		if (updatedUser) {
			await this.deps.updatePropagator.dispatchUserUpdate(updatedUser);
			if (hasPartialUserFieldsChanged(user, updatedUser)) {
				await this.deps.updatePropagator.updateUserCache(updatedUser);
			}
		}
	}

	private async buildPhoneGateEscapePlan(user: User): Promise<PhoneGateEscapePlan> {
		const {status, config} = await getDeferredPhoneGateConfig();
		const refused = {
			available: false,
			leavable: [],
			owned: [],
			gateStatus: status,
			threshold: config.memberThreshold,
		};
		if (!canEscapePhoneGate(user, status)) {
			return {...refused, refusedReason: 'ineligible'};
		}
		if (getEffectiveSuspiciousFlags(user) === 0) {
			return {...refused, refusedReason: 'not_locked'};
		}
		const guilds = await this.deps.guildRepository.listUserGuilds(user.id);
		const qualifying = guilds.filter((guild) => guildTriggersPhoneGate(guild, config.memberThreshold));
		return {
			available: true,
			leavable: qualifying.filter((guild) => guild.ownerId !== user.id),
			owned: qualifying.filter((guild) => guild.ownerId === user.id),
			gateStatus: status,
			threshold: config.memberThreshold,
			refusedReason: null,
		};
	}

	async previewPhoneGateEscape(userId: UserID): Promise<PhoneGateEscapePreview> {
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const plan = await this.buildPhoneGateEscapePlan(user);
		Logger.debug(
			{
				userId: userId.toString(),
				available: plan.available,
				refusedReason: plan.refusedReason,
				gateStatus: plan.gateStatus,
				qualifyingCount: plan.leavable.length + plan.owned.length,
			},
			'deferred_phone_gate.escape_previewed',
		);
		return {available: plan.available, guilds: plan.leavable, ownedGuilds: plan.owned};
	}

	async executePhoneGateEscape(userId: UserID): Promise<PhoneGateEscapeResult> {
		const startedAt = Date.now();
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const plan = await this.buildPhoneGateEscapePlan(user);
		const baseContext = {
			userId: userId.toString(),
			accountAgeMs: Date.now() - snowflakeToDate(BigInt(user.id)).getTime(),
			gateStatus: plan.gateStatus,
			threshold: plan.threshold,
		};
		if (!plan.available) {
			Logger.info({...baseContext, reason: plan.refusedReason}, 'deferred_phone_gate.escape_refused');
			throw new PhoneGateEscapeUnavailableError();
		}
		const batch = plan.leavable.slice(0, PHONE_GATE_ESCAPE_MAX_GUILDS);
		const remainingGuildCount = plan.leavable.length - batch.length;
		const leftGuildIds: Array<string> = [];
		const skippedGuildIds: Array<string> = [];
		for (const guild of batch) {
			try {
				await this.deps.guildService.members.leaveGuild({userId: user.id, guildId: guild.id});
				leftGuildIds.push(guild.id.toString());
			} catch (error) {
				if (error instanceof UnknownGuildMemberError) {
					skippedGuildIds.push(guild.id.toString());
					continue;
				}
				Logger.warn(
					{
						...baseContext,
						leftGuildIds,
						skippedGuildIds,
						unattemptedCount: batch.length - leftGuildIds.length - skippedGuildIds.length,
						error,
					},
					'deferred_phone_gate.escape_failed',
				);
				throw error;
			}
		}
		if (remainingGuildCount > 0) {
			Logger.info(
				{...baseContext, leftGuildIds, skippedGuildIds, remainingGuildCount},
				'deferred_phone_gate.escape_batch_completed',
			);
			return {user, remainingGuildCount};
		}
		const updated = await this.writeRestoredDeferral(userId);
		if ((getEffectiveSuspiciousFlags(updated) & DEFERRABLE_PHONE_FLAGS) !== 0) {
			Logger.warn(
				{...baseContext, flagsAfter: updated.suspiciousActivityFlags ?? 0},
				'deferred_phone_gate.escape_ineffective',
			);
		}
		try {
			await this.deps.updatePropagator.dispatchUserUpdate(updated);
		} catch (error) {
			Logger.warn({...baseContext, error}, 'deferred_phone_gate.escape_dispatch_failed');
		}
		Logger.info(
			{
				...baseContext,
				leftGuildIds,
				skippedGuildIds,
				keptOwnedGuildIds: plan.owned.map((guild) => guild.id.toString()),
				flagsBefore: user.suspiciousActivityFlags ?? 0,
				flagsAfter: updated.suspiciousActivityFlags ?? 0,
				durationMs: Date.now() - startedAt,
			},
			'deferred_phone_gate.escaped',
		);
		return {user: updated, remainingGuildCount: 0};
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
