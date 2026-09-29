// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminRepository} from '@app/api/admin/AdminRepository';
import {createUserID} from '@app/api/BrandedTypes';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import {IP_BAN_REFRESH_CHANNEL} from '@app/api/constants/IpBan';
import {withAccountChangeSource} from '@app/api/infrastructure/activity/ActivityMeta';
import type {
	ActionEnvelope,
	ActionOutcome,
	Observed,
	OutcomeStatus,
} from '@app/api/infrastructure/activity/Contract.generated';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {UserContactChangeLogService} from '@app/api/user/services/UserContactChangeLogService';
import {mapUserToPartialResponse, mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {getSameIpDecisionKey, isPublicIpAddress, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

export type ActionOf<T extends ActionEnvelope['type']> = Extract<ActionEnvelope, {type: T}>;

export interface AccountUpdateDispatch {
	userUpdated(user: User): Promise<void>;
	memberProfilesUpdated(user: User): Promise<void>;
}

export interface AccountStateDeps {
	users: Pick<IUserRepository, 'findUnique' | 'patchUpsert' | 'compareAndSetSuspiciousFlags'>;
	dispatch: AccountUpdateDispatch;
	contactChangeLog: Pick<UserContactChangeLogService, 'recordDiff'>;
	ipBans: Pick<AdminRepository, 'isIpBanned' | 'banIpTemp'>;
	cache: Pick<ICacheService, 'publish'>;
	now?: () => number;
}

const SUSPICIOUS_FLAGS_WRITE_ATTEMPTS = 3;
const MIN_TEMP_BAN_SECONDS = 60;

function gatewayDispatch(
	gateway: Pick<IGatewayService, 'dispatchPresence' | 'dispatchGuild' | 'getGuildMember'>,
	users: Pick<IUserRepository, 'getUserGuildIds'>,
): AccountUpdateDispatch {
	return {
		async userUpdated(user) {
			await gateway.dispatchPresence({userId: user.id, event: 'USER_UPDATE', data: mapUserToPrivateResponse(user)});
		},
		async memberProfilesUpdated(user) {
			const userPartial = mapUserToPartialResponse(user);
			for (const guildId of await users.getUserGuildIds(user.id)) {
				const member = await gateway.getGuildMember({guildId, userId: user.id});
				if (!member.success || !member.memberData) continue;
				await gateway.dispatchGuild({
					guildId,
					event: 'GUILD_MEMBER_UPDATE',
					data: {...member.memberData, user: userPartial},
				});
			}
		},
	};
}

export function accountStateDepsFromContext(ctx: ApiContext, ipBans: AccountStateDeps['ipBans']): AccountStateDeps {
	return {
		users: ctx.services.users,
		dispatch: gatewayDispatch(ctx.services.gateway, ctx.services.users),
		contactChangeLog: ctx.services.contactChangeLog,
		ipBans,
		cache: ctx.services.cache,
	};
}

export function observedOf(user: User): Observed {
	return {
		flags: user.flags.toString(),
		suspicious_flags: user.suspiciousActivityFlags ?? 0,
		has_verified_phone: user.hasVerifiedPhone,
		deleted: (user.flags & UserFlags.DELETED) !== 0n,
	};
}

export function outcomeOf(
	env: Pick<ActionEnvelope, 'id'> & {type: string},
	status: OutcomeStatus,
	user: User | null = null,
	detail: string | null = null,
): ActionOutcome {
	return {
		action_id: env.id,
		action_type: env.type,
		status,
		detail,
		observed: user ? observedOf(user) : null,
	};
}

function isIneligible(user: User): boolean {
	return user.isBot || (user.flags & UserFlags.DELETED) !== 0n;
}

export async function applySuspiciousFlags(
	deps: AccountStateDeps,
	env: ActionOf<'set_suspicious_flags'>,
): Promise<ActionOutcome> {
	return withAccountChangeSource('action', async () => {
		const userId = createUserID(BigInt(env.user_id));
		let user = await deps.users.findUnique(userId);
		for (let attempt = 0; attempt < SUSPICIOUS_FLAGS_WRITE_ATTEMPTS; attempt++) {
			if (!user) return outcomeOf(env, 'ineligible');
			if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
			const current = user.suspiciousActivityFlags ?? 0;
			const target = (current | env.set) & ~env.clear;
			if (target === current) return outcomeOf(env, 'noop', user);
			if (env.if_current !== null && env.if_current !== current) return outcomeOf(env, 'conflict', user);
			const updated = await deps.users.compareAndSetSuspiciousFlags(user, target);
			if (updated) {
				await deps.dispatch.userUpdated(updated);
				return outcomeOf(env, 'applied', updated);
			}
			user = await deps.users.findUnique(userId);
		}
		throw new Error('Suspicious activity flags kept changing during apply');
	});
}

export async function applySpammer(deps: AccountStateDeps, env: ActionOf<'set_spammer'>): Promise<ActionOutcome> {
	return withAccountChangeSource('action', async () => {
		const user = await deps.users.findUnique(createUserID(BigInt(env.user_id)));
		if (!user) return outcomeOf(env, 'ineligible');
		if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
		const has = (user.flags & UserFlags.SPAMMER) !== 0n;
		if (has === env.on) return outcomeOf(env, 'noop', user);
		const flags = env.on ? user.flags | UserFlags.SPAMMER : user.flags & ~UserFlags.SPAMMER;
		const updated = await deps.users.patchUpsert(user.id, {flags}, user.toRow());
		await deps.dispatch.userUpdated(updated);
		await deps.dispatch.memberProfilesUpdated(updated);
		return outcomeOf(env, 'applied', updated);
	});
}

export async function applyPhoneVerified(
	deps: AccountStateDeps,
	env: ActionOf<'phone_verified'>,
): Promise<ActionOutcome> {
	return withAccountChangeSource('phone_verify', async () => {
		const user = await deps.users.findUnique(createUserID(BigInt(env.user_id)));
		if (!user) return outcomeOf(env, 'ineligible');
		if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
		const suspicious = user.suspiciousActivityFlags ?? 0;
		const nextSuspicious = suspicious & ~env.clear_suspicious;
		if (user.hasVerifiedPhone && nextSuspicious === suspicious) {
			return outcomeOf(env, 'noop', user);
		}
		const updates: {has_verified_phone: boolean; suspicious_activity_flags?: number} = {
			has_verified_phone: true,
		};
		if (nextSuspicious !== suspicious) updates.suspicious_activity_flags = nextSuspicious;
		const updated = await deps.users.patchUpsert(user.id, updates, user.toRow());
		if (!user.hasVerifiedPhone) {
			await deps.contactChangeLog.recordDiff({
				oldUser: user,
				newUser: updated,
				reason: 'user_requested',
				actorUserId: user.id,
			});
		}
		await deps.dispatch.userUpdated(updated);
		return outcomeOf(env, 'applied', updated);
	});
}

function parseBanTarget(value: string): ReturnType<typeof parseIpAddress> {
	const direct = parseIpAddress(value);
	if (direct) return direct;
	const slash = value.lastIndexOf('/');
	if (slash <= 0) return null;
	const network = parseIpAddress(value.slice(0, slash));
	if (!network || getSameIpDecisionKey(network.normalized) !== value) return null;
	return network;
}

export async function applyTempBanIp(deps: AccountStateDeps, env: ActionOf<'temp_ban_ip'>): Promise<ActionOutcome> {
	const parsed = parseBanTarget(env.ip);
	if (!parsed || !isPublicIpAddress(parsed.normalized) || isIpBanExempt(parsed.normalized)) {
		return outcomeOf(env, 'exempt');
	}
	const now = deps.now?.() ?? Date.now();
	const ttlSeconds = Math.floor((env.until_ms - now) / 1000);
	if (ttlSeconds < MIN_TEMP_BAN_SECONDS) return outcomeOf(env, 'noop');
	if (await deps.ipBans.isIpBanned(parsed.normalized)) return outcomeOf(env, 'noop');
	await deps.ipBans.banIpTemp(getSameIpDecisionKey(parsed.normalized) ?? parsed.normalized, ttlSeconds);
	await deps.cache.publish(IP_BAN_REFRESH_CHANNEL, 'refresh');
	return outcomeOf(env, 'applied');
}
