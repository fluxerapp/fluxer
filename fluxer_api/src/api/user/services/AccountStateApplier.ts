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
import {isAccountLimitExempt} from '@app/api/user/AccountLimit';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {
	clearNewConversationLimit,
	isNewConversationLimitExempt,
	setNewConversationLimit,
} from '@app/api/user/NewConversationLimit';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {getSameIpDecisionKey, isPublicIpAddress, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

export type ActionOf<T extends ActionEnvelope['type']> = Extract<ActionEnvelope, {type: T}>;

export interface AccountUpdateDispatch {
	userUpdated(user: User): Promise<void>;
}

export interface AccountStateDeps {
	users: Pick<IUserRepository, 'findUnique' | 'compareAndSetFlags'>;
	dispatch: AccountUpdateDispatch;
	ipBans: Pick<AdminRepository, 'isIpBanned' | 'banIpTemp'>;
	cache: Pick<ICacheService, 'publish' | 'get' | 'set' | 'delete'>;
	now?: () => number;
}

const FLAGS_WRITE_ATTEMPTS = 3;
const MIN_TEMP_BAN_SECONDS = 60;

function gatewayDispatch(gateway: Pick<IGatewayService, 'dispatchPresence'>): AccountUpdateDispatch {
	return {
		async userUpdated(user) {
			await gateway.dispatchPresence({userId: user.id, event: 'USER_UPDATE', data: mapUserToPrivateResponse(user)});
		},
	};
}

export function accountStateDepsFromContext(ctx: ApiContext, ipBans: AccountStateDeps['ipBans']): AccountStateDeps {
	return {
		users: ctx.services.users,
		dispatch: gatewayDispatch(ctx.services.gateway),
		ipBans,
		cache: ctx.services.cache,
	};
}

export function observedOf(user: User): Observed {
	return {
		flags: user.flags.toString(),
		deleted: (user.flags & UserFlags.DELETED) !== 0n,
	};
}

export function outcomeOf(
	env: Pick<ActionEnvelope, 'id'> & {type: string; user_id?: string},
	status: OutcomeStatus,
	user: User | null = null,
	detail: string | null = null,
): ActionOutcome {
	const outcome: ActionOutcome = {
		action_id: env.id,
		action_type: env.type,
		status,
		detail,
		observed: user ? observedOf(user) : null,
	};
	if (env.user_id) outcome.user_id = env.user_id;
	return outcome;
}

function isIneligible(user: User): boolean {
	return user.isBot || (user.flags & UserFlags.DELETED) !== 0n;
}

export async function applySetAccountLimit(
	deps: AccountStateDeps,
	env: ActionOf<'set_account_limit'>,
): Promise<ActionOutcome> {
	return withAccountChangeSource('action', async () => {
		const userId = createUserID(BigInt(env.user_id));
		let user = await deps.users.findUnique(userId);
		for (let attempt = 0; attempt < FLAGS_WRITE_ATTEMPTS; attempt++) {
			if (!user) return outcomeOf(env, 'ineligible');
			if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
			if (env.on && isAccountLimitExempt(user)) return outcomeOf(env, 'exempt', user);
			const limited = (user.flags & UserFlags.ACCOUNT_LIMITED) !== 0n;
			if (limited === env.on) return outcomeOf(env, 'noop', user);
			const target = env.on ? user.flags | UserFlags.ACCOUNT_LIMITED : user.flags & ~UserFlags.ACCOUNT_LIMITED;
			const updated = await deps.users.compareAndSetFlags(user, target);
			if (updated) {
				await deps.dispatch.userUpdated(updated);
				return outcomeOf(env, 'applied', updated);
			}
			user = await deps.users.findUnique(userId);
		}
		throw new Error('User flags kept changing during apply');
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

export async function applyLimitNewConversations(
	deps: AccountStateDeps,
	env: ActionOf<'limit_new_conversations'>,
): Promise<ActionOutcome> {
	const user = await deps.users.findUnique(createUserID(BigInt(env.user_id)));
	if (!user) return outcomeOf(env, 'ineligible');
	if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
	const store = {cache: deps.cache, now: deps.now};
	if (!env.on) {
		const lifted = await clearNewConversationLimit(user.id, store);
		return outcomeOf(env, lifted ? 'applied' : 'noop', user);
	}
	if (isNewConversationLimitExempt(user)) return outcomeOf(env, 'exempt', user);
	const applied = await setNewConversationLimit(user.id, env.until_ms, store);
	return outcomeOf(env, applied ? 'applied' : 'noop', user);
}
