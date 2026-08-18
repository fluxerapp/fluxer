// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {
	DEFERRABLE_PHONE_FLAGS,
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	NEVER_DEFERRABLE_PHONE_FLAGS,
} from '@fluxer/constants/src/UserConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';
import {Logger} from '../Logger';
import {getInstanceConfigRepository} from '../middleware/ServiceSingletons';
import type {Guild} from '../models/Guild';
import type {User} from '../models/User';
import {resolveDeferredPhoneGateEnabled} from './DeferredPhoneGateCache';

export interface DeferredPhoneGateConfig {
	enabled: boolean;
	windowMs: number;
	memberThreshold: number;
}

export const DEFAULT_PHONE_GATE_MEMBER_THRESHOLD = 50;

const DISABLED_CONFIG: DeferredPhoneGateConfig = {
	enabled: false,
	windowMs: Number.POSITIVE_INFINITY,
	memberThreshold: Number.POSITIVE_INFINITY,
};

type DeferredPhoneGateConfigResult =
	| {status: 'ok'; config: DeferredPhoneGateConfig}
	| {status: 'disabled'; config: DeferredPhoneGateConfig}
	| {status: 'unreadable'; config: DeferredPhoneGateConfig};

export async function getDeferredPhoneGateConfig(): Promise<DeferredPhoneGateConfigResult> {
	try {
		const policy = await getInstanceConfigRepository().getInstancePolicyConfig();
		if (!resolveDeferredPhoneGateEnabled(policy)) {
			return {status: 'disabled', config: DISABLED_CONFIG};
		}
		return {
			status: 'ok',
			config: {
				enabled: true,
				windowMs: policy.deferred_phone_gate_window_hours * ms('1 hour'),
				memberThreshold: policy.deferred_phone_gate_member_threshold,
			},
		};
	} catch (error) {
		Logger.warn({error}, 'Failed to read deferred phone gate config');
		return {status: 'unreadable', config: DISABLED_CONFIG};
	}
}

export async function deferPhoneFlagsUntilCommunityJoin(flagBits: number): Promise<number> {
	if ((flagBits & DEFERRABLE_PHONE_FLAGS) === 0 || (flagBits & NEVER_DEFERRABLE_PHONE_FLAGS) !== 0) {
		return flagBits;
	}
	const {status} = await getDeferredPhoneGateConfig();
	if (status !== 'ok') {
		return flagBits;
	}
	return flagBits | DEFERRED_PHONE_ON_COMMUNITY_JOIN;
}

export function guildTriggersPhoneGate(guild: Guild, memberThreshold: number): boolean {
	return guild.features.has(GuildFeatures.DISCOVERABLE) || guild.memberCount > memberThreshold;
}

type DeferredPhoneGateOutcome =
	| {applies: false; reason: 'gate_disabled' | 'already_verified' | 'guild_below_threshold' | 'outside_window'}
	| {applies: true; flags: number};

export function evaluateDeferredPhoneGate(
	user: User,
	guild: Guild,
	config: DeferredPhoneGateConfig,
	now: number,
): DeferredPhoneGateOutcome {
	if (!config.enabled) {
		return {applies: false, reason: 'gate_disabled'};
	}
	if (user.hasVerifiedPhone) {
		return {applies: false, reason: 'already_verified'};
	}
	if (!guildTriggersPhoneGate(guild, config.memberThreshold)) {
		return {applies: false, reason: 'guild_below_threshold'};
	}
	if (now - snowflakeToDate(BigInt(user.id)).getTime() >= config.windowMs) {
		return {applies: false, reason: 'outside_window'};
	}
	return {applies: true, flags: (user.suspiciousActivityFlags ?? 0) & ~DEFERRED_PHONE_ON_COMMUNITY_JOIN};
}
