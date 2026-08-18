// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {DEFERRED_PHONE_ON_COMMUNITY_JOIN, SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';
import {describe, expect, it} from 'vitest';
import type {Guild} from '../models/Guild';
import type {User} from '../models/User';
import {type DeferredPhoneGateConfig, evaluateDeferredPhoneGate, guildTriggersPhoneGate} from './DeferredPhoneGate';
import {resolveDeferredPhoneGateEnabled} from './DeferredPhoneGateCache';

const CONFIG: DeferredPhoneGateConfig = {
	enabled: true,
	windowMs: 6 * ms('1 hour'),
	memberThreshold: 50,
};

const USER_SNOWFLAKE = 1485046297690587136n;
const REGISTERED_AT = snowflakeToDate(USER_SNOWFLAKE).getTime();

function createUser(overrides: Partial<Pick<User, 'hasVerifiedPhone' | 'suspiciousActivityFlags'>> = {}): User {
	return {
		id: USER_SNOWFLAKE,
		hasVerifiedPhone: false,
		suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		...overrides,
	} as unknown as User;
}

function createGuild(overrides: {discoverable?: boolean; memberCount?: number} = {}): Guild {
	return {
		id: 1n,
		features: new Set(overrides.discoverable ? [GuildFeatures.DISCOVERABLE] : []),
		memberCount: overrides.memberCount ?? 10,
	} as unknown as Guild;
}

describe('evaluateDeferredPhoneGate', () => {
	it('applies to a discoverable guild inside the window, promoting the real phone flags', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser(),
			createGuild({discoverable: true, memberCount: 3}),
			CONFIG,
			REGISTERED_AT + ms('1 hour'),
		);
		expect(outcome.applies).toBe(true);
		if (!outcome.applies) return;
		expect(outcome.flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
		expect(outcome.flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
	});

	it('applies to a large non-discoverable guild inside the window', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser(),
			createGuild({memberCount: 51}),
			CONFIG,
			REGISTERED_AT + ms('1 hour'),
		);
		expect(outcome.applies).toBe(true);
	});

	it('does not apply to a small non-discoverable guild', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser(),
			createGuild({memberCount: 50}),
			CONFIG,
			REGISTERED_AT + ms('1 hour'),
		);
		expect(outcome).toEqual({applies: false, reason: 'guild_below_threshold'});
	});

	it('applies on the last millisecond inside the window', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser(),
			createGuild({discoverable: true}),
			CONFIG,
			REGISTERED_AT + CONFIG.windowMs - 1,
		);
		expect(outcome.applies).toBe(true);
	});

	it('does not gate once the window has elapsed, and mutates nothing', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser(),
			createGuild({discoverable: true}),
			CONFIG,
			REGISTERED_AT + CONFIG.windowMs,
		);
		expect(outcome).toEqual({applies: false, reason: 'outside_window'});
	});
	it('is inert while the gate is disabled, even on a qualifying guild', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser(),
			createGuild({discoverable: true}),
			{...CONFIG, enabled: false},
			REGISTERED_AT + ms('1 hour'),
		);
		expect(outcome).toEqual({applies: false, reason: 'gate_disabled'});
	});

	it('does not apply to a user who already has a verified phone', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser({hasVerifiedPhone: true}),
			createGuild({discoverable: true}),
			CONFIG,
			REGISTERED_AT + ms('1 hour'),
		);
		expect(outcome).toEqual({applies: false, reason: 'already_verified'});
	});

	it('preserves non-phone requirements when promoting', () => {
		const outcome = evaluateDeferredPhoneGate(
			createUser({
				suspiciousActivityFlags:
					SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL |
					SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION |
					DEFERRED_PHONE_ON_COMMUNITY_JOIN,
			}),
			createGuild({discoverable: true}),
			CONFIG,
			REGISTERED_AT + ms('1 hour'),
		);
		expect(outcome.applies).toBe(true);
		if (!outcome.applies) return;
		expect(outcome.flags).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
		);
	});
});

describe('resolveDeferredPhoneGateEnabled', () => {
	it('is on only when the tunable is set and the instance is not single-community', () => {
		expect(resolveDeferredPhoneGateEnabled({deferred_phone_gate_enabled: true, single_community_enabled: false})).toBe(
			true,
		);
		expect(resolveDeferredPhoneGateEnabled({deferred_phone_gate_enabled: true, single_community_enabled: true})).toBe(
			false,
		);
		expect(resolveDeferredPhoneGateEnabled({deferred_phone_gate_enabled: false, single_community_enabled: false})).toBe(
			false,
		);
	});
});

describe('guildTriggersPhoneGate', () => {
	it('qualifies a discoverable guild regardless of size', () => {
		expect(guildTriggersPhoneGate(createGuild({discoverable: true, memberCount: 1}), 50)).toBe(true);
	});
	it('qualifies a guild strictly above the member threshold', () => {
		expect(guildTriggersPhoneGate(createGuild({memberCount: 51}), 50)).toBe(true);
		expect(guildTriggersPhoneGate(createGuild({memberCount: 50}), 50)).toBe(false);
	});
});
