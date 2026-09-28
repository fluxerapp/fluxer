// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AltchaCaptchaConfig,
	AltchaCaptchaConfigSchema,
	AltchaCaptchaConfigUpdateRequest,
	altchaCaptchaAppliesTo,
	DEFAULT_ALTCHA_CAPTCHA_CONFIG,
	resolveAltchaCaptchaAssignment,
} from '@fluxer/schema/src/domains/admin/AltchaCaptchaSchemas';
import {type ExperimentTargeting, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const NO_TARGETING: ExperimentTargeting = {memberGuildIds: new Set(), premium: false};

const TARGETED_USER_ID = '1000000000000000001';

function createConfig(overrides: Partial<AltchaCaptchaConfig> = {}): AltchaCaptchaConfig {
	return {...DEFAULT_ALTCHA_CAPTCHA_CONFIG, included_user_ids: [], excluded_user_ids: [], ...overrides};
}

function syntheticUserIds(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => (1400000000000000000n + BigInt(index)).toString());
}

describe('altcha captcha configuration', () => {
	test('defaults to disabled with no anonymous traffic', () => {
		expect(AltchaCaptchaConfigSchema.parse({})).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_salt: 'altcha-captcha-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			included_guild_ids: [],
			include_premium_users: false,
			anonymous_enabled: false,
			cost: 5000,
			max_counter: 10000,
		});
	});

	test('rejects difficulty outside the supported range', () => {
		expect(AltchaCaptchaConfigUpdateRequest.safeParse({cost: 999}).success).toBe(false);
		expect(AltchaCaptchaConfigUpdateRequest.safeParse({cost: 100001}).success).toBe(false);
		expect(AltchaCaptchaConfigUpdateRequest.safeParse({max_counter: 99}).success).toBe(false);
		expect(AltchaCaptchaConfigUpdateRequest.safeParse({max_counter: 1000001}).success).toBe(false);
		expect(AltchaCaptchaConfigUpdateRequest.safeParse({config_version: 3}).data).toEqual({});
	});
});

describe('resolveAltchaCaptchaAssignment', () => {
	test('serves nobody while disabled, even included users', () => {
		const config = createConfig({rollout_basis_points: 10000, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
	});

	test('applies exclusions before inclusions', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
	});

	test('serves included users at zero rollout', () => {
		const config = createConfig({enabled: true, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: true});
	});

	test('buckets the rollout by salt and user id', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 2500});
		for (const userId of syntheticUserIds(200)) {
			expect(resolveAltchaCaptchaAssignment(config, userId, NO_TARGETING).enabled).toBe(
				experimentBucket(userId, config.rollout_salt) < 2500,
			);
		}
	});
});

describe('altchaCaptchaAppliesTo', () => {
	test('serves anonymous requests only when anonymous_enabled is set', () => {
		expect(altchaCaptchaAppliesTo(createConfig({enabled: true}), null, NO_TARGETING)).toBe(false);
		expect(altchaCaptchaAppliesTo(createConfig({enabled: true, anonymous_enabled: true}), null, NO_TARGETING)).toBe(
			true,
		);
		expect(altchaCaptchaAppliesTo(createConfig({anonymous_enabled: true}), null, NO_TARGETING)).toBe(false);
	});

	test('keeps signed-in users on their own bucket regardless of the anonymous switch', () => {
		const config = createConfig({enabled: true, anonymous_enabled: true, excluded_user_ids: [TARGETED_USER_ID]});
		expect(altchaCaptchaAppliesTo(config, TARGETED_USER_ID, NO_TARGETING)).toBe(false);
	});
});

describe('resolveAltchaCaptchaAssignment guild targeting', () => {
	const INCLUDED_GUILD_ID = '3000000000000000001';
	const MEMBER_GUILDS: ExperimentTargeting = {
		memberGuildIds: new Set(['3000000000000000009', INCLUDED_GUILD_ID]),
		premium: false,
	};

	test('serves members of an included guild at zero rollout', () => {
		const config = createConfig({enabled: true, included_guild_ids: [INCLUDED_GUILD_ID]});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual({enabled: true});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
	});

	test('keeps user exclusions ahead of guild membership', () => {
		const config = createConfig({
			enabled: true,
			included_guild_ids: [INCLUDED_GUILD_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual({enabled: false});
	});

	test('serves no guild members while disabled', () => {
		const config = createConfig({included_guild_ids: [INCLUDED_GUILD_ID]});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual({enabled: false});
	});
});

describe('resolveAltchaCaptchaAssignment premium targeting', () => {
	const PREMIUM: ExperimentTargeting = {memberGuildIds: new Set(), premium: true};

	test('serves premium users only when the switch is on', () => {
		const on = createConfig({enabled: true, include_premium_users: true});
		expect(resolveAltchaCaptchaAssignment(on, TARGETED_USER_ID, PREMIUM)).toEqual({enabled: true});
		expect(resolveAltchaCaptchaAssignment(on, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
		expect(resolveAltchaCaptchaAssignment(createConfig({enabled: true}), TARGETED_USER_ID, PREMIUM)).toEqual({
			enabled: false,
		});
	});

	test('keeps user exclusions ahead of the premium switch', () => {
		const config = createConfig({
			enabled: true,
			include_premium_users: true,
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID, PREMIUM)).toEqual({enabled: false});
	});
});
