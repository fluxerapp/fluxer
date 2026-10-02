// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_PLUTONIUM_PAGE_CONFIG,
	type PlutoniumPageConfig,
	PlutoniumPageConfigSchema,
	PlutoniumPageConfigUpdateRequest,
	resolvePlutoniumPageAssignment,
} from '@fluxer/schema/src/domains/admin/PlutoniumPageSchemas';
import {type ExperimentTargeting, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const NO_TARGETING: ExperimentTargeting = {memberGuildIds: new Set(), premium: false};

const TARGETED_USER_ID = '1000000000000000001';

function createConfig(overrides: Partial<PlutoniumPageConfig> = {}): PlutoniumPageConfig {
	return {...DEFAULT_PLUTONIUM_PAGE_CONFIG, included_user_ids: [], excluded_user_ids: [], ...overrides};
}

function syntheticUserIds(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => (1400000000000000000n + BigInt(index)).toString());
}

describe('plutonium page configuration', () => {
	test('defaults to disabled', () => {
		expect(PlutoniumPageConfigSchema.parse({})).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_salt: 'plutonium-page-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			included_guild_ids: [],
			include_premium_users: false,
		});
	});

	test('strips config_version from admin updates', () => {
		expect(PlutoniumPageConfigUpdateRequest.safeParse({config_version: 3}).data).toEqual({});
		expect(PlutoniumPageConfigUpdateRequest.safeParse({rollout_basis_points: 10001}).success).toBe(false);
	});
});

describe('resolvePlutoniumPageAssignment', () => {
	test('serves nobody while disabled, even included users', () => {
		const config = createConfig({rollout_basis_points: 10000, included_user_ids: [TARGETED_USER_ID]});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
	});

	test('applies exclusions before inclusions', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
	});

	test('serves included users at zero rollout', () => {
		const config = createConfig({enabled: true, included_user_ids: [TARGETED_USER_ID]});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: true});
	});

	test('buckets the rollout by salt and user id', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 2500});
		for (const userId of syntheticUserIds(200)) {
			expect(resolvePlutoniumPageAssignment(config, userId, NO_TARGETING).enabled).toBe(
				experimentBucket(userId, config.rollout_salt) < 2500,
			);
		}
	});
});

describe('resolvePlutoniumPageAssignment guild targeting', () => {
	const INCLUDED_GUILD_ID = '3000000000000000001';
	const MEMBER_GUILDS: ExperimentTargeting = {
		memberGuildIds: new Set(['3000000000000000009', INCLUDED_GUILD_ID]),
		premium: false,
	};

	test('serves members of an included guild at zero rollout', () => {
		const config = createConfig({enabled: true, included_guild_ids: [INCLUDED_GUILD_ID]});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual({enabled: true});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
	});

	test('keeps user exclusions ahead of guild membership', () => {
		const config = createConfig({
			enabled: true,
			included_guild_ids: [INCLUDED_GUILD_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual({enabled: false});
	});

	test('serves no guild members while disabled', () => {
		const config = createConfig({included_guild_ids: [INCLUDED_GUILD_ID]});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual({enabled: false});
	});
});

describe('resolvePlutoniumPageAssignment premium targeting', () => {
	const PREMIUM: ExperimentTargeting = {memberGuildIds: new Set(), premium: true};

	test('serves premium users only when the switch is on', () => {
		const on = createConfig({enabled: true, include_premium_users: true});
		expect(resolvePlutoniumPageAssignment(on, TARGETED_USER_ID, PREMIUM)).toEqual({enabled: true});
		expect(resolvePlutoniumPageAssignment(on, TARGETED_USER_ID, NO_TARGETING)).toEqual({enabled: false});
		expect(resolvePlutoniumPageAssignment(createConfig({enabled: true}), TARGETED_USER_ID, PREMIUM)).toEqual({
			enabled: false,
		});
	});

	test('keeps user exclusions ahead of the premium switch', () => {
		const config = createConfig({
			enabled: true,
			include_premium_users: true,
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolvePlutoniumPageAssignment(config, TARGETED_USER_ID, PREMIUM)).toEqual({enabled: false});
	});
});
