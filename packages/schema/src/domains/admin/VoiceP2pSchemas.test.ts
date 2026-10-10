// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_VOICE_P2P_CONFIG,
	resolveVoiceP2pAssignment,
	type VoiceP2pConfig,
	VoiceP2pConfigSchema,
	VoiceP2pConfigUpdateRequest,
} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import {type ExperimentTargeting, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const NO_TARGETING: ExperimentTargeting = {memberGuildIds: new Set(), premium: false, countryCode: null};

const TARGETED_USER_ID = '1000000000000000001';

const SELECTED = {enabled: true, max_participants: 2};
const UNSELECTED = {enabled: false, max_participants: 2};

function createConfig(overrides: Partial<VoiceP2pConfig> = {}): VoiceP2pConfig {
	return {...DEFAULT_VOICE_P2P_CONFIG, included_user_ids: [], excluded_user_ids: [], ...overrides};
}

function syntheticUserIds(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => (1400000000000000000n + BigInt(index)).toString());
}

describe('voice p2p configuration', () => {
	test('defaults to disabled', () => {
		expect(VoiceP2pConfigSchema.parse({})).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_country_codes: [],
			rollout_salt: 'voice-p2p-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			included_guild_ids: [],
			include_premium_users: false,
			max_participants: 2,
		});
	});

	test('limits max_participants to 2 through 4', () => {
		expect(VoiceP2pConfigUpdateRequest.safeParse({max_participants: 4}).data).toEqual({max_participants: 4});
		expect(VoiceP2pConfigUpdateRequest.safeParse({max_participants: 1}).success).toBe(false);
		expect(VoiceP2pConfigUpdateRequest.safeParse({max_participants: 5}).success).toBe(false);
		expect(VoiceP2pConfigUpdateRequest.safeParse({max_participants: 2.5}).success).toBe(false);
	});

	test('strips config_version from admin updates', () => {
		expect(VoiceP2pConfigUpdateRequest.safeParse({config_version: 3}).data).toEqual({});
		expect(VoiceP2pConfigUpdateRequest.safeParse({rollout_basis_points: 10001}).success).toBe(false);
	});
});

describe('resolveVoiceP2pAssignment', () => {
	test('reports the configured cap whether or not the caller is selected', () => {
		const config = createConfig({enabled: true, max_participants: 4, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual({
			enabled: true,
			max_participants: 4,
		});
		expect(resolveVoiceP2pAssignment(config, '1000000000000000007', NO_TARGETING)).toEqual({
			enabled: false,
			max_participants: 4,
		});
		expect(resolveVoiceP2pAssignment({...config, enabled: false}, TARGETED_USER_ID, NO_TARGETING)).toEqual({
			enabled: false,
			max_participants: 4,
		});
	});

	test('serves nobody while disabled, even included users', () => {
		const config = createConfig({rollout_basis_points: 10000, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual(UNSELECTED);
	});

	test('applies exclusions before inclusions', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual(UNSELECTED);
	});

	test('serves included users at zero rollout', () => {
		const config = createConfig({enabled: true, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual(SELECTED);
	});

	test('buckets the rollout by salt and user id', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 2500});
		for (const userId of syntheticUserIds(200)) {
			expect(resolveVoiceP2pAssignment(config, userId, NO_TARGETING).enabled).toBe(
				experimentBucket(userId, config.rollout_salt) < 2500,
			);
		}
	});
});

describe('resolveVoiceP2pAssignment guild targeting', () => {
	const INCLUDED_GUILD_ID = '3000000000000000001';
	const MEMBER_GUILDS: ExperimentTargeting = {
		memberGuildIds: new Set(['3000000000000000009', INCLUDED_GUILD_ID]),
		premium: false,
		countryCode: null,
	};

	test('serves members of an included guild at zero rollout', () => {
		const config = createConfig({enabled: true, included_guild_ids: [INCLUDED_GUILD_ID]});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual(SELECTED);
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual(UNSELECTED);
	});

	test('keeps user exclusions ahead of guild membership', () => {
		const config = createConfig({
			enabled: true,
			included_guild_ids: [INCLUDED_GUILD_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual(UNSELECTED);
	});

	test('serves no guild members while disabled', () => {
		const config = createConfig({included_guild_ids: [INCLUDED_GUILD_ID]});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, MEMBER_GUILDS)).toEqual(UNSELECTED);
	});
});

describe('resolveVoiceP2pAssignment premium targeting', () => {
	const PREMIUM: ExperimentTargeting = {memberGuildIds: new Set(), premium: true, countryCode: null};

	test('serves premium users only when the switch is on', () => {
		const on = createConfig({enabled: true, include_premium_users: true});
		expect(resolveVoiceP2pAssignment(on, TARGETED_USER_ID, PREMIUM)).toEqual(SELECTED);
		expect(resolveVoiceP2pAssignment(on, TARGETED_USER_ID, NO_TARGETING)).toEqual(UNSELECTED);
		expect(resolveVoiceP2pAssignment(createConfig({enabled: true}), TARGETED_USER_ID, PREMIUM)).toEqual(UNSELECTED);
	});

	test('keeps user exclusions ahead of the premium switch', () => {
		const config = createConfig({
			enabled: true,
			include_premium_users: true,
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, PREMIUM)).toEqual(UNSELECTED);
	});
});

describe('resolveVoiceP2pAssignment country targeting', () => {
	const IN_SWEDEN: ExperimentTargeting = {...NO_TARGETING, countryCode: 'SE'};
	const IN_BRAZIL: ExperimentTargeting = {...NO_TARGETING, countryCode: 'BR'};

	test('limits the percentage rollout to the listed countries', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 10000, rollout_country_codes: ['SE', 'NO']});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, IN_SWEDEN)).toEqual(SELECTED);
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, IN_BRAZIL)).toEqual(UNSELECTED);
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual(UNSELECTED);
	});

	test('ignores the country while the list is empty', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 10000});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, IN_BRAZIL)).toEqual(SELECTED);
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, NO_TARGETING)).toEqual(SELECTED);
	});

	test('still buckets users inside a listed country', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 2500, rollout_country_codes: ['SE']});
		for (const userId of syntheticUserIds(200)) {
			expect(resolveVoiceP2pAssignment(config, userId, IN_SWEDEN).enabled).toBe(
				experimentBucket(userId, config.rollout_salt) < 2500,
			);
		}
	});

	test('keeps included users, included guilds and premium users outside the listed countries', () => {
		const guildId = '3000000000000000001';
		const config = createConfig({
			enabled: true,
			rollout_country_codes: ['SE'],
			included_user_ids: [TARGETED_USER_ID],
			included_guild_ids: [guildId],
			include_premium_users: true,
		});
		const otherUserId = '1000000000000000007';
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, IN_BRAZIL)).toEqual(SELECTED);
		expect(resolveVoiceP2pAssignment(config, otherUserId, {...IN_BRAZIL, memberGuildIds: new Set([guildId])})).toEqual(
			SELECTED,
		);
		expect(resolveVoiceP2pAssignment(config, otherUserId, {...IN_BRAZIL, premium: true})).toEqual(SELECTED);
		expect(resolveVoiceP2pAssignment(config, otherUserId, IN_BRAZIL)).toEqual(UNSELECTED);
	});

	test('keeps exclusions ahead of a listed country', () => {
		const config = createConfig({
			enabled: true,
			rollout_basis_points: 10000,
			rollout_country_codes: ['SE'],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveVoiceP2pAssignment(config, TARGETED_USER_ID, IN_SWEDEN)).toEqual(UNSELECTED);
	});
});
