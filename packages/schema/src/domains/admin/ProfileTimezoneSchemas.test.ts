// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_PROFILE_TIMEZONE_CONFIG,
	type ProfileTimezoneConfig,
	ProfileTimezoneConfigSchema,
	ProfileTimezoneConfigUpdateRequest,
	resolveProfileTimezoneAssignment,
} from '@fluxer/schema/src/domains/admin/ProfileTimezoneSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const TARGETED_USER_ID = '1000000000000000001';

function createConfig(overrides: Partial<ProfileTimezoneConfig> = {}): ProfileTimezoneConfig {
	return {...DEFAULT_PROFILE_TIMEZONE_CONFIG, included_user_ids: [], excluded_user_ids: [], ...overrides};
}

function syntheticUserIds(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => (1400000000000000000n + BigInt(index)).toString());
}

describe('profile timezone configuration', () => {
	test('defaults to disabled', () => {
		expect(ProfileTimezoneConfigSchema.parse({})).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_salt: 'profile-timezone-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
	});

	test('strips config_version from admin updates', () => {
		expect(ProfileTimezoneConfigUpdateRequest.safeParse({config_version: 3}).data).toEqual({});
		expect(ProfileTimezoneConfigUpdateRequest.safeParse({rollout_basis_points: 10001}).success).toBe(false);
	});
});

describe('resolveProfileTimezoneAssignment', () => {
	test('serves nobody while disabled, even included users', () => {
		const config = createConfig({rollout_basis_points: 10000, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveProfileTimezoneAssignment(config, TARGETED_USER_ID)).toEqual({enabled: false});
	});

	test('applies exclusions before inclusions', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveProfileTimezoneAssignment(config, TARGETED_USER_ID)).toEqual({enabled: false});
	});

	test('serves included users at zero rollout', () => {
		const config = createConfig({enabled: true, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveProfileTimezoneAssignment(config, TARGETED_USER_ID)).toEqual({enabled: true});
	});

	test('buckets the rollout by salt and user id', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 2500});
		for (const userId of syntheticUserIds(200)) {
			expect(resolveProfileTimezoneAssignment(config, userId).enabled).toBe(
				experimentBucket(userId, config.rollout_salt) < 2500,
			);
		}
	});
});
