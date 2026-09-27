// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AltchaCaptchaConfig,
	AltchaCaptchaConfigSchema,
	AltchaCaptchaConfigUpdateRequest,
	altchaCaptchaAppliesTo,
	DEFAULT_ALTCHA_CAPTCHA_CONFIG,
	resolveAltchaCaptchaAssignment,
} from '@fluxer/schema/src/domains/admin/AltchaCaptchaSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

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
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID)).toEqual({enabled: false});
	});

	test('applies exclusions before inclusions', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID)).toEqual({enabled: false});
	});

	test('serves included users at zero rollout', () => {
		const config = createConfig({enabled: true, included_user_ids: [TARGETED_USER_ID]});
		expect(resolveAltchaCaptchaAssignment(config, TARGETED_USER_ID)).toEqual({enabled: true});
	});

	test('buckets the rollout by salt and user id', () => {
		const config = createConfig({enabled: true, rollout_basis_points: 2500});
		for (const userId of syntheticUserIds(200)) {
			expect(resolveAltchaCaptchaAssignment(config, userId).enabled).toBe(
				experimentBucket(userId, config.rollout_salt) < 2500,
			);
		}
	});
});

describe('altchaCaptchaAppliesTo', () => {
	test('serves anonymous requests only when anonymous_enabled is set', () => {
		expect(altchaCaptchaAppliesTo(createConfig({enabled: true}), null)).toBe(false);
		expect(altchaCaptchaAppliesTo(createConfig({enabled: true, anonymous_enabled: true}), null)).toBe(true);
		expect(altchaCaptchaAppliesTo(createConfig({anonymous_enabled: true}), null)).toBe(false);
	});

	test('keeps signed-in users on their own bucket regardless of the anonymous switch', () => {
		const config = createConfig({enabled: true, anonymous_enabled: true, excluded_user_ids: [TARGETED_USER_ID]});
		expect(altchaCaptchaAppliesTo(config, TARGETED_USER_ID)).toBe(false);
	});
});
