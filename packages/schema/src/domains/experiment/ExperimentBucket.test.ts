// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ExperimentRolloutCountryCodesSchema,
	type ExperimentTargeting,
	experimentBucket,
	experimentRolloutCountryIncludes,
} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const TARGETED_USER_ID = '1000000000000000001';
const OTHER_USER_ID = '1000000000000000002';

function syntheticUserIds(count: number): Array<string> {
	const ids: Array<string> = [];
	for (let index = 0; index < count; index++) {
		ids.push((1400000000000000000n + BigInt(index)).toString());
	}
	return ids;
}

describe('experimentBucket', () => {
	test('stays inside the basis point range for every synthetic id', () => {
		for (const userId of syntheticUserIds(500)) {
			const bucket = experimentBucket(userId, 'voice-ns-v1');
			expect(Number.isInteger(bucket)).toBe(true);
			expect(bucket).toBeGreaterThanOrEqual(0);
			expect(bucket).toBeLessThan(10000);
		}
	});

	test.each([
		{userId: TARGETED_USER_ID, salt: 'voice-ns-v1', expected: 8241},
		{userId: TARGETED_USER_ID, salt: 'voice-ns-v2', expected: 8500},
		{userId: OTHER_USER_ID, salt: 'voice-ns-v1', expected: 5384},
		{userId: OTHER_USER_ID, salt: 'voice-ns-v2', expected: 1357},
	])('preserves the assignment bucket for $userId with salt $salt', ({userId, salt, expected}) => {
		expect(experimentBucket(userId, salt)).toBe(expected);
	});

	test('changes with the salt for at least most user ids', () => {
		const userIds = syntheticUserIds(200);
		const changed = userIds.filter(
			(userId) => experimentBucket(userId, 'voice-ns-v1') !== experimentBucket(userId, 'voice-ns-v2'),
		);
		expect(changed.length).toBeGreaterThan(userIds.length - 5);
	});
});

describe('ExperimentRolloutCountryCodesSchema', () => {
	test('accepts unique uppercase alpha-2 codes', () => {
		expect(ExperimentRolloutCountryCodesSchema.parse(['SE', 'BR'])).toEqual(['SE', 'BR']);
		expect(ExperimentRolloutCountryCodesSchema.parse([])).toEqual([]);
	});

	test.each([
		{name: 'lowercase codes', value: ['se']},
		{name: 'alpha-3 codes', value: ['SWE']},
		{name: 'duplicates', value: ['SE', 'SE']},
		{
			name: 'more than 250 codes',
			value: Array.from(
				{length: 251},
				(_, index) => `${String.fromCharCode(65 + Math.floor(index / 26))}${String.fromCharCode(65 + (index % 26))}`,
			),
		},
	])('rejects $name', ({value}) => {
		expect(ExperimentRolloutCountryCodesSchema.safeParse(value).success).toBe(false);
	});
});

describe('experimentRolloutCountryIncludes', () => {
	const targeting = (countryCode: string | null): ExperimentTargeting => ({
		memberGuildIds: new Set(),
		premium: false,
		countryCode,
	});

	test('matches every country while the list is empty', () => {
		expect(experimentRolloutCountryIncludes({rollout_country_codes: []}, targeting('SE'))).toBe(true);
		expect(experimentRolloutCountryIncludes({rollout_country_codes: []}, targeting(null))).toBe(true);
	});

	test('matches only listed countries and never an unknown country', () => {
		const rules = {rollout_country_codes: ['SE', 'NO']};
		expect(experimentRolloutCountryIncludes(rules, targeting('NO'))).toBe(true);
		expect(experimentRolloutCountryIncludes(rules, targeting('BR'))).toBe(false);
		expect(experimentRolloutCountryIncludes(rules, targeting(null))).toBe(false);
	});
});
