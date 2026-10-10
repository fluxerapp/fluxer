// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ExperimentRolloutCountryCodesSchema,
	type ExperimentTargeting,
	experimentRolloutCountryIncludes,
} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

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
