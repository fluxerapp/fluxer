// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseNumericInput, resolveNearestNumericOption} from '@app/features/ui/components/form/ComboboxNumericInput';
import {describe, expect, it} from 'vitest';

const RELEASE_DELAY_OPTIONS = [20, 100, 250, 500, 1000, 1500, 2000].map((value) => ({
	value,
	label: new Intl.NumberFormat('en-US', {style: 'unit', unit: 'millisecond', unitDisplay: 'narrow'}).format(value),
}));

describe('parseNumericInput', () => {
	it('reads grouped thousands in common locale formats', () => {
		expect(parseNumericInput('1,000ms')).toBe(1000);
		expect(parseNumericInput('1.500 ms')).toBe(1500);
		expect(parseNumericInput('2 000 ms')).toBe(2000);
		expect(parseNumericInput("1'000")).toBe(1000);
	});

	it('keeps decimals', () => {
		expect(parseNumericInput('1.5 s')).toBe(1.5);
		expect(parseNumericInput('1,5 s')).toBe(1.5);
		expect(parseNumericInput('250')).toBe(250);
	});

	it('returns undefined without digits', () => {
		expect(parseNumericInput('ms')).toBeUndefined();
	});
});

describe('resolveNearestNumericOption', () => {
	it('keeps release delays above 500 ms when the selected label is read back', () => {
		for (const option of RELEASE_DELAY_OPTIONS) {
			expect(resolveNearestNumericOption(option.label, RELEASE_DELAY_OPTIONS)).toBe(option.value);
		}
	});
});
