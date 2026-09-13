// SPDX-License-Identifier: AGPL-3.0-or-later

import {addMonthsClamp} from '@app/api/stripe/StripeUtils';
import {describe, expect, it} from 'vitest';

describe('addMonthsClamp', () => {
	it('adds months normally when no overflow occurs', () => {
		const result = addMonthsClamp(new Date('2024-01-15'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(15);
	});
	it('clamps Jan 31 + 1 month to Feb 29 (leap year)', () => {
		const result = addMonthsClamp(new Date('2024-01-31'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(29);
	});
	it('clamps Jan 31 + 1 month to Feb 28 (non-leap year)', () => {
		const result = addMonthsClamp(new Date('2023-01-31'), 1);
		expect(result.getFullYear()).toBe(2023);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(28);
	});
	it('clamps Jan 30 + 1 month to Feb 28 (non-leap year)', () => {
		const result = addMonthsClamp(new Date('2023-01-30'), 1);
		expect(result.getFullYear()).toBe(2023);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(28);
	});
	it('clamps Jan 29 + 1 month to Feb 28 (non-leap year)', () => {
		const result = addMonthsClamp(new Date('2023-01-29'), 1);
		expect(result.getFullYear()).toBe(2023);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(28);
	});
	it('clamps Mar 31 + 1 month to Apr 30', () => {
		const result = addMonthsClamp(new Date('2024-03-31'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(3);
		expect(result.getDate()).toBe(30);
	});
	it('clamps May 31 + 1 month to Jun 30', () => {
		const result = addMonthsClamp(new Date('2024-05-31'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(5);
		expect(result.getDate()).toBe(30);
	});
	it('clamps Jul 31 + 1 month to Aug 31 (no clamping needed)', () => {
		const result = addMonthsClamp(new Date('2024-07-31'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(7);
		expect(result.getDate()).toBe(31);
	});
	it('clamps Aug 31 + 1 month to Sep 30', () => {
		const result = addMonthsClamp(new Date('2024-08-31'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(8);
		expect(result.getDate()).toBe(30);
	});
	it('clamps Oct 31 + 1 month to Nov 30', () => {
		const result = addMonthsClamp(new Date('2024-10-31'), 1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(10);
		expect(result.getDate()).toBe(30);
	});
	it('clamps Dec 31 + 1 month to Jan 31 of next year (no clamping needed)', () => {
		const result = addMonthsClamp(new Date('2024-12-31'), 1);
		expect(result.getFullYear()).toBe(2025);
		expect(result.getMonth()).toBe(0);
		expect(result.getDate()).toBe(31);
	});
	it('handles year boundary: Feb 29 (leap) + 12 months = Feb 28 (non-leap)', () => {
		const result = addMonthsClamp(new Date('2024-02-29'), 12);
		expect(result.getFullYear()).toBe(2025);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(28);
	});
	it('handles multiple months: Jan 31 + 2 months = Mar 31', () => {
		const result = addMonthsClamp(new Date('2024-01-31'), 2);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(2);
		expect(result.getDate()).toBe(31);
	});
	it('handles multiple months with clamping: Jan 31 + 3 months = Apr 30', () => {
		const result = addMonthsClamp(new Date('2024-01-31'), 3);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(3);
		expect(result.getDate()).toBe(30);
	});
	it('handles negative months: Mar 31 - 1 month = Feb 29 (leap year)', () => {
		const result = addMonthsClamp(new Date('2024-03-31'), -1);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(29);
	});
	it('handles negative months: Mar 31 - 1 month = Feb 28 (non-leap year)', () => {
		const result = addMonthsClamp(new Date('2023-03-31'), -1);
		expect(result.getFullYear()).toBe(2023);
		expect(result.getMonth()).toBe(1);
		expect(result.getDate()).toBe(28);
	});
	it('handles year boundary with negative months: Jan 31 - 2 months = Nov 30', () => {
		const result = addMonthsClamp(new Date('2024-01-31'), -2);
		expect(result.getFullYear()).toBe(2023);
		expect(result.getMonth()).toBe(10);
		expect(result.getDate()).toBe(30);
	});
	it('handles zero months (returns same date)', () => {
		const result = addMonthsClamp(new Date('2024-01-31'), 0);
		expect(result.getFullYear()).toBe(2024);
		expect(result.getMonth()).toBe(0);
		expect(result.getDate()).toBe(31);
	});
	it('does not mutate the input date', () => {
		const input = new Date('2024-01-31');
		const inputTime = input.getTime();
		addMonthsClamp(input, 1);
		expect(input.getTime()).toBe(inputTime);
	});
});
