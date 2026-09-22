// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {GiftCodeDurationType, GiftCodeRow} from '@app/api/database/types/PaymentTypes';
import {
	addGiftCodeDuration,
	GiftCode,
	mapGiftCodeDurationToMonths,
	mapGiftDurationMonthsToFields,
} from '@app/api/models/GiftCode';
import {describe, expect, test} from 'vitest';

const TEST_USER_ID = createUserID(1n);
const BASE_DATE = new Date('2026-03-01T00:00:00.000Z');
const MILLISECONDS_PER_DAY = 24 * 60 * 60 * 1000;

function createGiftCodeRow(overrides: Partial<GiftCodeRow> = {}): GiftCodeRow {
	return {
		code: 'TESTCODE',
		duration_months: null,
		duration_type: 'months',
		duration_quantity: 1,
		created_at: BASE_DATE,
		created_by_user_id: TEST_USER_ID,
		redeemed_at: null,
		redeemed_by_user_id: null,
		stripe_payment_intent_id: null,
		visionary_sequence_number: null,
		checkout_session_id: null,
		version: 1,
		...overrides,
	};
}

describe('GiftCode', () => {
	describe('constructor normalisation', () => {
		test('prefers duration_type and duration_quantity when present', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: 'weeks',
					duration_quantity: 2,
					duration_months: 6,
				}),
			);
			expect(giftCode.durationType).toBe('weeks');
			expect(giftCode.durationQuantity).toBe(2);
		});
		test('falls back to duration_months when duration_type and duration_quantity are absent', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: undefined,
					duration_quantity: undefined,
					duration_months: 3,
				}),
			);
			expect(giftCode.durationType).toBe('months');
			expect(giftCode.durationQuantity).toBe(3);
		});
		test('converts 12 duration_months to 1 year when falling back', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: undefined,
					duration_quantity: undefined,
					duration_months: 12,
				}),
			);
			expect(giftCode.durationType).toBe('years');
			expect(giftCode.durationQuantity).toBe(1);
		});
		test('converts 24 duration_months to 2 years when falling back', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: undefined,
					duration_quantity: undefined,
					duration_months: 24,
				}),
			);
			expect(giftCode.durationType).toBe('years');
			expect(giftCode.durationQuantity).toBe(2);
		});
		test('throws when only duration_type is set without duration_quantity', () => {
			expect(
				() =>
					new GiftCode(
						createGiftCodeRow({
							duration_type: 'months',
							duration_quantity: undefined,
							duration_months: null,
						}),
					),
			).toThrow('Gift code duration_type and duration_quantity must both be set when either is present');
		});
		test('throws when only duration_quantity is set without duration_type', () => {
			expect(
				() =>
					new GiftCode(
						createGiftCodeRow({
							duration_type: undefined,
							duration_quantity: 3,
							duration_months: null,
						}),
					),
			).toThrow('Gift code duration_type and duration_quantity must both be set when either is present');
		});
		test('throws when all duration fields are missing', () => {
			expect(
				() =>
					new GiftCode(
						createGiftCodeRow({
							duration_type: undefined,
							duration_quantity: undefined,
							duration_months: null,
						}),
					),
			).toThrow('Gift code duration is missing from both duration_type/duration_quantity and duration_months');
		});
		test('sets durationMonths to null for days duration type', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: 'days',
					duration_quantity: 14,
				}),
			);
			expect(giftCode.durationType).toBe('days');
			expect(giftCode.durationQuantity).toBe(14);
			expect(giftCode.durationMonths).toBeNull();
		});
		test('sets durationMonths to null for weeks duration type', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: 'weeks',
					duration_quantity: 2,
				}),
			);
			expect(giftCode.durationType).toBe('weeks');
			expect(giftCode.durationQuantity).toBe(2);
			expect(giftCode.durationMonths).toBeNull();
		});
		test('computes durationMonths for months duration type', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: 'months',
					duration_quantity: 6,
				}),
			);
			expect(giftCode.durationMonths).toBe(6);
		});
		test('computes durationMonths for years duration type', () => {
			const giftCode = new GiftCode(
				createGiftCodeRow({
					duration_type: 'years',
					duration_quantity: 2,
				}),
			);
			expect(giftCode.durationMonths).toBe(24);
		});
	});
	describe('toRow round-trip', () => {
		test.each<{
			durationType: GiftCodeDurationType;
			durationQuantity: number;
		}>([
			{durationType: 'days', durationQuantity: 7},
			{durationType: 'weeks', durationQuantity: 3},
			{durationType: 'months', durationQuantity: 6},
			{durationType: 'years', durationQuantity: 1},
		])('preserves $durationType/$durationQuantity through toRow', ({durationType, durationQuantity}) => {
			const original = new GiftCode(
				createGiftCodeRow({
					duration_type: durationType,
					duration_quantity: durationQuantity,
				}),
			);
			const row = original.toRow();
			const restored = new GiftCode(row);
			expect(restored.durationType).toBe(durationType);
			expect(restored.durationQuantity).toBe(durationQuantity);
		});
	});
});

describe('mapGiftDurationMonthsToFields', () => {
	test('converts non-divisible-by-12 months to months type', () => {
		expect(mapGiftDurationMonthsToFields(3)).toEqual({durationType: 'months', durationQuantity: 3});
		expect(mapGiftDurationMonthsToFields(1)).toEqual({durationType: 'months', durationQuantity: 1});
		expect(mapGiftDurationMonthsToFields(6)).toEqual({durationType: 'months', durationQuantity: 6});
	});
	test('converts multiples of 12 months to years type', () => {
		expect(mapGiftDurationMonthsToFields(12)).toEqual({durationType: 'years', durationQuantity: 1});
		expect(mapGiftDurationMonthsToFields(24)).toEqual({durationType: 'years', durationQuantity: 2});
		expect(mapGiftDurationMonthsToFields(36)).toEqual({durationType: 'years', durationQuantity: 3});
	});
	test('converts 0 months to months/0 (lifetime sentinel)', () => {
		expect(mapGiftDurationMonthsToFields(0)).toEqual({durationType: 'months', durationQuantity: 0});
	});
	test('rejects negative values', () => {
		expect(() => mapGiftDurationMonthsToFields(-1)).toThrow('non-negative integer');
	});
	test('rejects non-integer values', () => {
		expect(() => mapGiftDurationMonthsToFields(1.5)).toThrow('non-negative integer');
	});
});

describe('mapGiftCodeDurationToMonths', () => {
	test('returns the quantity for months type', () => {
		expect(mapGiftCodeDurationToMonths('months', 3)).toBe(3);
		expect(mapGiftCodeDurationToMonths('months', 1)).toBe(1);
	});
	test('converts years to months', () => {
		expect(mapGiftCodeDurationToMonths('years', 1)).toBe(12);
		expect(mapGiftCodeDurationToMonths('years', 2)).toBe(24);
	});
	test('returns null for days type', () => {
		expect(mapGiftCodeDurationToMonths('days', 14)).toBeNull();
	});
	test('returns null for weeks type', () => {
		expect(mapGiftCodeDurationToMonths('weeks', 2)).toBeNull();
	});
});

describe('addGiftCodeDuration', () => {
	test('adds days correctly', () => {
		const result = addGiftCodeDuration(BASE_DATE, 'days', 14);
		expect(result).not.toBeNull();
		expect(result!.getTime()).toBe(BASE_DATE.getTime() + 14 * MILLISECONDS_PER_DAY);
	});
	test('adds weeks correctly', () => {
		const result = addGiftCodeDuration(BASE_DATE, 'weeks', 2);
		expect(result).not.toBeNull();
		expect(result!.getTime()).toBe(BASE_DATE.getTime() + 14 * MILLISECONDS_PER_DAY);
	});
	test('adds months correctly', () => {
		const result = addGiftCodeDuration(BASE_DATE, 'months', 1);
		expect(result).not.toBeNull();
		expect(result!.toISOString()).toBe('2026-04-01T00:00:00.000Z');
	});
	test('adds years correctly', () => {
		const result = addGiftCodeDuration(BASE_DATE, 'years', 1);
		expect(result).not.toBeNull();
		expect(result!.toISOString()).toBe('2027-03-01T00:00:00.000Z');
	});
	test('returns null for zero quantity (lifetime)', () => {
		expect(addGiftCodeDuration(BASE_DATE, 'months', 0)).toBeNull();
		expect(addGiftCodeDuration(BASE_DATE, 'days', 0)).toBeNull();
		expect(addGiftCodeDuration(BASE_DATE, 'weeks', 0)).toBeNull();
		expect(addGiftCodeDuration(BASE_DATE, 'years', 0)).toBeNull();
	});
	test('clamps month addition to end of shorter month', () => {
		const jan31 = new Date('2026-01-31T00:00:00.000Z');
		const result = addGiftCodeDuration(jan31, 'months', 1);
		expect(result).not.toBeNull();
		expect(result!.toISOString()).toBe('2026-02-28T00:00:00.000Z');
	});
	test('2 weeks equals 14 days', () => {
		const weeksResult = addGiftCodeDuration(BASE_DATE, 'weeks', 2);
		const daysResult = addGiftCodeDuration(BASE_DATE, 'days', 14);
		expect(weeksResult).not.toBeNull();
		expect(daysResult).not.toBeNull();
		expect(weeksResult!.getTime()).toBe(daysResult!.getTime());
	});
	test('does not mutate the base date when adding months', () => {
		const baseDate = new Date('2026-01-31T00:00:00.000Z');
		addGiftCodeDuration(baseDate, 'months', 1);
		expect(baseDate.toISOString()).toBe('2026-01-31T00:00:00.000Z');
	});
	test('clamps year addition from leap day to non-leap year end of february', () => {
		const leapDay = new Date('2024-02-29T00:00:00.000Z');
		const result = addGiftCodeDuration(leapDay, 'years', 1);
		expect(result).not.toBeNull();
		expect(result!.toISOString()).toBe('2025-02-28T00:00:00.000Z');
	});
	test('throws for negative duration quantity', () => {
		expect(() => addGiftCodeDuration(BASE_DATE, 'days', -1)).toThrow('non-negative integer');
	});
	test('throws for non-integer duration quantity', () => {
		expect(() => addGiftCodeDuration(BASE_DATE, 'weeks', 1.5)).toThrow('non-negative integer');
	});
});
