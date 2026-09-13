// SPDX-License-Identifier: AGPL-3.0-or-later

import {getActiveHolidaySlugs, millisecondsUntilNextLocalMidnight} from '@app/features/theme/utils/HolidayClassUtils';
import {describe, expect, it} from 'vitest';

describe('getActiveHolidaySlugs', () => {
	it('fires christmas + christmastide on Dec 25', () => {
		const slugs = getActiveHolidaySlugs(new Date(2026, 11, 25));
		expect(slugs).toContain('christmas');
		expect(slugs).toContain('christmastide');
	});
	it('fires both boxing-day and christmastide on Dec 26', () => {
		const slugs = getActiveHolidaySlugs(new Date(2026, 11, 26));
		expect(slugs).toContain('boxing-day');
		expect(slugs).toContain('christmastide');
		expect(slugs).not.toContain('christmas');
	});
	it('co-fires india-independence-day and korea-liberation-day on Aug 15', () => {
		const slugs = getActiveHolidaySlugs(new Date(2026, 7, 15));
		expect(slugs).toContain('india-independence-day');
		expect(slugs).toContain('korea-liberation-day');
	});
	it('fires pride-month every day in June', () => {
		expect(getActiveHolidaySlugs(new Date(2026, 5, 1))).toContain('pride-month');
		expect(getActiveHolidaySlugs(new Date(2026, 5, 15))).toContain('pride-month');
		expect(getActiveHolidaySlugs(new Date(2026, 5, 30))).toContain('pride-month');
		expect(getActiveHolidaySlugs(new Date(2026, 4, 31))).not.toContain('pride-month');
		expect(getActiveHolidaySlugs(new Date(2026, 6, 1))).not.toContain('pride-month');
	});
	it('co-fires stonewall-day with pride-month on Jun 28', () => {
		const slugs = getActiveHolidaySlugs(new Date(2026, 5, 28));
		expect(slugs).toContain('stonewall-day');
		expect(slugs).toContain('pride-month');
	});
	it('fires juneteenth on Jun 19', () => {
		expect(getActiveHolidaySlugs(new Date(2026, 5, 19))).toContain('juneteenth');
	});
	it('fires back-to-the-future-day on Oct 21', () => {
		expect(getActiveHolidaySlugs(new Date(2026, 9, 21))).toContain('back-to-the-future-day');
	});
	it('returns empty on a quiet day', () => {
		expect(getActiveHolidaySlugs(new Date(2026, 1, 17))).toEqual([]);
	});
	it('handles leap day without crashing', () => {
		expect(() => getActiveHolidaySlugs(new Date(2028, 1, 29))).not.toThrow();
		expect(getActiveHolidaySlugs(new Date(2028, 1, 29))).toEqual([]);
	});
});

describe('millisecondsUntilNextLocalMidnight', () => {
	it('returns the gap to next local midnight', () => {
		const now = new Date(2026, 5, 15, 23, 59, 59, 0);
		const ms = millisecondsUntilNextLocalMidnight(now);
		expect(ms).toBeGreaterThanOrEqual(1000);
		expect(ms).toBeLessThanOrEqual(1000 + 999);
	});
	it('returns close to 24h at the start of a day', () => {
		const now = new Date(2026, 5, 15, 0, 0, 1, 0);
		const ms = millisecondsUntilNextLocalMidnight(now);
		expect(ms).toBeGreaterThan(23 * 60 * 60 * 1000);
		expect(ms).toBeLessThanOrEqual(24 * 60 * 60 * 1000);
	});
});
