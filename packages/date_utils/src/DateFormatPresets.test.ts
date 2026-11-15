// SPDX-License-Identifier: AGPL-3.0-or-later

import {TimestampFormatTypes} from '@fluxer/constants/src/UserConstants';
import {formatPresetDate, formatPresetDateTime, toDateFormatPreset} from '@fluxer/date_utils/src/DateFormatPresets';
import {formatTimestampWithStyle, TimestampStyle} from '@fluxer/date_utils/src/DateTimestampStyle';
import {describe, expect, test} from 'vitest';

const EVENING = new Date(2026, 10, 6, 21, 3, 45);

describe('date format presets', () => {
	test('ISO 8601 writes year-month-day in Latin digits for any locale', () => {
		expect(formatPresetDate(EVENING, TimestampFormatTypes.ISO_8601, 'ar-EG')).toBe('2026-11-06');
		expect(formatPresetDateTime(EVENING, TimestampFormatTypes.ISO_8601, 'en-US', false)).toBe('2026-11-06 21:03');
		expect(formatPresetDateTime(EVENING, TimestampFormatTypes.ISO_8601, 'de-DE', false, true)).toBe(
			'2026-11-06 21:03:45',
		);
	});

	test('day first and month first keep the order whatever the locale', () => {
		expect(formatPresetDate(EVENING, TimestampFormatTypes.DAY_MONTH_YEAR, 'en-US')).toBe('06/11/2026');
		expect(formatPresetDate(EVENING, TimestampFormatTypes.MONTH_DAY_YEAR, 'en-GB')).toBe('11/06/2026');
		expect(formatPresetDate(EVENING, TimestampFormatTypes.DAY_MONTH_YEAR, 'de-DE')).toBe('06/11/2026');
	});

	test('the time follows the hour cycle it is given', () => {
		expect(formatPresetDateTime(EVENING, TimestampFormatTypes.MONTH_DAY_YEAR, 'en-US', true)).toMatch(
			/^11\/06\/2026 9:03\sPM$/,
		);
		expect(formatPresetDateTime(EVENING, TimestampFormatTypes.DAY_MONTH_YEAR, 'en-GB', false)).toBe('06/11/2026 21:03');
	});

	test('message timestamps use the preset for numeric styles only', () => {
		const seconds = EVENING.getTime() / 1000;
		const iso = toDateFormatPreset(TimestampFormatTypes.ISO_8601);
		expect(formatTimestampWithStyle(seconds, TimestampStyle.ShortDateMediumTime, 'en-US', false, iso)).toBe(
			'2026-11-06 21:03:45',
		);
		expect(formatTimestampWithStyle(seconds, TimestampStyle.LongDate, 'en-US', false, iso)).toBe('November 6, 2026');
		expect(toDateFormatPreset(TimestampFormatTypes.AUTO)).toBeNull();
	});
});
