// SPDX-License-Identifier: AGPL-3.0-or-later

import UserSettings from '@app/features/user/state/UserSettings';
import {getFormattingLocale} from '@app/features/user/utils/LocaleUtils';
import {TimeFormatTypes, TimestampFormatTypes} from '@fluxer/constants/src/UserConstants';
import {
	type DateFormatPreset,
	formatPresetDate,
	formatPresetDateTime,
	formatPresetTime,
	toDateFormatPreset,
} from '@fluxer/date_utils/src/DateFormatPresets';
import {
	getFormattedCompactDateTime as getFormattedCompactDateTimeBase,
	getFormattedDateTime as getFormattedDateTimeBase,
	getFormattedDateTimeWithSeconds as getFormattedDateTimeWithSecondsBase,
	getFormattedFullDate as getFormattedFullDateBase,
	getFormattedShortDate as getFormattedShortDateBase,
	getFormattedTime as getFormattedTimeBase,
	getRelativeDateString as getRelativeDateStringBase,
} from '@fluxer/date_utils/src/DateFormatting';
import {localeUses12Hour} from '@fluxer/date_utils/src/DateHourCycle';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const TODAY_AT_DESCRIPTOR = msg({
	message: 'Today at {timeString}',
	comment:
		'Short label in the date and time formatting. Keep it concise. Preserve {timeString}; it is inserted by code.',
});
const YESTERDAY_AT_DESCRIPTOR = msg({
	message: 'Yesterday at {timeString}',
	comment:
		'Short label in the date and time formatting. Keep it concise. Preserve {timeString}; it is inserted by code.',
});

export function resolveHour12(locale: string, timeFormat: number, timestampFormat: number): boolean {
	switch (timeFormat) {
		case TimeFormatTypes.TWELVE_HOUR:
			return true;
		case TimeFormatTypes.TWENTY_FOUR_HOUR:
			return false;
		default:
			return timestampFormat !== TimestampFormatTypes.ISO_8601 && localeUses12Hour(locale);
	}
}

export function shouldUse12HourFormat(locale: string): boolean {
	return resolveHour12(locale, UserSettings.timeFormat, UserSettings.timestampFormat);
}

export function getDateFormatPreset(): DateFormatPreset | null {
	return toDateFormatPreset(UserSettings.timestampFormat);
}

function formatTime(timestamp: number | Date | string, locale: string, hour12: boolean): string {
	const preset = getDateFormatPreset();
	return preset === null
		? getFormattedTimeBase(timestamp, locale, hour12)
		: formatPresetTime(timestamp, preset, locale, hour12);
}

export function getRelativeDateString(timestamp: number | Date | string, i18n: I18n): string {
	const locale = getFormattingLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const baseString = getRelativeDateStringBase(timestamp, locale, hour12);
	const date = new Date(
		typeof timestamp === 'string' || typeof timestamp === 'number' ? new Date(timestamp) : timestamp,
	);
	if (baseString.startsWith('Today at ')) {
		return i18n._(TODAY_AT_DESCRIPTOR, {timeString: formatTime(date, locale, hour12)});
	}
	if (baseString.startsWith('Yesterday at ')) {
		return i18n._(YESTERDAY_AT_DESCRIPTOR, {timeString: formatTime(date, locale, hour12)});
	}
	const preset = getDateFormatPreset();
	return preset === null ? baseString : formatPresetDateTime(date, preset, locale, hour12);
}

export function getFormattedDateTime(timestamp: number | Date | string): string {
	const locale = getFormattingLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const preset = getDateFormatPreset();
	return preset === null
		? getFormattedDateTimeBase(timestamp, locale, hour12)
		: formatPresetDateTime(timestamp, preset, locale, hour12);
}

export function getFormattedShortDate(timestamp: number | Date | string): string {
	const locale = getFormattingLocale();
	const preset = getDateFormatPreset();
	return preset === null ? getFormattedShortDateBase(timestamp, locale) : formatPresetDate(timestamp, preset, locale);
}

export function getFormattedTime(timestamp: number | Date | string): string {
	const locale = getFormattingLocale();
	return formatTime(timestamp, locale, shouldUse12HourFormat(locale));
}

export function getFormattedCompactDateTime(timestamp: number | Date | string): string {
	const locale = getFormattingLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const preset = getDateFormatPreset();
	return preset === null
		? getFormattedCompactDateTimeBase(timestamp, locale, hour12)
		: formatPresetDateTime(timestamp, preset, locale, hour12);
}

export function getFormattedFullDate(timestamp: number | Date | string): string {
	return getFormattedFullDateBase(timestamp, getFormattingLocale());
}

export function getFormattedDateTimeWithSeconds(timestamp: number | Date | string): string {
	const locale = getFormattingLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const preset = getDateFormatPreset();
	return preset === null
		? getFormattedDateTimeWithSecondsBase(timestamp, locale, hour12)
		: formatPresetDateTime(timestamp, preset, locale, hour12, true);
}
