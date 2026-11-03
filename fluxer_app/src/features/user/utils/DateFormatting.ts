// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import UserSettings from '@app/features/user/state/UserSettings';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {TimeFormatTypes} from '@fluxer/constants/src/UserConstants';
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

export function shouldUse12HourFormat(locale: string): boolean {
	const timeFormat = UserSettings.timeFormat;
	switch (timeFormat) {
		case TimeFormatTypes.TWELVE_HOUR:
			return true;
		case TimeFormatTypes.TWENTY_FOUR_HOUR:
			return false;
		default: {
			const useBrowserLocale = Accessibility.useBrowserLocaleForTimeFormat;
			const effectiveLocale = useBrowserLocale ? navigator.language : locale;
			return localeUses12Hour(effectiveLocale);
		}
	}
}

export function getRelativeDateString(timestamp: number | Date | string, i18n: I18n): string {
	const locale = getCurrentLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const baseString = getRelativeDateStringBase(timestamp, locale, hour12);
	const date = new Date(
		typeof timestamp === 'string' || typeof timestamp === 'number' ? new Date(timestamp) : timestamp,
	);
	if (baseString.startsWith('Today at ')) {
		const timeString = getFormattedTimeBase(date, locale, hour12);
		return i18n._(TODAY_AT_DESCRIPTOR, {timeString});
	}
	if (baseString.startsWith('Yesterday at ')) {
		const timeString = getFormattedTimeBase(date, locale, hour12);
		return i18n._(YESTERDAY_AT_DESCRIPTOR, {timeString});
	}
	return baseString;
}

export function getFormattedDateTime(timestamp: number | Date | string): string {
	const locale = getCurrentLocale();
	const hour12 = shouldUse12HourFormat(locale);
	return getFormattedDateTimeBase(timestamp, locale, hour12);
}

export function getFormattedShortDate(timestamp: number | Date | string): string {
	const locale = getCurrentLocale();
	return getFormattedShortDateBase(timestamp, locale);
}

export function getFormattedTime(timestamp: number | Date | string): string {
	const locale = getCurrentLocale();
	const hour12 = shouldUse12HourFormat(locale);
	return getFormattedTimeBase(timestamp, locale, hour12);
}

export function getFormattedCompactDateTime(timestamp: number | Date | string): string {
	const locale = getCurrentLocale();
	const hour12 = shouldUse12HourFormat(locale);
	return getFormattedCompactDateTimeBase(timestamp, locale, hour12);
}

export function getFormattedFullDate(timestamp: number | Date | string): string {
	const locale = getCurrentLocale();
	return getFormattedFullDateBase(timestamp, locale);
}

export function getFormattedDateTimeWithSeconds(timestamp: number | Date | string): string {
	const locale = getCurrentLocale();
	const hour12 = shouldUse12HourFormat(locale);
	return getFormattedDateTimeWithSecondsBase(timestamp, locale, hour12);
}
