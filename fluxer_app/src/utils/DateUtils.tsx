/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {DateTime} from 'luxon';
import {TimeFormatTypes} from '~/Constants';
import AccessibilityStore from '~/stores/AccessibilityStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {getCurrentLocale} from '~/utils/LocaleUtils';

const localeUses12Hour = (locale: string): boolean => {
	const lang = locale.toLowerCase();

	const twelveHourLocales = [
		'en-us',
		'en-ca',
		'en-au',
		'en-nz',
		'en-ph',
		'en-in',
		'en-pk',
		'en-bd',
		'en-za',
		'es-mx',
		'es-co',
		'ar',
		'hi',
		'bn',
		'ur',
		'fil',
		'tl',
	];

	return twelveHourLocales.some((l) => lang.startsWith(l));
};

export const shouldUse12HourFormat = (locale: string): boolean => {
	const timeFormat = UserSettingsStore.getTimeFormat();
	switch (timeFormat) {
		case TimeFormatTypes.TWELVE_HOUR:
			return true;
		case TimeFormatTypes.TWENTY_FOUR_HOUR:
			return false;
		default: {
			const useBrowserLocale = AccessibilityStore.useBrowserLocaleForTimeFormat;
			const effectiveLocale = useBrowserLocale ? navigator.language : locale;
			return localeUses12Hour(effectiveLocale);
		}
	}
};

const parseDateTime = (timestamp: number | Date | string): DateTime => {
	if (timestamp instanceof Date) {
		return DateTime.fromJSDate(timestamp);
	}
	if (typeof timestamp === 'string') {
		return DateTime.fromISO(timestamp);
	}

	if (timestamp == null || Number.isNaN(timestamp)) {
		console.warn('[DateUtils] Invalid timestamp provided, using current time:', timestamp);
		return DateTime.now();
	}
	return DateTime.fromMillis(timestamp);
};

export const isSameDay = (timestamp1: number | Date | string, timestamp2?: number | Date | string): boolean => {
	const dt1 = parseDateTime(timestamp1);
	const dt2 = timestamp2 != null ? parseDateTime(timestamp2) : DateTime.now();
	return dt1.hasSame(dt2, 'day');
};

export const getRelativeDateString = (timestamp: number | Date | string, i18n: I18n): string => {
	const locale = getCurrentLocale();
	const dt = parseDateTime(timestamp).setLocale(locale);
	const now = DateTime.now().setLocale(locale);

	const timeString = dt.toLocaleString({
		hour: 'numeric',
		minute: '2-digit',
		hour12: shouldUse12HourFormat(locale),
	});

	if (dt.hasSame(now, 'day')) {
		return i18n._(msg`Today at ${timeString}`);
	}
	if (dt.hasSame(now.minus({days: 1}), 'day')) {
		return i18n._(msg`Yesterday at ${timeString}`);
	}

	return dt.toLocaleString({
		month: 'numeric',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: '2-digit',
		hour12: shouldUse12HourFormat(locale),
	});
};

export const getFormattedDateTime = (timestamp: number | Date | string): string => {
	const locale = getCurrentLocale();
	const dt = parseDateTime(timestamp).setLocale(locale);
	return dt.toLocaleString({
		month: 'numeric',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: '2-digit',
		hour12: shouldUse12HourFormat(locale),
	});
};

export const getFormattedShortDate = (timestamp: number | Date | string): string => {
	return parseDateTime(timestamp).setLocale(getCurrentLocale()).toLocaleString(DateTime.DATE_MED);
};

export const getFormattedTime = (timestamp: number | Date | string): string => {
	const locale = getCurrentLocale();
	const dt = parseDateTime(timestamp).setLocale(locale);
	return dt.toLocaleString({
		hour: 'numeric',
		minute: '2-digit',
		hour12: shouldUse12HourFormat(locale),
	});
};

export const getFormattedCompactDateTime = (timestamp: number | Date | string): string => {
	const locale = getCurrentLocale();
	const dt = parseDateTime(timestamp).setLocale(locale);
	return dt.toFormat('M/d/yy, h:mm a');
};

export const getFormattedFullDate = (timestamp: number | Date | string): string => {
	return parseDateTime(timestamp).setLocale(getCurrentLocale()).toLocaleString(DateTime.DATE_FULL);
};

export const getFormattedDateTimeWithSeconds = (timestamp: number | Date | string): string => {
	const locale = getCurrentLocale();
	const dt = parseDateTime(timestamp).setLocale(locale);
	const datePart = dt.toLocaleString({
		weekday: 'long',
		month: 'long',
		day: 'numeric',
		year: 'numeric',
	});
	const timePart = dt.toLocaleString({
		hour: 'numeric',
		minute: '2-digit',
		second: '2-digit',
		hour12: shouldUse12HourFormat(locale),
	});
	return `${datePart} ${timePart}`;
};

export const getShortRelativeDateString = (timestamp: number | Date | string): string => {
	const result = parseDateTime(timestamp).setLocale(getCurrentLocale()).toRelative({style: 'short'});
	return result ?? '';
};
