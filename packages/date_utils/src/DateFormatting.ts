// SPDX-License-Identifier: AGPL-3.0-or-later

import {isSameDay} from '@fluxer/date_utils/src/DateComparison';
import {DEFAULT_LOCALE} from '@fluxer/date_utils/src/DateConstants';
import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import {localeUses12Hour} from '@fluxer/date_utils/src/DateHourCycle';
import {parseDate} from '@fluxer/date_utils/src/DateParsing';
import type {DateInput} from '@fluxer/date_utils/src/DateTypes';

function resolveHour12(locale: string, hour12?: boolean): boolean {
	return hour12 ?? localeUses12Hour(locale);
}

export function getFormattedDateTime(timestamp: DateInput, locale: string = DEFAULT_LOCALE, hour12?: boolean): string {
	const date = parseDate(timestamp);
	return getDateFormatter(locale, {
		month: 'numeric',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: '2-digit',
		hour12: resolveHour12(locale, hour12),
	}).format(date);
}

export function getFormattedDateTimeInZone(
	isoString: string,
	timezone: string,
	locale: string = DEFAULT_LOCALE,
	hour12?: boolean,
): string {
	try {
		const date = new Date(isoString);
		if (Number.isNaN(date.getTime())) {
			return isoString;
		}
		return getDateFormatter(locale, {
			month: 'short',
			day: 'numeric',
			year: 'numeric',
			hour: 'numeric',
			minute: '2-digit',
			hour12: resolveHour12(locale, hour12),
			timeZone: timezone,
		}).format(date);
	} catch {
		return isoString;
	}
}

export function getFormattedShortDate(timestamp: DateInput, locale: string = DEFAULT_LOCALE): string {
	return getDateFormatter(locale, {month: 'short', day: 'numeric', year: 'numeric'}).format(parseDate(timestamp));
}

export function getFormattedLongDate(timestamp: DateInput, locale: string = DEFAULT_LOCALE): string {
	return getDateFormatter(locale, {month: 'long', day: 'numeric', year: 'numeric'}).format(parseDate(timestamp));
}

export function getFormattedTime(timestamp: DateInput, locale: string = DEFAULT_LOCALE, hour12?: boolean): string {
	return getDateFormatter(locale, {
		hour: 'numeric',
		minute: '2-digit',
		hour12: resolveHour12(locale, hour12),
	}).format(parseDate(timestamp));
}

export function getFormattedCompactDateTime(
	timestamp: DateInput,
	locale: string = DEFAULT_LOCALE,
	hour12?: boolean,
): string {
	const date = parseDate(timestamp);
	const datePart = getDateFormatter(locale, {month: 'numeric', day: 'numeric', year: '2-digit'}).format(date);
	const timePart = getDateFormatter(locale, {
		hour: 'numeric',
		minute: '2-digit',
		hour12: resolveHour12(locale, hour12),
	}).format(date);
	return `${datePart}, ${timePart}`;
}

export function getFormattedFullDate(timestamp: DateInput, locale: string = DEFAULT_LOCALE): string {
	return getDateFormatter(locale, {
		weekday: 'long',
		month: 'long',
		day: 'numeric',
		year: 'numeric',
	}).format(parseDate(timestamp));
}

export function getFormattedDateTimeWithSeconds(
	timestamp: DateInput,
	locale: string = DEFAULT_LOCALE,
	hour12?: boolean,
): string {
	const date = parseDate(timestamp);
	const use12Hour = resolveHour12(locale, hour12);
	const datePart = getDateFormatter(locale, {
		weekday: 'long',
		month: 'long',
		day: 'numeric',
		year: 'numeric',
	}).format(date);
	const timePart = getDateFormatter(locale, {
		hour: 'numeric',
		minute: '2-digit',
		second: '2-digit',
		hour12: use12Hour,
	}).format(date);
	return `${datePart} ${timePart}`;
}

export function getRelativeDateString(
	timestamp: DateInput,
	locale: string = DEFAULT_LOCALE,
	hour12?: boolean,
	now?: Date,
): string {
	const date = parseDate(timestamp);
	const nowDate = now ?? new Date();
	const use12Hour = resolveHour12(locale, hour12);
	const timeString = getDateFormatter(locale, {
		hour: 'numeric',
		minute: '2-digit',
		hour12: use12Hour,
	}).format(date);
	if (isSameDay(date, nowDate)) {
		return `Today at ${timeString}`;
	}
	const yesterday = new Date(nowDate);
	yesterday.setDate(yesterday.getDate() - 1);
	if (isSameDay(date, yesterday)) {
		return `Yesterday at ${timeString}`;
	}
	return getDateFormatter(locale, {
		month: 'numeric',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: '2-digit',
		hour12: use12Hour,
	}).format(date);
}

export function formatLastActive(date: DateInput, locale: string = DEFAULT_LOCALE): string {
	const dateObj = parseDate(date);
	if (Number.isNaN(dateObj.getTime())) {
		return String(date);
	}
	return getDateFormatter(locale, {dateStyle: 'medium', timeStyle: 'short'}).format(dateObj);
}

export function formatScheduledMessage(date: DateInput, locale: string = DEFAULT_LOCALE, timeZone?: string): string {
	const dateObj = parseDate(date);
	if (Number.isNaN(dateObj.getTime())) {
		return String(date);
	}
	return getDateFormatter(locale, {dateStyle: 'medium', timeStyle: 'short', timeZone}).format(dateObj);
}
