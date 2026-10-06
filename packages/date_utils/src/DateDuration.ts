// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DAYS_PER_MONTH,
	DAYS_PER_WEEK,
	DAYS_PER_YEAR,
	HOURS_PER_DAY,
	MINUTES_PER_HOUR,
	SECONDS_PER_HOUR,
	SECONDS_PER_MINUTE,
} from '@fluxer/date_utils/src/DateConstants';
import {parseDate} from '@fluxer/date_utils/src/DateParsing';
import type {DateInput} from '@fluxer/date_utils/src/DateTypes';

const UNLOCALIZED_DURATION_LOCALE = 'en-US';
const durationSegmentFormatters = new Map<string, Intl.NumberFormat>();

function formatDurationSegment(value: number, locale: string, minimumIntegerDigits: number): string {
	const key = `${locale}|${minimumIntegerDigits}`;
	let formatter = durationSegmentFormatters.get(key);
	if (formatter === undefined) {
		formatter = new Intl.NumberFormat(locale, {minimumIntegerDigits, useGrouping: false});
		durationSegmentFormatters.set(key, formatter);
	}
	return formatter.format(value);
}

export function formatDuration(seconds: number, locale: string = UNLOCALIZED_DURATION_LOCALE): string {
	if (!Number.isFinite(seconds) || seconds < 0) {
		return `${formatDurationSegment(0, locale, 1)}:${formatDurationSegment(0, locale, 2)}`;
	}
	const hours = Math.floor(seconds / SECONDS_PER_HOUR);
	const minutes = Math.floor((seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE);
	const secs = Math.floor(seconds % SECONDS_PER_MINUTE);
	const secondsLabel = formatDurationSegment(secs, locale, 2);
	if (hours > 0) {
		const hoursLabel = formatDurationSegment(hours, locale, 1);
		return `${hoursLabel}:${formatDurationSegment(minutes, locale, 2)}:${secondsLabel}`;
	}
	return `${formatDurationSegment(minutes, locale, 1)}:${secondsLabel}`;
}

type ShortRelativeTimeUnit = 'now' | 'minute' | 'hour' | 'day' | 'week' | 'month' | 'year';

type ShortRelativeTimeParts = {unit: ShortRelativeTimeUnit; value: number};

export function getShortRelativeTimeParts(timestamp: DateInput, minUnit: '1m' | 'now' = 'now'): ShortRelativeTimeParts {
	const date = parseDate(timestamp);
	const now = new Date();
	const diffMs = Math.abs(date.getTime() - now.getTime());
	const diffSeconds = Math.floor(diffMs / 1000);
	const diffMinutes = Math.floor(diffSeconds / SECONDS_PER_MINUTE);
	const diffHours = Math.floor(diffMinutes / MINUTES_PER_HOUR);
	const diffDays = Math.floor(diffHours / HOURS_PER_DAY);
	if (diffSeconds < SECONDS_PER_MINUTE) {
		return minUnit === '1m' ? {unit: 'minute', value: 1} : {unit: 'now', value: 0};
	}
	if (diffMinutes < MINUTES_PER_HOUR) {
		return {unit: 'minute', value: diffMinutes};
	}
	if (diffHours < HOURS_PER_DAY) {
		return {unit: 'hour', value: diffHours};
	}
	if (diffDays < DAYS_PER_WEEK) {
		return {unit: 'day', value: diffDays};
	}
	if (diffDays < DAYS_PER_MONTH) {
		return {unit: 'week', value: Math.floor(diffDays / DAYS_PER_WEEK)};
	}
	if (diffDays < DAYS_PER_YEAR) {
		return {unit: 'month', value: Math.floor(diffDays / DAYS_PER_MONTH)};
	}
	return {unit: 'year', value: Math.floor(diffDays / DAYS_PER_YEAR)};
}
