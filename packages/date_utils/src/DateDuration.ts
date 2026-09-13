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

export function formatDuration(seconds: number): string {
	if (!Number.isFinite(seconds) || seconds < 0) {
		return '0:00';
	}
	const hours = Math.floor(seconds / SECONDS_PER_HOUR);
	const minutes = Math.floor((seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE);
	const secs = Math.floor(seconds % SECONDS_PER_MINUTE);
	if (hours > 0) {
		return `${hours}:${minutes.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
	}
	return `${minutes}:${secs.toString().padStart(2, '0')}`;
}

export function formatShortRelativeTime(timestamp: DateInput, minUnit: '1m' | 'now' = 'now'): string {
	const date = parseDate(timestamp);
	const now = new Date();
	const diffMs = Math.abs(date.getTime() - now.getTime());
	const diffSeconds = Math.floor(diffMs / 1000);
	const diffMinutes = Math.floor(diffSeconds / SECONDS_PER_MINUTE);
	const diffHours = Math.floor(diffMinutes / MINUTES_PER_HOUR);
	const diffDays = Math.floor(diffHours / HOURS_PER_DAY);
	if (diffSeconds < SECONDS_PER_MINUTE) {
		return minUnit === '1m' ? '1m' : 'now';
	}
	if (diffMinutes < MINUTES_PER_HOUR) {
		return `${diffMinutes}m`;
	}
	if (diffHours < HOURS_PER_DAY) {
		return `${diffHours}h`;
	}
	if (diffDays < DAYS_PER_WEEK) {
		return `${diffDays}d`;
	}
	if (diffDays < DAYS_PER_MONTH) {
		return `${Math.floor(diffDays / DAYS_PER_WEEK)}w`;
	}
	if (diffDays < DAYS_PER_YEAR) {
		return `${Math.floor(diffDays / DAYS_PER_MONTH)}mo`;
	}
	return `${Math.floor(diffDays / DAYS_PER_YEAR)}y`;
}
