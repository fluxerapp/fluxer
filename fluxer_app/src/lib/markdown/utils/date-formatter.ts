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
import {shouldUse12HourFormat} from '~/utils/DateUtils';
import {getCurrentLocale} from '~/utils/LocaleUtils';
import {TimestampStyle} from '../parser/types/enums';

function isToday(date: DateTime, now: DateTime): boolean {
	return date.hasSame(now, 'day');
}

function isYesterday(date: DateTime, now: DateTime): boolean {
	const yesterday = now.minus({days: 1});
	return date.hasSame(yesterday, 'day');
}

function formatRelativeTime(timestamp: number, i18n: I18n): string {
	const locale = getCurrentLocale();
	const date = DateTime.fromSeconds(timestamp).setLocale(locale);
	const now = DateTime.now().setLocale(locale);

	if (isToday(date, now)) {
		const timeString = date.toLocaleString({
			hour: 'numeric',
			minute: 'numeric',
			hour12: shouldUse12HourFormat(locale),
		});
		return i18n._(msg`Today at ${timeString}`);
	}

	if (isYesterday(date, now)) {
		const timeString = date.toLocaleString({
			hour: 'numeric',
			minute: 'numeric',
			hour12: shouldUse12HourFormat(locale),
		});
		return i18n._(msg`Yesterday at ${timeString}`);
	}

	const diff = date.diff(now).shiftTo('years', 'months', 'weeks', 'days', 'hours', 'minutes', 'seconds');
	const {years, months, weeks, days, hours, minutes, seconds} = diff.toObject();

	if (years && Math.abs(years) > 0) {
		const absYears = Math.abs(Math.round(years));
		return date > now
			? absYears === 1
				? i18n._(msg`next year`)
				: i18n._(msg`in ${absYears} years`)
			: absYears === 1
				? i18n._(msg`last year`)
				: i18n._(msg`${absYears} years ago`);
	}

	if (months && Math.abs(months) > 0) {
		const absMonths = Math.abs(Math.round(months));
		return date > now
			? absMonths === 1
				? i18n._(msg`next month`)
				: i18n._(msg`in ${absMonths} months`)
			: absMonths === 1
				? i18n._(msg`last month`)
				: i18n._(msg`${absMonths} months ago`);
	}

	if (weeks && Math.abs(weeks) > 0) {
		const absWeeks = Math.abs(Math.round(weeks));
		return date > now
			? absWeeks === 1
				? i18n._(msg`next week`)
				: i18n._(msg`in ${absWeeks} weeks`)
			: absWeeks === 1
				? i18n._(msg`last week`)
				: i18n._(msg`${absWeeks} weeks ago`);
	}

	if (days && Math.abs(days) > 0) {
		const absDays = Math.abs(Math.round(days));
		return date > now
			? absDays === 1
				? i18n._(msg`tomorrow`)
				: absDays === 2
					? i18n._(msg`in two days`)
					: i18n._(msg`in ${absDays} days`)
			: absDays === 1
				? i18n._(msg`yesterday`)
				: absDays === 2
					? i18n._(msg`two days ago`)
					: i18n._(msg`${absDays} days ago`);
	}

	if (hours && Math.abs(hours) > 0) {
		const absHours = Math.abs(Math.round(hours));
		return date > now
			? absHours === 1
				? i18n._(msg`in one hour`)
				: i18n._(msg`in ${absHours} hours`)
			: absHours === 1
				? i18n._(msg`one hour ago`)
				: i18n._(msg`${absHours} hours ago`);
	}

	if (minutes && Math.abs(minutes) > 0) {
		const absMinutes = Math.abs(Math.round(minutes));
		return date > now
			? absMinutes === 1
				? i18n._(msg`in one minute`)
				: i18n._(msg`in ${absMinutes} minutes`)
			: absMinutes === 1
				? i18n._(msg`one minute ago`)
				: i18n._(msg`${absMinutes} minutes ago`);
	}

	const absSeconds = Math.abs(Math.round(seconds ?? 1));
	return date > now
		? absSeconds === 0
			? i18n._(msg`now`)
			: absSeconds === 1
				? i18n._(msg`in one second`)
				: i18n._(msg`in ${absSeconds} seconds`)
		: absSeconds === 0
			? i18n._(msg`just now`)
			: absSeconds === 1
				? i18n._(msg`one second ago`)
				: i18n._(msg`${absSeconds} seconds ago`);
}

export function formatTimestamp(timestamp: number, style: TimestampStyle, i18n: I18n): string {
	const locale = getCurrentLocale();
	const date = DateTime.fromMillis(timestamp * 1000).setLocale(locale);

	switch (style) {
		case TimestampStyle.ShortTime:
			return date.toLocaleString({
				hour: 'numeric',
				minute: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});

		case TimestampStyle.LongTime:
			return date.toLocaleString({
				hour: 'numeric',
				minute: 'numeric',
				second: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});

		case TimestampStyle.ShortDate:
			return date.toLocaleString(DateTime.DATE_SHORT);

		case TimestampStyle.LongDate:
			return date.toLocaleString({
				month: 'long',
				day: 'numeric',
				year: 'numeric',
			});

		case TimestampStyle.ShortDateTime:
			return date.toLocaleString({
				month: 'long',
				day: 'numeric',
				year: 'numeric',
				hour: 'numeric',
				minute: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});

		case TimestampStyle.LongDateTime:
			return date.toLocaleString({
				weekday: 'long',
				month: 'long',
				day: 'numeric',
				year: 'numeric',
				hour: 'numeric',
				minute: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});

		case TimestampStyle.ShortDateShortTime:
			return date.toLocaleString({
				month: 'numeric',
				day: 'numeric',
				year: 'numeric',
				hour: 'numeric',
				minute: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});

		case TimestampStyle.ShortDateMediumTime:
			return date.toLocaleString({
				month: 'numeric',
				day: 'numeric',
				year: 'numeric',
				hour: 'numeric',
				minute: 'numeric',
				second: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});

		case TimestampStyle.RelativeTime:
			return formatRelativeTime(timestamp, i18n);

		default:
			return date.toLocaleString({
				month: 'long',
				day: 'numeric',
				year: 'numeric',
				hour: 'numeric',
				minute: 'numeric',
				hour12: shouldUse12HourFormat(locale),
			});
	}
}
