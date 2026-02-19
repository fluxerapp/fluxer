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

import {getDateFromUnixTimestampSeconds} from '@app/lib/markdown/utils/TimestampValidation';
import {shouldUse12HourFormat} from '@app/utils/DateUtils';
import {getCurrentLocale} from '@app/utils/LocaleUtils';
import {
	getDaysDiff,
	getHoursDiff,
	getMinutesDiff,
	getSecondsDiff,
	isSameDay,
	isYesterday,
} from '@fluxer/date_utils/src/DateComparison';
import {getFormattedTime} from '@fluxer/date_utils/src/DateFormatting';
import {formatTimestampWithStyle} from '@fluxer/date_utils/src/DateTimestampStyle';
import {TimestampStyle} from '@fluxer/markdown_parser/src/types/Enums';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

function formatRelativeTime(date: Date, i18n: I18n): string {
	const locale = getCurrentLocale();
	const now = new Date();

	if (isSameDay(date, now)) {
		const timeString = getFormattedTime(date, locale, shouldUse12HourFormat(locale));
		return i18n._(msg`Today at ${timeString}`);
	}

	if (isYesterday(date, now)) {
		const timeString = getFormattedTime(date, locale, shouldUse12HourFormat(locale));
		return i18n._(msg`Yesterday at ${timeString}`);
	}

	const daysDiff = getDaysDiff(date, now);
	const absDays = Math.abs(daysDiff);

	if (absDays >= 365) {
		const years = Math.floor(absDays / 365);
		return date > now
			? years === 1
				? i18n._(msg`next year`)
				: i18n._(msg`in ${years} years`)
			: years === 1
				? i18n._(msg`last year`)
				: i18n._(msg`${years} years ago`);
	}

	if (absDays >= 30) {
		const months = Math.floor(absDays / 30);
		return date > now
			? months === 1
				? i18n._(msg`next month`)
				: i18n._(msg`in ${months} months`)
			: months === 1
				? i18n._(msg`last month`)
				: i18n._(msg`${months} months ago`);
	}

	if (absDays >= 7) {
		const weeks = Math.floor(absDays / 7);
		return date > now
			? weeks === 1
				? i18n._(msg`next week`)
				: i18n._(msg`in ${weeks} weeks`)
			: weeks === 1
				? i18n._(msg`last week`)
				: i18n._(msg`${weeks} weeks ago`);
	}

	if (absDays > 0) {
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

	const hoursDiff = getHoursDiff(date, now);
	const absHours = Math.abs(hoursDiff);

	if (absHours > 0) {
		return date > now
			? absHours === 1
				? i18n._(msg`in one hour`)
				: i18n._(msg`in ${absHours} hours`)
			: absHours === 1
				? i18n._(msg`one hour ago`)
				: i18n._(msg`${absHours} hours ago`);
	}

	const minutesDiff = getMinutesDiff(date, now);
	const absMinutes = Math.abs(minutesDiff);

	if (absMinutes > 0) {
		return date > now
			? absMinutes === 1
				? i18n._(msg`in one minute`)
				: i18n._(msg`in ${absMinutes} minutes`)
			: absMinutes === 1
				? i18n._(msg`one minute ago`)
				: i18n._(msg`${absMinutes} minutes ago`);
	}

	const secondsDiff = getSecondsDiff(date, now);
	const absSeconds = Math.abs(secondsDiff);
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
	const hour12 = shouldUse12HourFormat(locale);
	const date = getDateFromUnixTimestampSeconds(timestamp);

	if (date == null) {
		return String(timestamp);
	}

	if (style === TimestampStyle.RelativeTime) {
		return formatRelativeTime(date, i18n);
	}

	return formatTimestampWithStyle(timestamp, style, locale, hour12);
}
