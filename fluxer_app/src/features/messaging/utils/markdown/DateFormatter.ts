// SPDX-License-Identifier: AGPL-3.0-or-later

import {TimestampStyle} from '@app/features/messaging/utils/markdown/parser/Enums';
import {getDateFromUnixTimestampSeconds} from '@app/features/messaging/utils/markdown/TimestampValidation';
import {getRelativeTimeFormat} from '@app/features/ui/utils/RelativeDayLabels';
import {getDateFormatPreset, shouldUse12HourFormat} from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale, getFormattingLocale} from '@app/features/user/utils/LocaleUtils';
import {MS_PER_DAY, MS_PER_HOUR, MS_PER_MINUTE, MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import {formatTimestampWithStyle} from '@fluxer/date_utils/src/DateTimestampStyle';
import type {I18n} from '@lingui/core';

let cachedNow: Date | null = null;
let cachedNowExpiry = 0;

function getSharedNow(): Date {
	const t = Date.now();
	if (cachedNow == null || t >= cachedNowExpiry) {
		cachedNow = new Date(t);
		cachedNowExpiry = t + 250;
	}
	return cachedNow;
}

function formatRelativeTime(date: Date): string {
	const locale = getCurrentLocale();
	const now = getSharedNow();
	const rtf = getRelativeTimeFormat(locale);
	const diffMs = date.getTime() - now.getTime();
	const absMs = Math.abs(diffMs);
	const direction = diffMs >= 0 ? 1 : -1;
	const absDays = Math.floor(absMs / MS_PER_DAY);
	if (absDays >= 365) {
		const years = Math.floor(absDays / 365);
		return rtf.format(direction * years, 'year');
	}
	if (absDays >= 30) {
		const months = Math.floor(absDays / 30);
		return rtf.format(direction * months, 'month');
	}
	if (absDays >= 7) {
		const weeks = Math.floor(absDays / 7);
		return rtf.format(direction * weeks, 'week');
	}
	if (absDays > 0) {
		return rtf.format(direction * absDays, 'day');
	}
	const absHours = Math.floor(absMs / MS_PER_HOUR);
	if (absHours > 0) {
		return rtf.format(direction * absHours, 'hour');
	}
	const absMinutes = Math.floor(absMs / MS_PER_MINUTE);
	if (absMinutes > 0) {
		return rtf.format(direction * absMinutes, 'minute');
	}
	const absSeconds = Math.floor(absMs / MS_PER_SECOND);
	return rtf.format(direction * absSeconds, 'second');
}

export function formatTimestamp(timestamp: number, style: TimestampStyle, _i18n: I18n): string {
	const locale = getFormattingLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const date = getDateFromUnixTimestampSeconds(timestamp);
	if (date == null) {
		return String(timestamp);
	}
	if (style === TimestampStyle.RelativeTime) {
		return formatRelativeTime(date);
	}
	return formatTimestampWithStyle(timestamp, style, locale, hour12, getDateFormatPreset());
}
