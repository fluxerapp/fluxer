// SPDX-License-Identifier: AGPL-3.0-or-later

import {getShortRelativeTimeParts} from '@fluxer/date_utils/src/DateDuration';
import type {DateInput} from '@fluxer/date_utils/src/DateTypes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const NOW_DESCRIPTOR = msg({
	message: 'now',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list) when the event happened less than a minute ago. Keep it as short as possible; it sits in a narrow column.',
});
const MINUTES_DESCRIPTOR = msg({
	message: '{count, plural, one {#m} other {#m}}',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list). count is a number of minutes; the "m" is an abbreviation of "minutes". Keep it as short as possible; it sits in a narrow column.',
});
const HOURS_DESCRIPTOR = msg({
	message: '{count, plural, one {#h} other {#h}}',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list). count is a number of hours; the "h" is an abbreviation of "hours". Keep it as short as possible; it sits in a narrow column.',
});
const DAYS_DESCRIPTOR = msg({
	message: '{count, plural, one {#d} other {#d}}',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list). count is a number of days; the "d" is an abbreviation of "days". Keep it as short as possible; it sits in a narrow column.',
});
const WEEKS_DESCRIPTOR = msg({
	message: '{count, plural, one {#w} other {#w}}',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list). count is a number of weeks; the "w" is an abbreviation of "weeks". Keep it as short as possible; it sits in a narrow column.',
});
const MONTHS_DESCRIPTOR = msg({
	message: '{count, plural, one {#mo} other {#mo}}',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list). count is a number of months; the "mo" is an abbreviation of "months". Keep it as short as possible; it sits in a narrow column.',
});
const YEARS_DESCRIPTOR = msg({
	message: '{count, plural, one {#y} other {#y}}',
	context: 'short-relative-time',
	comment:
		'Compact age label shown beside a list row (DM list, invite list). count is a number of years; the "y" is an abbreviation of "years". Keep it as short as possible; it sits in a narrow column.',
});

export function formatShortRelativeTime(i18n: I18n, timestamp: DateInput, minUnit: '1m' | 'now' = 'now'): string {
	const {unit, value} = getShortRelativeTimeParts(timestamp, minUnit);
	switch (unit) {
		case 'now':
			return i18n._(NOW_DESCRIPTOR);
		case 'minute':
			return i18n._(MINUTES_DESCRIPTOR, {count: value});
		case 'hour':
			return i18n._(HOURS_DESCRIPTOR, {count: value});
		case 'day':
			return i18n._(DAYS_DESCRIPTOR, {count: value});
		case 'week':
			return i18n._(WEEKS_DESCRIPTOR, {count: value});
		case 'month':
			return i18n._(MONTHS_DESCRIPTOR, {count: value});
		default:
			return i18n._(YEARS_DESCRIPTOR, {count: value});
	}
}
