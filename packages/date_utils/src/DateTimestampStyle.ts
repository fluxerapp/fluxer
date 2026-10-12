// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import {localeUses12Hour} from '@fluxer/date_utils/src/DateHourCycle';

export const TimestampStyle = {
	ShortTime: 'ShortTime',
	LongTime: 'LongTime',
	ShortDate: 'ShortDate',
	LongDate: 'LongDate',
	ShortDateTime: 'ShortDateTime',
	LongDateTime: 'LongDateTime',
	ShortDateShortTime: 'ShortDateShortTime',
	ShortDateMediumTime: 'ShortDateMediumTime',
	RelativeTime: 'RelativeTime',
} as const;
export type TimestampStyle = (typeof TimestampStyle)[keyof typeof TimestampStyle];

const TIMESTAMP_STYLE_OPTIONS: Record<string, Intl.DateTimeFormatOptions> = {
	[TimestampStyle.ShortTime]: {hour: 'numeric', minute: 'numeric'},
	[TimestampStyle.LongTime]: {hour: 'numeric', minute: 'numeric', second: 'numeric'},
	[TimestampStyle.ShortDate]: {year: 'numeric', month: 'numeric', day: 'numeric'},
	[TimestampStyle.LongDate]: {month: 'long', day: 'numeric', year: 'numeric'},
	[TimestampStyle.ShortDateTime]: {month: 'long', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric'},
	[TimestampStyle.LongDateTime]: {
		weekday: 'long',
		month: 'long',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: 'numeric',
	},
	[TimestampStyle.ShortDateShortTime]: {
		month: 'numeric',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: 'numeric',
	},
	[TimestampStyle.ShortDateMediumTime]: {
		month: 'numeric',
		day: 'numeric',
		year: 'numeric',
		hour: 'numeric',
		minute: 'numeric',
		second: 'numeric',
	},
};
const DEFAULT_STYLE_OPTIONS: Intl.DateTimeFormatOptions = {
	month: 'long',
	day: 'numeric',
	year: 'numeric',
	hour: 'numeric',
	minute: 'numeric',
};
const STYLES_WITHOUT_HOUR_CYCLE: ReadonlySet<TimestampStyle> = new Set([
	TimestampStyle.ShortDate,
	TimestampStyle.LongDate,
]);

export function formatTimestampWithStyle(
	timestamp: number,
	style: TimestampStyle,
	locale: string,
	hour12?: boolean,
): string {
	const date = new Date(timestamp * 1000);
	const baseOptions = TIMESTAMP_STYLE_OPTIONS[style] ?? DEFAULT_STYLE_OPTIONS;
	const needsHourCycle = !STYLES_WITHOUT_HOUR_CYCLE.has(style);
	const options = needsHourCycle ? {...baseOptions, hour12: hour12 ?? localeUses12Hour(locale)} : baseOptions;
	return getDateFormatter(locale, options).format(date);
}
