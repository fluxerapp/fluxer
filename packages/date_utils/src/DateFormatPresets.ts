// SPDX-License-Identifier: AGPL-3.0-or-later

import {type TimestampFormatType, TimestampFormatTypes} from '@fluxer/constants/src/UserConstants';
import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import {parseDate} from '@fluxer/date_utils/src/DateParsing';
import type {DateInput} from '@fluxer/date_utils/src/DateTypes';

export type DateFormatPreset = Exclude<TimestampFormatType, typeof TimestampFormatTypes.AUTO>;

export function toDateFormatPreset(value: number): DateFormatPreset | null {
	switch (value) {
		case TimestampFormatTypes.ISO_8601:
		case TimestampFormatTypes.DAY_MONTH_YEAR:
		case TimestampFormatTypes.MONTH_DAY_YEAR:
			return value;
		default:
			return null;
	}
}

function padTwo(value: number): string {
	return value.toString().padStart(2, '0');
}

function getPart(parts: Array<Intl.DateTimeFormatPart>, type: Intl.DateTimeFormatPartTypes): string {
	return parts.find((part) => part.type === type)?.value ?? '';
}

export function formatPresetDate(timestamp: DateInput, preset: DateFormatPreset, locale: string): string {
	const date = parseDate(timestamp);
	if (preset === TimestampFormatTypes.ISO_8601) {
		return `${date.getFullYear()}-${padTwo(date.getMonth() + 1)}-${padTwo(date.getDate())}`;
	}
	const parts = getDateFormatter(locale, {day: '2-digit', month: '2-digit', year: 'numeric'}).formatToParts(date);
	const day = getPart(parts, 'day');
	const month = getPart(parts, 'month');
	const year = getPart(parts, 'year');
	return preset === TimestampFormatTypes.DAY_MONTH_YEAR ? `${day}/${month}/${year}` : `${month}/${day}/${year}`;
}

export function formatPresetTime(
	timestamp: DateInput,
	preset: DateFormatPreset,
	locale: string,
	hour12: boolean,
	withSeconds = false,
): string {
	const isIso = preset === TimestampFormatTypes.ISO_8601;
	return getDateFormatter(locale, {
		hour: isIso ? '2-digit' : 'numeric',
		minute: '2-digit',
		...(withSeconds ? {second: '2-digit'} : {}),
		hour12,
		...(isIso ? {numberingSystem: 'latn'} : {}),
	}).format(parseDate(timestamp));
}

export function formatPresetDateTime(
	timestamp: DateInput,
	preset: DateFormatPreset,
	locale: string,
	hour12: boolean,
	withSeconds = false,
): string {
	const date = parseDate(timestamp);
	return `${formatPresetDate(date, preset, locale)} ${formatPresetTime(date, preset, locale, hour12, withSeconds)}`;
}
