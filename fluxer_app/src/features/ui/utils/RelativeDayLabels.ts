// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveFormattingLocale} from '@fluxer/constants/src/Locales';

const rtfCache = new Map<string, Intl.RelativeTimeFormat>();

export function getRelativeTimeFormat(locale: string): Intl.RelativeTimeFormat {
	const cached = rtfCache.get(locale);
	if (cached !== undefined) {
		return cached;
	}
	const formatter = new Intl.RelativeTimeFormat(resolveFormattingLocale(locale), {numeric: 'auto'});
	rtfCache.set(locale, formatter);
	return formatter;
}

function capitalizeFirst(value: string, locale: string): string {
	if (value.length === 0) return value;
	const codePoint = value.codePointAt(0);
	if (codePoint === undefined) return value;
	const first = String.fromCodePoint(codePoint);
	return first.toLocaleUpperCase(locale) + value.slice(first.length);
}

export function getRelativeDayLabelLower(locale: string, dayOffset: number): string {
	return getRelativeTimeFormat(locale).format(dayOffset, 'day');
}

export function getRelativeDayLabelCapitalized(locale: string, dayOffset: number): string {
	return capitalizeFirst(getRelativeDayLabelLower(locale, dayOffset), locale);
}
