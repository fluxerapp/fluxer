// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveFormattingLocale} from '@fluxer/constants/src/Locales';

const formatterCache = new Map<string, Intl.DateTimeFormat>();

function buildCacheKey(locale: string, options: Intl.DateTimeFormatOptions): string {
	return `${locale}:${JSON.stringify(options)}`;
}

export function getDateFormatter(locale: string, options: Intl.DateTimeFormatOptions = {}): Intl.DateTimeFormat {
	const key = buildCacheKey(locale, options);
	const cached = formatterCache.get(key);
	if (cached !== undefined) {
		return cached;
	}
	const formatter = new Intl.DateTimeFormat(resolveFormattingLocale(locale), options);
	formatterCache.set(key, formatter);
	return formatter;
}
