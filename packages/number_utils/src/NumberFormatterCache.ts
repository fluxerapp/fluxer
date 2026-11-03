// SPDX-License-Identifier: AGPL-3.0-or-later

const numberFormatterCache = new Map<string, Intl.NumberFormat>();

export function getNumberFormatter(
	locale: string,
	numberFormatOptions: Intl.NumberFormatOptions = {},
): Intl.NumberFormat {
	const cacheKey = buildFormatterCacheKey(locale, numberFormatOptions);
	const cachedFormatter = numberFormatterCache.get(cacheKey);
	if (cachedFormatter !== undefined) {
		return cachedFormatter;
	}
	const formatter = new Intl.NumberFormat(locale, numberFormatOptions);
	numberFormatterCache.set(cacheKey, formatter);
	return formatter;
}

function buildFormatterCacheKey(locale: string, numberFormatOptions: Intl.NumberFormatOptions): string {
	const optionEntries = Object.entries(numberFormatOptions)
		.filter(([, value]) => value !== undefined)
		.sort(([left], [right]) => left.localeCompare(right));
	const optionKey = optionEntries.map(([key, value]) => `${key}:${String(value)}`).join('|');
	return `${locale}|${optionKey}`;
}
