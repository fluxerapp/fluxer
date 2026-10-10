// SPDX-License-Identifier: AGPL-3.0-or-later

const numberFormatCache = new Map<string, Intl.NumberFormat>();
const dateTimeFormatCache = new Map<string, Intl.DateTimeFormat>();
const collatorCache = new Map<string, Intl.Collator>();
const listFormatCache = new Map<string, Intl.ListFormat>();

function getCacheKey(locale: string | undefined, options: object | undefined): string {
	if (!options) {
		return locale ?? '';
	}
	return `${locale ?? ''}|${JSON.stringify(options, Object.keys(options).sort())}`;
}

export function getCachedNumberFormat(locale?: string, options?: Intl.NumberFormatOptions): Intl.NumberFormat {
	const key = getCacheKey(locale, options);
	let formatter = numberFormatCache.get(key);
	if (!formatter) {
		formatter = new Intl.NumberFormat(locale, options);
		numberFormatCache.set(key, formatter);
	}
	return formatter;
}

export function getCachedDateTimeFormat(locale?: string, options?: Intl.DateTimeFormatOptions): Intl.DateTimeFormat {
	const key = getCacheKey(locale, options);
	let formatter = dateTimeFormatCache.get(key);
	if (!formatter) {
		formatter = new Intl.DateTimeFormat(locale, options);
		dateTimeFormatCache.set(key, formatter);
	}
	return formatter;
}

export function getCachedCollator(locale?: string, options?: Intl.CollatorOptions): Intl.Collator {
	const key = getCacheKey(locale, options);
	let collator = collatorCache.get(key);
	if (!collator) {
		collator = new Intl.Collator(locale, options);
		collatorCache.set(key, collator);
	}
	return collator;
}

export function getCachedListFormat(locale: string, options: Intl.ListFormatOptions): Intl.ListFormat {
	const key = getCacheKey(locale, options);
	let formatter = listFormatCache.get(key);
	if (!formatter) {
		formatter = new Intl.ListFormat(locale, options);
		listFormatCache.set(key, formatter);
	}
	return formatter;
}

export function formatNumber(value: number, locale: string): string {
	return getCachedNumberFormat(locale).format(Number.isFinite(value) ? value : 0);
}

function formatListWithoutIntl(items: ReadonlyArray<string>, type: Intl.ListFormatType): string {
	if (type === 'unit') {
		return items.join(', ');
	}
	const conjunction = type === 'disjunction' ? 'or' : 'and';
	if (items.length === 2) {
		return `${items[0]} ${conjunction} ${items[1]}`;
	}
	return `${items.slice(0, -1).join(', ')}, ${conjunction} ${items[items.length - 1]}`;
}

function isListFormatLocaleSupported(locale: string): boolean {
	try {
		return Intl.ListFormat.supportedLocalesOf([locale]).length > 0;
	} catch {
		return false;
	}
}

export function formatList(
	items: ReadonlyArray<string>,
	options: {locale: string; style: Intl.ListFormatStyle; type: Intl.ListFormatType},
): string {
	if (items.length === 0) {
		return '';
	}
	if (items.length === 1) {
		return items[0] ?? '';
	}
	if (typeof Intl === 'undefined' || typeof Intl.ListFormat === 'undefined') {
		return formatListWithoutIntl(items, options.type);
	}
	const requestedLocale = options.locale.trim();
	const locale = requestedLocale !== '' && isListFormatLocaleSupported(requestedLocale) ? requestedLocale : 'en-US';
	try {
		return getCachedListFormat(locale, {style: options.style, type: options.type}).format(items);
	} catch {
		return formatListWithoutIntl(items, options.type);
	}
}
