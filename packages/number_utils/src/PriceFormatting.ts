// SPDX-License-Identifier: AGPL-3.0-or-later

import {type LocaleCode, Locales} from '@fluxer/constants/src/Locales';
import {DEFAULT_NUMBER_LOCALE} from '@fluxer/number_utils/src/NumberConstants';
import {getNumberFormatter} from '@fluxer/number_utils/src/NumberFormatterCache';

const SIGN_SYMBOLS: Readonly<Record<string, string>> = {
	BRL: 'R$',
	EUR: '€',
	INR: '₹',
	TRY: '₺',
	USD: '$',
};

const WORD_SYMBOLS: Readonly<Record<string, string>> = {
	DKK: 'kr',
	ISK: 'kr',
	NOK: 'kr',
	PLN: 'zł',
	SEK: 'kr',
};

const DANISH_KRONE_ABBREVIATION = 'kr.';

const OWN_KRONE_BY_LANGUAGE: Readonly<Record<string, string | null>> = {
	da: 'DKK',
	en: null,
	nb: 'NOK',
	nn: 'NOK',
	no: 'NOK',
	sv: 'SEK',
};

const UNIQUE_KRONE_SYMBOLS_BY_LANGUAGE: Readonly<Record<string, Readonly<Record<string, string>>>> = {
	sv: {DKK: 'Dkr', NOK: 'Nkr'},
};

const QUALIFIED_DOLLAR_BY_LOCALE: Readonly<Partial<Record<LocaleCode, string>>> = {
	[Locales.AR]: 'US$',
	[Locales.EN_GB]: 'US$',
	[Locales.ES_ES]: 'US$',
	[Locales.ES_419]: 'US$',
	[Locales.FR]: '$US',
	[Locales.PT_BR]: 'US$',
	[Locales.ZH_TW]: 'US$',
};

const HOME_COUNTRIES_BY_CURRENCY: Readonly<Record<string, ReadonlySet<string>>> = {
	DKK: new Set(['DK', 'FO', 'GL']),
	ISK: new Set(['IS']),
	NOK: new Set(['BV', 'NO', 'SJ']),
	SEK: new Set(['SE']),
	USD: new Set([
		'AS',
		'BQ',
		'EC',
		'FM',
		'GU',
		'IO',
		'MH',
		'MP',
		'PA',
		'PR',
		'PW',
		'SV',
		'TC',
		'TL',
		'UM',
		'US',
		'VG',
		'VI',
	]),
};

const STRIPE_TWO_DECIMAL_WHOLE_UNIT_CURRENCIES = new Set(['ISK', 'UGX']);
const STRIPE_TWO_DECIMAL_CURRENCIES = new Set(['HUF']);
const RIGHT_TO_LEFT_LANGUAGES = new Set(['ar', 'he']);
const BIDI_MARKS = /[\u200E\u200F\u061C]/g;
const LEFT_TO_RIGHT_ISOLATE = '\u2066';
const POP_DIRECTIONAL_ISOLATE = '\u2069';
const NO_BREAK_SPACE = '\u00A0';

const localeByLowerCaseTag = new Map<string, LocaleCode>(
	Object.values(Locales).map((locale) => [locale.toLowerCase(), locale]),
);

function resolveLocale(locale: string | null | undefined): string {
	if (!locale) {
		return DEFAULT_NUMBER_LOCALE;
	}
	const tag = locale.replaceAll('_', '-');
	try {
		return Intl.NumberFormat.supportedLocalesOf(tag).length > 0 ? tag : DEFAULT_NUMBER_LOCALE;
	} catch (error) {
		if (error instanceof RangeError) {
			return DEFAULT_NUMBER_LOCALE;
		}
		throw error;
	}
}

function findSupportedLocale(locale: string): LocaleCode | null {
	const tag = locale.toLowerCase();
	return localeByLowerCaseTag.get(tag) ?? localeByLowerCaseTag.get(tag.split('-')[0]) ?? null;
}

function currencyFractionDigits(currency: string): number {
	return (
		getNumberFormatter(DEFAULT_NUMBER_LOCALE, {style: 'currency', currency}).resolvedOptions().maximumFractionDigits ??
		2
	);
}

function isHomeCurrency(currency: string, countryCode: string | null | undefined): boolean {
	return countryCode != null && (HOME_COUNTRIES_BY_CURRENCY[currency]?.has(countryCode.toUpperCase()) ?? false);
}

function signSymbol(
	currency: string,
	locale: LocaleCode | null,
	countryCode: string | null | undefined,
): string | undefined {
	if (currency === 'USD' && locale && !isHomeCurrency(currency, countryCode)) {
		return QUALIFIED_DOLLAR_BY_LOCALE[locale] ?? SIGN_SYMBOLS.USD;
	}
	return SIGN_SYMBOLS[currency];
}

function wordSymbolFor(
	currency: string,
	language: string,
	locale: LocaleCode | null,
	countryCode: string | null | undefined,
): string | null {
	const wordSymbol = WORD_SYMBOLS[currency];
	if (
		wordSymbol !== 'kr' ||
		isHomeCurrency(currency, countryCode) ||
		!Object.hasOwn(OWN_KRONE_BY_LANGUAGE, language) ||
		OWN_KRONE_BY_LANGUAGE[language] === currency
	) {
		return wordSymbol === 'kr' && locale === Locales.DA ? DANISH_KRONE_ABBREVIATION : wordSymbol;
	}
	return UNIQUE_KRONE_SYMBOLS_BY_LANGUAGE[language]?.[currency] ?? null;
}

function formatWithCurrency(
	amount: number,
	currency: string,
	locale: string,
	digits: number,
	countryCode: string | null | undefined,
): string {
	const supportedLocale = findSupportedLocale(locale);
	if (WORD_SYMBOLS[currency]) {
		const symbol = wordSymbolFor(currency, locale.split('-')[0].toLowerCase(), supportedLocale, countryCode);
		if (!symbol) {
			return getNumberFormatter(locale, {
				style: 'currency',
				currency,
				currencyDisplay: 'code',
				minimumFractionDigits: digits,
				maximumFractionDigits: digits,
			}).format(amount);
		}
		const number = getNumberFormatter(locale, {minimumFractionDigits: digits, maximumFractionDigits: digits}).format(
			amount,
		);
		return `${number}${NO_BREAK_SPACE}${symbol}`;
	}
	const symbol = signSymbol(currency, supportedLocale, countryCode);
	const parts = getNumberFormatter(locale, {
		style: 'currency',
		currency,
		currencyDisplay: symbol ? 'narrowSymbol' : 'symbol',
		minimumFractionDigits: digits,
		maximumFractionDigits: digits,
	}).formatToParts(amount);
	return parts.map((part) => (part.type === 'currency' && symbol ? symbol : part.value)).join('');
}

export function formatMinorUnitPrice(
	amountMinor: number,
	currency: string,
	locale: string | null | undefined,
	countryCode?: string | null,
): string;
export function formatMinorUnitPrice(
	amountMinor: number | null | undefined,
	currency: string | null | undefined,
	locale: string | null | undefined,
	countryCode?: string | null,
): string | null;
export function formatMinorUnitPrice(
	amountMinor: number | null | undefined,
	currency: string | null | undefined,
	locale: string | null | undefined,
	countryCode?: string | null,
): string | null {
	if (amountMinor == null || !currency) {
		return null;
	}
	return formatPrice(amountMinor, currency, locale, countryCode, false);
}

export function formatMinorUnitPriceWithFraction(
	amountMinor: number,
	currency: string,
	locale: string | null | undefined,
): string {
	return formatPrice(amountMinor, currency, locale, null, true);
}

function formatPrice(
	amountMinor: number,
	currency: string,
	locale: string | null | undefined,
	countryCode: string | null | undefined,
	keepFraction: boolean,
): string {
	const code = currency.toUpperCase();
	const resolvedLocale = resolveLocale(locale);
	const wholeUnitDisplay = STRIPE_TWO_DECIMAL_WHOLE_UNIT_CURRENCIES.has(code);
	const twoDecimal = STRIPE_TWO_DECIMAL_CURRENCIES.has(code);
	const exponent = wholeUnitDisplay || twoDecimal ? 2 : currencyFractionDigits(code);
	const fractionDigits = wholeUnitDisplay ? 0 : exponent;
	const digits = !keepFraction && amountMinor % 10 ** exponent === 0 ? 0 : fractionDigits;
	const formatted = formatWithCurrency(amountMinor / 10 ** exponent, code, resolvedLocale, digits, countryCode);
	if (!RIGHT_TO_LEFT_LANGUAGES.has(resolvedLocale.split(/[-_]/)[0].toLowerCase())) {
		return formatted;
	}
	return `${LEFT_TO_RIGHT_ISOLATE}${formatted.replace(BIDI_MARKS, '')}${POP_DIRECTIONAL_ISOLATE}`;
}
