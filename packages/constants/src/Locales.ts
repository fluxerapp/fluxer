// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const Locales = {
	AR: 'ar',
	BE: 'be',
	BG: 'bg',
	BN: 'bn',
	CA: 'ca',
	CS: 'cs',
	DA: 'da',
	DE: 'de',
	EL: 'el',
	EN_GB: 'en-GB',
	EN_US: 'en-US',
	ES_ES: 'es-ES',
	ES_419: 'es-419',
	FI: 'fi',
	FIL: 'fil',
	FR: 'fr',
	HE: 'he',
	HI: 'hi',
	HR: 'hr',
	HU: 'hu',
	ID: 'id',
	IT: 'it',
	JA: 'ja',
	KO: 'ko',
	LT: 'lt',
	LV: 'lv',
	NL: 'nl',
	NO: 'no',
	PL: 'pl',
	PT_BR: 'pt-BR',
	PT_PT: 'pt-PT',
	RO: 'ro',
	RU: 'ru',
	SK: 'sk',
	SL: 'sl',
	SQ: 'sq',
	SV_SE: 'sv-SE',
	TH: 'th',
	TOK: 'tok',
	TR: 'tr',
	UK: 'uk',
	VI: 'vi',
	ZH_CN: 'zh-CN',
	ZH_TW: 'zh-TW',
} as const;

export type LocaleCode = ValueOf<typeof Locales>;

export const AllLocales: ReadonlyArray<LocaleCode> = Object.values(Locales);

const FORMATTING_FALLBACK_LOCALE = 'en-US';
const formattingLocaleCache = new Map<string, string>();

function isFormattingLocaleSupported(locale: string): boolean {
	try {
		return locale !== '' && Intl.DateTimeFormat.supportedLocalesOf(locale).length > 0;
	} catch {
		return false;
	}
}

export function resolveFormattingLocale(locale: string): string {
	const cached = formattingLocaleCache.get(locale);
	if (cached !== undefined) {
		return cached;
	}
	const resolved = isFormattingLocaleSupported(locale) ? locale : FORMATTING_FALLBACK_LOCALE;
	formattingLocaleCache.set(locale, resolved);
	return resolved;
}

export const REGIONAL_FORMAT_SYSTEM = 'system';
export const REGIONAL_FORMAT_PATTERN = /^(?:system|[a-z]{2,3}-(?:[A-Z]{2}|\d{3}))$/;

export const RegionalFormatLocales: ReadonlyArray<string> = [
	'ar-AE',
	'ar-EG',
	'ar-SA',
	'be-BY',
	'bg-BG',
	'bn-BD',
	'bn-IN',
	'ca-ES',
	'cs-CZ',
	'da-DK',
	'de-AT',
	'de-CH',
	'de-DE',
	'el-GR',
	'en-AU',
	'en-CA',
	'en-GB',
	'en-IE',
	'en-IN',
	'en-NZ',
	'en-PH',
	'en-SG',
	'en-US',
	'en-ZA',
	'es-419',
	'es-AR',
	'es-CL',
	'es-CO',
	'es-ES',
	'es-MX',
	'es-US',
	'et-EE',
	'fi-FI',
	'fil-PH',
	'fr-BE',
	'fr-CA',
	'fr-CH',
	'fr-FR',
	'he-IL',
	'hi-IN',
	'hr-HR',
	'hu-HU',
	'id-ID',
	'is-IS',
	'it-CH',
	'it-IT',
	'ja-JP',
	'ko-KR',
	'lt-LT',
	'lv-LV',
	'ms-MY',
	'nb-NO',
	'nl-BE',
	'nl-NL',
	'pl-PL',
	'pt-BR',
	'pt-PT',
	'ro-RO',
	'ru-RU',
	'sk-SK',
	'sl-SI',
	'sq-AL',
	'sr-RS',
	'sv-FI',
	'sv-SE',
	'th-TH',
	'tr-TR',
	'uk-UA',
	'vi-VN',
	'zh-CN',
	'zh-HK',
	'zh-TW',
];

export function resolveRegionalFormatLocale(
	regionalFormat: string | null,
	language: string,
	systemLocale: string,
): string {
	if (regionalFormat === null) {
		return resolveFormattingLocale(language);
	}
	if (regionalFormat === REGIONAL_FORMAT_SYSTEM) {
		return resolveFormattingLocale(systemLocale);
	}
	return resolveFormattingLocale(regionalFormat);
}
