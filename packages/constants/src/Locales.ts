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
