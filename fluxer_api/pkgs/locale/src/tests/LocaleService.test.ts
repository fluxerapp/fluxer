// SPDX-License-Identifier: AGPL-3.0-or-later

import {AllLocales, type LocaleCode, Locales} from '@fluxer/constants/src/Locales';
import {
	getFlagCode,
	getLocaleCode,
	getLocaleFromCode,
	getLocaleMetadata,
	getLocaleName,
	parseAcceptLanguage,
} from '@pkgs/locale/src/LocaleService';
import {describe, expect, it} from 'vitest';

interface LocaleLookupCase {
	input: string;
	expected: LocaleCode | null;
}

interface AcceptLanguageCase {
	header: string | null | undefined;
	expected: LocaleCode;
}

const localeNameCases: Array<{
	locale: LocaleCode;
	expected: string;
}> = [
	{locale: Locales.EN_US, expected: 'English (US)'},
	{locale: Locales.EN_GB, expected: 'English'},
	{locale: Locales.DE, expected: 'Deutsch'},
	{locale: Locales.JA, expected: '日本語'},
	{locale: Locales.ZH_CN, expected: '简体中文'},
	{locale: Locales.ZH_TW, expected: '繁體中文'},
	{locale: Locales.ES_419, expected: 'Español (Latinoamérica)'},
	{locale: Locales.PT_BR, expected: 'Português (Brasil)'},
];
const flagCodeCases: Array<{
	locale: LocaleCode;
	expected: string;
}> = [
	{locale: Locales.EN_US, expected: '1f1fa-1f1f8'},
	{locale: Locales.EN_GB, expected: '1f1ec-1f1e7'},
	{locale: Locales.DE, expected: '1f1e9-1f1ea'},
	{locale: Locales.JA, expected: '1f1ef-1f1f5'},
	{locale: Locales.ZH_CN, expected: '1f1e8-1f1f3'},
	{locale: Locales.ES_419, expected: '1f30e'},
	{locale: Locales.PT_BR, expected: '1f1e7-1f1f7'},
];
const localeLookupCases: Array<LocaleLookupCase> = [
	{input: 'en', expected: Locales.EN_US},
	{input: 'en-US', expected: Locales.EN_US},
	{input: 'en-us', expected: Locales.EN_US},
	{input: 'en_GB', expected: Locales.EN_GB},
	{input: 'en-gb', expected: Locales.EN_GB},
	{input: 'de', expected: Locales.DE},
	{input: 'ja', expected: Locales.JA},
	{input: 'zh-CN', expected: Locales.ZH_CN},
	{input: 'zh-cn', expected: Locales.ZH_CN},
	{input: 'zh-TW', expected: Locales.ZH_TW},
	{input: 'zh-tw', expected: Locales.ZH_TW},
	{input: 'es-419', expected: Locales.ES_419},
	{input: 'pt-br', expected: Locales.PT_BR},
	{input: 'sv', expected: Locales.SV_SE},
	{input: 'sv-se', expected: Locales.SV_SE},
	{input: 'nb', expected: Locales.NO},
	{input: 'nn', expected: Locales.NO},
	{input: 'NB', expected: Locales.NO},
	{input: 'xx', expected: null},
	{input: '', expected: null},
	{input: 'en-AU', expected: null},
];
const localeCodeCases: Array<{
	locale: LocaleCode;
	expected: string;
}> = [
	{locale: Locales.EN_US, expected: 'en'},
	{locale: Locales.EN_GB, expected: 'en'},
	{locale: Locales.DE, expected: 'de'},
	{locale: Locales.ZH_CN, expected: 'zh'},
	{locale: Locales.ZH_TW, expected: 'zh'},
	{locale: Locales.ES_ES, expected: 'es'},
	{locale: Locales.ES_419, expected: 'es'},
	{locale: Locales.PT_BR, expected: 'pt'},
	{locale: Locales.SV_SE, expected: 'sv'},
	{locale: Locales.JA, expected: 'ja'},
];
const acceptLanguageCases: Array<AcceptLanguageCase> = [
	{header: null, expected: Locales.EN_US},
	{header: undefined, expected: Locales.EN_US},
	{header: '', expected: Locales.EN_US},
	{header: 'de', expected: Locales.DE},
	{header: 'en-GB', expected: Locales.EN_GB},
	{header: 'en-gb', expected: Locales.EN_GB},
	{header: 'de;q=0.9, fr;q=1.0', expected: Locales.FR},
	{header: 'en-US,en;q=0.9,de;q=0.8', expected: Locales.EN_US},
	{header: 'en-AU', expected: Locales.EN_US},
	{header: 'es', expected: Locales.ES_ES},
	{header: 'pt', expected: Locales.PT_BR},
	{header: 'zh', expected: Locales.ZH_CN},
	{header: 'sv', expected: Locales.SV_SE},
	{header: 'de;q=0.5, ja;q=0.9, fr;q=0.7', expected: Locales.JA},
	{header: 'de, fr;q=0.5', expected: Locales.DE},
	{header: 'xx-YY', expected: Locales.EN_US},
	{header: 'fr-CA', expected: Locales.FR},
	{header: 'de-DE', expected: Locales.DE},
	{header: 'de-CH, fr-FR;q=0.8', expected: Locales.DE},
	{header: 'sv-FI', expected: Locales.SV_SE},
	{header: 'pt-PT', expected: Locales.PT_BR},
	{header: 'es-MX', expected: Locales.ES_ES},
	{header: 'nb', expected: Locales.NO},
	{header: 'nn', expected: Locales.NO},
	{header: 'nb-NO', expected: Locales.NO},
	{header: 'nn_NO', expected: Locales.NO},
	{header: 'no-NO', expected: Locales.NO},
	{header: 'nb-NO,nb;q=0.9,en-US;q=0.8,en;q=0.7', expected: Locales.NO},
	{header: 'zh-Hant', expected: Locales.ZH_TW},
	{header: 'zh-Hant-TW', expected: Locales.ZH_TW},
	{header: 'zh-Hant-HK', expected: Locales.ZH_TW},
	{header: 'zh-HK', expected: Locales.ZH_TW},
	{header: 'zh-MO', expected: Locales.ZH_TW},
	{header: 'zh_hant_tw', expected: Locales.ZH_TW},
	{header: 'zh-Hans', expected: Locales.ZH_CN},
	{header: 'zh-Hans-CN', expected: Locales.ZH_CN},
	{header: 'zh-SG', expected: Locales.ZH_CN},
	{header: 'zh-Hans-HK', expected: Locales.ZH_CN},
	{header: 'zh-Hant-CN', expected: Locales.ZH_TW},
	{header: 'zh-HK, zh-Hans;q=0.9', expected: Locales.ZH_TW},
	{header: 'zh-HK,zh;q=0.9', expected: Locales.ZH_TW},
	{header: '  de  ,  fr  ', expected: Locales.DE},
	{header: 'zh-TW', expected: Locales.ZH_TW},
	{header: 'zh-CN', expected: Locales.ZH_CN},
	{header: 'es-419', expected: Locales.ES_419},
	{header: 'pt-BR', expected: Locales.PT_BR},
	{header: 'sv-SE', expected: Locales.SV_SE},
	{header: 'xx-YY, zz-AA, qq-BB', expected: Locales.EN_US},
	{header: 'zh-TW, zh;q=0.9', expected: Locales.ZH_TW},
	{header: 'en-US,en;q=0.9,ja;q=0.8,de;q=0.7,fr;q=0.6', expected: Locales.EN_US},
	{header: 'de;q=0.999, fr;q=0.998', expected: Locales.DE},
	{header: 'de;q=0, fr;q=1', expected: Locales.FR},
];

describe('LocaleService', () => {
	describe('getLocaleMetadata', () => {
		it('returns consolidated metadata for a locale', () => {
			expect(getLocaleMetadata(Locales.EN_US)).toEqual({
				code: Locales.EN_US,
				languageCode: 'en',
				name: 'English (US)',
				flagCode: '1f1fa-1f1f8',
			});
		});
		it('returns metadata for every supported locale', () => {
			for (const locale of AllLocales) {
				const metadata = getLocaleMetadata(locale);
				expect(metadata.code).toBe(locale);
				expect(metadata.name.length).toBeGreaterThan(0);
				expect(metadata.flagCode.length).toBeGreaterThan(0);
			}
		});
	});
	describe('getLocaleName', () => {
		for (const {locale, expected} of localeNameCases) {
			it(`returns ${expected} for ${locale}`, () => {
				expect(getLocaleName(locale)).toBe(expected);
			});
		}
	});
	describe('getFlagCode', () => {
		for (const {locale, expected} of flagCodeCases) {
			it(`returns ${expected} for ${locale}`, () => {
				expect(getFlagCode(locale)).toBe(expected);
			});
		}
	});
	describe('getLocaleFromCode', () => {
		for (const {input, expected} of localeLookupCases) {
			it(`resolves ${input || 'empty'} to ${expected ?? 'null'}`, () => {
				expect(getLocaleFromCode(input)).toBe(expected);
			});
		}
		it('resolves canonical locale codes for all supported locales', () => {
			for (const locale of AllLocales) {
				expect(getLocaleFromCode(locale)).toBe(locale);
			}
		});
	});
	describe('getLocaleCode', () => {
		for (const {locale, expected} of localeCodeCases) {
			it(`returns ${expected} for ${locale}`, () => {
				expect(getLocaleCode(locale)).toBe(expected);
			});
		}
	});
	describe('parseAcceptLanguage', () => {
		for (const {header, expected} of acceptLanguageCases) {
			it(`selects ${expected} for ${header ?? 'nullish header'}`, () => {
				expect(parseAcceptLanguage(header)).toBe(expected);
			});
		}
	});
});
