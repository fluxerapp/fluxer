// SPDX-License-Identifier: AGPL-3.0-or-later

import {type LocaleCode, Locales} from '@fluxer/constants/src/Locales';
import {parseAcceptLanguage} from '@pkgs/locale/src/resolution/AcceptLanguageNegotiation';
import {describe, expect, it} from 'vitest';

interface AcceptLanguageCase {
	header: string | null | undefined;
	expected: LocaleCode;
}

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
	describe('parseAcceptLanguage', () => {
		for (const {header, expected} of acceptLanguageCases) {
			it(`selects ${expected} for ${header ?? 'nullish header'}`, () => {
				expect(parseAcceptLanguage(header)).toBe(expected);
			});
		}
	});
});
