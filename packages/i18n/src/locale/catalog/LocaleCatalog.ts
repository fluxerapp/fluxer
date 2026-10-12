// SPDX-License-Identifier: AGPL-3.0-or-later

import {AllLocales, type LocaleCode, Locales} from '@fluxer/constants/src/Locales';

export const DEFAULT_LOCALE: LocaleCode = Locales.EN_US;
const SUPPORTED_LOCALES: ReadonlyArray<LocaleCode> = AllLocales;
const SUPPORTED_LOCALE_SET: ReadonlySet<LocaleCode> = new Set<LocaleCode>(SUPPORTED_LOCALES);
const LANGUAGE_FALLBACK_BY_LANGUAGE_CODE: Record<string, LocaleCode> = {
	en: Locales.EN_US,
	es: Locales.ES_ES,
	nb: Locales.NO,
	nn: Locales.NO,
	no: Locales.NO,
	pt: Locales.PT_BR,
	zh: Locales.ZH_CN,
	sv: Locales.SV_SE,
};
const LOCALE_ALIASES: Partial<Record<LocaleCode, ReadonlyArray<string>>> = {
	[Locales.EN_US]: ['en'],
	[Locales.NO]: ['nb', 'nn'],
	[Locales.SV_SE]: ['sv'],
};
const NORMALIZED_LOCALE_TO_CODE = createNormalizedLocaleLookup();

export function normalizeLocaleCode(code: string): string {
	return code.trim().replace(/_/g, '-').toLowerCase();
}

function isSupportedLocale(locale: LocaleCode): boolean {
	return SUPPORTED_LOCALE_SET.has(locale);
}

export function getLocaleByCode(code: string): LocaleCode | null {
	const normalizedCode = normalizeLocaleCode(code);
	if (!normalizedCode) {
		return null;
	}
	const locale = NORMALIZED_LOCALE_TO_CODE.get(normalizedCode);
	return locale ?? null;
}

export function getPreferredLocaleForLanguageCode(languageCode: string): LocaleCode | null {
	const normalizedLanguageCode = normalizeLocaleCode(languageCode);
	if (!normalizedLanguageCode) {
		return null;
	}
	const preferredLocale = LANGUAGE_FALLBACK_BY_LANGUAGE_CODE[normalizedLanguageCode];
	if (!preferredLocale) {
		return null;
	}
	if (!isSupportedLocale(preferredLocale)) {
		return null;
	}
	return preferredLocale;
}

export function findLocaleByLanguagePrefix(languageCode: string): LocaleCode | null {
	const normalizedLanguageCode = normalizeLocaleCode(languageCode);
	if (!normalizedLanguageCode) {
		return null;
	}
	const languagePrefix = `${normalizedLanguageCode}-`;
	for (const locale of SUPPORTED_LOCALES) {
		if (locale.toLowerCase().startsWith(languagePrefix)) {
			return locale;
		}
	}
	return null;
}

function createNormalizedLocaleLookup(): ReadonlyMap<string, LocaleCode> {
	const lookup = new Map<string, LocaleCode>();
	for (const locale of SUPPORTED_LOCALES) {
		lookup.set(normalizeLocaleCode(locale), locale);
	}
	for (const locale of SUPPORTED_LOCALES) {
		const aliases = LOCALE_ALIASES[locale] ?? [];
		for (const alias of aliases) {
			lookup.set(normalizeLocaleCode(alias), locale);
		}
	}
	return lookup;
}
