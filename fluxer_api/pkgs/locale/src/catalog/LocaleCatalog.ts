// SPDX-License-Identifier: AGPL-3.0-or-later

import {AllLocales, type LocaleCode, Locales} from '@fluxer/constants/src/Locales';

interface LocaleMetadata {
	displayName: string;
	flagCode: string;
	aliases?: ReadonlyArray<string>;
}

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
const LOCALE_METADATA_BY_CODE: Record<LocaleCode, LocaleMetadata> = {
	[Locales.AR]: {
		displayName: 'العربية',
		flagCode: '1f1f8-1f1e6',
	},
	[Locales.BG]: {
		displayName: 'Български',
		flagCode: '1f1e7-1f1ec',
	},
	[Locales.CS]: {
		displayName: 'Čeština',
		flagCode: '1f1e8-1f1ff',
	},
	[Locales.DA]: {
		displayName: 'Dansk',
		flagCode: '1f1e9-1f1f0',
	},
	[Locales.DE]: {
		displayName: 'Deutsch',
		flagCode: '1f1e9-1f1ea',
	},
	[Locales.EL]: {
		displayName: 'Ελληνικά',
		flagCode: '1f1ec-1f1f7',
	},
	[Locales.EN_GB]: {
		displayName: 'English',
		flagCode: '1f1ec-1f1e7',
	},
	[Locales.EN_US]: {
		displayName: 'English (US)',
		flagCode: '1f1fa-1f1f8',
		aliases: ['en'],
	},
	[Locales.ES_ES]: {
		displayName: 'Español (España)',
		flagCode: '1f1ea-1f1f8',
	},
	[Locales.ES_419]: {
		displayName: 'Español (Latinoamérica)',
		flagCode: '1f30e',
	},
	[Locales.FI]: {
		displayName: 'Suomi',
		flagCode: '1f1eb-1f1ee',
	},
	[Locales.FR]: {
		displayName: 'Français',
		flagCode: '1f1eb-1f1f7',
	},
	[Locales.HE]: {
		displayName: 'עברית',
		flagCode: '1f1ee-1f1f1',
	},
	[Locales.HI]: {
		displayName: 'हिन्दी',
		flagCode: '1f1ee-1f1f3',
	},
	[Locales.HR]: {
		displayName: 'Hrvatski',
		flagCode: '1f1ed-1f1f7',
	},
	[Locales.HU]: {
		displayName: 'Magyar',
		flagCode: '1f1ed-1f1fa',
	},
	[Locales.ID]: {
		displayName: 'Bahasa Indonesia',
		flagCode: '1f1ee-1f1e9',
	},
	[Locales.IT]: {
		displayName: 'Italiano',
		flagCode: '1f1ee-1f1f9',
	},
	[Locales.JA]: {
		displayName: '日本語',
		flagCode: '1f1ef-1f1f5',
	},
	[Locales.KO]: {
		displayName: '한국어',
		flagCode: '1f1f0-1f1f7',
	},
	[Locales.LT]: {
		displayName: 'Lietuvių',
		flagCode: '1f1f1-1f1f9',
	},
	[Locales.NL]: {
		displayName: 'Nederlands',
		flagCode: '1f1f3-1f1f1',
	},
	[Locales.NO]: {
		displayName: 'Norsk',
		flagCode: '1f1f3-1f1f4',
		aliases: ['nb', 'nn'],
	},
	[Locales.PL]: {
		displayName: 'Polski',
		flagCode: '1f1f5-1f1f1',
	},
	[Locales.PT_BR]: {
		displayName: 'Português (Brasil)',
		flagCode: '1f1e7-1f1f7',
	},
	[Locales.RO]: {
		displayName: 'Română',
		flagCode: '1f1f7-1f1f4',
	},
	[Locales.RU]: {
		displayName: 'Русский',
		flagCode: '1f1f7-1f1fa',
	},
	[Locales.SV_SE]: {
		displayName: 'Svenska',
		flagCode: '1f1f8-1f1ea',
		aliases: ['sv'],
	},
	[Locales.TH]: {
		displayName: 'ไทย',
		flagCode: '1f1f9-1f1ed',
	},
	[Locales.TR]: {
		displayName: 'Türkçe',
		flagCode: '1f1f9-1f1f7',
	},
	[Locales.UK]: {
		displayName: 'Українська',
		flagCode: '1f1fa-1f1e6',
	},
	[Locales.VI]: {
		displayName: 'Tiếng Việt',
		flagCode: '1f1fb-1f1f3',
	},
	[Locales.ZH_CN]: {
		displayName: '简体中文',
		flagCode: '1f1e8-1f1f3',
	},
	[Locales.ZH_TW]: {
		displayName: '繁體中文',
		flagCode: '1f1f9-1f1fc',
	},
};
const NORMALIZED_LOCALE_TO_CODE = createNormalizedLocaleLookup();

export function normalizeLocaleCode(code: string): string {
	return code.trim().replace(/_/g, '-').toLowerCase();
}

function isSupportedLocale(locale: LocaleCode): boolean {
	return SUPPORTED_LOCALE_SET.has(locale);
}

export function getLocaleDisplayName(locale: LocaleCode): string {
	return LOCALE_METADATA_BY_CODE[locale].displayName;
}

export function getLocaleFlagCode(locale: LocaleCode): string {
	return LOCALE_METADATA_BY_CODE[locale].flagCode;
}

export function getLocaleByCode(code: string): LocaleCode | null {
	const normalizedCode = normalizeLocaleCode(code);
	if (!normalizedCode) {
		return null;
	}
	const locale = NORMALIZED_LOCALE_TO_CODE.get(normalizedCode);
	return locale ?? null;
}

export function getLocaleLanguageCode(locale: LocaleCode): string {
	return locale.split('-')[0];
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
		const aliases = LOCALE_METADATA_BY_CODE[locale].aliases ?? [];
		for (const alias of aliases) {
			lookup.set(normalizeLocaleCode(alias), locale);
		}
	}
	return lookup;
}
