// SPDX-License-Identifier: AGPL-3.0-or-later

import {messages as messagesEnUS} from '@app/features/i18n/locales/en-US/messages.mjs';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {getNativeLocaleIdentifier} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {noteLocale} from '@app/features/theme/fonts/ScriptFontLoader';
import {type I18n, i18n, type Messages} from '@lingui/core';
import {createAtom, runInAction} from 'mobx';

const supportedLocales = [
	'ar',
	'bg',
	'cs',
	'da',
	'de',
	'el',
	'en-GB',
	'en-US',
	'es-ES',
	'es-419',
	'fi',
	'fr',
	'he',
	'hi',
	'hr',
	'hu',
	'id',
	'it',
	'ja',
	'ko',
	'lt',
	'nl',
	'no',
	'pl',
	'pt-BR',
	'ro',
	'ru',
	'sv-SE',
	'th',
	'tr',
	'uk',
	'vi',
	'zh-CN',
	'zh-TW',
] as const;

type LocaleCode = (typeof supportedLocales)[number];

const DEFAULT_LOCALE: LocaleCode = 'en-US';
const supportedLocaleSet = new Set<LocaleCode>(supportedLocales);
const logger = new Logger('i18n');
const LANGUAGE_OVERRIDES: Record<string, LocaleCode> = {
	en: 'en-US',
	es: 'es-ES',
	nb: 'no',
	pt: 'pt-BR',
	sv: 'sv-SE',
	zh: 'zh-CN',
};

type LocaleLoader = () => Promise<{
	messages: Messages;
}>;

const loaders: Record<LocaleCode, LocaleLoader> = {
	ar: () => import('@app/features/i18n/locales/ar/messages.mjs'),
	bg: () => import('@app/features/i18n/locales/bg/messages.mjs'),
	cs: () => import('@app/features/i18n/locales/cs/messages.mjs'),
	da: () => import('@app/features/i18n/locales/da/messages.mjs'),
	de: () => import('@app/features/i18n/locales/de/messages.mjs'),
	el: () => import('@app/features/i18n/locales/el/messages.mjs'),
	'en-GB': () => import('@app/features/i18n/locales/en-GB/messages.mjs'),
	'en-US': () => Promise.resolve({messages: messagesEnUS}),
	'es-ES': () => import('@app/features/i18n/locales/es-ES/messages.mjs'),
	'es-419': () => import('@app/features/i18n/locales/es-419/messages.mjs'),
	fi: () => import('@app/features/i18n/locales/fi/messages.mjs'),
	fr: () => import('@app/features/i18n/locales/fr/messages.mjs'),
	he: () => import('@app/features/i18n/locales/he/messages.mjs'),
	hi: () => import('@app/features/i18n/locales/hi/messages.mjs'),
	hr: () => import('@app/features/i18n/locales/hr/messages.mjs'),
	hu: () => import('@app/features/i18n/locales/hu/messages.mjs'),
	id: () => import('@app/features/i18n/locales/id/messages.mjs'),
	it: () => import('@app/features/i18n/locales/it/messages.mjs'),
	ja: () => import('@app/features/i18n/locales/ja/messages.mjs'),
	ko: () => import('@app/features/i18n/locales/ko/messages.mjs'),
	lt: () => import('@app/features/i18n/locales/lt/messages.mjs'),
	nl: () => import('@app/features/i18n/locales/nl/messages.mjs'),
	no: () => import('@app/features/i18n/locales/no/messages.mjs'),
	pl: () => import('@app/features/i18n/locales/pl/messages.mjs'),
	'pt-BR': () => import('@app/features/i18n/locales/pt-BR/messages.mjs'),
	ro: () => import('@app/features/i18n/locales/ro/messages.mjs'),
	ru: () => import('@app/features/i18n/locales/ru/messages.mjs'),
	'sv-SE': () => import('@app/features/i18n/locales/sv-SE/messages.mjs'),
	th: () => import('@app/features/i18n/locales/th/messages.mjs'),
	tr: () => import('@app/features/i18n/locales/tr/messages.mjs'),
	uk: () => import('@app/features/i18n/locales/uk/messages.mjs'),
	vi: () => import('@app/features/i18n/locales/vi/messages.mjs'),
	'zh-CN': () => import('@app/features/i18n/locales/zh-CN/messages.mjs'),
	'zh-TW': () => import('@app/features/i18n/locales/zh-TW/messages.mjs'),
};

function formatLocaleValue(value: string): string {
	const trimmed = value.trim();
	if (!trimmed) {
		return '';
	}
	const segments = trimmed.split(/[-_]/).filter(Boolean);
	if (segments.length === 0) {
		return '';
	}
	const language = segments[0].toLowerCase();
	if (segments.length === 1) {
		return language;
	}
	const region = segments
		.slice(1)
		.map((segment) => segment.toUpperCase())
		.join('-');
	return `${language}-${region}`;
}

function resolveLocale(value: string): LocaleCode | null {
	const formatted = formatLocaleValue(value);
	if (!formatted) {
		return null;
	}
	if (supportedLocaleSet.has(formatted as LocaleCode)) {
		return formatted as LocaleCode;
	}
	const segments = formatted.split('-');
	const [language, scriptOrRegion, region] = segments;
	if (!language) {
		return null;
	}
	if (language === 'zh') {
		if (scriptOrRegion === 'HANT') {
			return 'zh-TW';
		}
		if (scriptOrRegion === 'HANS') {
			return 'zh-CN';
		}
		if (scriptOrRegion === 'HK' || scriptOrRegion === 'MO') {
			return 'zh-TW';
		}
	}
	const regionCode = scriptOrRegion?.length === 4 ? region : scriptOrRegion;
	if (regionCode) {
		const regionalLocale = `${language}-${regionCode}`;
		if (supportedLocaleSet.has(regionalLocale as LocaleCode)) {
			return regionalLocale as LocaleCode;
		}
	}
	const override = LANGUAGE_OVERRIDES[language];
	if (override) {
		return override;
	}
	const fallback = supportedLocales.find((code) => code.split('-')[0].toLowerCase() === language);
	if (fallback) {
		return fallback;
	}
	return null;
}

export function normalizeLocale(value?: string | null): LocaleCode {
	return value ? (resolveLocale(value) ?? DEFAULT_LOCALE) : DEFAULT_LOCALE;
}

function detectBrowserLocale(): LocaleCode | null {
	const preferredLanguages = navigator.languages?.length ? navigator.languages : [navigator.language];
	for (const language of preferredLanguages) {
		const locale = resolveLocale(language);
		if (locale) {
			return locale;
		}
	}
	return null;
}

function detectPreferredLocale(forceLocale?: string): LocaleCode {
	if (forceLocale) {
		return normalizeLocale(forceLocale);
	}
	const storedLocale = AppStorage.getItem('locale');
	if (storedLocale) {
		return normalizeLocale(storedLocale);
	}
	const nativeLocale = getNativeLocaleIdentifier();
	if (nativeLocale) {
		return normalizeLocale(nativeLocale);
	}
	const browserLocale = detectBrowserLocale();
	if (browserLocale) {
		return normalizeLocale(browserLocale);
	}
	return DEFAULT_LOCALE;
}

function activateLocale(localeCode: LocaleCode, messages: Messages): void {
	i18n.loadAndActivate({locale: localeCode, messages});
	if (typeof document !== 'undefined') {
		document.documentElement.lang = localeCode;
	}
	noteLocale(localeCode);
	AppStorage.setItem('locale', localeCode);
}

const inFlightLoads = new Map<LocaleCode, Promise<Messages>>();
let requestedLocale: LocaleCode | null = null;

export async function loadLocaleCatalog(localeCode: string): Promise<LocaleCode> {
	const normalized = normalizeLocale(localeCode);
	requestedLocale = normalized;
	if (normalized === i18n.locale) {
		return normalized;
	}
	let loadPromise = inFlightLoads.get(normalized);
	if (!loadPromise) {
		loadPromise = loadLazyModule(loaders[normalized])
			.then(({messages}) => messages)
			.finally(() => {
				inFlightLoads.delete(normalized);
			});
		inFlightLoads.set(normalized, loadPromise);
	}
	const messages = await loadPromise;
	if (requestedLocale === normalized && i18n.locale !== normalized) {
		activateLocale(normalized, messages);
	}
	return normalized;
}

export function applyLocaleChange(localeCode: string): LocaleCode {
	const normalized = normalizeLocale(localeCode);
	requestedLocale = normalized;
	if (normalized === i18n.locale) {
		AppStorage.setItem('locale', normalized);
		return normalized;
	}
	AppStorage.setItem('locale', normalized);
	void loadLocaleCatalog(normalized).catch((error) => {
		logger.error(`Failed to apply locale ${normalized}`, error);
	});
	return normalized;
}

let initPromise: Promise<typeof i18n> | null = null;

export async function initI18n(forceLocale?: string) {
	if (!initPromise) {
		initPromise = (async () => {
			try {
				const localeToLoad = detectPreferredLocale(forceLocale);
				await loadLocaleCatalog(localeToLoad);
			} catch (error) {
				logger.error('Failed to initialize i18n, falling back to default locale', error);
				activateLocale(DEFAULT_LOCALE, messagesEnUS);
			}
			return i18n;
		})();
	}
	return initPromise;
}

const localeAtom = createAtom('i18nLocale');
const localeSensitiveProps = new Set<PropertyKey>(['_', 't', 'date', 'number', 'locale', 'locales', 'messages']);
const boundMethods = new Map<PropertyKey, unknown>();

const reactiveI18n = new Proxy(i18n, {
	get(target, prop) {
		if (localeSensitiveProps.has(prop)) {
			localeAtom.reportObserved();
		}
		const value = Reflect.get(target, prop);
		if (typeof value !== 'function') {
			return value;
		}
		let bound = boundMethods.get(prop);
		if (!bound) {
			bound = value.bind(target);
			boundMethods.set(prop, bound);
		}
		return bound;
	},
}) as I18n;

i18n.on('change', () => {
	try {
		runInAction(() => {
			localeAtom.reportChanged();
		});
	} catch (error) {
		logger.error('Failed to invalidate locale atom:', error);
	}
});

export default reactiveI18n;
