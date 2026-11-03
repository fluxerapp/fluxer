// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';

declare const FLUXER_BUNDLED_LOCALES_DIR: string | undefined;

export const STATIC_I18N_LOCALES = [
	'ar',
	'bg',
	'cs',
	'da',
	'de',
	'el',
	'en-GB',
	'es-419',
	'es-ES',
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

export type StaticI18nLocale = (typeof STATIC_I18N_LOCALES)[number];

function catalogDirectory(catalog: string, sourceDirectory: URL): URL {
	if (typeof FLUXER_BUNDLED_LOCALES_DIR === 'string') {
		return new URL(`${FLUXER_BUNDLED_LOCALES_DIR}${catalog}/`, import.meta.url);
	}
	return sourceDirectory;
}

export function loadStaticLocaleFiles<T>(catalog: string, sourceDirectory: URL): Record<StaticI18nLocale, T> {
	const directory = catalogDirectory(catalog, sourceDirectory);
	const messages = {} as Record<StaticI18nLocale, T>;
	for (const locale of STATIC_I18N_LOCALES) {
		Object.defineProperty(messages, locale, {
			configurable: true,
			enumerable: true,
			get() {
				const value = JSON.parse(readFileSync(new URL(`${locale}.json`, directory), 'utf8')) as T;
				Object.defineProperty(messages, locale, {enumerable: true, value});
				return value;
			},
		});
	}
	return messages;
}
