// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StaticI18nConfig} from '@fluxer/i18n/src/runtime/CreateStaticI18n';

export function hasStaticLocale<TKey extends string, TValue, TVariables>(
	config: StaticI18nConfig<TKey, TValue, TVariables>,
	normalizedLocale: string,
): boolean {
	return (
		normalizedLocale === config.defaultLocale ||
		(Object.hasOwn(config.localeMessages, normalizedLocale) && config.localeMessages[normalizedLocale] !== undefined)
	);
}

export function getEffectiveStaticLocale<TKey extends string, TValue, TVariables>(
	config: StaticI18nConfig<TKey, TValue, TVariables>,
	locale: string | null | undefined,
): string {
	if (!locale) {
		return config.defaultLocale;
	}
	const normalizedLocale = config.normalizeLocale?.(locale) ?? locale;
	if (!hasStaticLocale(config, normalizedLocale)) {
		config.onWarning?.(`Unsupported locale, falling back to ${config.defaultLocale}: ${locale}`);
		return config.defaultLocale;
	}
	return normalizedLocale;
}
