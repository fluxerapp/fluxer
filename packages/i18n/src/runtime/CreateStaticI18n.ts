// SPDX-License-Identifier: AGPL-3.0-or-later

import type {I18nResult, TemplateCompiler} from '@fluxer/i18n/src/runtime/I18nTypes';
import {renderTemplate} from '@fluxer/i18n/src/runtime/RenderTemplate';
import {getEffectiveStaticLocale, hasStaticLocale} from '@fluxer/i18n/src/runtime/StaticLocale';
import type MessageFormat from '@messageformat/core';

export type StaticLocaleMessages<TKey extends string, TValue> = Partial<Record<TKey, TValue>>;

export interface StaticI18nConfig<TKey extends string, TValue, TVariables> {
	defaultLocale: string;
	defaultMessages: Record<TKey, TValue>;
	localeMessages: Record<string, StaticLocaleMessages<TKey, TValue>>;
	normalizeLocale?: (locale: string) => string;
	onWarning?: (message: string) => void;
	validateVariables?: (key: TKey, template: TValue, variables: TVariables) => string | null;
}

interface StaticI18nModule<TKey extends string, TValue, TVariables> {
	getTemplate(key: TKey, locale: string | null, variables: TVariables): I18nResult<TKey, TValue>;
	hasLocale(locale: string): boolean;
	getLoadedLocales(): Set<string>;
	reset(): void;
}

export function createStaticI18n<TKey extends string, TValue, TVariables>(
	config: StaticI18nConfig<TKey, TValue, TVariables>,
	compile: TemplateCompiler<TValue, TVariables>,
): StaticI18nModule<TKey, TValue, TVariables> {
	const loadedLocales = new Set<string>([config.defaultLocale]);
	const messageFormatCache = new Map<string, MessageFormat>();
	return {
		getTemplate(key: TKey, locale: string | null, variables: TVariables): I18nResult<TKey, TValue> {
			const effectiveLocale = getEffectiveStaticLocale(config, locale);
			loadedLocales.add(effectiveLocale);
			const sourceTemplate = Object.hasOwn(config.defaultMessages, key) ? config.defaultMessages[key] : undefined;
			if (sourceTemplate === undefined) {
				return {
					ok: false,
					error: {
						kind: 'missing-template',
						key,
						message: `Missing template ${key}`,
					},
					locale: effectiveLocale,
				};
			}
			const localeMessages = Object.hasOwn(config.localeMessages, effectiveLocale)
				? config.localeMessages[effectiveLocale]
				: undefined;
			const translatedTemplate =
				localeMessages != null && Object.hasOwn(localeMessages, key) ? localeMessages[key] : undefined;
			const template = translatedTemplate ?? sourceTemplate;
			const validationError = config.validateVariables?.(key, template, variables);
			if (validationError) {
				return {
					ok: false,
					error: {
						kind: 'invalid-variables',
						key,
						message: validationError,
					},
					locale: effectiveLocale,
				};
			}
			return renderTemplate({
				key,
				locale: effectiveLocale,
				template,
				variables,
				compile,
				messageFormatCache,
			});
		},
		hasLocale(locale: string): boolean {
			const normalizedLocale = config.normalizeLocale?.(locale) ?? locale;
			return hasStaticLocale(config, normalizedLocale);
		},
		getLoadedLocales(): Set<string> {
			return new Set(loadedLocales);
		},
		reset(): void {
			loadedLocales.clear();
			loadedLocales.add(config.defaultLocale);
			messageFormatCache.clear();
		},
	};
}
