// SPDX-License-Identifier: AGPL-3.0-or-later

import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {ERROR_I18N_LOCALE_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nLocales';
import {ERROR_I18N_MESSAGES, type ErrorI18nCatalog, type ErrorI18nKey} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {identityLocale} from '@fluxer/i18n/src/normalization/IdentityLocale';
import {compileMessage} from '@fluxer/i18n/src/runtime/CompileMessage';
import {createStaticI18n} from '@fluxer/i18n/src/runtime/CreateStaticI18n';
import type {I18nResult} from '@fluxer/i18n/src/runtime/I18nTypes';
import type {MessageArgsForTemplate} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {validateMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';

const DEFAULT_LOCALE = 'en-US';

type ErrorI18nRuntimeVariables = Record<string, unknown>;
type ErrorI18nResultArgs<TKey extends ErrorI18nKey> = TKey extends ErrorI18nKey
	? MessageArgsForTemplate<ErrorI18nCatalog[TKey]>
	: never;

const errorI18n = createStaticI18n<ErrorI18nKey, string, ErrorI18nRuntimeVariables>(
	{
		defaultLocale: DEFAULT_LOCALE,
		defaultMessages: ERROR_I18N_MESSAGES,
		localeMessages: ERROR_I18N_LOCALE_MESSAGES,
		normalizeLocale: (locale) => identityLocale(locale),
		onWarning: (message) => {
			console.warn(message);
		},
		validateVariables: (_key, template, variables) => validateMessageTemplateVariables(template, variables),
	},
	(template, variables, mf) => {
		return String(compileMessage(mf, template)(variables));
	},
);

function isErrorI18nKey(value: string): value is ErrorI18nKey {
	return Object.hasOwn(ERROR_I18N_MESSAGES, value);
}

function resolveErrorI18nKey(value: string): ErrorI18nKey | null {
	if (isErrorI18nKey(value)) {
		return value;
	}
	const mappedKey = (ErrorCodeToI18nKey as Record<string, string | undefined>)[value];
	if (mappedKey && isErrorI18nKey(mappedKey)) {
		return mappedKey;
	}
	return null;
}

function getErrorMessageResultUnsafe(
	key: ErrorI18nKey,
	locale: string | null | undefined,
	variables: ErrorI18nRuntimeVariables | undefined,
): I18nResult<ErrorI18nKey, string> {
	return errorI18n.getTemplate(key, locale ?? null, variables ?? {});
}

export function getErrorMessageResult<TKey extends ErrorI18nKey>(
	key: TKey,
	locale: string | null | undefined,
	...args: ErrorI18nResultArgs<TKey>
): I18nResult<TKey, string> {
	return getErrorMessageResultUnsafe(key, locale, args[0] as ErrorI18nRuntimeVariables | undefined) as I18nResult<
		TKey,
		string
	>;
}

function getResolvedErrorMessage(
	key: ErrorI18nKey,
	locale: string | null | undefined,
	variables: ErrorI18nRuntimeVariables | undefined,
	fallbackMessage: string | undefined,
): string {
	const result = getErrorMessageResultUnsafe(key, locale, variables);
	if (result.ok) {
		return result.value;
	}
	if (result.error.kind === 'missing-template') {
		console.warn(`Missing translation for error message: ${key} (locale: ${locale ?? DEFAULT_LOCALE})`);
		return fallbackMessage ?? key;
	}
	return fallbackMessage ?? key;
}

export function getErrorMessageUnsafe(
	key: string,
	locale: string | null | undefined,
	variables?: ErrorI18nRuntimeVariables,
	fallbackMessage?: string,
): string {
	const resolvedKey = resolveErrorI18nKey(key);
	if (!resolvedKey) {
		console.warn(`Missing translation for error message: ${key} (locale: ${locale ?? DEFAULT_LOCALE})`);
		return fallbackMessage ?? key;
	}
	return getResolvedErrorMessage(resolvedKey, locale, variables, fallbackMessage);
}
