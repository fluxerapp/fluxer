// SPDX-License-Identifier: AGPL-3.0-or-later

import {CONTENT_I18N_LOCALE_MESSAGES} from '@app/api/content_i18n/ContentI18nLocales';
import {
	CONTENT_I18N_MESSAGES,
	type ContentI18nCatalog,
	type ContentI18nKey,
} from '@app/api/content_i18n/ContentI18nMessages';
import {identityLocale} from '@fluxer/i18n/src/normalization/IdentityLocale';
import {createStaticI18n} from '@fluxer/i18n/src/runtime/CreateStaticI18n';
import type {MessageArgsForTemplate} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {validateMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';

const DEFAULT_LOCALE = 'en-US';

type ContentI18nRuntimeVariables = Record<string, unknown>;
type ContentI18nMessageArgs<TKey extends ContentI18nKey> = TKey extends ContentI18nKey
	? MessageArgsForTemplate<ContentI18nCatalog[TKey]>
	: never;

const contentI18n = createStaticI18n<ContentI18nKey, string, ContentI18nRuntimeVariables>(
	{
		defaultLocale: DEFAULT_LOCALE,
		defaultMessages: CONTENT_I18N_MESSAGES,
		localeMessages: CONTENT_I18N_LOCALE_MESSAGES,
		normalizeLocale: (locale) => identityLocale(locale),
		onWarning: (message) => {
			console.warn(message);
		},
		validateVariables: (_key, template, variables) => validateMessageTemplateVariables(template, variables),
	},
	(template, variables, mf) => {
		return String(mf.compile(template)(variables));
	},
);

export function getContentMessage<TKey extends ContentI18nKey>(
	key: TKey,
	locale: string | null | undefined,
	...args: ContentI18nMessageArgs<TKey>
): string {
	const variables = args[0] as ContentI18nRuntimeVariables | undefined;
	const result = contentI18n.getTemplate(key, locale ?? null, variables ?? {});
	if (result.ok) {
		return result.value;
	}
	console.warn(`Missing translation for content message: ${key} (locale: ${locale ?? DEFAULT_LOCALE})`);
	return key;
}
