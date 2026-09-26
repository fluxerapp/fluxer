// SPDX-License-Identifier: AGPL-3.0-or-later

import type {I18nResult, TemplateCompiler} from '@fluxer/i18n/src/runtime/I18nTypes';
import MessageFormat from '@messageformat/core';

interface TemplateRenderOptions<TKey extends string, TValue, TVariables> {
	key: TKey;
	locale: string;
	template: TValue;
	variables: TVariables;
	compile: TemplateCompiler<TValue, TVariables>;
	messageFormatCache: Map<string, MessageFormat>;
}

export function renderTemplate<TKey extends string, TValue, TVariables>({
	key,
	locale,
	template,
	variables,
	compile,
	messageFormatCache,
}: TemplateRenderOptions<TKey, TValue, TVariables>): I18nResult<TKey, TValue> {
	try {
		let messageFormat = messageFormatCache.get(locale);
		if (!messageFormat) {
			messageFormat = new MessageFormat(locale);
			messageFormatCache.set(locale, messageFormat);
		}
		return {
			ok: true,
			value: compile(template, variables, messageFormat),
			locale,
		};
	} catch (error) {
		return {
			ok: false,
			error: {
				kind: 'compile-failed',
				key,
				message: error instanceof Error ? error.message : 'Failed to compile template',
			},
			locale,
		};
	}
}
