// SPDX-License-Identifier: AGPL-3.0-or-later

import {getEffectiveLocale} from '@fluxer/i18n/src/runtime/GetEffectiveLocale';
import type {I18nResult, I18nState, TemplateCompiler} from '@fluxer/i18n/src/runtime/I18nTypes';
import {loadLocaleIfNotLoaded} from '@fluxer/i18n/src/runtime/LoadLocale';
import {renderTemplate} from '@fluxer/i18n/src/runtime/RenderTemplate';

export function getTemplate<TKey extends string, TValue, TVariables>(
	state: I18nState<TKey, TValue, TVariables>,
	key: TKey,
	locale: string | null,
	variables: TVariables,
	compile: TemplateCompiler<TValue, TVariables>,
): I18nResult<TKey, TValue> {
	const effectiveLocale = getEffectiveLocale(state, locale);
	loadLocaleIfNotLoaded(state, effectiveLocale);
	const sourceTemplate = state.templatesByLocale.get(state.config.defaultLocale)?.get(key);
	if (!sourceTemplate) {
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
	const translatedTemplate = state.templatesByLocale.get(effectiveLocale)?.get(key);
	const template = translatedTemplate ?? sourceTemplate;
	const validationError = state.config.validateVariables?.(key, template, variables);
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
		messageFormatCache: state.messageFormatCache,
	});
}
