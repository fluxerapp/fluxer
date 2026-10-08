// SPDX-License-Identifier: AGPL-3.0-or-later

import type MessageFormat from '@messageformat/core';

type I18nErrorKind = 'missing-template' | 'invalid-variables' | 'compile-failed';

interface I18nError<TKey extends string> {
	kind: I18nErrorKind;
	key: TKey;
	message: string;
}

export type I18nResult<TKey extends string, TValue> =
	| {
			ok: true;
			value: TValue;
			locale: string;
	  }
	| {
			ok: false;
			error: I18nError<TKey>;
			locale: string;
	  };

export type TemplateCompiler<TValue, TVariables> = (
	template: TValue,
	variables: TVariables,
	mf: MessageFormat,
) => TValue;
