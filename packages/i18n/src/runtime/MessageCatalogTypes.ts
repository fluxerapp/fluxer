// SPDX-License-Identifier: AGPL-3.0-or-later

import {parse, type Token} from '@messageformat/parser';

export type MessageVariableValue = string | number | boolean | Date;
type Whitespace = ' ' | '\n' | '\r' | '\t';
type TrimLeft<T extends string> = T extends `${Whitespace}${infer Rest}` ? TrimLeft<Rest> : T;
type TrimRight<T extends string> = T extends `${infer Rest}${Whitespace}` ? TrimRight<Rest> : T;
type Trim<T extends string> = TrimLeft<TrimRight<T>>;
type PlaceholderHead<T extends string> = T extends `${infer Name},${string}` ? Trim<Name> : Trim<T>;
type ValidPlaceholderName<T extends string> = T extends ''
	? never
	: T extends `#${string}`
		? never
		: T extends `${number}${string}`
			? never
			: T extends `${string}${Whitespace}${string}`
				? never
				: T extends `${string}{${string}`
					? never
					: T extends `${string}}${string}`
						? never
						: T;
type PlaceholderName<T extends string> = ValidPlaceholderName<PlaceholderHead<T>>;
type PlaceholderNameAfterOpen<T extends string> = T extends `${infer Placeholder}}${string}`
	? PlaceholderName<Placeholder>
	: never;
type RestAfterPlaceholder<T extends string> = T extends `${string}}${infer Rest}` ? Rest : '';
type NumericPlaceholderNameAfterOpen<T extends string> = T extends `${infer Name}, plural,${string}`
	? PlaceholderName<Name>
	: T extends `${infer Name}, selectordinal,${string}`
		? PlaceholderName<Name>
		: never;
export type TemplatePlaceholderNames<T extends string> = T extends `${string}{${infer AfterOpen}`
	? PlaceholderNameAfterOpen<AfterOpen> | TemplatePlaceholderNames<RestAfterPlaceholder<AfterOpen>>
	: never;
export type NumericTemplatePlaceholderNames<T extends string> = T extends `${string}{${infer AfterOpen}`
	? NumericPlaceholderNameAfterOpen<AfterOpen> | NumericTemplatePlaceholderNames<RestAfterPlaceholder<AfterOpen>>
	: never;
export type MessageVariablesForTemplate<T extends string> = [TemplatePlaceholderNames<T>] extends [never]
	? never
	: {
			[Name in TemplatePlaceholderNames<T>]: Name extends NumericTemplatePlaceholderNames<T>
				? number
				: MessageVariableValue;
		};
export type MessageArgsForTemplate<T extends string> = [MessageVariablesForTemplate<T>] extends [never]
	? [variables?: undefined]
	: [variables: MessageVariablesForTemplate<T>];
export type MessageArgsWithFallbackForTemplate<T extends string> = [MessageVariablesForTemplate<T>] extends [never]
	? [variables?: undefined, fallbackMessage?: string]
	: [variables: MessageVariablesForTemplate<T>, fallbackMessage?: string];
type LocaleMessageForTemplate<Source extends string, Translation extends string> = [
	Exclude<TemplatePlaceholderNames<Source>, TemplatePlaceholderNames<Translation>>,
] extends [never]
	? Translation
	: never;
type StaticLocaleMessagesForCatalog<
	TCatalog extends Record<string, string>,
	TMessages extends Partial<Record<keyof TCatalog, string>>,
> = {
	readonly [Key in keyof TMessages]: Key extends keyof TCatalog
		? TMessages[Key] extends string
			? LocaleMessageForTemplate<TCatalog[Key], TMessages[Key]>
			: never
		: never;
};

export function defineStaticLocaleMessages<TCatalog extends Record<string, string>>() {
	return <const TMessages extends Partial<Record<keyof TCatalog, string>>>(
		messages: TMessages & StaticLocaleMessagesForCatalog<TCatalog, TMessages>,
	): TMessages => messages;
}

function collectMessageTemplateVariables(tokens: ReadonlyArray<Token>, variables: Set<string>): void {
	for (const token of tokens) {
		switch (token.type) {
			case 'argument': {
				variables.add(token.arg);
				break;
			}
			case 'function': {
				variables.add(token.arg);
				if (token.param) {
					collectMessageTemplateVariables(token.param, variables);
				}
				break;
			}
			case 'plural':
			case 'select':
			case 'selectordinal': {
				variables.add(token.arg);
				for (const selectCase of token.cases) {
					collectMessageTemplateVariables(selectCase.tokens, variables);
				}
				break;
			}
			case 'content':
			case 'octothorpe': {
				break;
			}
		}
	}
}

export function extractMessageTemplateVariables(template: string): Set<string> {
	const variables = new Set<string>();
	collectMessageTemplateVariables(parse(template), variables);
	return variables;
}

function collectMessageTemplatePlaceholders(tokens: ReadonlyArray<Token>, placeholders: Set<string>): void {
	for (const token of tokens) {
		switch (token.type) {
			case 'argument': {
				placeholders.add(token.arg);
				break;
			}
			case 'function': {
				const param = (token.param ?? [])
					.map((paramToken) => (paramToken.type === 'content' ? paramToken.value : ''))
					.join('')
					.trim();
				placeholders.add(`${token.arg},${token.key},${param}`);
				if (token.param) {
					collectMessageTemplatePlaceholders(token.param, placeholders);
				}
				break;
			}
			case 'select': {
				const keys = token.cases.map((selectCase) => selectCase.key).sort();
				placeholders.add(`${token.arg},select[${keys.join(',')}]`);
				for (const selectCase of token.cases) {
					collectMessageTemplatePlaceholders(selectCase.tokens, placeholders);
				}
				break;
			}
			case 'plural':
			case 'selectordinal': {
				placeholders.add(`${token.arg},plural`);
				for (const selectCase of token.cases) {
					collectMessageTemplatePlaceholders(selectCase.tokens, placeholders);
				}
				break;
			}
			case 'content':
			case 'octothorpe': {
				break;
			}
		}
	}
}

export function extractMessageTemplatePlaceholders(template: string): Array<string> {
	const placeholders = new Set<string>();
	collectMessageTemplatePlaceholders(parse(template), placeholders);
	return [...placeholders].sort();
}

export function validateMessageTemplateVariables(
	template: string,
	variables: Record<string, unknown> | undefined,
): string | null {
	let requiredVariables: Set<string>;
	try {
		requiredVariables = extractMessageTemplateVariables(template);
	} catch (error) {
		const message = error instanceof Error ? error.message : 'Unknown parser error';
		return `Invalid i18n message template: ${message}`;
	}
	for (const name of requiredVariables) {
		if (variables?.[name] === undefined) {
			return `Missing required i18n variable: ${name}`;
		}
	}
	return null;
}
