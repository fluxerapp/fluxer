// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';
import {getErrorMessageResult} from '@fluxer/errors/src/i18n/ErrorI18n';
import {ERROR_I18N_LOCALE_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nLocales';
import {ERROR_I18N_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {extractMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {parse, type Token} from '@messageformat/parser';
import {describe, expect, it} from 'vitest';

const WEBLATE_DIR = path.join(path.dirname(fileURLToPath(import.meta.url)), 'weblate');
const SOURCE_SYNC_COMMAND = 'pnpm i18n:source-sync';
const COMPILE_COMMAND = 'pnpm i18n:compile';

const ENGLISH_VARIANT_LOCALES = new Set<string>(['en-GB']);

const ENGLISH_IS_THE_ADJUDICATED_TRANSLATION = new Set<string>(['nl:http.conflict', 'ro:http.conflict']);

type FlatCatalog = Record<string, string>;

const SOURCE_CATALOG = ERROR_I18N_MESSAGES as FlatCatalog;
const COMPILED_LOCALE_CATALOGS = ERROR_I18N_LOCALE_MESSAGES as Record<string, FlatCatalog>;
const STATIC_LOCALES = Object.keys(COMPILED_LOCALE_CATALOGS).sort();

type ErrorMessageProbe = (
	key: string,
	locale: string,
	variables: Record<string, number>,
) => ReturnType<typeof getErrorMessageResult>;

const probeErrorMessage = getErrorMessageResult as unknown as ErrorMessageProbe;

function readJsonCatalog(filePath: string): FlatCatalog {
	return JSON.parse(fs.readFileSync(filePath, 'utf8')) as FlatCatalog;
}

function readWeblateCatalog(locale: string): FlatCatalog {
	return readJsonCatalog(path.join(WEBLATE_DIR, 'locales', `${locale}.json`));
}

function readCompiledCatalog(locale: string): FlatCatalog {
	return COMPILED_LOCALE_CATALOGS[locale];
}

function quoteMessage(value: string): string {
	return value.length > 90 ? `"${value.slice(0, 90)}…"` : `"${value}"`;
}

function unionOfKeys(...catalogs: Array<FlatCatalog>): Array<string> {
	const keys = new Set<string>();
	for (const catalog of catalogs) {
		for (const key of Object.keys(catalog)) {
			keys.add(key);
		}
	}
	return [...keys].sort();
}

function parseTemplate(template: string): {tokens: Array<Token>} | {parseError: string} {
	try {
		return {tokens: parse(template)};
	} catch (error) {
		return {parseError: error instanceof Error ? error.message.split('\n')[0] : 'unknown ICU parser error'};
	}
}

function placeholdersOf(template: string): Set<string> {
	return extractMessageTemplateVariables(template);
}

function selectorWithoutOtherBranch(tokens: ReadonlyArray<Token>): string | null {
	for (const token of tokens) {
		if (token.type === 'plural' || token.type === 'selectordinal' || token.type === 'select') {
			if (!token.cases.some((branch) => branch.key === 'other')) {
				return `${token.type} on {${token.arg}}`;
			}
			for (const branch of token.cases) {
				const nested = selectorWithoutOtherBranch(branch.tokens);
				if (nested !== null) {
					return nested;
				}
			}
		}
		if (token.type === 'function' && token.param) {
			const nested = selectorWithoutOtherBranch(token.param);
			if (nested !== null) {
				return nested;
			}
		}
	}
	return null;
}

function sampleVariables(names: Iterable<string>): Record<string, number> {
	const variables: Record<string, number> = {};
	for (const name of names) {
		variables[name] = 1;
	}
	return variables;
}

function describeIcuProblem(locale: string, key: string, template: string, sourceTemplate: string): string | null {
	const parsed = parseTemplate(template);
	if ('parseError' in parsed) {
		return `${locale} / ${key}: ${quoteMessage(template)} is not a valid ICU message (${parsed.parseError}).`;
	}
	let sourcePlaceholders: Set<string>;
	let translatedPlaceholders: Set<string>;
	try {
		sourcePlaceholders = placeholdersOf(sourceTemplate);
		translatedPlaceholders = placeholdersOf(template);
	} catch (error) {
		const detail = error instanceof Error ? error.message.split('\n')[0] : 'unknown ICU parser error';
		return `${locale} / ${key}: placeholders could not be read (${detail}).`;
	}
	const dropped = [...sourcePlaceholders].filter((name) => !translatedPlaceholders.has(name)).sort();
	if (dropped.length > 0) {
		return `${locale} / ${key}: drops placeholder ${dropped.map((name) => `{${name}}`).join(', ')} that the English source fills in, so the value would be shown to users with the data missing.`;
	}
	const invented = [...translatedPlaceholders].filter((name) => !sourcePlaceholders.has(name)).sort();
	if (invented.length > 0) {
		return `${locale} / ${key}: introduces placeholder ${invented.map((name) => `{${name}}`).join(', ')} that no caller passes, so rendering it fails at runtime.`;
	}
	const selectorProblem = selectorWithoutOtherBranch(parsed.tokens);
	if (selectorProblem !== null) {
		return `${locale} / ${key}: declares a ${selectorProblem} with no "other" branch, which MessageFormat refuses to compile.`;
	}
	const result = probeErrorMessage(key, locale, sampleVariables(sourcePlaceholders));
	if (!result.ok) {
		return `${locale} / ${key}: ${quoteMessage(template)} failed to compile (${result.error.kind}: ${result.error.message.split('\n')[0]}).`;
	}
	return null;
}

describe('error i18n catalog integrity', () => {
	it('keeps weblate/messages.json byte-identical to the English source catalog', () => {
		const extracted = readJsonCatalog(path.join(WEBLATE_DIR, 'messages.json'));
		const problems: Array<string> = [];
		for (const key of unionOfKeys(SOURCE_CATALOG, extracted)) {
			const source = SOURCE_CATALOG[key];
			const translatorFacing = extracted[key];
			if (source === undefined) {
				problems.push(
					`en-US / ${key}: weblate/messages.json still offers this key to translators but ErrorI18nMessages.ts no longer defines it. Run ${SOURCE_SYNC_COMMAND}.`,
				);
				continue;
			}
			if (translatorFacing === undefined) {
				problems.push(
					`en-US / ${key}: ErrorI18nMessages.ts defines it but weblate/messages.json does not, so translators never see it. Run ${SOURCE_SYNC_COMMAND}.`,
				);
				continue;
			}
			if (translatorFacing !== source) {
				problems.push(
					`en-US / ${key}: weblate/messages.json shows translators the stale English ${quoteMessage(translatorFacing)} while ErrorI18nMessages.ts now says ${quoteMessage(source)}. Run ${SOURCE_SYNC_COMMAND}.`,
				);
			}
		}
		expect(problems).toEqual([]);
	});

	it('keeps every compiled locale module byte-identical to the weblate JSON it is generated from', () => {
		const problems: Array<string> = [];
		const weblateLocales = fs
			.readdirSync(path.join(WEBLATE_DIR, 'locales'))
			.filter((entry) => entry.endsWith('.json'))
			.map((entry) => entry.slice(0, -'.json'.length))
			.sort();
		for (const locale of weblateLocales) {
			if (!STATIC_LOCALES.includes(locale)) {
				problems.push(
					`${locale} / *: weblate/locales/${locale}.json is translated but ErrorI18nLocales.ts ships no ${locale} catalog, so none of it reaches users. Run ${COMPILE_COMMAND} and register the locale in ErrorI18nLocales.ts.`,
				);
			}
		}
		for (const locale of STATIC_LOCALES) {
			if (!weblateLocales.includes(locale)) {
				problems.push(
					`${locale} / *: ErrorI18nLocales.ts ships a ${locale} catalog with no weblate/locales/${locale}.json behind it, so translators cannot reach it. Run ${SOURCE_SYNC_COMMAND}.`,
				);
				continue;
			}
			const weblate = readWeblateCatalog(locale);
			const compiled = readCompiledCatalog(locale);
			for (const key of unionOfKeys(weblate, compiled)) {
				const translated = weblate[key];
				const shipped = compiled[key];
				if (translated === undefined) {
					problems.push(
						`${locale} / ${key}: locales/${locale}.ts ships ${quoteMessage(shipped)} but weblate/locales/${locale}.json has no such key. Run ${COMPILE_COMMAND}.`,
					);
					continue;
				}
				if (shipped === undefined) {
					problems.push(
						`${locale} / ${key}: weblate/locales/${locale}.json holds ${quoteMessage(translated)} but locales/${locale}.ts does not ship it. Run ${COMPILE_COMMAND}.`,
					);
					continue;
				}
				if (shipped !== translated) {
					problems.push(
						`${locale} / ${key}: locales/${locale}.ts ships ${quoteMessage(shipped)} but weblate/locales/${locale}.json holds ${quoteMessage(translated)}. The compiled catalog is stale. Run ${COMPILE_COMMAND}.`,
					);
				}
			}
		}
		expect(problems).toEqual([]);
	});

	it('ships no translated value that is still byte-identical to the English source', () => {
		const problems: Array<string> = [];
		for (const locale of STATIC_LOCALES) {
			if (ENGLISH_VARIANT_LOCALES.has(locale)) {
				continue;
			}
			const compiled = readCompiledCatalog(locale);
			for (const key of Object.keys(compiled).sort()) {
				const source = SOURCE_CATALOG[key];
				if (source === undefined || compiled[key] !== source) {
					continue;
				}
				if (ENGLISH_IS_THE_ADJUDICATED_TRANSLATION.has(`${locale}:${key}`)) {
					continue;
				}
				problems.push(
					`${locale} / ${key}: ships the English source ${quoteMessage(source)} verbatim, so ${locale} users read English. Translate it, or add '${locale}:${key}' to ENGLISH_IS_THE_ADJUDICATED_TRANSLATION if English really is the right ${locale} wording.`,
				);
			}
		}
		expect(problems).toEqual([]);
	});

	it('keeps every catalog message compilable with the placeholders its English source declares', () => {
		const problems: Array<string> = [];
		for (const key of Object.keys(SOURCE_CATALOG).sort()) {
			const problem = describeIcuProblem('en-US', key, SOURCE_CATALOG[key], SOURCE_CATALOG[key]);
			if (problem !== null) {
				problems.push(problem);
			}
		}
		for (const locale of STATIC_LOCALES) {
			const compiled = readCompiledCatalog(locale);
			for (const key of Object.keys(compiled).sort()) {
				const source = SOURCE_CATALOG[key];
				if (source === undefined) {
					continue;
				}
				const problem = describeIcuProblem(locale, key, compiled[key], source);
				if (problem !== null) {
					problems.push(problem);
				}
			}
		}
		expect(problems).toEqual([]);
	});
});
