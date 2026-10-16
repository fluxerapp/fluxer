// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';
import {extractMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import MessageFormat from '@messageformat/core';
import {describe, expect, it} from 'vitest';

const REPO_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../../../..');

const CATALOG_DIRS = {
	content: 'fluxer_api/src/api/content_i18n/weblate',
	email: 'fluxer_api/pkgs/email/src/email_i18n/weblate',
	errors: 'packages/errors/src/i18n/weblate',
} as const;

type CatalogEntry = string | {subject: string; body: string};

function readCatalog(file: string): Record<string, CatalogEntry> {
	return JSON.parse(fs.readFileSync(file, 'utf8')) as Record<string, CatalogEntry>;
}

function templates(entry: CatalogEntry): Array<[string, string]> {
	return typeof entry === 'string'
		? [['', entry]]
		: [
				['.subject', entry.subject],
				['.body', entry.body],
			];
}

function sortedVariables(template: string): Array<string> {
	return [...extractMessageTemplateVariables(template)].sort();
}

const CASES = Object.entries(CATALOG_DIRS).flatMap(([catalog, dir]) => {
	const localeDir = path.join(REPO_ROOT, dir, 'locales');
	return fs
		.readdirSync(localeDir)
		.filter((file) => file.endsWith('.json'))
		.map((file) => ({
			catalog,
			locale: file.slice(0, -'.json'.length),
			source: readCatalog(path.join(REPO_ROOT, dir, 'messages.json')),
			messages: readCatalog(path.join(localeDir, file)),
		}));
});

describe('static i18n catalogs', () => {
	it('finds every catalog', () => {
		expect(new Set(CASES.map((entry) => entry.catalog))).toEqual(new Set(Object.keys(CATALOG_DIRS)));
	});

	it.each(CASES.map((entry) => [`${entry.catalog} ${entry.locale}`, entry] as const))(
		'%s compiles under the plural rules of its locale',
		(_label, {locale, messages}) => {
			const messageFormat = new MessageFormat(locale);
			const failures = Object.entries(messages).flatMap(([key, entry]) =>
				templates(entry).flatMap(([part, template]) => {
					try {
						messageFormat.compile(template);
						return [];
					} catch (error) {
						return [`${key}${part}: ${String(error).split('\n')[0]}`];
					}
				}),
			);
			expect(failures).toEqual([]);
		},
	);

	it.each(CASES.map((entry) => [`${entry.catalog} ${entry.locale}`, entry] as const))(
		'%s uses the variables of the English source',
		(_label, {source, messages}) => {
			const mismatches = Object.entries(messages).flatMap(([key, entry]) => {
				const sourceEntry = source[key];
				if (sourceEntry === undefined) return [`${key}: not in the source catalog`];
				const sourceTemplates = new Map(templates(sourceEntry));
				return templates(entry).flatMap(([part, template]) => {
					const expected = sortedVariables(sourceTemplates.get(part) ?? '');
					const actual = sortedVariables(template);
					return expected.join(',') === actual.join(',') ? [] : [`${key}${part}: ${actual} != ${expected}`];
				});
			});
			expect(mismatches).toEqual([]);
		},
	);
});
