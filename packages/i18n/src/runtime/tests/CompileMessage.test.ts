// SPDX-License-Identifier: AGPL-3.0-or-later

import {compileMessage} from '@fluxer/i18n/src/runtime/CompileMessage';
import {createStaticI18n} from '@fluxer/i18n/src/runtime/CreateStaticI18n';
import type {I18nResult} from '@fluxer/i18n/src/runtime/I18nTypes';
import {
	extractMessageTemplateVariables,
	validateMessageTemplateVariables,
} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import MessageFormat from '@messageformat/core';
import {parse} from '@messageformat/parser';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@messageformat/parser', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@messageformat/parser')>();
	return {...actual, parse: vi.fn(actual.parse)};
});

const FILES_TEMPLATE = '{count, plural, one {# file one} other {# files other}}';

type TestKey = 'files' | 'greeting';

function unwrap(result: I18nResult<TestKey, string>): string {
	if (!result.ok) {
		throw new Error(result.error.message);
	}
	return result.value;
}

function createTestI18n() {
	return createStaticI18n<TestKey, string, Record<string, unknown>>(
		{
			defaultLocale: 'en-US',
			defaultMessages: {files: FILES_TEMPLATE, greeting: 'Hello {name}'},
			localeMessages: {fr: {files: FILES_TEMPLATE}},
			validateVariables: (_key, template, variables) => validateMessageTemplateVariables(template, variables),
		},
		(template, variables, mf) => String(compileMessage(mf, template)(variables)),
	);
}

describe('compileMessage', () => {
	afterEach(() => {
		vi.restoreAllMocks();
	});
	it('compiles a template once and reuses it across renders', () => {
		const messageFormat = new MessageFormat('en-US');
		const compileSpy = vi.spyOn(messageFormat, 'compile');
		const first = compileMessage(messageFormat, 'Hello {name}');
		const second = compileMessage(messageFormat, 'Hello {name}');
		expect(second).toBe(first);
		expect(first({name: 'Taylor'})).toBe('Hello Taylor');
		expect(second({name: 'Jordan'})).toBe('Hello Jordan');
		expect(compileSpy).toHaveBeenCalledTimes(1);
		compileMessage(messageFormat, 'Bye {name}');
		expect(compileSpy).toHaveBeenCalledTimes(2);
	});
	it('keeps compiled templates separate per locale so plural rules stay correct', () => {
		const english = new MessageFormat('en-US');
		const french = new MessageFormat('fr');
		const englishFiles = compileMessage(english, FILES_TEMPLATE);
		const frenchFiles = compileMessage(french, FILES_TEMPLATE);
		expect(frenchFiles).not.toBe(englishFiles);
		expect(englishFiles({count: 0})).toBe('0 files other');
		expect(frenchFiles({count: 0})).toBe('0 file one');
		expect(compileMessage(english, FILES_TEMPLATE)({count: 1})).toBe('1 file one');
		expect(compileMessage(french, FILES_TEMPLATE)({count: 2})).toBe('2 files other');
	});
	it('returns the raw value for a lone argument', () => {
		expect(compileMessage(new MessageFormat('en-US'), '{count}')({count: 3})).toBe(3);
	});
	it('compiles once per locale when rendering through createStaticI18n', () => {
		const compileSpy = vi.spyOn(MessageFormat.prototype, 'compile');
		const i18n = createTestI18n();
		expect(unwrap(i18n.getTemplate('files', 'en-US', {count: 0}))).toBe('0 files other');
		expect(unwrap(i18n.getTemplate('files', 'en-US', {count: 1}))).toBe('1 file one');
		expect(unwrap(i18n.getTemplate('files', 'fr', {count: 0}))).toBe('0 file one');
		expect(unwrap(i18n.getTemplate('files', 'fr', {count: 2}))).toBe('2 files other');
		expect(unwrap(i18n.getTemplate('greeting', 'en-US', {name: 'Taylor'}))).toBe('Hello Taylor');
		expect(unwrap(i18n.getTemplate('greeting', 'en-US', {name: 'Jordan'}))).toBe('Hello Jordan');
		expect(compileSpy).toHaveBeenCalledTimes(3);
		i18n.reset();
		expect(unwrap(i18n.getTemplate('files', 'en-US', {count: 3}))).toBe('3 files other');
		expect(compileSpy).toHaveBeenCalledTimes(4);
	});
});

describe('message template variables', () => {
	it('parses a template once across validations', () => {
		const template = 'Hello {cachedName}, you have {cachedCount, plural, one {# invite} other {# invites}}';
		const parseMock = vi.mocked(parse);
		parseMock.mockClear();
		expect(validateMessageTemplateVariables(template, {cachedName: 'Taylor', cachedCount: 1})).toBeNull();
		expect(validateMessageTemplateVariables(template, {cachedName: 'Jordan', cachedCount: 2})).toBeNull();
		expect(validateMessageTemplateVariables(template, {cachedCount: 2})).toBe(
			'Missing required i18n variable: cachedName',
		);
		expect([...extractMessageTemplateVariables(template)].sort()).toEqual(['cachedCount', 'cachedName']);
		expect(parseMock).toHaveBeenCalledTimes(1);
	});
	it('hands out copies so callers cannot change the cached set', () => {
		const template = 'Welcome {copiedName}';
		const variables = extractMessageTemplateVariables(template);
		variables.add('injected');
		variables.delete('copiedName');
		expect([...extractMessageTemplateVariables(template)]).toEqual(['copiedName']);
		expect(validateMessageTemplateVariables(template, {copiedName: 'Taylor'})).toBeNull();
		expect(validateMessageTemplateVariables(template, {})).toBe('Missing required i18n variable: copiedName');
	});
	it('does not cache templates that fail to parse', () => {
		const parseMock = vi.mocked(parse);
		parseMock.mockClear();
		expect(validateMessageTemplateVariables('Broken {unclosed', {})).toMatch(/^Invalid i18n message template: /);
		expect(validateMessageTemplateVariables('Broken {unclosed', {})).toMatch(/^Invalid i18n message template: /);
		expect(parseMock).toHaveBeenCalledTimes(2);
	});
});
