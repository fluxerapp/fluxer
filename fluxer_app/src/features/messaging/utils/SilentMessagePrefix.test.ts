// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseSilentMessagePrefix} from '@app/features/messaging/utils/SilentMessagePrefix';
import {describe, expect, it} from 'vitest';

describe('parseSilentMessagePrefix', () => {
	it('matches @silent followed by a space', () => {
		expect(parseSilentMessagePrefix('@silent hello')).toEqual({tokenStart: 0, tokenEnd: 7, end: 8});
	});

	it('matches @silent on its own, with or without a trailing space', () => {
		expect(parseSilentMessagePrefix('@silent')).toEqual({tokenStart: 0, tokenEnd: 7, end: 7});
		expect(parseSilentMessagePrefix('@silent ')).toEqual({tokenStart: 0, tokenEnd: 7, end: 8});
	});

	it.each([
		['a newline', '\n'],
		['a tab', '\t'],
		['a no-break space', '\u00a0'],
	])('matches @silent followed by %s', (_name, whitespace) => {
		expect(parseSilentMessagePrefix(`@silent${whitespace}hello`)).toEqual({tokenStart: 0, tokenEnd: 7, end: 8});
	});

	it('consumes only one whitespace character after the token', () => {
		expect(parseSilentMessagePrefix('@silent  hello')).toEqual({tokenStart: 0, tokenEnd: 7, end: 8});
	});

	it('skips leading whitespace before the token', () => {
		expect(parseSilentMessagePrefix('  @silent hello')).toEqual({tokenStart: 2, tokenEnd: 9, end: 10});
		expect(parseSilentMessagePrefix('\n@silent hello')).toEqual({tokenStart: 1, tokenEnd: 8, end: 9});
	});

	it.each([
		'',
		'hello',
		'hello @silent',
		'@silently hello',
		'@Silent hello',
		'@silent@everyone',
		'`@silent` hello',
		'> @silent hello',
	])('does not match %j', (content) => {
		expect(parseSilentMessagePrefix(content)).toBeNull();
	});

	it.each([
		'@silent hello',
		'  @silent hello',
		'\n@silent hello',
		'\t@silent hello',
		'\u00a0@silent hello',
		'\u3000@silent hello',
		'\ufeff@silent hello',
		'\u200b@silent hello',
		'@silent',
		'@silent ',
		'@silent\u00a0',
		'@silent\u200b',
		'@silently hello',
		'hello @silent',
		'   ',
	])('matches %j the same way before and after trimming', (content) => {
		expect(parseSilentMessagePrefix(content) != null).toBe(parseSilentMessagePrefix(content.trim()) != null);
	});
});
