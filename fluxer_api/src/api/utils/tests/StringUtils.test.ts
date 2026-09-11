// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {hasVisibleContent, parseString} from '../StringUtils';

describe('parseString', () => {
	it.each([
		{input: 'Hello World', max: 50, expected: 'Hello World'},
		{input: '  Hello World  ', max: 50, expected: 'Hello World'},
		{input: 'This is a very long string that needs truncation', max: 20, expected: 'This is a very lo...'},
		{input: '12345', max: 5, expected: '12345'},
		{input: 'Hello &amp; World', max: 50, expected: 'Hello & World'},
		{input: '&lt;script&gt;', max: 50, expected: '<script>'},
		{input: '', max: 50, expected: ''},
		{input: '   ', max: 50, expected: ''},
		{input: 'Hello 😀 World', max: 50, expected: 'Hello 😀 World'},
		{input: 'This is a test string for truncation', max: 15, expected: 'This is a te...'},
		{input: 'Hello', max: 0, expected: '...'},
		{input: 'Hello', max: 1, expected: '...'},
		{input: 'Hello', max: 3, expected: '...'},
		{input: 'Hello', max: 4, expected: 'H...'},
		{input: 'Hello\nWorld\tTab', max: 50, expected: 'Hello\nWorld\tTab'},
		{input: '&amp;&amp;&amp;abc', max: 5, expected: '&&...'},
		{input: '😀abcd', max: 4, expected: '😀...'},
	])('decodes, trims, and truncates $input to $expected', ({input, max, expected}) => {
		expect(parseString(input, max)).toBe(expected);
	});
});

describe('hasVisibleContent', () => {
	it.each([
		'',
		' \t\n',
		'\u200e \u200b\ufeff',
		'\u2800\u3164\u{e0100}',
	])('rejects whitespace or invisible-only input %j', (input) => {
		expect(hasVisibleContent(input)).toBe(false);
	});

	it.each(['hello', '\u200e hello', '🙂', '` `'])('accepts visible input %j', (input) => {
		expect(hasVisibleContent(input)).toBe(true);
	});
});
