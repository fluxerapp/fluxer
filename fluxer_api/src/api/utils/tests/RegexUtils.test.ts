// SPDX-License-Identifier: AGPL-3.0-or-later

import {escapeRegex} from '@app/api/utils/RegexUtils';
import {describe, expect, it} from 'vitest';

describe('escapeRegex', () => {
	it('escapes hyphen', () => {
		expect(escapeRegex('-')).toBe('\\-');
	});
	it('escapes square brackets', () => {
		expect(escapeRegex('[')).toBe('\\[');
		expect(escapeRegex(']')).toBe('\\]');
	});
	it('escapes forward slash', () => {
		expect(escapeRegex('/')).toBe('\\/');
	});
	it('escapes curly braces', () => {
		expect(escapeRegex('{')).toBe('\\{');
		expect(escapeRegex('}')).toBe('\\}');
	});
	it('escapes parentheses', () => {
		expect(escapeRegex('(')).toBe('\\(');
		expect(escapeRegex(')')).toBe('\\)');
	});
	it('escapes asterisk', () => {
		expect(escapeRegex('*')).toBe('\\*');
	});
	it('escapes plus', () => {
		expect(escapeRegex('+')).toBe('\\+');
	});
	it('escapes question mark', () => {
		expect(escapeRegex('?')).toBe('\\?');
	});
	it('escapes period', () => {
		expect(escapeRegex('.')).toBe('\\.');
	});
	it('escapes backslash', () => {
		expect(escapeRegex('\\')).toBe('\\\\');
	});
	it('escapes caret', () => {
		expect(escapeRegex('^')).toBe('\\^');
	});
	it('escapes dollar sign', () => {
		expect(escapeRegex('$')).toBe('\\$');
	});
	it('escapes pipe', () => {
		expect(escapeRegex('|')).toBe('\\|');
	});
	it('returns empty string for empty input', () => {
		expect(escapeRegex('')).toBe('');
	});
	it('preserves normal characters', () => {
		expect(escapeRegex('abc')).toBe('abc');
		expect(escapeRegex('123')).toBe('123');
		expect(escapeRegex('Hello World')).toBe('Hello World');
	});
	it('escapes multiple special characters in a string', () => {
		expect(escapeRegex('[a-z]+')).toBe('\\[a\\-z\\]\\+');
		expect(escapeRegex('hello.*world')).toBe('hello\\.\\*world');
	});
	it('escapes URL patterns', () => {
		expect(escapeRegex('https://example.com/path?query=value')).toBe('https:\\/\\/example\\.com\\/path\\?query=value');
	});
	it('escapes regex character class patterns', () => {
		expect(escapeRegex('[^a-zA-Z0-9]')).toBe('\\[\\^a\\-zA\\-Z0\\-9\\]');
	});
	it('creates valid regex from escaped string', () => {
		const input = 'price: $10.00 (tax*)';
		const escaped = escapeRegex(input);
		const regex = new RegExp(escaped);
		expect(regex.test(input)).toBe(true);
		expect(regex.test('price: $1000 (tax)')).toBe(false);
	});
	it('handles repeated special characters', () => {
		expect(escapeRegex('...')).toBe('\\.\\.\\.');
		expect(escapeRegex('***')).toBe('\\*\\*\\*');
	});
});
