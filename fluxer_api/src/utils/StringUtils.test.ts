/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, it} from 'vitest';
import {parseString} from './StringUtils';

describe('StringUtils', () => {
	describe('parseString', () => {
		it('should decode HTML entities', () => {
			const result = parseString('&amp;&lt;&gt;&quot;', 50);
			expect(result).toBe('&<>"');
		});

		it('should trim whitespace', () => {
			const result = parseString('  hello world  ', 50);
			expect(result).toBe('hello world');
		});

		it('should truncate to max length', () => {
			const longString = 'This is a very long string that should be truncated';
			const result = parseString(longString, 20);

			expect(result.length).toBeLessThanOrEqual(20);
			expect(result).toBe('This is a very lo...');
		});

		it('should handle empty strings', () => {
			const result = parseString('', 50);
			expect(result).toBe('');
		});

		it('should handle strings with only whitespace', () => {
			const result = parseString('   ', 50);
			expect(result).toBe('');
		});

		it('should handle strings shorter than max length', () => {
			const shortString = 'hello';
			const result = parseString(shortString, 50);
			expect(result).toBe('hello');
		});

		it('should decode complex HTML entities and truncate', () => {
			const complexString = '&amp;lt;div&amp;gt;This is a test with HTML entities&amp;lt;/div&amp;gt;';
			const result = parseString(complexString, 20);

			expect(result.length).toBeLessThanOrEqual(20);
			expect(result.includes('&lt;div&gt;')).toBe(true);
		});

		it('should handle unicode characters', () => {
			const unicodeString = 'Hello 🌍 World';
			const result = parseString(unicodeString, 50);
			expect(result).toBe('Hello 🌍 World');
		});

		it('should handle newlines and tabs in trimming', () => {
			const result = parseString('\n\t  hello world  \t\n', 50);
			expect(result).toBe('hello world');
		});

		it('should preserve internal whitespace while trimming external', () => {
			const result = parseString('  hello   world  ', 50);
			expect(result).toBe('hello   world');
		});

		it('should handle maxLength of 0', () => {
			const result = parseString('hello world', 0);
			expect(result).toBe('...');
		});

		it('should handle maxLength of 1', () => {
			const result = parseString('hello world', 1);
			expect(result).toBe('...');
		});
	});
});
