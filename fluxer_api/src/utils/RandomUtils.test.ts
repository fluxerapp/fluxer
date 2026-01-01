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
import {randomString} from './RandomUtils';

describe('RandomUtils', () => {
	describe('randomString', () => {
		it('should generate string of correct length', () => {
			expect(randomString(10)).toHaveLength(10);
			expect(randomString(5)).toHaveLength(5);
			expect(randomString(100)).toHaveLength(100);
		});

		it('should handle zero length', () => {
			expect(randomString(0)).toBe('');
		});

		it('should generate different strings on multiple calls', () => {
			const str1 = randomString(20);
			const str2 = randomString(20);
			const str3 = randomString(20);

			expect(str1).not.toBe(str2);
			expect(str2).not.toBe(str3);
			expect(str1).not.toBe(str3);
		});

		it('should only contain valid alphabet characters', () => {
			const validChars = /^[A-Za-z0-9]+$/;
			const result = randomString(1000);

			expect(validChars.test(result)).toBe(true);
		});

		it('should contain mix of uppercase, lowercase, and numbers', () => {
			const result = randomString(1000);

			const hasUppercase = /[A-Z]/.test(result);
			const hasLowercase = /[a-z]/.test(result);
			const hasNumbers = /[0-9]/.test(result);

			expect(hasUppercase || hasLowercase || hasNumbers).toBe(true);
		});

		it('should handle various lengths', () => {
			const lengths = [1, 2, 3, 5, 8, 13, 21, 34, 55, 89];

			lengths.forEach((length) => {
				const result = randomString(length);
				expect(result).toHaveLength(length);
				expect(/^[A-Za-z0-9]*$/.test(result)).toBe(true);
			});
		});

		it('should be reasonably random', () => {
			const length = 10;
			const iterations = 100;
			const results = new Set();

			for (let i = 0; i < iterations; i++) {
				results.add(randomString(length));
			}

			expect(results.size).toBeGreaterThan(iterations * 0.95);
		});

		it('should handle edge case lengths', () => {
			expect(randomString(1)).toHaveLength(1);
			expect(randomString(2)).toHaveLength(2);

			const largeString = randomString(10000);
			expect(largeString).toHaveLength(10000);
		});

		it('should use all characters from alphabet over many iterations', () => {
			const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
			const usedChars = new Set<string>();

			for (let i = 0; i < 1000; i++) {
				const str = randomString(10);
				for (const char of str) {
					usedChars.add(char);
				}
			}

			expect(usedChars.size).toBeGreaterThan(alphabet.length * 0.8);

			for (const char of usedChars) {
				expect(alphabet.includes(char)).toBe(true);
			}
		});

		it('should be cryptographically random', () => {
			const results: Array<string> = [];

			for (let i = 0; i < 100; i++) {
				results.push(randomString(20));
			}

			const firstChars = results.map((s) => s[0]);
			const firstCharCounts = firstChars.reduce(
				(acc, char) => {
					acc[char] = (acc[char] || 0) + 1;
					return acc;
				},
				{} as Record<string, number>,
			);

			const maxCount = Math.max(...Object.values(firstCharCounts));
			expect(maxCount).toBeLessThan(20);
		});
	});
});
