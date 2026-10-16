// SPDX-License-Identifier: AGPL-3.0-or-later

import {isValidSingleUnicodeEmoji} from '@fluxer/schema/src/primitives/EmojiValidators';
import {describe, expect, it} from 'vitest';

describe('isValidSingleUnicodeEmoji', () => {
	describe('invalid inputs', () => {
		it('rejects empty string', () => {
			expect(isValidSingleUnicodeEmoji('')).toBe(false);
		});
		it('rejects plain text', () => {
			expect(isValidSingleUnicodeEmoji('hello')).toBe(false);
			expect(isValidSingleUnicodeEmoji('abc')).toBe(false);
		});
		it('rejects single ascii characters', () => {
			expect(isValidSingleUnicodeEmoji('a')).toBe(false);
			expect(isValidSingleUnicodeEmoji('1')).toBe(false);
			expect(isValidSingleUnicodeEmoji('#')).toBe(false);
			expect(isValidSingleUnicodeEmoji(' ')).toBe(false);
		});
		it('rejects multiple emojis', () => {
			expect(isValidSingleUnicodeEmoji('👍👍')).toBe(false);
			expect(isValidSingleUnicodeEmoji('🎉🎊')).toBe(false);
			expect(isValidSingleUnicodeEmoji('👨‍👩‍👧‍👦👨‍👩‍👧')).toBe(false);
		});
		it('rejects multiple regional indicator symbols', () => {
			expect(isValidSingleUnicodeEmoji('\u{1F1E6}\u{1F1E7}')).toBe(false);
		});
		it('rejects emoji with trailing text', () => {
			expect(isValidSingleUnicodeEmoji('👍abc')).toBe(false);
			expect(isValidSingleUnicodeEmoji('🎉!')).toBe(false);
		});
		it('rejects emoji with leading text', () => {
			expect(isValidSingleUnicodeEmoji('abc👍')).toBe(false);
			expect(isValidSingleUnicodeEmoji('!🎉')).toBe(false);
		});
		it('rejects unicode characters that are not emoji', () => {
			expect(isValidSingleUnicodeEmoji('é')).toBe(false);
			expect(isValidSingleUnicodeEmoji('中')).toBe(false);
			expect(isValidSingleUnicodeEmoji('α')).toBe(false);
		});
		it('rejects regional indicator with trailing text', () => {
			expect(isValidSingleUnicodeEmoji('\u{1F1F5}abc')).toBe(false);
		});
		it('rejects regional indicator with leading text', () => {
			expect(isValidSingleUnicodeEmoji('abc\u{1F1F5}')).toBe(false);
		});
	});
	describe('malformed emoji sequences', () => {
		it('rejects skin tone at wrong position in ZWJ sequence', () => {
			expect(isValidSingleUnicodeEmoji('🧑‍🎄🏿')).toBe(false);
		});
		it('rejects standalone ZWJ character', () => {
			expect(isValidSingleUnicodeEmoji('\u200D')).toBe(false);
		});
		it('rejects emoji followed by standalone skin tone', () => {
			expect(isValidSingleUnicodeEmoji('🎄🏿')).toBe(false);
		});
		it('rejects double skin tone modifiers', () => {
			expect(isValidSingleUnicodeEmoji('👍🏿🏻')).toBe(false);
		});
	});
});
