import {SubstringMatcher} from '@app/api/utils/SubstringMatcher';
import {describe, expect, test} from 'vitest';

function createRandom(seed: number): () => number {
	let state = seed >>> 0;
	return () => {
		state = (state + 0x6d2b79f5) >>> 0;
		let value = state;
		value = Math.imul(value ^ (value >>> 15), value | 1);
		value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
		return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
	};
}

const CORPUS_ALPHABET: ReadonlyArray<string> = [
	'a',
	'b',
	'c',
	'n',
	'o',
	'r',
	't',
	'u',
	'0',
	'1',
	'4',
	'8',
	' ',
	' ',
	'\t',
	'\n',
	'.',
	'-',
	'_',
	'/',
	'\\',
	'+',
	'*',
	'?',
	'[',
	']',
	'(',
	')',
	'{',
	'}',
	'|',
	'^',
	'$',
	'\u200B',
	'\u200C',
	'\u200D',
	'\uFE0F',
	'\u0301',
	'\u0335',
	'\u3000',
	'\uFF55',
	'\uFF4E',
	'\u043E',
	'\u03BF',
	'\u00E9',
	'ß',
	'ﬁ',
	'中',
	'א',
	'🔥',
	'😀',
];

function randomString(random: () => number, maxUnits: number): string {
	const count = Math.floor(random() * (maxUnits + 1));
	let result = '';
	for (let index = 0; index < count; index++) {
		result += CORPUS_ALPHABET[Math.floor(random() * CORPUS_ALPHABET.length)]!;
	}
	return result;
}

describe('SubstringMatcher', () => {
	test('is null only for an empty pattern set', () => {
		expect(SubstringMatcher.fromPatterns([])).toBeNull();
		expect(SubstringMatcher.fromPatterns(new Set<string>())).toBeNull();
		expect(SubstringMatcher.fromPatterns([''])).not.toBeNull();
	});

	test('agrees with String.includes on generated patterns and texts', () => {
		const random = createRandom(0x2468ace0);
		const mismatches: Array<string> = [];
		for (let round = 0; round < 500; round++) {
			const patternCount = 1 + Math.floor(random() * 6);
			const patterns: Array<string> = [];
			for (let index = 0; index < patternCount; index++) {
				patterns.push(randomString(random, 5));
			}
			const matcher = SubstringMatcher.fromPatterns(patterns);
			if (matcher === null) {
				mismatches.push(`unexpected null for ${JSON.stringify(patterns)}`);
				continue;
			}
			for (let index = 0; index < 10; index++) {
				const text = randomString(random, 30);
				const expected = text.length > 0 && patterns.some((pattern) => text.includes(pattern));
				const actual = matcher.test(text);
				if (expected !== actual) {
					mismatches.push(`${JSON.stringify(patterns)} / ${JSON.stringify(text)}: ${expected} vs ${actual}`);
				}
			}
		}
		expect(mismatches).toEqual([]);
	});

	test('an empty pattern matches any non-empty text', () => {
		const matcher = SubstringMatcher.fromPatterns(['', 'zzz']);
		expect(matcher).not.toBeNull();
		expect(matcher!.test('')).toBe(false);
		expect(matcher!.test('anything')).toBe(true);
	});

	test('finds patterns that are suffixes of other pattern prefixes', () => {
		const matcher = SubstringMatcher.fromPatterns(['abcd', 'bc']);
		expect(matcher!.test('xabcx')).toBe(true);
		expect(matcher!.test('abd')).toBe(false);
		expect(matcher!.test('zabcdz')).toBe(true);
	});

	test('scales to a very large pattern set without a compile limit', () => {
		const random = createRandom(0x13579bdf);
		const patterns: Array<string> = [];
		for (let index = 0; index < 20000; index++) {
			patterns.push(`${randomString(random, 4)}${index}`);
		}
		const matcher = SubstringMatcher.fromPatterns(patterns);
		expect(matcher).not.toBeNull();
		const mismatches: Array<string> = [];
		for (let index = 0; index < 100; index++) {
			const text = index % 2 === 0 ? randomString(random, 400) : `${randomString(random, 50)}${patterns[index]!}`;
			const expected = text.length > 0 && patterns.some((pattern) => text.includes(pattern));
			const actual = matcher!.test(text);
			if (expected !== actual) {
				mismatches.push(`${JSON.stringify(text)}: ${expected} vs ${actual}`);
			}
		}
		expect(mismatches).toEqual([]);
	});
});
