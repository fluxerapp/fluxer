// SPDX-License-Identifier: AGPL-3.0-or-later

import {transliterate} from 'transliteration';
import {describe, expect, test} from 'vitest';
import {buildPhraseMatchForms, canonicalizeStoredPhrase} from '../../utils/PhraseBlocklistNormalization';
import {SubstringMatcher} from '../../utils/SubstringMatcher';
import {PhraseBlocklistCache} from '../PhraseBlocklistCache';

const LEGACY_CONTROL_OR_FORMAT_REGEX = /[\p{Cc}\p{Cf}\uFFFE\uFFFF]/gu;
const LEGACY_VARIATION_SELECTOR_REGEX = /(?:[\uFE00-\uFE0F]|[\u{E0100}-\u{E01EF}])/gu;
const LEGACY_COMBINING_MARKS_REGEX = /\p{M}+/gu;
const LEGACY_NON_ALPHANUMERIC_REGEX = /[^\p{L}\p{N}]+/gu;
const LEGACY_ASCII_NON_ALPHANUMERIC_REGEX = /[^a-z0-9]+/g;
const LEGACY_WHITESPACE_REGEX = /\s+/gu;
const LEGACY_MIN_AGGRESSIVE_FORM_LENGTH = 3;

interface LegacyForms {
	raw: string;
	words: string;
	compact: string;
	asciiWords: string;
	asciiCompact: string;
}

function legacyCollapseWhitespace(value: string): string {
	return value.replace(LEGACY_WHITESPACE_REGEX, ' ').trim();
}

function legacyStripIgnorableCharacters(value: string): string {
	return value.replace(LEGACY_CONTROL_OR_FORMAT_REGEX, '').replace(LEGACY_VARIATION_SELECTOR_REGEX, '');
}

function legacyMaybeKeepAggressiveForm(value: string): string {
	return value.length >= LEGACY_MIN_AGGRESSIVE_FORM_LENGTH ? value : '';
}

function legacyBuildWordForm(value: string): string {
	return legacyCollapseWhitespace(
		value.normalize('NFKD').replace(LEGACY_COMBINING_MARKS_REGEX, '').replace(LEGACY_NON_ALPHANUMERIC_REGEX, ' '),
	);
}

function legacyBuildAsciiWordForm(value: string): string {
	return legacyCollapseWhitespace(
		transliterate(value)
			.toLowerCase()
			.normalize('NFKD')
			.replace(LEGACY_COMBINING_MARKS_REGEX, '')
			.replace(LEGACY_ASCII_NON_ALPHANUMERIC_REGEX, ' '),
	);
}

function legacyCanonicalize(value: string): string {
	return legacyStripIgnorableCharacters(value.normalize('NFKC')).toLowerCase().trim();
}

function legacyBuildPhraseMatchForms(value: string): LegacyForms {
	const raw = legacyCanonicalize(value);
	const words = legacyMaybeKeepAggressiveForm(legacyBuildWordForm(raw));
	const compact = legacyMaybeKeepAggressiveForm(words.replace(LEGACY_WHITESPACE_REGEX, ''));
	const asciiWords = legacyMaybeKeepAggressiveForm(legacyBuildAsciiWordForm(raw));
	const asciiCompact = legacyMaybeKeepAggressiveForm(asciiWords.replace(LEGACY_WHITESPACE_REGEX, ''));
	return {raw, words, compact, asciiWords, asciiCompact};
}

class LegacyPhraseBlocklist {
	private rawPhrases: Array<string> = [];
	private rawPhraseSet = new Set<string>();
	private wordPhrases: Array<string> = [];
	private compactPhrases: Array<string> = [];
	private asciiWordPhrases: Array<string> = [];
	private asciiCompactPhrases: Array<string> = [];

	add(phrase: string): void {
		const canonical = legacyCanonicalize(phrase);
		if (!canonical || this.rawPhraseSet.has(canonical)) return;
		this.rawPhrases.push(canonical);
		this.rebuildMatchers();
	}

	remove(phrase: string): void {
		const canonical = legacyCanonicalize(phrase);
		if (!canonical || !this.rawPhraseSet.has(canonical)) return;
		this.rawPhrases = this.rawPhrases.filter((item) => item !== canonical);
		this.rebuildMatchers();
	}

	private rebuildMatchers(): void {
		const rawPhraseSet = new Set<string>();
		const wordPhraseSet = new Set<string>();
		const compactPhraseSet = new Set<string>();
		const asciiWordPhraseSet = new Set<string>();
		const asciiCompactPhraseSet = new Set<string>();
		for (const phrase of this.rawPhrases) {
			const canonical = legacyCanonicalize(phrase);
			if (!canonical) continue;
			const forms = legacyBuildPhraseMatchForms(canonical);
			rawPhraseSet.add(forms.raw);
			if (forms.words) wordPhraseSet.add(forms.words);
			if (forms.compact) compactPhraseSet.add(forms.compact);
			if (forms.asciiWords) asciiWordPhraseSet.add(forms.asciiWords);
			if (forms.asciiCompact) asciiCompactPhraseSet.add(forms.asciiCompact);
		}
		this.rawPhraseSet = rawPhraseSet;
		this.rawPhrases = Array.from(rawPhraseSet);
		this.wordPhrases = Array.from(wordPhraseSet);
		this.compactPhrases = Array.from(compactPhraseSet);
		this.asciiWordPhrases = Array.from(asciiWordPhraseSet);
		this.asciiCompactPhrases = Array.from(asciiCompactPhraseSet);
	}

	private matchAny(text: string, phrases: Array<string>): boolean {
		if (!text || phrases.length === 0) return false;
		for (const phrase of phrases) {
			if (text.includes(phrase)) return true;
		}
		return false;
	}

	containsBannedPhrase(text: string): boolean {
		if (this.rawPhrases.length === 0) return false;
		const forms = legacyBuildPhraseMatchForms(text);
		return (
			this.matchAny(forms.raw, this.rawPhrases) ||
			this.matchAny(forms.words, this.wordPhrases) ||
			this.matchAny(forms.compact, this.compactPhrases) ||
			this.matchAny(forms.asciiWords, this.asciiWordPhrases) ||
			this.matchAny(forms.asciiCompact, this.asciiCompactPhrases)
		);
	}

	isPhraseBanned(phrase: string): boolean {
		const canonical = legacyCanonicalize(phrase);
		return !!canonical && this.rawPhraseSet.has(canonical);
	}

	get size(): number {
		return this.rawPhraseSet.size;
	}
}

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

const TABLE_PHRASE_LISTS: ReadonlyArray<ReadonlyArray<string>> = [
	[],
	[''],
	['a'],
	['ab', 'b'],
	['abc', 'bc', 'c'],
	['unban tor', 'unban', 'ban'],
	['c++', 'c+', '++'],
	['a.b', 'a*b', 'a|b', 'a(b)c', 'a[b]c', 'a{2}', '^a$', 'a\\b', 'a?b'],
	['🔥', '🔥🔥'],
	['он', 'on', 'o'],
	['\u00E9', 'e', 'e\u0301'],
	['ｕｎ', 'un'],
	['aaa', 'aa', 'a'],
	['aaaa', 'aab', 'aba', 'baa'],
	['\u200B', 'x\u200By'],
	['tor', 'rot', 'ort'],
];

const TABLE_TEXTS: ReadonlyArray<string> = [
	'',
	'a',
	'ab',
	'abc',
	'xabcx',
	'aa',
	'aaa',
	'aaaa',
	'banana',
	'please UNBAN TOR right now',
	'u n b a n t o r',
	'u.n-b_a_n t/o\\r',
	'ｕｎｂａｎ　ｔｏｒ',
	'u\u200Bn\u200Bb\u200Ba\u200Bn t\u200Co\u200Dr',
	'u̵n̵b̵a̵n̵ t̵o̵r̵',
	'unban tоr',
	'unban tοr',
	'please keep tor banned',
	'ship c++ code',
	'compiler',
	'a\\b',
	'a?b',
	'^a$',
	'a{2}',
	'🔥🔥🔥',
	'x🔥y',
	'caf\u00E9',
	'cafe\u0301',
	'中中中',
	'אא',
	'   ',
	'\t\n\t',
	'\u200B\u200C\u200D',
];

function collectPhraseMismatches(phrases: ReadonlyArray<string>, texts: ReadonlyArray<string>): Array<string> {
	const legacy = new LegacyPhraseBlocklist();
	const current = new PhraseBlocklistCache();
	for (const phrase of phrases) {
		legacy.add(phrase);
		current.add(phrase);
	}
	const mismatches: Array<string> = [];
	if (legacy.size !== current.size) {
		mismatches.push(`size ${JSON.stringify(phrases)}: legacy=${legacy.size} current=${current.size}`);
	}
	for (const phrase of phrases) {
		const expected = legacy.isPhraseBanned(phrase);
		const actual = current.isPhraseBanned(phrase);
		if (expected !== actual) {
			mismatches.push(`isPhraseBanned ${JSON.stringify(phrase)}: legacy=${expected} current=${actual}`);
		}
	}
	for (const text of texts) {
		const expected = legacy.containsBannedPhrase(text);
		const actual = current.containsBannedPhrase(text);
		if (expected !== actual) {
			mismatches.push(
				`containsBannedPhrase ${JSON.stringify(phrases)} / ${JSON.stringify(text)}: legacy=${expected} current=${actual}`,
			);
		}
	}
	return mismatches;
}

describe('phrase blocklist matcher equivalence', () => {
	test('table-driven phrase lists match the legacy linear scan', () => {
		const mismatches: Array<string> = [];
		for (const phrases of TABLE_PHRASE_LISTS) {
			mismatches.push(...collectPhraseMismatches(phrases, TABLE_TEXTS));
		}
		expect(mismatches).toEqual([]);
	});

	test('generated phrase lists and texts match the legacy linear scan', () => {
		const random = createRandom(0x5eed1234);
		const mismatches: Array<string> = [];
		for (let round = 0; round < 300; round++) {
			const phraseCount = 1 + Math.floor(random() * 8);
			const phrases: Array<string> = [];
			for (let index = 0; index < phraseCount; index++) {
				phrases.push(randomString(random, 6));
			}
			const texts: Array<string> = [];
			for (let index = 0; index < 8; index++) {
				texts.push(randomString(random, 40));
			}
			for (const phrase of phrases) {
				texts.push(phrase);
				texts.push(`${randomString(random, 5)}${phrase}${randomString(random, 5)}`);
			}
			mismatches.push(...collectPhraseMismatches(phrases, texts));
		}
		expect(mismatches).toEqual([]);
	});

	test('removal keeps both implementations in step', () => {
		const random = createRandom(0x1234abcd);
		const mismatches: Array<string> = [];
		const legacy = new LegacyPhraseBlocklist();
		const current = new PhraseBlocklistCache();
		const phrases: Array<string> = [];
		for (let index = 0; index < 40; index++) {
			phrases.push(randomString(random, 6));
		}
		for (const phrase of phrases) {
			legacy.add(phrase);
			current.add(phrase);
		}
		for (let index = 0; index < phrases.length; index += 3) {
			legacy.remove(phrases[index]!);
			current.remove(phrases[index]!);
		}
		for (let index = 0; index < 200; index++) {
			const text = randomString(random, 30);
			const expected = legacy.containsBannedPhrase(text);
			const actual = current.containsBannedPhrase(text);
			if (expected !== actual) {
				mismatches.push(`${JSON.stringify(text)}: legacy=${expected} current=${actual}`);
			}
		}
		expect(mismatches).toEqual([]);
	});

	test('a large blocklist stays exact', () => {
		const random = createRandom(0xfeedface);
		const legacy = new LegacyPhraseBlocklist();
		const current = new PhraseBlocklistCache();
		for (let index = 0; index < 300; index++) {
			const phrase = `${randomString(random, 4)}${index}`;
			legacy.add(phrase);
			current.add(phrase);
		}
		expect(current.size).toBe(legacy.size);
		const mismatches: Array<string> = [];
		for (let index = 0; index < 300; index++) {
			const text = randomString(random, 200);
			const expected = legacy.containsBannedPhrase(text);
			const actual = current.containsBannedPhrase(text);
			if (expected !== actual) {
				mismatches.push(`${JSON.stringify(text)}: legacy=${expected} current=${actual}`);
			}
		}
		expect(mismatches).toEqual([]);
	});
});

describe('buildPhraseMatchForms laziness', () => {
	test('lazy forms equal the eager forms for the whole corpus', () => {
		const random = createRandom(0xc0ffee11);
		const values: Array<string> = [...TABLE_TEXTS];
		for (let index = 0; index < 500; index++) {
			values.push(randomString(random, 40));
		}
		const mismatches: Array<string> = [];
		for (const value of values) {
			const expected = legacyBuildPhraseMatchForms(value);
			const forms = buildPhraseMatchForms(value);
			const actual = {
				raw: forms.raw,
				words: forms.words,
				compact: forms.compact,
				asciiWords: forms.asciiWords,
				asciiCompact: forms.asciiCompact,
			};
			if (JSON.stringify(expected) !== JSON.stringify(actual)) {
				mismatches.push(`${JSON.stringify(value)}: ${JSON.stringify(expected)} vs ${JSON.stringify(actual)}`);
			}
		}
		expect(mismatches).toEqual([]);
	});

	test('reading a later form after an earlier one is still correct', () => {
		const value = 'unban tоr';
		const forms = buildPhraseMatchForms(value);
		const expected = legacyBuildPhraseMatchForms(value);
		expect(forms.asciiCompact).toBe(expected.asciiCompact);
		expect(forms.raw).toBe(expected.raw);
		expect(forms.words).toBe(expected.words);
		expect(forms.compact).toBe(expected.compact);
		expect(forms.asciiWords).toBe(expected.asciiWords);
	});

	test('canonicalizeStoredPhrase is unchanged', () => {
		const random = createRandom(0x0badf00d);
		const mismatches: Array<string> = [];
		for (let index = 0; index < 500; index++) {
			const value = randomString(random, 30);
			if (canonicalizeStoredPhrase(value) !== legacyCanonicalize(value)) {
				mismatches.push(JSON.stringify(value));
			}
		}
		expect(mismatches).toEqual([]);
	});
});

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
