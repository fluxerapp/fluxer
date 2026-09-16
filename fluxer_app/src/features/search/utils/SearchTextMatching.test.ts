// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildChannelSearchTerms,
	consumeBestSearchTerm,
	createSearchTerm,
	escapeSearchPattern,
	fuzzySearch,
	getLoadedUnicodeConfusables,
	loadUnicodeConfusables,
	scoreSearchTerm,
	stripCombiningMarks,
	toConfusableSkeleton,
} from '@app/features/search/utils/SearchTextMatching';
import {describe, expect, it} from 'vitest';

const PATTERN_SPECIAL_CHARACTERS = '-[]/{}()*+?.\\^$|';

describe('escapeSearchPattern', () => {
	it('escapes every regex special character', () => {
		const expected = [...PATTERN_SPECIAL_CHARACTERS].map((character) => `\\${character}`).join('');
		expect(escapeSearchPattern(PATTERN_SPECIAL_CHARACTERS)).toBe(expected);
	});

	it('leaves whitespace and other punctuation alone', () => {
		expect(escapeSearchPattern('a b\tc#,!@&')).toBe('a b\tc#,!@&');
	});

	it('produces a pattern that matches the literal text', () => {
		const text = `gen (${PATTERN_SPECIAL_CHARACTERS}) chat`;
		expect(new RegExp(`^${escapeSearchPattern(text)}$`).test(text)).toBe(true);
	});
});

describe('stripCombiningMarks', () => {
	it('removes combining diacritics from precomposed and decomposed text', () => {
		expect(stripCombiningMarks('José')).toBe('Jose');
		expect(stripCombiningMarks('cafe\u{301}')).toBe('cafe');
	});

	it('keeps marks outside the combining diacritical block', () => {
		expect(stripCombiningMarks('x\u{483}')).toBe('x\u{483}');
	});
});

describe('fuzzySearch', () => {
	it('matches an in-order subsequence', () => {
		expect(fuzzySearch('gnrl', 'general')).toBe(true);
		expect(fuzzySearch('lrg', 'general')).toBe(false);
	});

	it('requires equality when the lengths match', () => {
		expect(fuzzySearch('gen', 'gen')).toBe(true);
		expect(fuzzySearch('abc', 'abd')).toBe(false);
	});

	it('rejects a needle longer than the haystack', () => {
		expect(fuzzySearch('general', 'gen')).toBe(false);
	});

	it('is case-sensitive', () => {
		expect(fuzzySearch('G', 'general')).toBe(false);
	});

	it('compares UTF-16 code units', () => {
		expect(fuzzySearch('\u{1f600}', 'a\u{1f600}b')).toBe(true);
	});
});

describe('scoreSearchTerm', () => {
	it('ranks exact, prefix, contains, every word and fuzzy matches', () => {
		expect(scoreSearchTerm('general', createSearchTerm('general'))).toBe(10);
		expect(scoreSearchTerm('general-chat', createSearchTerm('general'))).toBe(7);
		expect(scoreSearchTerm('my-general', createSearchTerm('general'))).toBe(5);
		expect(scoreSearchTerm('general-chat', createSearchTerm('chat general'))).toBe(3);
		expect(scoreSearchTerm('general', createSearchTerm('gnrl'))).toBe(1);
		expect(scoreSearchTerm('general', createSearchTerm('xyz'))).toBe(0);
	});

	it('skips the fuzzy tier when fuzzy matching is off', () => {
		expect(scoreSearchTerm('general', createSearchTerm('gnrl'), false)).toBe(0);
	});

	it('splits words on commas and spaces', () => {
		expect(scoreSearchTerm('alice and bob', createSearchTerm('bob,alice'))).toBe(3);
	});
});

describe('buildChannelSearchTerms', () => {
	it('builds one term per word and drops empty tokens', () => {
		const terms = buildChannelSearchTerms('general');
		expect(terms.map((term) => [term.queryLower, term.isFullMatch])).toEqual([['general', false]]);
	});

	it('puts a full-phrase term first when the query has a space', () => {
		const terms = buildChannelSearchTerms(' General  Chat');
		expect(terms.map((term) => [term.queryLower, term.isFullMatch])).toEqual([
			[' general  chat', true],
			['general', false],
			['chat', false],
		]);
	});

	it('lets only the first space of the full phrase match a dash', () => {
		const [fullPhrase] = buildChannelSearchTerms('a b c');
		expect(fullPhrase.exactQuery.test('a-b c')).toBe(true);
		expect(fullPhrase.exactQuery.test('a b c')).toBe(true);
		expect(fullPhrase.exactQuery.test('a b-c')).toBe(false);
	});
});

describe('consumeBestSearchTerm', () => {
	it('removes the winning word term and returns its score', () => {
		const terms = [...buildChannelSearchTerms('chat general')];
		expect(consumeBestSearchTerm('general-chat', terms, true)).toBe(7);
		expect(terms.map((term) => term.queryLower)).toEqual(['chat general', 'chat']);
	});

	it('empties the terms when the full phrase wins', () => {
		const terms = [...buildChannelSearchTerms('general chat')];
		expect(consumeBestSearchTerm('general-chat', terms, true)).toBe(7);
		expect(terms).toEqual([]);
	});

	it('keeps the terms when nothing matches', () => {
		const terms = [...buildChannelSearchTerms('xyz')];
		expect(consumeBestSearchTerm('general', terms, true)).toBe(0);
		expect(terms).toHaveLength(1);
	});
});

describe('unicode confusables', () => {
	it('has no table before it loads', () => {
		expect(getLoadedUnicodeConfusables()).toBeNull();
	});

	it('loads the full Unicode 15.1 table once', async () => {
		const confusables = await loadUnicodeConfusables();
		expect(confusables.size).toBe(6311);
		expect(await loadUnicodeConfusables()).toBe(confusables);
		expect(getLoadedUnicodeConfusables()).toBe(confusables);
	});

	it('maps known look-alikes', async () => {
		const confusables = await loadUnicodeConfusables();
		expect(confusables.get('0')).toBe('O');
		expect(confusables.get('1')).toBe('l');
		expect(confusables.get('I')).toBe('l');
		expect(confusables.get('m')).toBe('rn');
		expect(confusables.get('\u{1d7ce}')).toBe('O');
	});

	it('builds lowercase skeletons', async () => {
		const confusables = await loadUnicodeConfusables();
		expect(toConfusableSkeleton('b0b', confusables)).toBe('bob');
		expect(toConfusableSkeleton('Il1|', confusables)).toBe('llll');
		expect(toConfusableSkeleton('mod', confusables)).toBe('rnod');
	});

	it('only lowercases and decomposes without a table', () => {
		expect(toConfusableSkeleton('B0B', new Map())).toBe('b0b');
		expect(toConfusableSkeleton('É', new Map())).toBe('e\u{301}');
	});
});
