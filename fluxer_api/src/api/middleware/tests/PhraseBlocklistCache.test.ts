// SPDX-License-Identifier: AGPL-3.0-or-later

import {PhraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {beforeEach, describe, expect, it} from 'vitest';

describe('PhraseBlocklistCache', () => {
	let cache: PhraseBlocklistCache;
	beforeEach(() => {
		cache = new PhraseBlocklistCache();
	});
	it('matches banned phrases case-insensitively', () => {
		cache.add('Unban Tor');
		expect(cache.containsBannedPhrase('please UNBAN TOR right now')).toBe(true);
	});
	it('matches phrases split apart with whitespace and punctuation', () => {
		cache.add('unban tor');
		expect(cache.containsBannedPhrase('u n b a n t o r')).toBe(true);
		expect(cache.containsBannedPhrase('u.n-b_a_n t/o\\r')).toBe(true);
		expect(cache.containsBannedPhrase('u 🔥 n 🔥 b 🔥 a 🔥 n t o r')).toBe(true);
	});
	it('matches compatibility, invisible, and combining-mark bypasses', () => {
		cache.add('unban tor');
		expect(cache.containsBannedPhrase('ｕｎｂａｎ　ｔｏｒ')).toBe(true);
		expect(cache.containsBannedPhrase('u\u200Bn\u200Bb\u200Ba\u200Bn t\u200Co\u200Dr')).toBe(true);
		expect(cache.containsBannedPhrase('u̵n̵b̵a̵n̵ t̵o̵r̵')).toBe(true);
	});
	it('matches mixed-script lookalikes after transliteration', () => {
		cache.add('unban tor');
		expect(cache.containsBannedPhrase('unban tоr')).toBe(true);
		expect(cache.containsBannedPhrase('unban tοr')).toBe(true);
	});
	it('does not match unrelated text', () => {
		cache.add('unban tor');
		expect(cache.containsBannedPhrase('please keep tor banned')).toBe(false);
	});
	it('does not over-broaden punctuation-heavy bans into single letters', () => {
		cache.add('c++');
		expect(cache.containsBannedPhrase('compiler')).toBe(false);
		expect(cache.containsBannedPhrase('ship c++ code')).toBe(true);
	});
	it('remove() stops matching all normalized variants', () => {
		cache.add('unban tor');
		expect(cache.containsBannedPhrase('u n b a n t o r')).toBe(true);
		cache.remove('UNBAN TOR');
		expect(cache.containsBannedPhrase('u n b a n t o r')).toBe(false);
	});
	it('isPhraseBanned uses canonical exact membership', () => {
		cache.add('Unban Tor');
		expect(cache.isPhraseBanned('unban tor')).toBe(true);
		expect(cache.isPhraseBanned('ｕｎｂａｎ tor')).toBe(true);
		expect(cache.isPhraseBanned('u n b a n t o r')).toBe(false);
	});
	it('ignores phrases that normalize down to empty content', () => {
		cache.add('\u200B\u200C\u200D');
		expect(cache.size).toBe(0);
	});
});
