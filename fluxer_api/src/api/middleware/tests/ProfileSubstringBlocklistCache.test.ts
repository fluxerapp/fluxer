// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {ProfileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('ProfileSubstringBlocklistCache', () => {
	let cache: ProfileSubstringBlocklistCache;
	beforeEach(() => {
		cache = new ProfileSubstringBlocklistCache();
	});
	afterEach(() => {
		cache.resetForTesting();
	});
	it('refresh replaces every scope while preserving name widening and normalized matches', async () => {
		cache.add('bio', 'oldbio');
		const repository = new AdminRepository();
		await repository.banProfileSubstring({
			scope: 'username',
			substring: 'Unban Tor',
			added_at: null,
			added_by: null,
			notes: null,
		});
		await repository.banProfileSubstring({
			scope: 'bio',
			substring: 'blockedbio',
			added_at: null,
			added_by: null,
			notes: null,
		});
		await cache.refresh();
		expect(cache.containsBannedSubstring('nickname', 'ｕｎｂａｎ　ｔｏｒ')).toBe(true);
		expect(cache.containsBannedSubstring('global_name', 'u n b a n t o r')).toBe(true);
		expect(cache.containsBannedSubstring('bio', 'unban tor')).toBe(false);
		expect(cache.containsBannedSubstring('username', 'blockedbio')).toBe(false);
		expect(cache.containsBannedSubstring('bio', 'blockedbio')).toBe(true);
		expect(cache.containsBannedSubstring('bio', 'oldbio')).toBe(false);
		cache.remove('username', 'UNBAN TOR');
		expect(cache.containsBannedSubstring('nickname', 'u n b a n t o r')).toBe(false);
	});
	it('shares banned substrings across profile name scopes', () => {
		cache.add('username', 'blockedslug');
		expect(cache.containsBannedSubstring('global_name', 'BlockedSlug Display')).toBe(true);
		expect(cache.containsBannedSubstring('nickname', 'BlockedSlug Nick')).toBe(true);
		expect(cache.isSubstringBanned('global_name', 'blockedslug')).toBe(true);
	});
	it('keeps bio and pronoun scopes separate from profile names', () => {
		cache.add('username', 'blockedslug');
		expect(cache.containsBannedSubstring('bio', 'bio with blockedslug')).toBe(false);
		expect(cache.containsBannedSubstring('pronouns', 'blockedslug')).toBe(false);
	});
});
