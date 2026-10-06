// SPDX-License-Identifier: AGPL-3.0-or-later

import {ProfileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {beforeEach, describe, expect, it} from 'vitest';

describe('ProfileSubstringBlocklistCache', () => {
	let cache: ProfileSubstringBlocklistCache;
	beforeEach(() => {
		cache = new ProfileSubstringBlocklistCache();
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
