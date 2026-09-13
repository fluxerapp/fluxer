// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MentionFilters,
	messageMatchesMentionTypeFilters,
} from '@app/features/notification/utils/MentionFeedFilters';
import {describe, expect, it} from 'vitest';

const DIRECT_ONLY = {mention_everyone: false, mention_roles: []};
const EVERYONE_ONLY = {mention_everyone: true, mention_roles: []};
const ROLE_ONLY = {mention_everyone: false, mention_roles: ['10']};
const EVERYONE_AND_ROLE = {mention_everyone: true, mention_roles: ['10']};

function filters(overrides: Partial<MentionFilters>): MentionFilters {
	return {
		includeEveryone: true,
		includeRoles: true,
		includeGuilds: true,
		...overrides,
	};
}

describe('messageMatchesMentionTypeFilters', () => {
	it('keeps direct user mentions regardless of mass mention filters', () => {
		expect(messageMatchesMentionTypeFilters(DIRECT_ONLY, filters({includeEveryone: false, includeRoles: false}))).toBe(
			true,
		);
	});
	it('excludes role mentions when role mentions are disabled', () => {
		expect(messageMatchesMentionTypeFilters(ROLE_ONLY, filters({includeRoles: false}))).toBe(false);
	});
	it('excludes everyone mentions when everyone mentions are disabled', () => {
		expect(messageMatchesMentionTypeFilters(EVERYONE_ONLY, filters({includeEveryone: false}))).toBe(false);
	});
	it('excludes mixed mass mentions when either matching mass mention type is disabled', () => {
		expect(messageMatchesMentionTypeFilters(EVERYONE_AND_ROLE, filters({includeEveryone: false}))).toBe(false);
		expect(messageMatchesMentionTypeFilters(EVERYONE_AND_ROLE, filters({includeRoles: false}))).toBe(false);
	});
});
