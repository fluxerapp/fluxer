// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {type ForwardChannelSearchValues, matchesForwardChannelSearch} from './ForwardChannelSearchMatch';

const PERSONAL_NOTES = 'personal notes';

function recipient(overrides: Partial<ForwardChannelSearchValues> = {}): ForwardChannelSearchValues {
	return {
		channelNameSearchValue: '',
		displayNameSearchValue: 'ada lovelace',
		guildNameSearchValue: '',
		isPersonalNotes: false,
		searchAliasValues: ['ada_dev', 'ada lovelace', 'countess', 'raid lead'],
		...overrides,
	};
}

function matches(query: string, values: ForwardChannelSearchValues = recipient()): boolean {
	return matchesForwardChannelSearch({
		normalizedQuery: query,
		personalNotesSearchValue: PERSONAL_NOTES,
		values,
	});
}

describe('forward channel search matching', () => {
	it('matches the rendered global display name', () => {
		expect(matches('ada lovelace')).toBe(true);
	});

	it('matches the username', () => {
		expect(matches('ada_dev')).toBe(true);
	});

	it('matches a nickname from a guild the reader is not currently viewing', () => {
		expect(matches('raid lead')).toBe(true);
	});

	it('matches a relationship nickname', () => {
		expect(matches('countess')).toBe(true);
	});

	it('does not match an unrelated query', () => {
		expect(matches('grace hopper')).toBe(false);
	});

	it('does not match a recipient with no aliases indexed beyond the display name', () => {
		const values = recipient({searchAliasValues: []});
		expect(matches('ada_dev', values)).toBe(false);
		expect(matches('ada lovelace', values)).toBe(true);
	});

	it('still matches channel and guild names', () => {
		const guildChannel = recipient({
			channelNameSearchValue: 'general',
			displayNameSearchValue: 'general',
			guildNameSearchValue: 'fluxer hq',
			searchAliasValues: [],
		});
		expect(matches('general', guildChannel)).toBe(true);
		expect(matches('fluxer hq', guildChannel)).toBe(true);
	});

	it('still matches personal notes', () => {
		const notes = recipient({displayNameSearchValue: '', isPersonalNotes: true, searchAliasValues: []});
		expect(matches('personal', notes)).toBe(true);
	});
});
