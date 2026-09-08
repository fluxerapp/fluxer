// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DiscoveryGuild, DiscoverySearchResponse} from '@app/features/discovery/commands/DiscoveryCommands';
import * as DiscoveryCommands from '@app/features/discovery/commands/DiscoveryCommands';
import {beforeEach, describe, expect, test, vi} from 'vitest';

import Discovery from './Discovery';

vi.mock('@app/features/discovery/commands/DiscoveryCommands', () => ({
	searchGuilds: vi.fn(),
	getCategories: vi.fn(),
}));

function guild(id: string, memberCount: number): DiscoveryGuild {
	return {
		id,
		name: `Guild ${id}`,
		icon: null,
		banner: null,
		description: null,
		category_type: 0,
		primary_language: null,
		custom_tags: [],
		member_count: memberCount,
		online_count: 0,
		features: [],
		verification_level: 0,
	};
}

function page(guilds: Array<DiscoveryGuild>, total: number): DiscoverySearchResponse {
	return {guilds, total, categoryCounts: null};
}

describe('Discovery.search', () => {
	beforeEach(() => {
		vi.mocked(DiscoveryCommands.searchGuilds).mockReset();
		Discovery.reset();
	});

	test('does not append a guild that a previous page already returned', async () => {
		vi.mocked(DiscoveryCommands.searchGuilds)
			.mockResolvedValueOnce(page([guild('1', 50), guild('2', 40)], 4))
			.mockResolvedValueOnce(page([guild('2', 40), guild('3', 30)], 4));
		await Discovery.search({limit: 2, offset: 0});
		await Discovery.search({limit: 2, offset: Discovery.loadedCount});
		expect(Discovery.guilds.map((entry) => entry.id)).toEqual(['1', '2', '3']);
	});

	test('advances the pagination offset by what the server returned, not by what survived deduping', async () => {
		vi.mocked(DiscoveryCommands.searchGuilds)
			.mockResolvedValueOnce(page([guild('1', 50), guild('2', 40)], 4))
			.mockResolvedValueOnce(page([guild('2', 40), guild('3', 30)], 4));
		await Discovery.search({limit: 2, offset: 0});
		await Discovery.search({limit: 2, offset: Discovery.loadedCount});
		expect(Discovery.loadedCount).toBe(4);
	});
});
