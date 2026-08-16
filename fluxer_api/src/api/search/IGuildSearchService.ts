// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ISearchAdapter as SchemaISearchAdapter,
	SearchOptions as SchemaSearchOptions,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {GuildSearchFilters, SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {GuildID} from '../BrandedTypes';
import type {Guild} from '../models/Guild';
import type {GuildDiscoveryContext} from './guild/GuildSearchSerializer';

export interface IGuildSearchService extends SchemaISearchAdapter<GuildSearchFilters, SearchableGuild> {
	indexGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void>;
	indexGuilds(guilds: Array<Guild>): Promise<void>;
	updateGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void>;
	deleteGuild(guildId: GuildID): Promise<void>;
	deleteGuilds(guildIds: Array<GuildID>): Promise<void>;
	searchGuilds(
		query: string,
		filters: GuildSearchFilters,
		options?: SchemaSearchOptions,
	): Promise<SchemaSearchResult<SearchableGuild>>;
}
