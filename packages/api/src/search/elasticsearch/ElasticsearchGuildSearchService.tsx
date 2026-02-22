/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {GuildID} from '@fluxer/api/src/BrandedTypes';
import type {Guild} from '@fluxer/api/src/models/Guild';
import {ElasticsearchSearchServiceBase} from '@fluxer/api/src/search/elasticsearch/ElasticsearchSearchServiceBase';
import {convertToSearchableGuild, type GuildDiscoveryContext} from '@fluxer/api/src/search/guild/GuildSearchSerializer';
import type {IGuildSearchService} from '@fluxer/api/src/search/IGuildSearchService';
import {
	ElasticsearchGuildAdapter,
	type ElasticsearchGuildAdapterOptions,
} from '@fluxer/elasticsearch_search/src/adapters/ElasticsearchGuildAdapter';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {GuildSearchFilters, SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface ElasticsearchGuildSearchServiceOptions extends ElasticsearchGuildAdapterOptions {}

export class ElasticsearchGuildSearchService
	extends ElasticsearchSearchServiceBase<GuildSearchFilters, SearchableGuild, ElasticsearchGuildAdapter>
	implements IGuildSearchService
{
	constructor(options: ElasticsearchGuildSearchServiceOptions) {
		super(new ElasticsearchGuildAdapter({client: options.client}));
	}

	async indexGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void> {
		await this.indexDocument(convertToSearchableGuild(guild, discovery));
	}

	async indexGuilds(guilds: Array<Guild>): Promise<void> {
		if (guilds.length === 0) return;
		await this.indexDocuments(guilds.map((g) => convertToSearchableGuild(g)));
	}

	async updateGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void> {
		await this.updateDocument(convertToSearchableGuild(guild, discovery));
	}

	async deleteGuild(guildId: GuildID): Promise<void> {
		await this.deleteDocument(guildId.toString());
	}

	async deleteGuilds(guildIds: Array<GuildID>): Promise<void> {
		await this.deleteDocuments(guildIds.map((id) => id.toString()));
	}

	searchGuilds(
		query: string,
		filters: GuildSearchFilters,
		options?: {limit?: number; offset?: number},
	): Promise<SchemaSearchResult<SearchableGuild>> {
		return this.search(query, filters, options);
	}
}
