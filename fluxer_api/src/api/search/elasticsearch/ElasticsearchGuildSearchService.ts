// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	SearchOptions as SchemaSearchOptions,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {GuildSearchFilters, SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchGuildAdapter,
	type ElasticsearchGuildAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchGuildAdapter';
import type {GuildID} from '../../BrandedTypes';
import type {IGuildDiscoveryRepository} from '../../guild/repositories/GuildDiscoveryRepository';
import type {Guild} from '../../models/Guild';
import {convertToSearchableGuild, type GuildDiscoveryContext} from '../guild/GuildSearchSerializer';
import {resolveDiscoveryContextForIndexing} from '../guild/LazyDiscoveryMigration';
import type {IGuildSearchService} from '../IGuildSearchService';
import {SearchAdapterServiceBase} from '../SearchAdapterServiceBase';

interface ElasticsearchGuildSearchServiceOptions extends ElasticsearchGuildAdapterOptions {
	discoveryRepository?: IGuildDiscoveryRepository;
}

export class ElasticsearchGuildSearchService
	extends SearchAdapterServiceBase<GuildSearchFilters, SearchableGuild, ElasticsearchGuildAdapter>
	implements IGuildSearchService
{
	private readonly discoveryRepository: IGuildDiscoveryRepository | undefined;

	constructor(options: ElasticsearchGuildSearchServiceOptions) {
		super(new ElasticsearchGuildAdapter({client: options.client, lock: options.lock}));
		this.discoveryRepository = options.discoveryRepository;
	}

	async indexGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void> {
		const context = await resolveDiscoveryContextForIndexing(guild, discovery, this.discoveryRepository);
		await this.indexDocument(convertToSearchableGuild(guild, context));
	}

	async indexGuilds(guilds: Array<Guild>): Promise<void> {
		if (guilds.length === 0) return;
		const docs = await Promise.all(
			guilds.map(async (g) =>
				convertToSearchableGuild(g, await resolveDiscoveryContextForIndexing(g, undefined, this.discoveryRepository)),
			),
		);
		await this.indexDocuments(docs);
	}

	async updateGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void> {
		const context = await resolveDiscoveryContextForIndexing(guild, discovery, this.discoveryRepository);
		await this.updateDocument(convertToSearchableGuild(guild, context));
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
		options?: SchemaSearchOptions,
	): Promise<SchemaSearchResult<SearchableGuild>> {
		return this.search(query, filters, options);
	}
}
