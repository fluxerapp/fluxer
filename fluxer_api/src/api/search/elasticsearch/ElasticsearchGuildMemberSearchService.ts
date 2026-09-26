// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import {convertToSearchableGuildMember} from '@app/api/search/guild_member/GuildMemberSearchSerializer';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import {SearchAdapterServiceBase} from '@app/api/search/SearchAdapterServiceBase';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {
	GuildMemberSearchFilters,
	SearchableGuildMember,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchGuildMemberAdapter,
	type ElasticsearchGuildMemberAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchGuildMemberAdapter';

const DEFAULT_LIMIT = 25;

function toSearchOptions(options?: {limit?: number; offset?: number}): {
	limit?: number;
	offset?: number;
} {
	return {
		limit: options?.limit ?? DEFAULT_LIMIT,
		offset: options?.offset ?? 0,
	};
}

interface ElasticsearchGuildMemberSearchServiceOptions extends ElasticsearchGuildMemberAdapterOptions {}

export class ElasticsearchGuildMemberSearchService
	extends SearchAdapterServiceBase<GuildMemberSearchFilters, SearchableGuildMember, ElasticsearchGuildMemberAdapter>
	implements IGuildMemberSearchService
{
	constructor(options: ElasticsearchGuildMemberSearchServiceOptions) {
		super(new ElasticsearchGuildMemberAdapter({client: options.client, lock: options.lock}));
	}

	async indexMember(member: GuildMember, user: User): Promise<void> {
		await this.indexDocument(convertToSearchableGuildMember(member, user));
	}

	async indexMembers(
		members: Array<{
			member: GuildMember;
			user: User;
		}>,
	): Promise<void> {
		if (members.length === 0) return;
		await this.indexDocuments(members.map(({member, user}) => convertToSearchableGuildMember(member, user)));
	}

	async updateMember(member: GuildMember, user: User): Promise<void> {
		await this.updateDocument(convertToSearchableGuildMember(member, user));
	}

	async deleteMember(guildId: GuildID, userId: UserID): Promise<void> {
		await this.deleteDocument(`${guildId}_${userId}`);
	}

	async deleteGuildMembers(guildId: GuildID): Promise<void> {
		await this.adapter.deleteByQuery({term: {guildId: guildId.toString()}});
	}

	searchMembers(
		query: string,
		filters: GuildMemberSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableGuildMember>> {
		return this.search(query, filters, toSearchOptions(options));
	}
}
