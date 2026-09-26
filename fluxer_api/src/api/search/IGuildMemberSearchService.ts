// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import type {
	ISearchAdapter as SchemaISearchAdapter,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {
	GuildMemberSearchFilters,
	SearchableGuildMember,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface IGuildMemberSearchService
	extends SchemaISearchAdapter<GuildMemberSearchFilters, SearchableGuildMember> {
	indexMember(member: GuildMember, user: User): Promise<void>;
	indexMembers(
		members: Array<{
			member: GuildMember;
			user: User;
		}>,
	): Promise<void>;
	updateMember(member: GuildMember, user: User): Promise<void>;
	deleteMember(guildId: GuildID, userId: UserID): Promise<void>;
	deleteGuildMembers(guildId: GuildID): Promise<void>;
	searchMembers(
		query: string,
		filters: GuildMemberSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableGuildMember>>;
}
