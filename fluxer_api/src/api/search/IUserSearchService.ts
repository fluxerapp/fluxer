// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import type {
	ISearchAdapter as SchemaISearchAdapter,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {SearchableUser, UserSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface IUserSearchService extends SchemaISearchAdapter<UserSearchFilters, SearchableUser> {
	indexUser(user: User): Promise<void>;
	indexUsers(users: Array<User>): Promise<void>;
	updateUser(user: User): Promise<void>;
	deleteUser(userId: UserID): Promise<void>;
	deleteUsers(userIds: Array<UserID>): Promise<void>;
	searchUsers(
		query: string,
		filters: UserSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableUser>>;
}
