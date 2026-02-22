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

import type {UserID} from '@fluxer/api/src/BrandedTypes';
import type {User} from '@fluxer/api/src/models/User';
import {ElasticsearchSearchServiceBase} from '@fluxer/api/src/search/elasticsearch/ElasticsearchSearchServiceBase';
import type {IUserSearchService} from '@fluxer/api/src/search/IUserSearchService';
import {convertToSearchableUser} from '@fluxer/api/src/search/user/UserSearchSerializer';
import {
	ElasticsearchUserAdapter,
	type ElasticsearchUserAdapterOptions,
} from '@fluxer/elasticsearch_search/src/adapters/ElasticsearchUserAdapter';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {SearchableUser, UserSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface ElasticsearchUserSearchServiceOptions extends ElasticsearchUserAdapterOptions {}

export class ElasticsearchUserSearchService
	extends ElasticsearchSearchServiceBase<UserSearchFilters, SearchableUser, ElasticsearchUserAdapter>
	implements IUserSearchService
{
	constructor(options: ElasticsearchUserSearchServiceOptions) {
		super(new ElasticsearchUserAdapter({client: options.client}));
	}

	async indexUser(user: User): Promise<void> {
		await this.indexDocument(convertToSearchableUser(user));
	}

	async indexUsers(users: Array<User>): Promise<void> {
		if (users.length === 0) return;
		await this.indexDocuments(users.map(convertToSearchableUser));
	}

	async updateUser(user: User): Promise<void> {
		await this.updateDocument(convertToSearchableUser(user));
	}

	async deleteUser(userId: UserID): Promise<void> {
		await this.deleteDocument(userId.toString());
	}

	async deleteUsers(userIds: Array<UserID>): Promise<void> {
		await this.deleteDocuments(userIds.map((id) => id.toString()));
	}

	searchUsers(
		query: string,
		filters: UserSearchFilters,
		options?: {limit?: number; offset?: number},
	): Promise<SchemaSearchResult<SearchableUser>> {
		return this.search(query, filters, options);
	}
}
