import type {User} from '@app/api/models/User';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import {SearchAdapterServiceBase} from '@app/api/search/SearchAdapterServiceBase';
import {convertToSearchableUser} from '@app/api/search/user/UserSearchSerializer';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {SearchableUser, UserSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchUserAdapter,
	type ElasticsearchUserAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchUserAdapter';

interface ElasticsearchUserSearchServiceOptions extends ElasticsearchUserAdapterOptions {}

export class ElasticsearchUserSearchService
	extends SearchAdapterServiceBase<UserSearchFilters, SearchableUser, ElasticsearchUserAdapter>
	implements IUserSearchService
{
	constructor(options: ElasticsearchUserSearchServiceOptions) {
		super(new ElasticsearchUserAdapter({client: options.client, lock: options.lock}));
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

	searchUsers(
		query: string,
		filters: UserSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableUser>> {
		return this.search(query, filters, options);
	}
}
