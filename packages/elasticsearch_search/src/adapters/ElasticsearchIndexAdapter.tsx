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

import type {Client} from '@elastic/elasticsearch';
import type {ElasticsearchFilter} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {compactFilters} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import type {ElasticsearchIndexDefinition} from '@fluxer/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import type {ISearchAdapter, SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';

export interface ElasticsearchIndexAdapterOptions<TFilters> {
	client: Client;
	index: ElasticsearchIndexDefinition;
	searchableFields: Array<string>;
	buildFilters: (filters: TFilters) => Array<ElasticsearchFilter | undefined>;
	buildSort?: (filters: TFilters) => Array<Record<string, unknown>> | undefined;
}

export class ElasticsearchIndexAdapter<TFilters, TResult extends {id: string}>
	implements ISearchAdapter<TFilters, TResult>
{
	protected readonly client: Client;
	protected readonly indexDefinition: ElasticsearchIndexDefinition;
	protected readonly searchableFields: Array<string>;
	protected readonly buildFilters: (filters: TFilters) => Array<ElasticsearchFilter | undefined>;
	protected readonly buildSort: ((filters: TFilters) => Array<Record<string, unknown>> | undefined) | undefined;

	private initialized = false;

	constructor(options: ElasticsearchIndexAdapterOptions<TFilters>) {
		this.client = options.client;
		this.indexDefinition = options.index;
		this.searchableFields = options.searchableFields;
		this.buildFilters = options.buildFilters;
		this.buildSort = options.buildSort;
	}

	async initialize(): Promise<void> {
		const indexName = this.indexDefinition.indexName;

		const exists = await this.client.indices.exists({index: indexName});
		if (!exists) {
			try {
				await this.client.indices.create({
					index: indexName,
					settings: this.indexDefinition.settings ?? {},
					mappings: this.indexDefinition.mappings,
				});
			} catch (error) {
				if (!isResourceAlreadyExistsError(error)) {
					throw error;
				}
			}
		}

		await this.client.indices.putMapping({
			index: indexName,
			...this.indexDefinition.mappings,
		});

		this.initialized = true;
	}

	async shutdown(): Promise<void> {
		this.initialized = false;
	}

	isAvailable(): boolean {
		return this.initialized;
	}

	async indexDocument(doc: TResult): Promise<void> {
		await this.indexDocuments([doc]);
	}

	async indexDocuments(docs: Array<TResult>): Promise<void> {
		if (docs.length === 0) {
			return;
		}
		this.assertInitialised();

		const operations = docs.flatMap((doc) => [{index: {_index: this.indexDefinition.indexName, _id: doc.id}}, doc]);
		await this.client.bulk({operations, refresh: 'wait_for'});
	}

	async updateDocument(doc: TResult): Promise<void> {
		this.assertInitialised();

		await this.client.index({
			index: this.indexDefinition.indexName,
			id: doc.id,
			document: doc,
			refresh: 'wait_for',
		});
	}

	async deleteDocument(id: string): Promise<void> {
		await this.deleteDocuments([id]);
	}

	async deleteDocuments(ids: Array<string>): Promise<void> {
		if (ids.length === 0) {
			return;
		}
		this.assertInitialised();

		const operations = ids.map((id) => ({delete: {_index: this.indexDefinition.indexName, _id: id}}));
		await this.client.bulk({operations, refresh: 'wait_for'});
	}

	async deleteAllDocuments(): Promise<void> {
		this.assertInitialised();

		await this.client.deleteByQuery({
			index: this.indexDefinition.indexName,
			query: {match_all: {}},
			refresh: true,
		});
	}

	async search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TResult>> {
		this.assertInitialised();

		const limit = options?.limit ?? options?.hitsPerPage ?? 25;
		const offset = options?.offset ?? (options?.page ? (options.page - 1) * (options.hitsPerPage ?? 25) : 0);

		const filterClauses = compactFilters(this.buildFilters(filters));
		const sort = this.buildSort?.(filters);

		const must: Array<Record<string, unknown>> = query
			? [{multi_match: {query, fields: this.searchableFields, type: 'best_fields'}}]
			: [{match_all: {}}];

		const searchParams: Record<string, unknown> = {
			index: this.indexDefinition.indexName,
			query: {
				bool: {
					must,
					filter: filterClauses.length > 0 ? filterClauses : undefined,
				},
			},
			from: offset,
			size: limit,
		};

		if (sort && sort.length > 0) {
			searchParams.sort = sort;
		}

		const result = await this.client.search<TResult>(searchParams);

		const totalValue = result.hits.total;
		const total = typeof totalValue === 'number' ? totalValue : (totalValue?.value ?? 0);
		const hits = result.hits.hits.map((hit) => ({...hit._source!, id: hit._id!}));

		return {hits, total};
	}

	private assertInitialised(): void {
		if (!this.initialized) {
			throw new Error('Elasticsearch adapter not initialised');
		}
	}
}

function isResourceAlreadyExistsError(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const meta = (error as {meta?: {body?: {error?: {type?: string}}}}).meta;
	if (meta?.body?.error?.type === 'resource_already_exists_exception') {
		return true;
	}
	const message = (error as {message?: string}).message ?? '';
	return message.includes('resource_already_exists_exception');
}
