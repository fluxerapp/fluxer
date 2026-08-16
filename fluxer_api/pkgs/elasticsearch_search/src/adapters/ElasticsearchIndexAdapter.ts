// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SearchRequest, SortCombinations, SortResults} from '@elastic/elasticsearch/lib/api/types';
import type {ISearchAdapter, SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {ElasticsearchFilter} from '../ElasticsearchFilterUtils';
import {compactFilters} from '../ElasticsearchFilterUtils';
import type {ElasticsearchIndexDefinition} from '../ElasticsearchIndexDefinitions';

const ELASTICSEARCH_MAX_RESULT_WINDOW = 10000;
const DEEP_PAGINATION_BATCH_SIZE = 1000;
const MAX_SEARCH_LIMIT = 1000;
const FACET_TERM_LIMIT = 200;

interface ElasticsearchSearchHit<TResult> {
	_source?: TResult;
	_id?: string;
	sort?: SortResults;
}

interface ElasticsearchTermsAggregation {
	buckets?: Array<{key: string | number; doc_count: number}>;
}

interface ElasticsearchSearchResponse<TResult> {
	hits: {
		total?:
			| number
			| {
					value?: number;
			  };
		hits: Array<ElasticsearchSearchHit<TResult>>;
	};
	aggregations?: Record<string, ElasticsearchTermsAggregation>;
}

type ElasticsearchBaseSearchRequest = Omit<SearchRequest, 'from' | 'search_after' | 'size' | 'track_total_hits'>;

export interface ElasticsearchDistributedLock {
	acquireLock(key: string, ttlSeconds: number): Promise<string | null>;
	releaseLock(key: string, token: string): Promise<boolean>;
}

interface ElasticsearchIndexAdapterOptions<TFilters> {
	client: Client;
	index: ElasticsearchIndexDefinition;
	searchableFields: Array<string>;
	searchType?: 'best_fields' | 'bool_prefix';
	fuzziness?: string;
	minimumShouldMatch?: string;
	buildFilters: (filters: TFilters) => Array<ElasticsearchFilter | undefined>;
	buildSort?: (filters: TFilters) => Array<SortCombinations> | undefined;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchIndexAdapter<
	TFilters,
	TResult extends {
		id: string;
	},
> implements ISearchAdapter<TFilters, TResult>
{
	protected readonly client: Client;
	protected readonly indexDefinition: ElasticsearchIndexDefinition;
	protected readonly searchableFields: Array<string>;
	protected readonly searchType: 'best_fields' | 'bool_prefix';
	protected readonly fuzziness: string | undefined;
	protected readonly minimumShouldMatch: string | undefined;
	protected readonly buildFilters: (filters: TFilters) => Array<ElasticsearchFilter | undefined>;
	protected readonly buildSort: ((filters: TFilters) => Array<SortCombinations> | undefined) | undefined;
	private readonly lock: ElasticsearchDistributedLock | undefined;
	private initialized = false;

	constructor(options: ElasticsearchIndexAdapterOptions<TFilters>) {
		this.client = options.client;
		this.indexDefinition = options.index;
		this.searchableFields = options.searchableFields;
		this.searchType = options.searchType ?? 'best_fields';
		this.fuzziness = options.fuzziness;
		this.minimumShouldMatch = options.minimumShouldMatch;
		this.buildFilters = options.buildFilters;
		this.buildSort = options.buildSort;
		this.lock = options.lock;
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
		try {
			await this.client.indices.putMapping({
				index: indexName,
				...this.indexDefinition.mappings,
			});
		} catch (error) {
			if (isAnalysisSettingsMissingError(error) && this.indexDefinition.settings?.analysis) {
				await this.applyMissingAnalysisSettings(indexName);
				try {
					await this.client.indices.putMapping({
						index: indexName,
						...this.indexDefinition.mappings,
					});
				} catch (retryError) {
					if (!isMappingConflictError(retryError)) {
						throw retryError;
					}
					await this.recreateIndex(indexName);
				}
			} else if (isMappingConflictError(error)) {
				await this.recreateIndex(indexName);
			} else {
				throw error;
			}
		}
		this.initialized = true;
	}

	private async applyMissingAnalysisSettings(indexName: string): Promise<void> {
		const analysis = this.indexDefinition.settings?.analysis;
		if (!analysis) return;
		await this.client.indices.close({index: indexName});
		try {
			await this.client.indices.putSettings({
				index: indexName,
				settings: {analysis},
			});
		} finally {
			await this.client.indices.open({index: indexName});
		}
	}

	private async recreateIndex(indexName: string): Promise<void> {
		const lockKey = `es_index_reinit:${indexName}`;
		const lockTtlSeconds = 120;
		if (this.lock) {
			const token = await this.lock.acquireLock(lockKey, lockTtlSeconds);
			if (token) {
				try {
					await this.deleteAndRecreateIndex(indexName);
				} finally {
					await this.lock.releaseLock(lockKey, token);
				}
				return;
			}
			await new Promise((resolve) => setTimeout(resolve, 10000));
			await this.client.indices.putMapping({
				index: indexName,
				...this.indexDefinition.mappings,
			});
		} else {
			await this.deleteAndRecreateIndex(indexName);
		}
	}

	private async deleteAndRecreateIndex(indexName: string): Promise<void> {
		await this.client.indices.delete({index: indexName});
		await this.client.indices.create({
			index: indexName,
			settings: this.indexDefinition.settings ?? {},
			mappings: this.indexDefinition.mappings,
		});
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
		await this.client.bulk({operations, refresh: false});
	}

	async updateDocument(doc: TResult): Promise<void> {
		this.assertInitialised();
		await this.client.index({
			index: this.indexDefinition.indexName,
			id: doc.id,
			document: doc,
			refresh: false,
		});
	}

	async bulkIndexDocuments(docs: Array<TResult>): Promise<void> {
		if (docs.length === 0) {
			return;
		}
		this.assertInitialised();
		const operations = docs.flatMap((doc) => [{index: {_index: this.indexDefinition.indexName, _id: doc.id}}, doc]);
		await this.client.bulk({operations, refresh: false});
	}

	async refreshIndex(): Promise<void> {
		this.assertInitialised();
		await this.client.indices.refresh({index: this.indexDefinition.indexName});
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
		await this.client.bulk({operations, refresh: false});
	}

	async deleteByQuery(query: Record<string, unknown>): Promise<void> {
		this.assertInitialised();
		await this.client.deleteByQuery({
			index: this.indexDefinition.indexName,
			query,
			refresh: false,
		});
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
		const requestedLimit = options?.limit ?? options?.hitsPerPage ?? 25;
		const limit = Math.min(Math.max(requestedLimit, 0), MAX_SEARCH_LIMIT);
		const usesCursor = options?.cursor != null && options.cursor.length > 0;
		const offset = options?.offset ?? (options?.page ? (options.page - 1) * (options.hitsPerPage ?? 25) : 0);
		const filterClauses = compactFilters(this.buildFilters(filters));
		const sort = this.buildSort?.(filters);
		const must: Array<Record<string, unknown>> = query
			? [
					{
						multi_match: {
							query,
							fields: this.searchableFields,
							type: this.searchType,
							operator: 'or',
							...(this.fuzziness != null ? {fuzziness: this.fuzziness} : {}),
							...(this.minimumShouldMatch != null ? {minimum_should_match: this.minimumShouldMatch} : {}),
						},
					},
				]
			: [{match_all: {}}];
		const facets = options?.facets;
		const searchParams: ElasticsearchBaseSearchRequest = {
			index: this.indexDefinition.indexName,
			query: {
				bool: {
					must,
					filter: filterClauses.length > 0 ? filterClauses : undefined,
				},
			},
			...(facets && facets.length > 0
				? {
						aggs: Object.fromEntries(facets.map((facet) => [facet, {terms: {field: facet, size: FACET_TERM_LIMIT}}])),
					}
				: {}),
		};
		const defaultSort: SortCombinations = {id: {order: 'desc'}};
		const effectiveSort: NonNullable<SearchRequest['sort']> =
			sort && sort.length > 0 ? [...sort, defaultSort] : [defaultSort];
		searchParams.sort = effectiveSort;
		if (usesCursor) {
			const result = await this.executeSearchRequest(searchParams, {size: limit, searchAfter: options!.cursor});
			return this.toSearchResult(result);
		}
		if (offset + limit > ELASTICSEARCH_MAX_RESULT_WINDOW) {
			return this.executeDeepPaginationSearch(searchParams, offset, limit);
		}
		const result = await this.executeSearchRequest(searchParams, {size: limit, from: offset});
		return this.toSearchResult(result);
	}

	private assertInitialised(): void {
		if (!this.initialized) {
			throw new Error('Elasticsearch adapter not initialised');
		}
	}

	private async executeDeepPaginationSearch(
		searchParams: ElasticsearchBaseSearchRequest,
		offset: number,
		limit: number,
	): Promise<SearchResult<TResult>> {
		let remainingOffset = offset;
		let remainingLimit = limit;
		let total: number | undefined;
		let searchAfter: SortResults | undefined;
		const hits: Array<TResult> = [];
		while (remainingOffset > 0 || remainingLimit > 0) {
			const batchSize = Math.min(DEEP_PAGINATION_BATCH_SIZE, remainingOffset > 0 ? remainingOffset : remainingLimit);
			const result = await this.executeSearchRequest(searchParams, {
				size: batchSize,
				searchAfter,
				trackTotalHits: total == null,
			});
			if (total == null) {
				total = this.getTotalHits(result);
				if (offset >= total) {
					return {hits: [], total, cursor: undefined};
				}
			}
			const batchHits = result.hits.hits;
			if (batchHits.length === 0) {
				break;
			}
			searchAfter = batchHits.at(-1)?.sort;
			if (remainingOffset > 0) {
				remainingOffset -= batchHits.length;
				continue;
			}
			hits.push(...this.mapHits(batchHits));
			remainingLimit -= batchHits.length;
		}
		return {
			hits,
			total: total ?? 0,
			cursor: searchAfter?.map((value) => String(value)),
		};
	}

	private async executeSearchRequest(
		searchParams: ElasticsearchBaseSearchRequest,
		options: {
			size: number;
			from?: number;
			searchAfter?: SortResults;
			trackTotalHits?: boolean;
		},
	): Promise<ElasticsearchSearchResponse<TResult>> {
		const request: SearchRequest = {
			...searchParams,
			size: options.size,
			track_total_hits: options.trackTotalHits ?? true,
			...(options.from != null ? {from: options.from} : {}),
			...(options.searchAfter != null ? {search_after: options.searchAfter} : {}),
		};
		return (await this.client.search<TResult>(request)) as ElasticsearchSearchResponse<TResult>;
	}

	private toSearchResult(result: ElasticsearchSearchResponse<TResult>): SearchResult<TResult> {
		const facetCounts = this.toFacetCounts(result.aggregations);
		return {
			hits: this.mapHits(result.hits.hits),
			total: this.getTotalHits(result),
			cursor: result.hits.hits.at(-1)?.sort?.map((value) => String(value)),
			...(facetCounts ? {facetCounts} : {}),
		};
	}

	private toFacetCounts(
		aggregations: Record<string, ElasticsearchTermsAggregation> | undefined,
	): Record<string, Record<string, number>> | undefined {
		if (!aggregations) {
			return undefined;
		}
		const counts: Record<string, Record<string, number>> = {};
		for (const [facet, aggregation] of Object.entries(aggregations)) {
			counts[facet] = Object.fromEntries(
				(aggregation.buckets ?? []).map((bucket) => [String(bucket.key), bucket.doc_count]),
			);
		}
		return counts;
	}

	private mapHits(hits: Array<ElasticsearchSearchHit<TResult>>): Array<TResult> {
		return hits.map((hit) => ({...hit._source!, id: hit._id!}));
	}

	private getTotalHits(result: ElasticsearchSearchResponse<TResult>): number {
		const totalValue = result.hits.total;
		return typeof totalValue === 'number' ? totalValue : (totalValue?.value ?? 0);
	}
}

function isResourceAlreadyExistsError(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const meta = (
		error as {
			meta?: {
				body?: {
					error?: {
						type?: string;
					};
				};
			};
		}
	).meta;
	if (meta?.body?.error?.type === 'resource_already_exists_exception') {
		return true;
	}
	const message =
		(
			error as {
				message?: string;
			}
		).message ?? '';
	return message.includes('resource_already_exists_exception');
}

function isMappingConflictError(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const message =
		(
			error as {
				message?: string;
			}
		).message ?? '';
	if (message.includes('mapper') && message.includes('cannot be changed from type')) {
		return true;
	}
	if (
		message.includes('Cannot update parameter') &&
		(message.includes('[analyzer]') || message.includes('[search_analyzer]'))
	) {
		return true;
	}
	return false;
}

function isAnalysisSettingsMissingError(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const meta = (
		error as {
			meta?: {
				body?: {
					error?: {
						type?: string;
					};
				};
			};
		}
	).meta;
	const type = meta?.body?.error?.type;
	const message =
		(
			error as {
				message?: string;
			}
		).message ?? '';
	const looksLikeMapperParsing = type === 'mapper_parsing_exception' || message.includes('mapper_parsing_exception');
	const referencesMissingAnalysisComponent =
		message.includes('has not been configured in mappings') ||
		(message.includes('Custom') && message.includes("doesn't exist for field"));
	return looksLikeMapperParsing && referencesMissingAnalysisComponent;
}
