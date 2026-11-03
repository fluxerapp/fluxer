// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MeilisearchClient, MeilisearchTask} from '@app/api/search/meilisearch/MeilisearchClient';
import {MeilisearchTaskError} from '@app/api/search/meilisearch/MeilisearchClient';
import {
	joinMeiliFilters,
	type MeilisearchFilter,
	meiliTermFilter,
} from '@app/api/search/meilisearch/MeilisearchFilterUtils';
import type {MeilisearchIndexDefinition} from '@app/api/search/meilisearch/MeilisearchIndexDefinitions';
import type {SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';

const MAX_SEARCH_LIMIT = 1000;
const MAX_TOTAL_HITS = 10000;

const MEILISEARCH_MAX_TRACKED_BULK_TASKS = 4096;

interface MeilisearchIndexAdapterOptions<TFilters> {
	client: MeilisearchClient;
	index: MeilisearchIndexDefinition;
	buildFilters: (filters: TFilters) => Array<MeilisearchFilter | undefined>;
	buildSort?: (filters: TFilters) => Array<string> | undefined;
	buildQuery?: (query: string, filters: TFilters) => string;
	matchingStrategy?: 'all' | 'last';
}

interface MeilisearchIndexSettings {
	searchableAttributes?: Array<unknown>;
	filterableAttributes?: Array<unknown>;
	sortableAttributes?: Array<unknown>;
	pagination?: {maxTotalHits?: number};
}

function sameAttributes(current: Array<unknown> | undefined, wanted: Array<string>): boolean {
	return current !== undefined && current.length === wanted.length && current.every((value, i) => value === wanted[i]);
}

function sameAttributeSet(current: Array<unknown> | undefined, wanted: Array<string>): boolean {
	return sameAttributes(current?.slice().sort(), wanted.slice().sort());
}

interface MeilisearchSearchResponse<TResult> {
	hits: Array<TResult>;
	estimatedTotalHits?: number;
	totalHits?: number;
	limit?: number;
	offset?: number;
	facetDistribution?: Record<string, Record<string, number>>;
}

export class MeilisearchIndexAdapter<
	TFilters,
	TResult extends {
		id: string;
	},
> {
	protected readonly client: MeilisearchClient;
	protected readonly indexDefinition: MeilisearchIndexDefinition;
	protected readonly buildFilters: (filters: TFilters) => Array<MeilisearchFilter | undefined>;
	protected readonly buildSort: ((filters: TFilters) => Array<string> | undefined) | undefined;
	protected readonly buildQuery: ((query: string, filters: TFilters) => string) | undefined;
	protected readonly matchingStrategy: 'all' | 'last' | undefined;
	private readonly pendingBulkTaskIds = new Set<number>();
	private initialized = false;

	constructor(options: MeilisearchIndexAdapterOptions<TFilters>) {
		this.client = options.client;
		this.indexDefinition = options.index;
		this.buildFilters = options.buildFilters;
		this.buildSort = options.buildSort;
		this.buildQuery = options.buildQuery;
		this.matchingStrategy = options.matchingStrategy;
	}

	async initialize(): Promise<void> {
		const uid = this.indexDefinition.uid;
		const exists = await this.indexExists(uid);
		if (!exists) {
			const task = await this.client.request<MeilisearchTask>('POST', '/indexes', {
				uid,
				primaryKey: this.indexDefinition.primaryKey,
			});
			try {
				await this.client.waitForTask(task.taskUid);
			} catch (error) {
				if (!(error instanceof MeilisearchTaskError && error.code === 'index_already_exists')) {
					throw error;
				}
			}
		}
		const current = await this.client.request<MeilisearchIndexSettings>(
			'GET',
			`/indexes/${encodeURIComponent(uid)}/settings`,
		);
		const {searchableAttributes, filterableAttributes, sortableAttributes} = this.indexDefinition;
		const updates: Array<Promise<void>> = [];
		if (!sameAttributes(current.searchableAttributes, searchableAttributes)) {
			updates.push(this.applySetting('PUT', 'searchable-attributes', searchableAttributes));
		}
		if (!sameAttributeSet(current.filterableAttributes, filterableAttributes)) {
			updates.push(this.applySetting('PUT', 'filterable-attributes', filterableAttributes));
		}
		if (!sameAttributeSet(current.sortableAttributes, sortableAttributes)) {
			updates.push(this.applySetting('PUT', 'sortable-attributes', sortableAttributes));
		}
		if (current.pagination?.maxTotalHits !== MAX_TOTAL_HITS) {
			updates.push(this.applySetting('PATCH', 'pagination', {maxTotalHits: MAX_TOTAL_HITS}));
		}
		await Promise.all(updates);
		this.initialized = true;
	}

	async shutdown(): Promise<void> {
		this.pendingBulkTaskIds.clear();
		this.initialized = false;
	}

	isAvailable(): boolean {
		return this.initialized;
	}

	async indexDocument(doc: TResult): Promise<void> {
		await this.indexDocuments([doc]);
	}

	async indexDocuments(docs: Array<TResult>): Promise<void> {
		await this.addDocuments(docs);
	}

	async updateDocument(doc: TResult): Promise<void> {
		await this.addDocuments([doc]);
	}

	async deleteDocument(id: string): Promise<void> {
		await this.deleteDocuments([id]);
	}

	async deleteDocuments(ids: Array<string>): Promise<void> {
		if (ids.length === 0) {
			return;
		}
		this.assertInitialised();
		await this.client.request<MeilisearchTask>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents/delete-batch`,
			ids,
		);
	}

	async deleteByTerm(field: string, value: string): Promise<void> {
		await this.deleteByFilter(meiliTermFilter(field, value));
	}

	async deleteByFilter(filter: MeilisearchFilter): Promise<void> {
		this.assertInitialised();
		await this.client.request<MeilisearchTask>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents/delete`,
			{filter},
		);
	}

	async deleteAllDocuments(): Promise<void> {
		this.assertInitialised();
		await this.client.request<MeilisearchTask>(
			'DELETE',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents`,
		);
	}

	async bulkIndexDocuments(docs: Array<TResult>): Promise<void> {
		const taskUid = await this.addDocuments(docs);
		if (taskUid === undefined) {
			return;
		}
		this.trackBulkTask(taskUid);
	}

	async refreshIndex(): Promise<void> {
		const taskIds = Array.from(this.pendingBulkTaskIds);
		this.pendingBulkTaskIds.clear();
		await Promise.all(taskIds.map((taskId) => this.client.waitForTask(taskId)));
	}

	async search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TResult>> {
		this.assertInitialised();
		const requestedLimit = options?.limit ?? options?.hitsPerPage ?? 25;
		const limit = Math.min(Math.max(requestedLimit, 0), MAX_SEARCH_LIMIT);
		const offset = options?.offset ?? (options?.page ? (options.page - 1) * (options.hitsPerPage ?? 25) : 0);
		const filter = joinMeiliFilters(this.buildFilters(filters));
		const facets = options?.facets;
		const result = await this.client.request<MeilisearchSearchResponse<TResult>>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/search`,
			{
				q: this.buildQuery?.(query, filters) ?? query,
				filter,
				limit,
				offset,
				sort: this.buildSort?.(filters),
				attributesToSearchOn: this.indexDefinition.searchableAttributes,
				showRankingScore: false,
				...(this.matchingStrategy ? {matchingStrategy: this.matchingStrategy} : {}),
				...(facets && facets.length > 0 ? {facets} : {}),
			},
		);
		return {
			hits: result.hits,
			total: result.totalHits ?? result.estimatedTotalHits ?? result.hits.length,
			...(result.facetDistribution ? {facetCounts: result.facetDistribution} : {}),
		};
	}

	protected assertInitialised(): void {
		if (!this.initialized) {
			throw new Error('Meilisearch adapter not initialised');
		}
	}

	private async indexExists(uid: string): Promise<boolean> {
		try {
			await this.client.request<unknown>('GET', `/indexes/${encodeURIComponent(uid)}`);
			return true;
		} catch (error) {
			if (error instanceof Error && error.message.includes('index_not_found')) {
				return false;
			}
			if (error instanceof Error && error.message.includes('404')) {
				return false;
			}
			throw error;
		}
	}

	private async applySetting(
		method: 'PUT' | 'PATCH',
		setting: string,
		value: Array<string> | {maxTotalHits: number},
	): Promise<void> {
		const task = await this.client.request<MeilisearchTask>(
			method,
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/settings/${setting}`,
			value,
		);
		await this.client.waitForTask(task.taskUid);
	}

	private async addDocuments(docs: Array<TResult>): Promise<number | undefined> {
		if (docs.length === 0) {
			return undefined;
		}
		this.assertInitialised();
		const task = await this.client.request<MeilisearchTask>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents`,
			docs,
		);
		return task.taskUid;
	}

	private trackBulkTask(taskUid: number): void {
		if (this.pendingBulkTaskIds.size >= MEILISEARCH_MAX_TRACKED_BULK_TASKS) {
			const oldest = this.pendingBulkTaskIds.values().next().value;
			if (oldest !== undefined) {
				this.pendingBulkTaskIds.delete(oldest);
			}
		}
		this.pendingBulkTaskIds.add(taskUid);
	}
}
