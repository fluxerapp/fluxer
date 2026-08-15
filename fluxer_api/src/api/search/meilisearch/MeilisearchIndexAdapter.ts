// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MeilisearchClient, MeilisearchTask} from './MeilisearchClient';
import type {MeilisearchFilter} from './MeilisearchFilterUtils';
import {joinMeiliFilters} from './MeilisearchFilterUtils';
import type {MeilisearchIndexDefinition} from './MeilisearchIndexDefinitions';

const MAX_SEARCH_LIMIT = 1000;
const MAX_TOTAL_HITS = 10000;

interface MeilisearchIndexAdapterOptions<TFilters> {
	client: MeilisearchClient;
	index: MeilisearchIndexDefinition;
	buildFilters: (filters: TFilters) => Array<MeilisearchFilter | undefined>;
	buildSort?: (filters: TFilters) => Array<string> | undefined;
	buildQuery?: (query: string, filters: TFilters) => string;
}

interface MeilisearchSearchResponse<TResult> {
	hits: Array<TResult>;
	estimatedTotalHits?: number;
	totalHits?: number;
	limit?: number;
	offset?: number;
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
	private readonly pendingTaskIds = new Set<number>();
	private initialized = false;

	constructor(options: MeilisearchIndexAdapterOptions<TFilters>) {
		this.client = options.client;
		this.indexDefinition = options.index;
		this.buildFilters = options.buildFilters;
		this.buildSort = options.buildSort;
		this.buildQuery = options.buildQuery;
	}

	async initialize(): Promise<void> {
		const uid = this.indexDefinition.uid;
		const exists = await this.indexExists(uid);
		if (!exists) {
			const task = await this.client.request<MeilisearchTask>('POST', '/indexes', {
				uid,
				primaryKey: this.indexDefinition.primaryKey,
			});
			await this.waitForTask(task.taskUid);
		}
		await Promise.all([
			this.applySetting('PUT', 'searchable-attributes', this.indexDefinition.searchableAttributes),
			this.applySetting('PUT', 'filterable-attributes', this.indexDefinition.filterableAttributes),
			this.applySetting('PUT', 'sortable-attributes', this.indexDefinition.sortableAttributes),
			this.applySetting('PATCH', 'pagination', {maxTotalHits: MAX_TOTAL_HITS}),
		]);
		this.initialized = true;
	}

	async shutdown(): Promise<void> {
		this.pendingTaskIds.clear();
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
		const task = await this.client.request<MeilisearchTask>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents/delete-batch`,
			ids,
		);
		this.trackTask(task.taskUid);
	}

	async deleteByFilter(filter: MeilisearchFilter): Promise<void> {
		this.assertInitialised();
		const task = await this.client.request<MeilisearchTask>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents/delete`,
			{filter},
		);
		this.trackTask(task.taskUid);
	}

	async deleteAllDocuments(): Promise<void> {
		this.assertInitialised();
		const task = await this.client.request<MeilisearchTask>(
			'DELETE',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents`,
		);
		this.trackTask(task.taskUid);
	}

	async bulkIndexDocuments(docs: Array<TResult>): Promise<void> {
		await this.addDocuments(docs);
	}

	async refreshIndex(): Promise<void> {
		const taskIds = Array.from(this.pendingTaskIds);
		this.pendingTaskIds.clear();
		await Promise.all(taskIds.map((taskId) => this.client.waitForTask(taskId)));
	}

	async search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TResult>> {
		this.assertInitialised();
		const requestedLimit = options?.limit ?? options?.hitsPerPage ?? 25;
		const limit = Math.min(Math.max(requestedLimit, 0), MAX_SEARCH_LIMIT);
		const offset = options?.offset ?? (options?.page ? (options.page - 1) * (options.hitsPerPage ?? 25) : 0);
		const filter = joinMeiliFilters(this.buildFilters(filters));
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
			},
		);
		return {
			hits: result.hits,
			total: result.totalHits ?? result.estimatedTotalHits ?? result.hits.length,
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
		await this.waitForTask(task.taskUid);
	}

	private async addDocuments(docs: Array<TResult>): Promise<void> {
		if (docs.length === 0) {
			return;
		}
		this.assertInitialised();
		const task = await this.client.request<MeilisearchTask>(
			'POST',
			`/indexes/${encodeURIComponent(this.indexDefinition.uid)}/documents`,
			docs,
		);
		this.trackTask(task.taskUid);
	}

	private trackTask(taskUid: number): void {
		this.pendingTaskIds.add(taskUid);
	}

	private async waitForTask(taskUid: number): Promise<void> {
		this.pendingTaskIds.delete(taskUid);
		await this.client.waitForTask(taskUid);
	}
}
