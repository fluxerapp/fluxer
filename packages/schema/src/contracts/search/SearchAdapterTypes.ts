// SPDX-License-Identifier: AGPL-3.0-or-later

export interface SearchOptions {
	hitsPerPage?: number;
	page?: number;
	limit?: number;
	offset?: number;
	cursor?: Array<string>;
	facets?: Array<string>;
}

export type SearchFacetCounts = Readonly<Record<string, Readonly<Record<string, number>>>>;

export interface SearchResult<TResult> {
	hits: Array<TResult>;
	total: number;
	cursor?: Array<string>;
	facetCounts?: SearchFacetCounts;
}

export interface ISearchAdapter<TFilters, TResult> {
	initialize(): Promise<void>;
	shutdown(): Promise<void>;
	indexDocument(doc: TResult): Promise<void>;
	indexDocuments(docs: Array<TResult>): Promise<void>;
	updateDocument(doc: TResult): Promise<void>;
	deleteDocument(id: string): Promise<void>;
	deleteDocuments(ids: Array<string>): Promise<void>;
	deleteAllDocuments(): Promise<void>;
	bulkIndexDocuments(docs: Array<TResult>): Promise<void>;
	refreshIndex(): Promise<void>;
	search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TResult>>;
	isAvailable(): boolean;
}
