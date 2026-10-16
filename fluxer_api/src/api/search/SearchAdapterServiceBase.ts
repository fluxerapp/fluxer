// SPDX-License-Identifier: AGPL-3.0-or-later

import {trackSearchTask} from '@app/api/search/SearchTaskTracker';
import type {ISearchAdapter, SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';

export abstract class SearchAdapterServiceBase<
	TFilters,
	TDocument,
	TAdapter extends ISearchAdapter<TFilters, TDocument>,
> {
	protected readonly adapter: TAdapter;

	protected constructor(adapter: TAdapter) {
		this.adapter = adapter;
	}

	initialize(): Promise<void> {
		return this.adapter.initialize();
	}

	shutdown(): Promise<void> {
		return this.adapter.shutdown();
	}

	isAvailable(): boolean {
		return this.adapter.isAvailable();
	}

	indexDocument(doc: TDocument): Promise<void> {
		return trackSearchTask(this.adapter.indexDocument(doc));
	}

	indexDocuments(docs: Array<TDocument>): Promise<void> {
		return trackSearchTask(this.adapter.indexDocuments(docs));
	}

	updateDocument(doc: TDocument): Promise<void> {
		return trackSearchTask(this.adapter.updateDocument(doc));
	}

	deleteDocument(id: string): Promise<void> {
		return trackSearchTask(this.adapter.deleteDocument(id));
	}

	deleteDocuments(ids: Array<string>): Promise<void> {
		return trackSearchTask(this.adapter.deleteDocuments(ids));
	}

	deleteAllDocuments(): Promise<void> {
		return trackSearchTask(this.adapter.deleteAllDocuments());
	}

	bulkIndexDocuments(docs: Array<TDocument>): Promise<void> {
		return trackSearchTask(this.adapter.bulkIndexDocuments(docs));
	}

	refreshIndex(): Promise<void> {
		return this.adapter.refreshIndex();
	}

	search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TDocument>> {
		return this.adapter.search(query, filters, options);
	}
}
