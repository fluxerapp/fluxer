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

import {trackSearchTask} from '@fluxer/api/src/search/SearchTaskTracker';
import type {ISearchAdapter, SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';

export abstract class ElasticsearchSearchServiceBase<
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

	search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TDocument>> {
		return this.adapter.search(query, filters, options);
	}
}
