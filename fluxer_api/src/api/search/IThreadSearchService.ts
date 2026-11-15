// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ISearchAdapter} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {SearchableThread, ThreadSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface IThreadSearchService extends ISearchAdapter<ThreadSearchFilters, SearchableThread> {
	ready(): Promise<void>;
}
