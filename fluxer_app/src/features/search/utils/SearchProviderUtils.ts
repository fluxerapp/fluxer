// SPDX-License-Identifier: AGPL-3.0-or-later

import Translation from '@app/features/messaging/state/Translation';
import ReverseImageSearch from '@app/features/search/state/ReverseImageSearch';
import SearchEngine from '@app/features/search/state/SearchEngine';

export function getSearchProviderStore(mode: 'text' | 'image' | 'translate') {
	if (mode === 'image') {
		return ReverseImageSearch;
	}
	if (mode === 'translate') {
		return Translation;
	}
	return SearchEngine;
}
