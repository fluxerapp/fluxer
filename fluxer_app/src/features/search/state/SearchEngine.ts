// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {UrlTemplateProviderStore} from '@app/features/search/state/UrlTemplateProviderStore';

export default new UrlTemplateProviderStore({
	name: 'SearchEngine',
	persist() {
		return makePersistent(this, 'SearchEngine', ['engines'], {version: 2});
	},
	builtIns: [
		{id: 'google', name: 'Google', urlTemplate: 'https://www.google.com/search?q={query}'},
		{id: 'bing', name: 'Bing', urlTemplate: 'https://www.bing.com/search?q={query}'},
		{id: 'duckduckgo', name: 'DuckDuckGo', urlTemplate: 'https://duckduckgo.com/?q={query}'},
		{id: 'yahoo', name: 'Yahoo', urlTemplate: 'https://search.yahoo.com/search?p={query}'},
		{id: 'ecosia', name: 'Ecosia', urlTemplate: 'https://www.ecosia.org/search?q={query}'},
		{id: 'brave', name: 'Brave Search', urlTemplate: 'https://search.brave.com/search?q={query}'},
		{
			id: 'startpage',
			name: 'Startpage',
			urlTemplate: 'https://www.startpage.com/do/dsearch?query={query}',
		},
		{id: 'yandex', name: 'Yandex', urlTemplate: 'https://yandex.com/search/?text={query}'},
		{
			id: 'wikipedia',
			name: 'Wikipedia',
			urlTemplate: 'https://en.wikipedia.org/w/index.php?search={query}',
		},
		{
			id: 'youtube',
			name: 'YouTube',
			urlTemplate: 'https://www.youtube.com/results?search_query={query}',
		},
		{id: 'github', name: 'GitHub', urlTemplate: 'https://github.com/search?q={query}'},
		{id: 'reddit', name: 'Reddit', urlTemplate: 'https://www.reddit.com/search/?q={query}'},
	],
	suggestedDefaultId: 'google',
	defaultField: 'textSearchEngineId',
	placeholder: /\{query\}/gu,
	persistFailureMessage: 'Failed to persist default web-search engine',
});
