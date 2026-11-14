// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {UrlTemplateProviderStore} from '@app/features/search/state/UrlTemplateProviderStore';

export default new UrlTemplateProviderStore({
	name: 'Translation',
	persist() {
		return makePersistent(this, 'Translation', ['engines'], {version: 1});
	},
	builtIns: [
		{
			id: 'google_translate',
			name: 'Google Translate',
			urlTemplate: 'https://translate.google.com/?sl=auto&tl=auto&text={query}&op=translate',
		},
		{
			id: 'deepl',
			name: 'DeepL',
			urlTemplate: 'https://www.deepl.com/translator#auto/auto/{query}',
		},
		{
			id: 'bing_translator',
			name: 'Bing Translator',
			urlTemplate: 'https://www.bing.com/translator/?text={query}',
		},
		{
			id: 'yandex_translate',
			name: 'Yandex Translate',
			urlTemplate: 'https://translate.yandex.com/?text={query}',
		},
		{
			id: 'reverso',
			name: 'Reverso',
			urlTemplate: 'https://www.reverso.net/text-translation#sl=auto&tl=eng&text={query}',
		},
		{
			id: 'linguee',
			name: 'Linguee',
			urlTemplate: 'https://www.linguee.com/english-german/search?source=auto&query={query}',
		},
		{
			id: 'papago',
			name: 'Papago',
			urlTemplate: 'https://papago.naver.com/?st={query}',
		},
	],
	suggestedDefaultId: 'google_translate',
	defaultField: 'translationProviderId',
	placeholder: /\{query\}/gu,
	persistFailureMessage: 'Failed to persist default translation provider',
});
