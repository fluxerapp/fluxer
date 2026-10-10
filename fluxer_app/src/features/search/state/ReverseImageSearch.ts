// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {UrlTemplateProviderStore} from '@app/features/search/state/UrlTemplateProviderStore';

export default new UrlTemplateProviderStore({
	name: 'ReverseImageSearch',
	persist() {
		return makePersistent(this, 'ReverseImageSearch', ['engines'], {version: 2});
	},
	builtIns: [
		{
			id: 'google_lens',
			name: 'Google Lens',
			urlTemplate: 'https://lens.google.com/uploadbyurl?url={url}',
		},
		{
			id: 'yandex',
			name: 'Yandex',
			urlTemplate: 'https://yandex.com/images/search?rpt=imageview&url={url}',
		},
		{
			id: 'bing',
			name: 'Bing',
			urlTemplate: 'https://www.bing.com/images/searchbyimage?cbir=sbi&imgurl={url}',
		},
		{
			id: 'tineye',
			name: 'TinEye',
			urlTemplate: 'https://tineye.com/search?url={url}',
		},
		{
			id: 'saucenao',
			name: 'SauceNAO',
			urlTemplate: 'https://saucenao.com/search.php?url={url}',
		},
		{
			id: 'iqdb',
			name: 'IQDB',
			urlTemplate: 'https://iqdb.org/?url={url}',
		},
	],
	suggestedDefaultId: 'google_lens',
	defaultField: 'reverseImageSearchEngineId',
	placeholder: /\{url\}/gu,
	persistFailureMessage: 'Failed to persist default reverse-image-search engine',
});
