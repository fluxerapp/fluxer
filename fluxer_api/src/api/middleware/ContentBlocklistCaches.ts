// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export async function startContentBlocklistCaches({
	kvClient,
	storageService,
}: {
	kvClient: IKVProvider;
	storageService: IStorageService;
}): Promise<void> {
	urlBlocklistCache.setRefreshSubscriber(kvClient);
	urlBlocklistCache.setStorageService(storageService);
	fileShaCache.setRefreshSubscriber(kvClient);
	phraseBlocklistCache.setRefreshSubscriber(kvClient);
	await Promise.all([urlBlocklistCache.initialize(), fileShaCache.initialize(), phraseBlocklistCache.initialize()]);
}

export async function stopContentBlocklistCaches(): Promise<void> {
	await Promise.all([urlBlocklistCache.shutdown(), fileShaCache.shutdown(), phraseBlocklistCache.shutdown()]);
}
