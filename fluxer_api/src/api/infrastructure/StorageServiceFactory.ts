// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ChangeFeedStorageService} from '@app/api/infrastructure/ChangeFeedStorageService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {JetStreamStorageChangeSink, StorageChangeFeed} from '@app/api/infrastructure/StorageChangeFeed';
import {StorageService} from '@app/api/infrastructure/StorageService';

const CHANGE_FEED_SHUTDOWN_TIMEOUT_MS = 5000;

let changeFeed: StorageChangeFeed | null = null;

function withChangeFeed(service: IStorageService): IStorageService {
	const {enabled, stream, skipBuckets} = Config.storageChangeFeed;
	if (!enabled) {
		return service;
	}
	changeFeed ??= new StorageChangeFeed(
		new JetStreamStorageChangeSink({url: Config.nats.jetStreamUrl, token: Config.nats.authToken, stream}),
	);
	return new ChangeFeedStorageService(service, changeFeed, skipBuckets);
}

export function createStorageService(): IStorageService {
	return withChangeFeed(new StorageService());
}

export async function shutdownStorageChangeFeed(): Promise<void> {
	const feed = changeFeed;
	changeFeed = null;
	await feed?.close(CHANGE_FEED_SHUTDOWN_TIMEOUT_MS);
}
