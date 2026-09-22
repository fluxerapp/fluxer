// SPDX-License-Identifier: AGPL-3.0-or-later

import {canonicalizePurgeUrl} from '@app/api/infrastructure/CachePurgePaths';
import {Logger} from '@app/api/Logger';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export interface IPurgeQueue {
	addUrls(urls: Array<string>): Promise<void>;
}

const QUEUE_KEY = 'cache_purge:queue';
const BUDGET_KEY = 'cache_purge:budget';
const REJECTED_KEY = 'cache_purge:rejected';
const MAX_TOKENS = 120;
const REFILL_RATE = 5;
const REFILL_INTERVAL_MS = 1000;

export class CachePurgeQueue implements IPurgeQueue {
	constructor(private readonly kvClient: IKVProvider) {}

	async addUrls(urls: Array<string>): Promise<void> {
		const prefixes = new Set<string>();
		for (const url of urls) {
			const canonical = canonicalizePurgeUrl(url);
			if (canonical.length === 0) {
				Logger.warn({url}, 'Skipped a cache purge URL that is not a media CDN object');
				continue;
			}
			for (const prefix of canonical) {
				prefixes.add(prefix);
			}
		}
		if (prefixes.size === 0) {
			return;
		}
		try {
			await this.kvClient.sadd(QUEUE_KEY, ...prefixes);
			Logger.debug({prefixes: prefixes.size}, 'Added prefixes to the cache purge queue');
		} catch (error) {
			Logger.error({error, prefixes: prefixes.size}, 'Failed to add prefixes to the cache purge queue');
			throw error;
		}
	}

	async dequeueBatch(): Promise<Array<string>> {
		const batch = await this.kvClient.dequeuePurgeBatch(
			QUEUE_KEY,
			BUDGET_KEY,
			MAX_TOKENS,
			MAX_TOKENS,
			REFILL_RATE,
			REFILL_INTERVAL_MS,
		);
		return batch.entries;
	}

	async requeue(prefixes: ReadonlyArray<string>): Promise<void> {
		if (prefixes.length === 0) {
			return;
		}
		try {
			await this.kvClient.sadd(QUEUE_KEY, ...prefixes);
		} catch (error) {
			Logger.error({error, prefixes: prefixes.length}, 'Failed to requeue cache purge prefixes');
			throw error;
		}
	}

	async reject(prefixes: ReadonlyArray<string>): Promise<void> {
		if (prefixes.length === 0) {
			return;
		}
		await this.kvClient.sadd(REJECTED_KEY, ...prefixes);
	}
}

export class NoopPurgeQueue implements IPurgeQueue {
	async addUrls(_urls: Array<string>): Promise<void> {}
}
