// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {IKVProvider, KVPurgeBatchResult} from '@pkgs/kv_client/src/IKVProvider';

export interface IPurgeQueue {
	addUrls(urls: Array<string>): Promise<void>;
	getQueueSize(): Promise<number>;
	clear(): Promise<void>;
}

const EXACT_QUEUE_KEY = 'bunny:purge:exact';
const PREFIX_QUEUE_KEY = 'bunny:purge:prefix';
const EXACT_BUCKET_KEY = 'bunny:purge:budget:exact';
const PREFIX_BUCKET_KEY = 'bunny:purge:budget:prefix';
const EXACT_MAX_TOKENS = 120;
const EXACT_REFILL_RATE = 5;
const EXACT_REFILL_INTERVAL_MS = 1000;
const PREFIX_MAX_TOKENS = 20;
const PREFIX_REFILL_RATE = 1;
const PREFIX_REFILL_INTERVAL_MS = 2000;

function isPrefix(url: string): boolean {
	return url.endsWith('*') || url.endsWith('/');
}

export class BunnyPurgeQueue implements IPurgeQueue {
	private readonly kvClient: IKVProvider;

	constructor(kvClient: IKVProvider) {
		this.kvClient = kvClient;
	}

	async addUrls(urls: Array<string>): Promise<void> {
		if (urls.length === 0) {
			return;
		}
		const exactUrls: Array<string> = [];
		const prefixUrls: Array<string> = [];
		for (const url of urls) {
			const trimmed = url.trim();
			if (trimmed === '') {
				continue;
			}
			if (isPrefix(trimmed)) {
				prefixUrls.push(trimmed);
			} else {
				exactUrls.push(trimmed);
			}
		}
		try {
			const ops: Array<Promise<number>> = [];
			if (exactUrls.length > 0) {
				ops.push(this.kvClient.sadd(EXACT_QUEUE_KEY, ...exactUrls));
			}
			if (prefixUrls.length > 0) {
				ops.push(this.kvClient.sadd(PREFIX_QUEUE_KEY, ...prefixUrls));
			}
			await Promise.all(ops);
			Logger.debug({exact: exactUrls.length, prefix: prefixUrls.length}, 'Added URLs to CDN purge queue');
		} catch (error) {
			Logger.error(
				{error, exact: exactUrls.length, prefix: prefixUrls.length},
				'Failed to add URLs to CDN purge queue',
			);
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			const [exactSize, prefixSize] = await Promise.all([
				this.kvClient.scard(EXACT_QUEUE_KEY),
				this.kvClient.scard(PREFIX_QUEUE_KEY),
			]);
			return exactSize + prefixSize;
		} catch (error) {
			Logger.error({error}, 'Failed to get CDN purge queue size');
			throw error;
		}
	}

	async clear(): Promise<void> {
		try {
			await this.kvClient.del(EXACT_QUEUE_KEY, PREFIX_QUEUE_KEY);
			Logger.debug('Cleared CDN purge queue');
		} catch (error) {
			Logger.error({error}, 'Failed to clear CDN purge queue');
			throw error;
		}
	}

	async dequeueExactBatch(maxItems: number): Promise<KVPurgeBatchResult> {
		return this.kvClient.dequeuePurgeBatch(
			EXACT_QUEUE_KEY,
			EXACT_BUCKET_KEY,
			maxItems,
			EXACT_MAX_TOKENS,
			EXACT_REFILL_RATE,
			EXACT_REFILL_INTERVAL_MS,
		);
	}

	async dequeuePrefixBatch(maxItems: number): Promise<KVPurgeBatchResult> {
		return this.kvClient.dequeuePurgeBatch(
			PREFIX_QUEUE_KEY,
			PREFIX_BUCKET_KEY,
			maxItems,
			PREFIX_MAX_TOKENS,
			PREFIX_REFILL_RATE,
			PREFIX_REFILL_INTERVAL_MS,
		);
	}
}

export class NoopPurgeQueue implements IPurgeQueue {
	async addUrls(_urls: Array<string>): Promise<void> {}

	async getQueueSize(): Promise<number> {
		return 0;
	}

	async clear(): Promise<void> {}
}
