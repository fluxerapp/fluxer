// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CachePurgeBatch} from '@app/api/infrastructure/CachePurgeAdapter';
import {Logger} from '@app/api/Logger';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export interface IPurgeQueue {
	addUrls(urls: Array<string>): Promise<void>;
}

const EXACT_QUEUE_KEY = 'cache_purge:exact';
const PREFIX_QUEUE_KEY = 'cache_purge:prefix';
const EXACT_BUCKET_KEY = 'cache_purge:budget:exact';
const PREFIX_BUCKET_KEY = 'cache_purge:budget:prefix';
const REJECTED_KEY = 'cache_purge:rejected';
const EXACT_MAX_TOKENS = 120;
const EXACT_REFILL_RATE = 5;
const EXACT_REFILL_INTERVAL_MS = 1000;
const PREFIX_MAX_TOKENS = 20;
const PREFIX_REFILL_RATE = 1;
const PREFIX_REFILL_INTERVAL_MS = 2000;

function isPrefix(url: string): boolean {
	return url.endsWith('*') || url.endsWith('/');
}

export class CachePurgeQueue implements IPurgeQueue {
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
			await this.addToSets({exact: exactUrls, prefix: prefixUrls});
			Logger.debug({exact: exactUrls.length, prefix: prefixUrls.length}, 'Added URLs to cache purge queue');
		} catch (error) {
			Logger.error(
				{error, exact: exactUrls.length, prefix: prefixUrls.length},
				'Failed to add URLs to cache purge queue',
			);
			throw error;
		}
	}

	async dequeueBatch(): Promise<CachePurgeBatch> {
		const exact = await this.kvClient.dequeuePurgeBatch(
			EXACT_QUEUE_KEY,
			EXACT_BUCKET_KEY,
			EXACT_MAX_TOKENS,
			EXACT_MAX_TOKENS,
			EXACT_REFILL_RATE,
			EXACT_REFILL_INTERVAL_MS,
		);
		try {
			const prefix = await this.kvClient.dequeuePurgeBatch(
				PREFIX_QUEUE_KEY,
				PREFIX_BUCKET_KEY,
				PREFIX_MAX_TOKENS,
				PREFIX_MAX_TOKENS,
				PREFIX_REFILL_RATE,
				PREFIX_REFILL_INTERVAL_MS,
			);
			return {exact: exact.urls, prefix: prefix.urls};
		} catch (error) {
			await this.requeue({exact: exact.urls, prefix: []});
			throw error;
		}
	}

	async requeue(batch: CachePurgeBatch): Promise<void> {
		try {
			await this.addToSets(batch);
		} catch (error) {
			Logger.error(
				{error, exact: batch.exact.length, prefix: batch.prefix.length},
				'Failed to requeue cache purge entries',
			);
			throw error;
		}
	}

	async reject(batch: CachePurgeBatch): Promise<void> {
		await this.kvClient.sadd(REJECTED_KEY, ...batch.exact, ...batch.prefix);
		Logger.error(
			{exact: batch.exact.length, prefix: batch.prefix.length, key: REJECTED_KEY},
			'Set aside cache purge entries the endpoint rejected',
		);
	}

	private async addToSets(batch: CachePurgeBatch): Promise<void> {
		const ops: Array<Promise<number>> = [];
		if (batch.exact.length > 0) {
			ops.push(this.kvClient.sadd(EXACT_QUEUE_KEY, ...batch.exact));
		}
		if (batch.prefix.length > 0) {
			ops.push(this.kvClient.sadd(PREFIX_QUEUE_KEY, ...batch.prefix));
		}
		await Promise.all(ops);
	}
}

export class NoopPurgeQueue implements IPurgeQueue {
	async addUrls(_urls: Array<string>): Promise<void> {}
}
