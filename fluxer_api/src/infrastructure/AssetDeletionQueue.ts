/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Redis} from 'ioredis';
import {Logger} from '~/Logger';
import type {IAssetDeletionQueue, QueuedAssetDeletion} from './IAssetDeletionQueue';

const QUEUE_KEY = 'asset:deletion:queue';
const MAX_RETRIES = 5;

export class AssetDeletionQueue implements IAssetDeletionQueue {
	constructor(private readonly redis: Redis) {}

	async queueDeletion(item: Omit<QueuedAssetDeletion, 'queuedAt' | 'retryCount'>): Promise<void> {
		const fullItem: QueuedAssetDeletion = {
			...item,
			queuedAt: Date.now(),
			retryCount: 0,
		};

		try {
			await this.redis.rpush(QUEUE_KEY, JSON.stringify(fullItem));
			Logger.debug({s3Key: item.s3Key, reason: item.reason}, 'Queued asset for deletion');
		} catch (error) {
			Logger.error({error, item}, 'Failed to queue asset for deletion');
			throw error;
		}
	}

	async queueCloudflarePurge(cdnUrl: string): Promise<void> {
		const item: QueuedAssetDeletion = {
			s3Key: '',
			cdnUrl,
			reason: 'cdn_purge_only',
			queuedAt: Date.now(),
			retryCount: 0,
		};

		try {
			await this.redis.rpush(QUEUE_KEY, JSON.stringify(item));
			Logger.debug({cdnUrl}, 'Queued CDN URL for purge');
		} catch (error) {
			Logger.error({error, cdnUrl}, 'Failed to queue CDN URL for purge');
			throw error;
		}
	}

	async getBatch(count: number): Promise<Array<QueuedAssetDeletion>> {
		if (count <= 0) {
			return [];
		}

		try {
			const items = await this.redis.lpop(QUEUE_KEY, count);

			if (!items) {
				return [];
			}

			const itemArray = Array.isArray(items) ? items : [items];
			return itemArray.map((item) => JSON.parse(item) as QueuedAssetDeletion);
		} catch (error) {
			Logger.error({error, count}, 'Failed to get batch from asset deletion queue');
			throw error;
		}
	}

	async requeueItem(item: QueuedAssetDeletion): Promise<void> {
		const retryCount = (item.retryCount ?? 0) + 1;

		if (retryCount > MAX_RETRIES) {
			Logger.error(
				{s3Key: item.s3Key, cdnUrl: item.cdnUrl, retryCount},
				'Asset deletion exceeded max retries, dropping from queue',
			);
			return;
		}

		const requeuedItem: QueuedAssetDeletion = {
			...item,
			retryCount,
		};

		try {
			await this.redis.rpush(QUEUE_KEY, JSON.stringify(requeuedItem));
			Logger.debug({s3Key: item.s3Key, retryCount}, 'Requeued failed asset deletion');
		} catch (error) {
			Logger.error({error, item}, 'Failed to requeue asset deletion');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.redis.llen(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get asset deletion queue size');
			throw error;
		}
	}

	async clear(): Promise<void> {
		try {
			await this.redis.del(QUEUE_KEY);
			Logger.debug('Cleared asset deletion queue');
		} catch (error) {
			Logger.error({error}, 'Failed to clear asset deletion queue');
			throw error;
		}
	}
}
