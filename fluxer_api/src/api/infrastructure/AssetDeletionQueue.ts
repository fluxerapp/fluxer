// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	IAssetDeletionQueue,
	QueuedAssetDeletion,
	QueuedAssetReference,
} from '@app/api/infrastructure/IAssetDeletionQueue';
import {Logger} from '@app/api/Logger';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {isValidTimestamp} from '@app/api/utils/TimestampUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const QUEUE_KEY = 'asset:deletion:queue';
const MAX_RETRIES = 5;

function isQueuedAssetReference(value: unknown): value is QueuedAssetReference {
	if (!isJsonRecord(value)) return false;
	return (
		(value.entityType === 'user' || value.entityType === 'guild' || value.entityType === 'guild_member') &&
		(value.assetType === 'avatar' ||
			value.assetType === 'banner' ||
			value.assetType === 'icon' ||
			value.assetType === 'splash' ||
			value.assetType === 'embed_splash') &&
		typeof value.entityId === 'string' &&
		(value.guildId === undefined || typeof value.guildId === 'string') &&
		typeof value.hash === 'string'
	);
}

function isQueuedAssetDeletion(value: unknown): value is QueuedAssetDeletion {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value.s3Key === 'string' &&
		(typeof value.cdnUrl === 'string' || value.cdnUrl === null) &&
		typeof value.reason === 'string' &&
		(value.staleReference === undefined || isQueuedAssetReference(value.staleReference)) &&
		(value.queuedAt === undefined || isValidTimestamp(value.queuedAt)) &&
		(value.retryCount === undefined ||
			(typeof value.retryCount === 'number' &&
				Number.isInteger(value.retryCount) &&
				value.retryCount >= 0 &&
				value.retryCount <= MAX_RETRIES))
	);
}

export class AssetDeletionQueue implements IAssetDeletionQueue {
	constructor(private readonly kvClient: IKVProvider) {}

	async queueDeletion(item: Omit<QueuedAssetDeletion, 'queuedAt' | 'retryCount'>): Promise<void> {
		const fullItem: QueuedAssetDeletion = {
			...item,
			queuedAt: Date.now(),
			retryCount: 0,
		};
		try {
			await this.kvClient.rpush(QUEUE_KEY, JSON.stringify(fullItem));
			Logger.debug({s3Key: item.s3Key, reason: item.reason}, 'Queued asset for deletion');
		} catch (error) {
			Logger.error({error, item}, 'Failed to queue asset for deletion');
			throw error;
		}
	}

	async queueCdnPurge(cdnUrl: string): Promise<void> {
		await this.queueDeletion({
			s3Key: '',
			cdnUrl,
			reason: 'cdn_purge_only',
		});
	}

	async getBatch(count: number): Promise<Array<QueuedAssetDeletion>> {
		if (count <= 0) {
			return [];
		}
		try {
			const items = await this.kvClient.lpop(QUEUE_KEY, count);
			const parsed: Array<QueuedAssetDeletion> = [];
			for (const item of items) {
				const queuedItem = parseJsonWithGuard(item, isQueuedAssetDeletion);
				if (queuedItem) {
					parsed.push(queuedItem);
				} else {
					Logger.warn({item: item.slice(0, 1000)}, 'Dropping malformed asset deletion queue item');
				}
			}
			return parsed;
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
			await this.kvClient.rpush(QUEUE_KEY, JSON.stringify(requeuedItem));
			Logger.debug({s3Key: item.s3Key, retryCount}, 'Requeued failed asset deletion');
		} catch (error) {
			Logger.error({error, item}, 'Failed to requeue asset deletion');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.kvClient.llen(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get asset deletion queue size');
			throw error;
		}
	}

	async clear(): Promise<void> {
		try {
			await this.kvClient.del(QUEUE_KEY);
			Logger.debug('Cleared asset deletion queue');
		} catch (error) {
			Logger.error({error}, 'Failed to clear asset deletion queue');
			throw error;
		}
	}
}
