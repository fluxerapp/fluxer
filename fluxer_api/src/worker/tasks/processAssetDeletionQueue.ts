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

import type {Task} from 'graphile-worker';
import {Config} from '~/Config';
import type {AssetDeletionQueue} from '~/infrastructure/AssetDeletionQueue';
import type {CloudflarePurgeQueue, NoopCloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {QueuedAssetDeletion} from '~/infrastructure/IAssetDeletionQueue';
import type {StorageService} from '~/infrastructure/StorageService';
import {Logger} from '~/Logger';
import {getWorkerDependencies} from '../WorkerContext';

const BATCH_SIZE = 50;
const MAX_ITEMS_PER_RUN = 500;

const processAssetDeletionQueue: Task = async (_payload, _helpers) => {
	const {assetDeletionQueue, cloudflarePurgeQueue, storageService} = getWorkerDependencies();

	const queueSize = await assetDeletionQueue.getQueueSize();
	if (queueSize === 0) {
		Logger.debug('Asset deletion queue is empty');
		return;
	}

	Logger.info({queueSize}, 'Starting asset deletion queue processing');

	let totalProcessed = 0;
	let totalDeleted = 0;
	let totalFailed = 0;
	let totalCdnPurged = 0;

	while (totalProcessed < MAX_ITEMS_PER_RUN) {
		const batch = await assetDeletionQueue.getBatch(BATCH_SIZE);
		if (batch.length === 0) {
			break;
		}

		const results = await Promise.allSettled(
			batch.map((item) => processItem(item, storageService, cloudflarePurgeQueue, assetDeletionQueue)),
		);

		for (let i = 0; i < results.length; i++) {
			const result = results[i];
			const item = batch[i];

			if (result.status === 'fulfilled') {
				totalDeleted++;
				if (item.cdnUrl) {
					totalCdnPurged++;
				}
			} else {
				totalFailed++;
				Logger.error(
					{error: result.reason, s3Key: item.s3Key, cdnUrl: item.cdnUrl},
					'Failed to process asset deletion',
				);
			}
		}

		totalProcessed += batch.length;
	}

	const remainingSize = await assetDeletionQueue.getQueueSize();

	Logger.info(
		{
			totalProcessed,
			totalDeleted,
			totalFailed,
			totalCdnPurged,
			remainingSize,
		},
		'Finished asset deletion queue processing',
	);
};

async function processItem(
	item: QueuedAssetDeletion,
	storageService: StorageService,
	cloudflarePurgeQueue: CloudflarePurgeQueue | NoopCloudflarePurgeQueue,
	assetDeletionQueue: AssetDeletionQueue,
): Promise<void> {
	try {
		if (item.s3Key) {
			try {
				await storageService.deleteObject(Config.s3.buckets.cdn, item.s3Key);
				Logger.debug({s3Key: item.s3Key, reason: item.reason}, 'Deleted asset from S3');
			} catch (error: unknown) {
				const isNotFound =
					error instanceof Error &&
					(('name' in error && error.name === 'NotFound') ||
						('code' in error && (error as {code?: string}).code === 'NoSuchKey'));

				if (!isNotFound) {
					throw error;
				}
				Logger.debug({s3Key: item.s3Key}, 'Asset already deleted from S3 (NotFound)');
			}
		}

		if (item.cdnUrl && cloudflarePurgeQueue) {
			await cloudflarePurgeQueue.addUrls([item.cdnUrl]);
			Logger.debug({cdnUrl: item.cdnUrl}, 'Queued asset CDN URL for Cloudflare purge');
		}
	} catch (error) {
		await assetDeletionQueue.requeueItem(item);
		throw error;
	}
}

export default processAssetDeletionQueue;
