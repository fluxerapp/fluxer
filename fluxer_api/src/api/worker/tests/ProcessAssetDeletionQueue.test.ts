// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, describe, expect, it, vi} from 'vitest';
import {AssetDeletionQueue} from '../../infrastructure/AssetDeletionQueue';
import {NoopPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {NoopLogger} from '../../test/mocks/NoopLogger';
import processAssetDeletionQueue from '../tasks/ProcessAssetDeletionQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '../WorkerContext';

const HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;

function createHarness(deleteObject: IStorageService['deleteObject']) {
	const kvClient = new MockKVProvider();
	const assetDeletionQueue = new AssetDeletionQueue(kvClient);
	setWorkerDependenciesForTest({
		assetDeletionQueue,
		purgeQueue: new NoopPurgeQueue(),
		storageService: {deleteObject} as unknown as IStorageService,
		userRepository: {findUnique: async () => null} as never,
		guildRepository: {findUnique: async () => null, getMember: async () => null} as never,
	});
	return {assetDeletionQueue, kvClient};
}

async function queueAssets(assetDeletionQueue: AssetDeletionQueue, count: number): Promise<void> {
	for (let index = 0; index < count; index++) {
		await assetDeletionQueue.queueDeletion({
			s3Key: `attachments/100/200/file-${index}.png`,
			cdnUrl: null,
			reason: 'test',
		});
	}
}

describe('processAssetDeletionQueue', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	it('attempts each queued asset once per run when storage is failing', async () => {
		const deleteObject = vi.fn().mockRejectedValue(new Error('s3 unavailable'));
		const {assetDeletionQueue} = createHarness(deleteObject);
		await queueAssets(assetDeletionQueue, 3);

		await expect(processAssetDeletionQueue({}, HELPERS)).rejects.toThrow(/3 failures/);

		expect(deleteObject).toHaveBeenCalledTimes(3);
		expect(await assetDeletionQueue.getQueueSize()).toBe(3);
		const remaining = await assetDeletionQueue.getBatch(10);
		expect(remaining.map((item) => item.retryCount)).toEqual([1, 1, 1]);
	});

	it('attempts each queued asset once per run across batch boundaries', async () => {
		const deleteObject = vi.fn().mockRejectedValue(new Error('s3 unavailable'));
		const {assetDeletionQueue} = createHarness(deleteObject);
		await queueAssets(assetDeletionQueue, 60);

		await expect(processAssetDeletionQueue({}, HELPERS)).rejects.toThrow(/60 failures/);

		expect(deleteObject).toHaveBeenCalledTimes(60);
		expect(await assetDeletionQueue.getQueueSize()).toBe(60);
	});

	it('drains the queue when storage succeeds', async () => {
		const deleteObject = vi.fn().mockResolvedValue(undefined);
		const {assetDeletionQueue} = createHarness(deleteObject);
		await queueAssets(assetDeletionQueue, 60);

		await processAssetDeletionQueue({}, HELPERS);

		expect(deleteObject).toHaveBeenCalledTimes(60);
		expect(await assetDeletionQueue.getQueueSize()).toBe(0);
	});
});
