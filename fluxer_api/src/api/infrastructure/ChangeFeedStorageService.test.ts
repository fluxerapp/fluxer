// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ChangeFeedStorageService} from '@app/api/infrastructure/ChangeFeedStorageService';
import {
	type StorageChangeEvent,
	StorageChangeFeed,
	type StorageChangeSink,
} from '@app/api/infrastructure/StorageChangeFeed';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {describe, expect, it} from 'vitest';

class RecordingSink implements StorageChangeSink {
	readonly events: Array<StorageChangeEvent & {subject: string}> = [];

	async publish(subject: string, body: string): Promise<void> {
		this.events.push({subject, ...(JSON.parse(body) as StorageChangeEvent)});
	}

	async close(): Promise<void> {}
}

function createHarness(options: {inner?: MockStorageService; sink?: StorageChangeSink; capacity?: number} = {}): {
	service: ChangeFeedStorageService;
	inner: MockStorageService;
	feed: StorageChangeFeed;
	sink: RecordingSink;
} {
	const inner = options.inner ?? new MockStorageService();
	const sink = new RecordingSink();
	const feed = new StorageChangeFeed(options.sink ?? sink, {
		capacity: options.capacity,
		maxAttempts: 1,
		retryDelayMs: 0,
	});
	const service = new ChangeFeedStorageService(inner, feed, [Config.s3.buckets.uploads]);
	return {service, inner, feed, sink};
}

const bytes = (length: number) => new Uint8Array(length).fill(7);

describe('ChangeFeedStorageService', () => {
	it('emits nothing for the uploads bucket but still reports promotions out of it', async () => {
		const {service, inner, feed, sink} = createHarness();
		const uploads = Config.s3.buckets.uploads;

		await service.uploadObject({bucket: uploads, key: 'staged', body: bytes(4)});
		await service.copyObject({
			sourceBucket: uploads,
			sourceKey: 'staged',
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey: 'attachments/1/2/staged.bin',
		});
		await service.deleteObject(uploads, 'staged');
		await feed.idle();

		expect(inner.uploadObjectSpy).toHaveBeenCalledTimes(1);
		expect(sink.events.map(({bucket, key, op}) => ({bucket, key, op}))).toEqual([
			{bucket: Config.s3.buckets.cdn, key: 'attachments/1/2/staged.bin', op: 'put'},
		]);
	});

	it('does not let a failing publish reach the caller', async () => {
		const sink: StorageChangeSink = {
			publish: async () => {
				throw new Error('nats: timeout');
			},
			close: async () => undefined,
		};
		const {service, inner, feed} = createHarness({sink});

		await expect(
			service.uploadObject({bucket: Config.s3.buckets.cdn, key: 'kept', body: bytes(2)}),
		).resolves.toBeUndefined();
		await expect(service.deleteObject(Config.s3.buckets.cdn, 'kept')).resolves.toBeUndefined();
		await feed.idle();

		expect(inner.deleteObjectSpy).toHaveBeenCalledWith(Config.s3.buckets.cdn, 'kept');
		expect(feed.stats()).toEqual({queued: 0, published: 0, dropped: 0, failed: 2});
	});

	it('drops and counts events when the queue is full without failing the storage call', async () => {
		const sink: StorageChangeSink = {
			publish: () => new Promise<void>(() => undefined),
			close: async () => undefined,
		};
		const {service, inner, feed} = createHarness({sink, capacity: 1});

		for (const key of ['a', 'b', 'c']) {
			await service.uploadObject({bucket: Config.s3.buckets.cdn, key, body: bytes(1)});
		}

		expect(inner.uploadObjectSpy).toHaveBeenCalledTimes(3);
		expect(feed.stats()).toEqual({queued: 1, published: 0, dropped: 2, failed: 0});
	});
});
