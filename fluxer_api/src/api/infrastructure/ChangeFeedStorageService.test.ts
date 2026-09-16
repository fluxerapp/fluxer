// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ChangeFeedStorageService} from '@app/api/infrastructure/ChangeFeedStorageService';
import {
	type StorageChangeEvent,
	StorageChangeFeed,
	type StorageChangeSink,
} from '@app/api/infrastructure/StorageChangeFeed';
import {StorageService} from '@app/api/infrastructure/StorageService';
import {createStorageService, shutdownStorageChangeFeed} from '@app/api/infrastructure/StorageServiceFactory';
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
	it('emits a put event after an upload lands', async () => {
		const {service, inner, feed, sink} = createHarness();

		await service.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: 'attachments/1/2/cat.png',
			body: bytes(5),
			contentType: 'image/png',
		});
		await feed.idle();

		expect(await inner.getObjectMetadata(Config.s3.buckets.cdn, 'attachments/1/2/cat.png')).not.toBeNull();
		expect(sink.events).toEqual([
			{
				subject: `storage.${Config.s3.buckets.cdn}.put`,
				bucket: Config.s3.buckets.cdn,
				key: 'attachments/1/2/cat.png',
				op: 'put',
				size: 5,
				etag: null,
				contentType: 'image/png',
				at: expect.any(String),
			},
		]);
	});

	it('emits delete events for single and batch deletes', async () => {
		const {service, feed, sink} = createHarness();

		await service.deleteObject(Config.s3.buckets.cdn, 'icons/1/a.webp');
		await service.deleteObjects({bucket: Config.s3.buckets.reports, objects: [{Key: 'r/1'}, {Key: 'r/2'}]});
		await service.deleteAvatar({prefix: 'avatars/9', key: 'hash'});
		await feed.idle();

		expect(sink.events.map(({subject, bucket, key, op}) => ({subject, bucket, key, op}))).toEqual([
			{
				subject: `storage.${Config.s3.buckets.cdn}.delete`,
				bucket: Config.s3.buckets.cdn,
				key: 'icons/1/a.webp',
				op: 'delete',
			},
			{
				subject: `storage.${Config.s3.buckets.reports}.delete`,
				bucket: Config.s3.buckets.reports,
				key: 'r/1',
				op: 'delete',
			},
			{
				subject: `storage.${Config.s3.buckets.reports}.delete`,
				bucket: Config.s3.buckets.reports,
				key: 'r/2',
				op: 'delete',
			},
			{
				subject: `storage.${Config.s3.buckets.cdn}.delete`,
				bucket: Config.s3.buckets.cdn,
				key: 'avatars/9/hash',
				op: 'delete',
			},
		]);
	});

	it('emits a put and a delete for a move between durable buckets', async () => {
		const {service, inner, feed, sink} = createHarness();
		await inner.uploadObject({bucket: Config.s3.buckets.cdn, key: 'from', body: bytes(3)});

		await service.moveObject({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey: 'from',
			destinationBucket: Config.s3.buckets.reports,
			destinationKey: 'to',
		});
		await feed.idle();

		expect(sink.events.map(({bucket, key, op}) => ({bucket, key, op}))).toEqual([
			{bucket: Config.s3.buckets.reports, key: 'to', op: 'put'},
			{bucket: Config.s3.buckets.cdn, key: 'from', op: 'delete'},
		]);
	});

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

	it('emits nothing when the storage call itself fails', async () => {
		const {service, feed, sink} = createHarness({inner: new MockStorageService({shouldFailUpload: true})});

		await expect(service.uploadObject({bucket: Config.s3.buckets.cdn, key: 'broken', body: bytes(1)})).rejects.toThrow(
			'Mock storage upload failure',
		);
		await feed.idle();

		expect(sink.events).toEqual([]);
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

	it('does not wait for a publish that never answers', async () => {
		const sink: StorageChangeSink = {
			publish: () => new Promise<void>(() => undefined),
			close: async () => undefined,
		};
		const {service, feed} = createHarness({sink});

		await service.uploadObject({bucket: Config.s3.buckets.cdn, key: 'first', body: bytes(1)});
		await new Promise((resolve) => setImmediate(resolve));
		await service.uploadObject({bucket: Config.s3.buckets.cdn, key: 'second', body: bytes(1)});

		expect(feed.stats()).toEqual({queued: 1, published: 0, dropped: 0, failed: 0});
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

describe('createStorageService', () => {
	it('returns the plain storage service while the change feed is disabled', () => {
		expect(Config.storageChangeFeed.enabled).toBe(false);
		expect(Config.storageChangeFeed.skipBuckets).toEqual([Config.s3.buckets.uploads]);
		expect(createStorageService()).toBeInstanceOf(StorageService);
	});

	it('wraps the storage service once the change feed is enabled', async () => {
		const original = Config.storageChangeFeed.enabled;
		Config.storageChangeFeed.enabled = true;
		try {
			expect(createStorageService()).toBeInstanceOf(ChangeFeedStorageService);
		} finally {
			Config.storageChangeFeed.enabled = original;
			await shutdownStorageChangeFeed();
		}
	});
});
