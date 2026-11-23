// SPDX-License-Identifier: AGPL-3.0-or-later

import {type DualStoreCounters, DualStoreStorageService} from '@app/api/infrastructure/DualStoreStorageService';
import {parseBucketMap, StoreBucketResolver} from '@app/api/infrastructure/StoreBucketMap';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {describe, expect, it, vi} from 'vitest';

const OLD = parseBucketMap('cdn=media,uploads=uploads,reports=reports,harvests=harvests');
const NEW = parseBucketMap('cdn=media-new,uploads=uploads-new,reports=reports-new,harvests=harvests-new');

function serverError(): S3ServiceException {
	return new S3ServiceException({name: 'InternalError', $fault: 'server', $metadata: {}, message: 'boom'});
}

function harness(options: {primaryIsNew?: boolean; fallbackReads?: boolean} = {}) {
	const primaryIsNew = options.primaryIsNew ?? true;
	const primary = new MockStorageService();
	const fallback = new MockStorageService();
	const counters: DualStoreCounters = {fallbackHit: vi.fn(), doubleMiss: vi.fn(), storeError: vi.fn()};
	const service = new DualStoreStorageService({
		primary,
		primaryStoreId: primaryIsNew ? 'store-b' : 'store-a',
		fallback,
		fallbackStoreId: primaryIsNew ? 'store-a' : 'store-b',
		buckets: primaryIsNew ? new StoreBucketResolver(NEW, OLD) : new StoreBucketResolver(OLD, NEW),
		cdnBucket: primaryIsNew ? 'media-new' : 'media',
		fallbackReadsEnabled: options.fallbackReads ?? true,
		counters,
	});
	return {service, primary, fallback, counters};
}

describe('StoreBucketMap', () => {
	it('parses logical=physical pairs and rejects malformed entries', () => {
		expect(parseBucketMap(' cdn=media-new , uploads=uploads-new ').get('uploads')).toBe('uploads-new');
		expect(() => parseBucketMap('cdn')).toThrow();
		expect(() => parseBucketMap('cdn=a,cdn=b')).toThrow();
	});

	it('resolves an old name and a new name to the same logical bucket', () => {
		const resolver = new StoreBucketResolver(NEW, OLD);
		expect(resolver.primaryBucket('uploads')).toBe('uploads-new');
		expect(resolver.primaryBucket('uploads-new')).toBe('uploads-new');
		expect(resolver.fallbackBucket('uploads-new')).toBe('uploads');
		expect(resolver.fallbackBucket('media')).toBe('media');
		expect(resolver.primaryBucket('media')).toBe('media-new');
		expect(resolver.primaryBucket('unknown-bucket')).toBe('unknown-bucket');
	});
});

describe('DualStoreStorageService reads', () => {
	it('serves from the primary without touching the fallback', async () => {
		const {service, primary, fallback} = harness();
		await primary.uploadObject({bucket: 'media-new', key: 'a', body: new Uint8Array([1])});
		const spy = vi.spyOn(fallback, 'getObjectMetadata');
		expect(await service.getObjectMetadata('media-new', 'a')).not.toBeNull();
		expect(spy).not.toHaveBeenCalled();
	});

	it('falls back on a primary 404 and maps the bucket name for the fallback store', async () => {
		const {service, fallback, counters} = harness();
		await fallback.uploadObject({bucket: 'media', key: 'old', body: new Uint8Array([7, 8])});
		expect(Array.from(await service.readObject('media-new', 'old'))).toEqual([7, 8]);
		expect((await service.getObjectMetadata('media-new', 'old'))?.contentLength).toBe(2);
		expect(counters.fallbackHit).toHaveBeenCalledWith('media-new', 'get', 'store-a');
	});

	it('returns not found only when both stores miss', async () => {
		const {service, counters} = harness();
		expect(await service.getObjectMetadata('media-new', 'missing')).toBeNull();
		await expect(service.readObject('media-new', 'missing')).rejects.toMatchObject({name: 'NoSuchKey'});
		expect(counters.doubleMiss).toHaveBeenCalled();
	});

	it('reports a primary 5xx, not a miss, when the fallback also misses', async () => {
		const {service, primary} = harness();
		vi.spyOn(primary, 'getObjectMetadata').mockRejectedValue(serverError());
		await expect(service.getObjectMetadata('media-new', 'k')).rejects.toMatchObject({name: 'InternalError'});
	});

	it('serves the fallback copy when the primary errors and the fallback has the object', async () => {
		const {service, primary, fallback} = harness();
		await fallback.uploadObject({bucket: 'media', key: 'k', body: new Uint8Array([3])});
		vi.spyOn(primary, 'readObject').mockRejectedValue(serverError());
		expect(Array.from(await service.readObject('media-new', 'k'))).toEqual([3]);
	});

	it('does not read the fallback when fallback reads are off', async () => {
		const {service, fallback} = harness({fallbackReads: false});
		await fallback.uploadObject({bucket: 'media', key: 'k', body: new Uint8Array([3])});
		const spy = vi.spyOn(fallback, 'getObjectMetadata');
		expect(await service.getObjectMetadata('media-new', 'k')).toBeNull();
		expect(spy).not.toHaveBeenCalled();
	});
});

describe('DualStoreStorageService writes and deletes', () => {
	it('writes only to the primary, with the primary bucket name even for an old name', async () => {
		const {service, primary, fallback} = harness();
		await service.uploadObject({bucket: 'uploads', key: 'u', body: new Uint8Array([1])});
		expect(primary.hasObject('uploads-new', 'u')).toBe(true);
		expect(fallback.hasObject('uploads', 'u')).toBe(false);
	});

	it('deletes the fallback copy before the primary copy', async () => {
		const {service, primary, fallback} = harness();
		const order: Array<string> = [];
		vi.spyOn(fallback, 'deleteObject').mockImplementation(async (bucket) => {
			order.push(`fallback:${bucket}`);
		});
		vi.spyOn(primary, 'deleteObject').mockImplementation(async (bucket) => {
			order.push(`primary:${bucket}`);
		});
		await service.deleteObject('media-new', 'x');
		expect(order).toEqual(['fallback:media', 'primary:media-new']);
	});

	it('keeps the primary copy when the fallback delete fails', async () => {
		const {service, primary, fallback} = harness();
		await primary.uploadObject({bucket: 'media-new', key: 'x', body: new Uint8Array([1])});
		vi.spyOn(fallback, 'deleteObject').mockRejectedValue(serverError());
		await expect(service.deleteObject('media-new', 'x')).rejects.toMatchObject({name: 'InternalError'});
		expect(primary.hasObject('media-new', 'x')).toBe(true);
	});

	it('deletes avatars from both stores under each store cdn bucket', async () => {
		const {service, primary, fallback} = harness();
		await primary.uploadObject({bucket: 'media-new', key: 'avatars/1/h', body: new Uint8Array([1])});
		await fallback.uploadObject({bucket: 'media', key: 'avatars/1/h', body: new Uint8Array([1])});
		await service.deleteAvatar({prefix: 'avatars/1', key: 'h'});
		expect(primary.hasObject('media-new', 'avatars/1/h')).toBe(false);
		expect(fallback.hasObject('media', 'avatars/1/h')).toBe(false);
	});

	it('presigns uploads against the primary store', async () => {
		const {service, primary, fallback} = harness();
		const p = vi.spyOn(primary, 'getPresignedUploadURL');
		const f = vi.spyOn(fallback, 'getPresignedUploadURL');
		await service.getPresignedUploadURL({bucket: 'uploads', key: 'k'});
		expect(p).toHaveBeenCalledWith(expect.objectContaining({bucket: 'uploads-new'}));
		expect(f).not.toHaveBeenCalled();
	});

	it('presigns downloads on whichever store holds the object', async () => {
		const {service, primary, fallback} = harness();
		await fallback.uploadObject({bucket: 'reports', key: 'r', body: new Uint8Array([1])});
		const f = vi.spyOn(fallback, 'getPresignedDownloadURL');
		const p = vi.spyOn(primary, 'getPresignedDownloadURL');
		await service.getPresignedDownloadURL({bucket: 'reports-new', key: 'r'});
		expect(f).toHaveBeenCalledWith(expect.objectContaining({bucket: 'reports'}));
		expect(p).not.toHaveBeenCalled();
	});

	it('copies an object that exists only on the fallback by streaming it to the primary', async () => {
		const {service, primary, fallback} = harness();
		await fallback.uploadObject({bucket: 'uploads', key: 'src', body: new Uint8Array([9, 9])});
		await service.copyObject({
			sourceBucket: 'uploads-new',
			sourceKey: 'src',
			destinationBucket: 'media-new',
			destinationKey: 'dst',
		});
		expect(Array.from(await primary.readObject('media-new', 'dst'))).toEqual([9, 9]);
	});

	it('retries multipart completion on the fallback when the primary has no such upload', async () => {
		const {service, primary, fallback} = harness();
		vi.spyOn(primary, 'completeMultipartUpload').mockRejectedValue(
			Object.assign(new Error('gone'), {name: 'NoSuchUpload'}),
		);
		const f = vi.spyOn(fallback, 'completeMultipartUpload').mockResolvedValue();
		await service.completeMultipartUpload({bucket: 'uploads-new', key: 'k', uploadId: 'u', parts: []});
		expect(f).toHaveBeenCalledWith(expect.objectContaining({bucket: 'uploads'}));
	});
});

describe('DualStoreStorageService with the old store still primary', () => {
	it('keeps every write and read on the old names', async () => {
		const {service, primary} = harness({primaryIsNew: false});
		await service.uploadObject({bucket: 'media', key: 'a', body: new Uint8Array([1])});
		expect(primary.hasObject('media', 'a')).toBe(true);
		expect(await service.getObjectMetadata('media', 'a')).not.toBeNull();
	});
});
