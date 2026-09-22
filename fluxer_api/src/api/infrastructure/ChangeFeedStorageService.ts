// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {IStorageService, ProcessedStorageObjectMetadata} from '@app/api/infrastructure/IStorageService';
import type {StorageChangeFeed, StorageChangeOp} from '@app/api/infrastructure/StorageChangeFeed';

type Params<K extends keyof IStorageService> = Parameters<IStorageService[K]>;
type Result<K extends keyof IStorageService> = ReturnType<IStorageService[K]>;

export class ChangeFeedStorageService implements IStorageService {
	private readonly inner: IStorageService;
	private readonly feed: StorageChangeFeed;
	private readonly skipBuckets: ReadonlySet<string>;

	constructor(inner: IStorageService, feed: StorageChangeFeed, skipBuckets: Iterable<string>) {
		this.inner = inner;
		this.feed = feed;
		this.skipBuckets = new Set(skipBuckets);
	}

	private emit(
		bucket: string,
		key: string,
		op: StorageChangeOp,
		details: {size?: number | null; contentType?: string | null} = {},
	): void {
		if (this.skipBuckets.has(bucket)) return;
		this.feed.record({
			bucket,
			key,
			op,
			size: details.size ?? null,
			etag: null,
			contentType: details.contentType ?? null,
			at: new Date().toISOString(),
		});
	}

	async uploadObject(params: Params<'uploadObject'>[0]): Promise<void> {
		await this.inner.uploadObject(params);
		this.emit(params.bucket, params.key, 'put', {
			size: params.body instanceof Uint8Array ? params.body.byteLength : null,
			contentType: params.contentType,
		});
	}

	async uploadObjectFromFile(params: Params<'uploadObjectFromFile'>[0]): Promise<void> {
		await this.inner.uploadObjectFromFile(params);
		this.emit(params.bucket, params.key, 'put', {size: params.contentLength, contentType: params.contentType});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		await this.inner.deleteObject(bucket, key);
		this.emit(bucket, key, 'delete');
	}

	getObjectMetadata(...args: Params<'getObjectMetadata'>): Result<'getObjectMetadata'> {
		return this.inner.getObjectMetadata(...args);
	}

	computeObjectSha256(...args: Params<'computeObjectSha256'>): Result<'computeObjectSha256'> {
		return this.inner.computeObjectSha256(...args);
	}

	readObject(...args: Params<'readObject'>): Result<'readObject'> {
		return this.inner.readObject(...args);
	}

	streamObject(...args: Params<'streamObject'>): Result<'streamObject'> {
		return this.inner.streamObject(...args);
	}

	writeObjectToDisk(...args: Params<'writeObjectToDisk'>): Result<'writeObjectToDisk'> {
		return this.inner.writeObjectToDisk(...args);
	}

	async copyObject(params: Params<'copyObject'>[0]): Promise<void> {
		await this.inner.copyObject(params);
		this.emit(params.destinationBucket, params.destinationKey, 'put', {contentType: params.newContentType});
	}

	async copyObjectWithMetadataStripping(
		params: Params<'copyObjectWithMetadataStripping'>[0],
	): Promise<ProcessedStorageObjectMetadata | null> {
		const processed = await this.inner.copyObjectWithMetadataStripping(params);
		this.emit(params.destinationBucket, params.destinationKey, 'put', {
			size: processed?.contentLength,
			contentType: processed?.contentType ?? params.contentType,
		});
		return processed;
	}

	async moveObject(params: Params<'moveObject'>[0]): Promise<void> {
		try {
			await this.inner.moveObject(params);
		} catch (error) {
			this.emit(params.destinationBucket, params.destinationKey, 'put', {contentType: params.newContentType});
			throw error;
		}
		this.emit(params.destinationBucket, params.destinationKey, 'put', {contentType: params.newContentType});
		this.emit(params.sourceBucket, params.sourceKey, 'delete');
	}

	getPresignedDownloadURL(...args: Params<'getPresignedDownloadURL'>): Result<'getPresignedDownloadURL'> {
		return this.inner.getPresignedDownloadURL(...args);
	}

	getPresignedUploadURL(...args: Params<'getPresignedUploadURL'>): Result<'getPresignedUploadURL'> {
		return this.inner.getPresignedUploadURL(...args);
	}

	getPresignedUploadPartURL(...args: Params<'getPresignedUploadPartURL'>): Result<'getPresignedUploadPartURL'> {
		return this.inner.getPresignedUploadPartURL(...args);
	}

	async purgeBucket(bucket: string): Promise<void> {
		const objects = await this.inner.listObjects({bucket, prefix: ''});
		await Promise.all(objects.map(({key}) => this.deleteObject(bucket, key)));
	}

	async uploadAvatar(params: Params<'uploadAvatar'>[0]): Promise<void> {
		await this.inner.uploadAvatar(params);
		this.emit(Config.s3.buckets.cdn, `${params.prefix}/${params.key}`, 'put', {size: params.body.byteLength});
	}

	async deleteAvatar(params: Params<'deleteAvatar'>[0]): Promise<void> {
		await this.inner.deleteAvatar(params);
		this.emit(Config.s3.buckets.cdn, `${params.prefix}/${params.key}`, 'delete');
	}

	listObjects(...args: Params<'listObjects'>): Result<'listObjects'> {
		return this.inner.listObjects(...args);
	}

	async deleteObjects(params: Params<'deleteObjects'>[0]): Promise<void> {
		await this.inner.deleteObjects(params);
		for (const object of params.objects) {
			this.emit(params.bucket, object.Key, 'delete');
		}
	}

	createMultipartUpload(...args: Params<'createMultipartUpload'>): Result<'createMultipartUpload'> {
		return this.inner.createMultipartUpload(...args);
	}

	uploadPart(...args: Params<'uploadPart'>): Result<'uploadPart'> {
		return this.inner.uploadPart(...args);
	}

	listParts(...args: Params<'listParts'>): Result<'listParts'> {
		return this.inner.listParts(...args);
	}

	async completeMultipartUpload(params: Params<'completeMultipartUpload'>[0]): Promise<void> {
		await this.inner.completeMultipartUpload(params);
		this.emit(params.bucket, params.key, 'put');
	}

	abortMultipartUpload(...args: Params<'abortMultipartUpload'>): Result<'abortMultipartUpload'> {
		return this.inner.abortMultipartUpload(...args);
	}
}
