// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Readable} from 'node:stream';
import {type IStorageService, isStorageObjectMissing} from '@app/api/infrastructure/IStorageService';
import type {StoreBucketResolver} from '@app/api/infrastructure/StoreBucketMap';
import {Logger} from '@app/api/Logger';

type StreamResult = Awaited<ReturnType<IStorageService['streamObject']>>;

export interface DualStoreCounters {
	fallbackHit(bucket: string, op: string, storeId: string): void;
	doubleMiss(bucket: string, op: string): void;
	storeError(bucket: string, op: string, storeId: string): void;
}

const noopCounters: DualStoreCounters = {
	fallbackHit: () => undefined,
	doubleMiss: () => undefined,
	storeError: () => undefined,
};

interface DualStoreOptions {
	primary: IStorageService;
	primaryStoreId: string;
	fallback: IStorageService;
	fallbackStoreId: string;
	buckets: StoreBucketResolver;
	cdnBucket: string;
	fallbackReadsEnabled: boolean;
	counters?: DualStoreCounters;
}

function compareKeys(a: string, b: string): number {
	if (a < b) return -1;
	if (a > b) return 1;
	return 0;
}

function isUploadMissing(error: unknown): boolean {
	if (!error || typeof error !== 'object') return false;
	const {name, code} = error as {name?: unknown; code?: unknown};
	return name === 'NoSuchUpload' || code === 'NoSuchUpload';
}

export class DualStoreStorageService implements IStorageService {
	private readonly primary: IStorageService;
	private readonly fallback: IStorageService;
	private readonly primaryStoreId: string;
	private readonly fallbackStoreId: string;
	private readonly buckets: StoreBucketResolver;
	private readonly cdnBucket: string;
	private readonly fallbackReadsEnabled: boolean;
	private readonly counters: DualStoreCounters;

	constructor(options: DualStoreOptions) {
		this.primary = options.primary;
		this.fallback = options.fallback;
		this.primaryStoreId = options.primaryStoreId;
		this.fallbackStoreId = options.fallbackStoreId;
		this.buckets = options.buckets;
		this.cdnBucket = options.cdnBucket;
		this.fallbackReadsEnabled = options.fallbackReadsEnabled;
		this.counters = options.counters ?? noopCounters;
	}

	private p(bucket: string): string {
		return this.buckets.primaryBucket(bucket);
	}

	private f(bucket: string): string {
		return this.buckets.fallbackBucket(bucket);
	}

	private async readWithFallback<T>(
		bucket: string,
		key: string,
		op: string,
		primaryCall: () => Promise<T>,
		fallbackCall: () => Promise<T>,
		isMiss: (value: T) => boolean,
	): Promise<T> {
		let primaryError: unknown;
		let primaryMissed = false;
		try {
			const value = await primaryCall();
			if (!isMiss(value)) return value;
			primaryMissed = true;
			if (!this.fallbackReadsEnabled) return value;
		} catch (error) {
			if (!this.fallbackReadsEnabled) throw error;
			if (isStorageObjectMissing(error)) {
				primaryMissed = true;
			} else {
				primaryError = error;
				this.counters.storeError(bucket, op, this.primaryStoreId);
			}
		}
		try {
			const value = await fallbackCall();
			if (isMiss(value)) {
				if (primaryError !== undefined) throw primaryError;
				this.counters.doubleMiss(bucket, op);
				return value;
			}
			this.counters.fallbackHit(bucket, op, this.fallbackStoreId);
			Logger.info({bucket, key, op, store: this.fallbackStoreId}, 'Storage read served by the fallback store');
			return value;
		} catch (error) {
			if (primaryError !== undefined) throw primaryError;
			if (isStorageObjectMissing(error) && primaryMissed) {
				this.counters.doubleMiss(bucket, op);
			} else if (!isStorageObjectMissing(error)) {
				this.counters.storeError(bucket, op, this.fallbackStoreId);
			}
			throw error;
		}
	}

	private throwsOnMiss<T>(): (value: T) => boolean {
		return () => false;
	}

	async uploadObject(params: Parameters<IStorageService['uploadObject']>[0]): Promise<void> {
		await this.primary.uploadObject({...params, bucket: this.p(params.bucket)});
	}

	async uploadObjectFromFile(params: Parameters<IStorageService['uploadObjectFromFile']>[0]): Promise<void> {
		await this.primary.uploadObjectFromFile({...params, bucket: this.p(params.bucket)});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		await this.deleteFromBoth(bucket, key);
	}

	private async deleteFromBoth(bucket: string, key: string): Promise<void> {
		try {
			await this.fallback.deleteObject(this.f(bucket), key);
		} catch (error) {
			if (!isStorageObjectMissing(error)) {
				this.counters.storeError(bucket, 'delete', this.fallbackStoreId);
				Logger.error(
					{err: error, bucket, key, store: this.fallbackStoreId},
					'Fallback store delete failed, the primary copy is kept so the object is not half deleted',
				);
				throw error;
			}
		}
		try {
			await this.primary.deleteObject(this.p(bucket), key);
		} catch (error) {
			if (!isStorageObjectMissing(error)) {
				this.counters.storeError(bucket, 'delete', this.primaryStoreId);
				throw error;
			}
		}
	}

	getObjectMetadata(bucket: string, key: string): ReturnType<IStorageService['getObjectMetadata']> {
		return this.readWithFallback(
			bucket,
			key,
			'head',
			() => this.primary.getObjectMetadata(this.p(bucket), key),
			() => this.fallback.getObjectMetadata(this.f(bucket), key),
			(value) => value === null,
		);
	}

	computeObjectSha256(bucket: string, key: string): Promise<string> {
		return this.readWithFallback(
			bucket,
			key,
			'sha256',
			() => this.primary.computeObjectSha256(this.p(bucket), key),
			() => this.fallback.computeObjectSha256(this.f(bucket), key),
			this.throwsOnMiss<string>(),
		);
	}

	readObject(bucket: string, key: string, maxBytes?: number): Promise<Uint8Array> {
		return this.readWithFallback(
			bucket,
			key,
			'get',
			() => this.primary.readObject(this.p(bucket), key, maxBytes),
			() => this.fallback.readObject(this.f(bucket), key, maxBytes),
			this.throwsOnMiss<Uint8Array>(),
		);
	}

	streamObject(params: {bucket: string; key: string; range?: string}): Promise<StreamResult> {
		return this.readWithFallback(
			params.bucket,
			params.key,
			'stream',
			() => this.primary.streamObject({...params, bucket: this.p(params.bucket)}),
			() => this.fallback.streamObject({...params, bucket: this.f(params.bucket)}),
			(value) => value === null,
		);
	}

	writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		return this.readWithFallback(
			bucket,
			key,
			'download',
			() => this.primary.writeObjectToDisk(this.p(bucket), key, filePath),
			() => this.fallback.writeObjectToDisk(this.f(bucket), key, filePath),
			this.throwsOnMiss<void>(),
		);
	}

	async copyObject(params: Parameters<IStorageService['copyObject']>[0]): Promise<void> {
		const destinationBucket = this.p(params.destinationBucket);
		const onPrimary = await this.primary.getObjectMetadata(this.p(params.sourceBucket), params.sourceKey);
		if (onPrimary !== null || !this.fallbackReadsEnabled) {
			await this.primary.copyObject({...params, sourceBucket: this.p(params.sourceBucket), destinationBucket});
			return;
		}
		const source = await this.fallback.streamObject({bucket: this.f(params.sourceBucket), key: params.sourceKey});
		if (source === null) {
			this.counters.doubleMiss(params.sourceBucket, 'copy');
			await this.primary.copyObject({...params, sourceBucket: this.p(params.sourceBucket), destinationBucket});
			return;
		}
		this.counters.fallbackHit(params.sourceBucket, 'copy', this.fallbackStoreId);
		await this.primary.uploadObject({
			bucket: destinationBucket,
			key: params.destinationKey,
			body: source.body as Readable,
			contentType: params.newContentType ?? source.contentType ?? undefined,
		});
	}

	async getPresignedDownloadURL(params: Parameters<IStorageService['getPresignedDownloadURL']>[0]): Promise<string> {
		if (this.fallbackReadsEnabled) {
			const onPrimary = await this.primary.getObjectMetadata(this.p(params.bucket), params.key);
			if (onPrimary === null) {
				const onFallback = await this.fallback.getObjectMetadata(this.f(params.bucket), params.key);
				if (onFallback !== null) {
					this.counters.fallbackHit(params.bucket, 'presign', this.fallbackStoreId);
					return this.fallback.getPresignedDownloadURL({...params, bucket: this.f(params.bucket)});
				}
			}
		}
		return this.primary.getPresignedDownloadURL({...params, bucket: this.p(params.bucket)});
	}

	getPresignedUploadURL(params: Parameters<IStorageService['getPresignedUploadURL']>[0]): Promise<string> {
		return this.primary.getPresignedUploadURL({...params, bucket: this.p(params.bucket)});
	}

	getPresignedUploadPartURL(params: Parameters<IStorageService['getPresignedUploadPartURL']>[0]): Promise<string> {
		return this.primary.getPresignedUploadPartURL({...params, bucket: this.p(params.bucket)});
	}

	async uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void> {
		await this.primary.uploadObject({
			bucket: this.p(this.cdnBucket),
			key: `${params.prefix}/${params.key}`,
			body: params.body,
		});
	}

	async deleteAvatar(params: {prefix: string; key: string}): Promise<void> {
		await this.deleteFromBoth(this.cdnBucket, `${params.prefix}/${params.key}`);
	}

	async listObjects(params: {
		bucket: string;
		prefix: string;
		maxObjects?: number;
	}): Promise<ReadonlyArray<{key: string; lastModified?: Date}>> {
		const fromPrimary = await this.primary.listObjects({...params, bucket: this.p(params.bucket)});
		if (!this.fallbackReadsEnabled) return fromPrimary;
		const fromFallback = await this.fallback.listObjects({...params, bucket: this.f(params.bucket)});
		const merged = new Map<string, {key: string; lastModified?: Date}>();
		for (const entry of fromFallback) merged.set(entry.key, entry);
		for (const entry of fromPrimary) merged.set(entry.key, entry);
		const all = [...merged.values()].sort((a, b) => compareKeys(a.key, b.key));
		return params.maxObjects === undefined ? all : all.slice(0, params.maxObjects);
	}

	createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{uploadId: string}> {
		return this.primary.createMultipartUpload({...params, bucket: this.p(params.bucket)});
	}

	private async onUploadStore<T>(
		params: {bucket: string},
		call: (store: IStorageService, bucket: string) => Promise<T>,
	): Promise<T> {
		try {
			return await call(this.primary, this.p(params.bucket));
		} catch (error) {
			if (!this.fallbackReadsEnabled || !isUploadMissing(error)) throw error;
			return call(this.fallback, this.f(params.bucket));
		}
	}

	listParts(params: {bucket: string; key: string; uploadId: string}): ReturnType<IStorageService['listParts']> {
		return this.onUploadStore(params, (store, bucket) => store.listParts({...params, bucket}));
	}

	completeMultipartUpload(params: Parameters<IStorageService['completeMultipartUpload']>[0]): Promise<void> {
		return this.onUploadStore(params, (store, bucket) => store.completeMultipartUpload({...params, bucket}));
	}

	abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		return this.onUploadStore(params, (store, bucket) => store.abortMultipartUpload({...params, bucket}));
	}
}
