// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto, {createHash} from 'node:crypto';
import fs from 'node:fs';
import {Readable} from 'node:stream';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {isSupportedMediaContentType} from '@pkgs/mime_utils/src/ContentTypeUtils';
import {vi} from 'vitest';
import {Config} from '../../Config';
import {
	type IStorageService,
	type ProcessedStorageObjectMetadata,
	StorageObjectListingOverflowError,
	StorageObjectRangeNotSatisfiableError,
} from '../../infrastructure/IStorageService';

const OBJECT_ID_SEPARATOR = '\u0000';
const BYTE_RANGE_PATTERN = /^bytes=(\d*)-(\d*)$/u;

interface StoredObject {
	data: Uint8Array;
	contentType?: string;
	etag: string;
	lastModified: Date;
}

type ParsedByteRange = {start: number; end: number} | 'ignored' | 'unsatisfiable';

interface MockStorageServiceConfig {
	fileData?: Uint8Array | null;
	shouldFail?: boolean;
	shouldFailRead?: boolean;
	shouldFailUpload?: boolean;
	shouldFailDelete?: boolean;
	shouldFailCopy?: boolean;
}

function computeEtag(data: Uint8Array): string {
	return `"${createHash('md5').update(data).digest('hex')}"`;
}

function parseByteRange(range: string, totalLength: number): ParsedByteRange {
	const match = BYTE_RANGE_PATTERN.exec(range.trim());
	if (!match) return 'ignored';
	const rawStart = match[1] ?? '';
	const rawEnd = match[2] ?? '';
	if (rawStart === '' && rawEnd === '') return 'ignored';
	if (rawStart === '') {
		const suffixLength = Number(rawEnd);
		if (suffixLength === 0 || totalLength === 0) return 'unsatisfiable';
		return {start: Math.max(totalLength - suffixLength, 0), end: totalLength - 1};
	}
	const start = Number(rawStart);
	if (start >= totalLength) return 'unsatisfiable';
	if (rawEnd === '') return {start, end: totalLength - 1};
	const end = Number(rawEnd);
	if (end < start) return 'ignored';
	return {start, end: Math.min(end, totalLength - 1)};
}

function noSuchKeyError(key: string): S3ServiceException {
	return new S3ServiceException({
		name: 'NoSuchKey',
		$fault: 'client',
		$metadata: {},
		message: `The specified key does not exist: ${key}`,
	});
}

export class MockStorageService implements IStorageService {
	private objects: Map<string, StoredObject> = new Map();
	private multipartUploads: Map<
		string,
		{
			parts: Map<number, Uint8Array>;
			key: string;
			bucket: string;
		}
	> = new Map();
	private deletedObjects: Array<{
		bucket: string;
		key: string;
	}> = [];
	private copiedObjects: Array<{
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
	}> = [];
	readonly uploadObjectSpy = vi.fn();
	readonly uploadObjectFromFileSpy = vi.fn();
	readonly deleteObjectSpy = vi.fn();
	readonly getObjectMetadataSpy = vi.fn();
	readonly readObjectSpy = vi.fn();
	readonly computeObjectSha256Spy = vi.fn();
	readonly streamObjectSpy = vi.fn();
	readonly writeObjectToDiskSpy = vi.fn();
	readonly copyObjectSpy = vi.fn();
	readonly copyObjectWithMetadataStrippingSpy = vi.fn();
	readonly moveObjectSpy = vi.fn();
	readonly getPresignedDownloadURLSpy = vi.fn();
	readonly getPresignedUploadURLSpy = vi.fn();
	readonly getPresignedUploadPartURLSpy = vi.fn();
	readonly listPartsSpy = vi.fn();
	readonly purgeBucketSpy = vi.fn();
	readonly uploadAvatarSpy = vi.fn();
	readonly deleteAvatarSpy = vi.fn();
	readonly listObjectsSpy = vi.fn();
	readonly deleteObjectsSpy = vi.fn();
	readonly createMultipartUploadSpy = vi.fn();
	readonly uploadPartSpy = vi.fn();
	readonly completeMultipartUploadSpy = vi.fn();
	readonly abortMultipartUploadSpy = vi.fn();
	private config: MockStorageServiceConfig;

	constructor(config: MockStorageServiceConfig = {}) {
		this.config = config;
	}

	configure(config: MockStorageServiceConfig): void {
		this.config = {...this.config, ...config};
	}

	private objectId(bucket: string, key: string): string {
		return `${bucket}${OBJECT_ID_SEPARATOR}${key}`;
	}

	private storeObject(bucket: string, key: string, data: Uint8Array, contentType?: string): void {
		this.objects.set(this.objectId(bucket, key), {
			data,
			contentType,
			etag: computeEtag(data),
			lastModified: new Date(),
		});
	}

	async uploadObject(params: {
		bucket: string;
		key: string;
		body: Uint8Array | Readable;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void> {
		this.uploadObjectSpy(params);
		if (this.config.shouldFail || this.config.shouldFailUpload) {
			throw new Error('Mock storage upload failure');
		}
		const data = params.body instanceof Uint8Array ? params.body : await this.readableToBuffer(params.body);
		this.storeObject(params.bucket, params.key, data, params.contentType);
	}

	async uploadObjectFromFile(params: {
		bucket: string;
		key: string;
		filePath: string;
		contentType?: string;
		contentLength?: number;
		expiresAt?: Date;
	}): Promise<void> {
		this.uploadObjectFromFileSpy(params);
		if (this.config.shouldFail || this.config.shouldFailUpload) {
			throw new Error('Mock storage upload failure');
		}
		const data = await fs.promises.readFile(params.filePath);
		this.storeObject(params.bucket, params.key, new Uint8Array(data), params.contentType);
	}

	private async readableToBuffer(stream: Readable): Promise<Uint8Array> {
		const chunks: Array<Uint8Array> = [];
		for await (const chunk of stream) {
			chunks.push(chunk instanceof Uint8Array ? chunk : new Uint8Array(chunk));
		}
		const totalLength = chunks.reduce((sum, c) => sum + c.length, 0);
		const result = new Uint8Array(totalLength);
		let offset = 0;
		for (const chunk of chunks) {
			result.set(chunk, offset);
			offset += chunk.length;
		}
		return result;
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		this.deleteObjectSpy(bucket, key);
		if (this.config.shouldFail || this.config.shouldFailDelete) {
			throw new Error('Mock storage delete failure');
		}
		this.deletedObjects.push({bucket, key});
		this.objects.delete(this.objectId(bucket, key));
	}

	async getObjectMetadata(
		bucket: string,
		key: string,
	): Promise<{
		contentLength: number;
		contentType: string;
		etag?: string;
		lastModified?: Date;
	} | null> {
		this.getObjectMetadataSpy(bucket, key);
		const obj = this.objects.get(this.objectId(bucket, key));
		if (!obj) return null;
		return {
			contentLength: obj.data.length,
			contentType: obj.contentType ?? 'application/octet-stream',
			etag: obj.etag,
			lastModified: obj.lastModified,
		};
	}

	async computeObjectSha256(bucket: string, key: string): Promise<string> {
		this.computeObjectSha256Spy(bucket, key);
		const data = this.config.fileData ?? this.objects.get(this.objectId(bucket, key))?.data ?? new Uint8Array();
		return createHash('sha256').update(data).digest('hex');
	}

	async readObject(bucket: string, key: string, maxBytes?: number): Promise<Uint8Array> {
		this.readObjectSpy(bucket, key, maxBytes);
		if (this.config.shouldFail || this.config.shouldFailRead) {
			throw new Error('Mock storage read failure');
		}
		function assertWithinLimit(data: Uint8Array): Uint8Array {
			if (maxBytes !== undefined && data.length > maxBytes) {
				throw new Error(`Stream exceeds maximum buffer size of ${maxBytes} bytes (got ${data.length} bytes)`);
			}
			return data;
		}
		if (this.config.fileData !== undefined) {
			if (this.config.fileData === null) {
				throw noSuchKeyError(key);
			}
			return assertWithinLimit(this.config.fileData);
		}
		const obj = this.objects.get(this.objectId(bucket, key));
		if (!obj) {
			throw noSuchKeyError(key);
		}
		return assertWithinLimit(obj.data);
	}

	async streamObject(params: {bucket: string; key: string; range?: string}): Promise<{
		body: Readable;
		contentLength: number;
		contentRange?: string | null;
		contentType?: string | null;
		cacheControl?: string | null;
		contentDisposition?: string | null;
		expires?: Date | null;
		etag?: string | null;
		lastModified?: Date | null;
	} | null> {
		this.streamObjectSpy(params);
		if (this.config.shouldFail || this.config.shouldFailRead) {
			throw new Error('Mock storage read failure');
		}
		if (this.config.fileData === null) {
			return null;
		}
		const obj = this.objects.get(this.objectId(params.bucket, params.key));
		const data = this.config.fileData ?? obj?.data;
		if (!data) {
			return null;
		}
		let slice = data;
		let contentRange: string | null = null;
		if (params.range !== undefined) {
			const parsedRange = parseByteRange(params.range, data.length);
			if (parsedRange === 'unsatisfiable') {
				throw new StorageObjectRangeNotSatisfiableError(params.bucket, params.key, params.range);
			}
			if (parsedRange !== 'ignored') {
				slice = data.subarray(parsedRange.start, parsedRange.end + 1);
				contentRange = `bytes ${parsedRange.start}-${parsedRange.end}/${data.length}`;
			}
		}
		return {
			body: Readable.from([Buffer.from(slice)]),
			contentLength: slice.length,
			contentRange,
			contentType: obj?.contentType ?? 'application/octet-stream',
			cacheControl: null,
			contentDisposition: null,
			expires: null,
			etag: computeEtag(data),
			lastModified: obj?.lastModified ?? null,
		};
	}

	async writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		this.writeObjectToDiskSpy(bucket, key, filePath);
		const data = this.config.fileData ?? this.objects.get(this.objectId(bucket, key))?.data;
		if (!data) {
			throw noSuchKeyError(key);
		}
		await fs.promises.writeFile(filePath, data);
	}

	async copyObject(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void> {
		this.copyObjectSpy(params);
		if (this.config.shouldFail || this.config.shouldFailCopy) {
			throw new Error('Mock storage copy failure');
		}
		this.copiedObjects.push({
			sourceBucket: params.sourceBucket,
			sourceKey: params.sourceKey,
			destinationBucket: params.destinationBucket,
			destinationKey: params.destinationKey,
		});
		const sourceObj = this.objects.get(this.objectId(params.sourceBucket, params.sourceKey));
		if (sourceObj) {
			this.storeObject(
				params.destinationBucket,
				params.destinationKey,
				sourceObj.data,
				params.newContentType ?? sourceObj.contentType,
			);
		}
	}

	async copyObjectWithMetadataStripping(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
		sourceLocalPath?: string;
	}): Promise<ProcessedStorageObjectMetadata | null> {
		this.copyObjectWithMetadataStrippingSpy(params);
		await this.copyObject({
			sourceBucket: params.sourceBucket,
			sourceKey: params.sourceKey,
			destinationBucket: params.destinationBucket,
			destinationKey: params.destinationKey,
			newContentType: params.contentType,
		});
		if (!isSupportedMediaContentType(params.contentType)) {
			return null;
		}
		const data =
			this.objects.get(this.objectId(params.destinationBucket, params.destinationKey))?.data ?? new Uint8Array();
		return {
			contentType: params.contentType,
			contentLength: data.length,
			contentHash: createHash('sha256').update(data).digest('hex'),
			width: 100,
			height: 100,
		};
	}

	async moveObject(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void> {
		this.moveObjectSpy(params);
		await this.copyObject(params);
		await this.deleteObject(params.sourceBucket, params.sourceKey);
	}

	async getPresignedDownloadURL(_params: {
		bucket: string;
		key: string;
		expiresIn?: number;
		responseContentType?: string;
		responseContentDisposition?: string;
	}): Promise<string> {
		this.getPresignedDownloadURLSpy(_params);
		return 'https://presigned.url/test';
	}

	async getPresignedUploadURL(_params: {
		bucket: string;
		key: string;
		contentType?: string;
		contentLength?: number;
		expiresIn?: number;
	}): Promise<string> {
		this.getPresignedUploadURLSpy(_params);
		return 'https://presigned-upload.url/test';
	}

	async getPresignedUploadPartURL(params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		expiresIn?: number;
	}): Promise<string> {
		this.getPresignedUploadPartURLSpy(params);
		return `https://presigned-upload.url/test?partNumber=${params.partNumber}&uploadId=${params.uploadId}`;
	}

	async purgeBucket(bucket: string): Promise<void> {
		this.purgeBucketSpy(bucket);
		const prefix = this.objectId(bucket, '');
		for (const id of [...this.objects.keys()]) {
			if (id.startsWith(prefix)) {
				this.objects.delete(id);
				this.deletedObjects.push({bucket, key: id.slice(prefix.length)});
			}
		}
	}

	async uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void> {
		this.uploadAvatarSpy(params);
		await this.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: `${params.prefix}/${params.key}`,
			body: params.body,
		});
	}

	async deleteAvatar(params: {prefix: string; key: string}): Promise<void> {
		this.deleteAvatarSpy(params);
		await this.deleteObject(Config.s3.buckets.cdn, `${params.prefix}/${params.key}`);
	}

	async listObjects(params: {bucket: string; prefix: string; maxObjects?: number}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	> {
		this.listObjectsSpy(params);
		if (params.maxObjects !== undefined && (!Number.isSafeInteger(params.maxObjects) || params.maxObjects <= 0)) {
			throw new RangeError('maxObjects must be a positive safe integer');
		}
		const keyOffset = params.bucket.length + OBJECT_ID_SEPARATOR.length;
		const matches = [...this.objects.entries()]
			.filter(([id]) => id.startsWith(this.objectId(params.bucket, params.prefix)))
			.map(([id, object]) => ({key: id.slice(keyOffset), lastModified: object.lastModified}))
			.sort((a, b) => (a.key < b.key ? -1 : a.key > b.key ? 1 : 0));
		if (params.maxObjects !== undefined && matches.length > params.maxObjects) {
			throw new StorageObjectListingOverflowError(params.bucket, params.prefix, params.maxObjects);
		}
		return matches;
	}

	async deleteObjects(params: {
		bucket: string;
		objects: ReadonlyArray<{
			Key: string;
		}>;
	}): Promise<void> {
		this.deleteObjectsSpy(params);
		for (const object of params.objects) {
			this.objects.delete(this.objectId(params.bucket, object.Key));
			this.deletedObjects.push({bucket: params.bucket, key: object.Key});
		}
	}

	async createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{
		uploadId: string;
	}> {
		this.createMultipartUploadSpy(params);
		const uploadId = crypto.randomUUID();
		this.multipartUploads.set(uploadId, {parts: new Map(), key: params.key, bucket: params.bucket});
		return {uploadId};
	}

	async uploadPart(params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		body: Uint8Array;
	}): Promise<{
		etag: string;
	}> {
		this.uploadPartSpy(params);
		const upload = this.multipartUploads.get(params.uploadId);
		if (!upload) {
			throw new Error(`Mock: multipart upload ${params.uploadId} not found`);
		}
		upload.parts.set(params.partNumber, params.body);
		const etag = `"etag-${params.partNumber}"`;
		return {etag};
	}

	async listParts(params: {bucket: string; key: string; uploadId: string}): Promise<
		Array<{
			partNumber: number;
			etag: string;
			size?: number;
		}>
	> {
		this.listPartsSpy(params);
		const upload = this.multipartUploads.get(params.uploadId);
		if (!upload) return [];
		return [...upload.parts.entries()]
			.sort(([a], [b]) => a - b)
			.map(([partNumber, data]) => ({
				partNumber,
				etag: `"etag-${partNumber}"`,
				size: data.length,
			}));
	}

	async completeMultipartUpload(params: {
		bucket: string;
		key: string;
		uploadId: string;
		parts: Array<{
			partNumber: number;
			etag: string;
		}>;
	}): Promise<void> {
		this.completeMultipartUploadSpy(params);
		const upload = this.multipartUploads.get(params.uploadId);
		if (!upload) {
			throw new Error(`Mock: multipart upload ${params.uploadId} not found`);
		}
		const sortedParts = [...upload.parts.entries()].sort(([a], [b]) => a - b);
		const totalSize = sortedParts.reduce((sum, [, data]) => sum + data.length, 0);
		const combined = new Uint8Array(totalSize);
		let offset = 0;
		for (const [, data] of sortedParts) {
			combined.set(data, offset);
			offset += data.length;
		}
		this.storeObject(upload.bucket, upload.key, combined);
		this.multipartUploads.delete(params.uploadId);
	}

	async abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		this.abortMultipartUploadSpy(params);
		this.multipartUploads.delete(params.uploadId);
	}

	getDeletedObjects(): Array<{
		bucket: string;
		key: string;
	}> {
		return [...this.deletedObjects];
	}

	getCopiedObjects(): Array<{
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
	}> {
		return [...this.copiedObjects];
	}

	hasObject(bucket: string, key: string): boolean {
		return this.objects.has(this.objectId(bucket, key));
	}

	reset(): void {
		this.objects.clear();
		this.multipartUploads.clear();
		this.deletedObjects = [];
		this.copiedObjects = [];
		this.config = {};
		this.uploadObjectSpy.mockClear();
		this.uploadObjectFromFileSpy.mockClear();
		this.deleteObjectSpy.mockClear();
		this.getObjectMetadataSpy.mockClear();
		this.readObjectSpy.mockClear();
		this.computeObjectSha256Spy.mockClear();
		this.streamObjectSpy.mockClear();
		this.writeObjectToDiskSpy.mockClear();
		this.copyObjectSpy.mockClear();
		this.copyObjectWithMetadataStrippingSpy.mockClear();
		this.moveObjectSpy.mockClear();
		this.getPresignedDownloadURLSpy.mockClear();
		this.getPresignedUploadURLSpy.mockClear();
		this.getPresignedUploadPartURLSpy.mockClear();
		this.listPartsSpy.mockClear();
		this.purgeBucketSpy.mockClear();
		this.uploadAvatarSpy.mockClear();
		this.deleteAvatarSpy.mockClear();
		this.listObjectsSpy.mockClear();
		this.deleteObjectsSpy.mockClear();
		this.createMultipartUploadSpy.mockClear();
		this.uploadPartSpy.mockClear();
		this.completeMultipartUploadSpy.mockClear();
		this.abortMultipartUploadSpy.mockClear();
	}
}
