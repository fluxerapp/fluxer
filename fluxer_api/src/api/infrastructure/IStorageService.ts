// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Readable} from 'node:stream';
import {S3ServiceException} from '@aws-sdk/client-s3';

export class StorageObjectRangeNotSatisfiableError extends Error {
	readonly bucket: string;
	readonly key: string;
	readonly range: string;

	constructor(bucket: string, key: string, range: string) {
		super(`Requested range ${range} is not satisfiable for ${bucket}/${key}`);
		this.name = 'StorageObjectRangeNotSatisfiableError';
		this.bucket = bucket;
		this.key = key;
		this.range = range;
	}
}

export class StorageObjectListingOverflowError extends Error {
	readonly bucket: string;
	readonly prefix: string;
	readonly maxObjects: number;

	constructor(bucket: string, prefix: string, maxObjects: number) {
		super(`Object listing exceeds maximum of ${maxObjects} objects for ${bucket}/${prefix}`);
		this.name = 'StorageObjectListingOverflowError';
		this.bucket = bucket;
		this.prefix = prefix;
		this.maxObjects = maxObjects;
	}
}

export class StorageObjectNotFoundError extends Error {
	readonly code = 'NoSuchKey';
	readonly bucket: string;
	readonly key: string;

	constructor(bucket: string, key: string) {
		super(`No object stored at ${bucket}/${key}`);
		this.name = 'NoSuchKey';
		this.bucket = bucket;
		this.key = key;
	}
}

export class StorageUploadNotFoundError extends Error {
	readonly code = 'NoSuchUpload';
	readonly uploadId: string;

	constructor(uploadId: string) {
		super(`No multipart upload ${uploadId}`);
		this.name = 'NoSuchUpload';
		this.uploadId = uploadId;
	}
}

export class StorageReadOnlyError extends Error {
	readonly code = 'StorageReadOnly';

	constructor(operation: string) {
		super(`Storage is read-only while the storage migration is checked, ${operation} was refused`);
		this.name = 'StorageReadOnly';
	}
}

export class StorageFullError extends Error {
	readonly code = 'ENOSPC';

	constructor(operation: string, detail: string, options?: {cause?: unknown}) {
		super(`The ${operation} was refused because storage is almost full. ${detail}.`, options);
		this.name = 'StorageFull';
	}
}

const MISSING_OBJECT_NAMES = new Set(['NoSuchKey', 'NotFound']);
const RECOVERABLE_S3_ERROR_NAMES = new Set([
	'InternalError',
	'OperationAborted',
	'RequestTimeout',
	'ServiceUnavailable',
	'SlowDown',
]);
const RECOVERABLE_ERROR_CODES = new Set([
	'EAI_AGAIN',
	'ECONNREFUSED',
	'ECONNRESET',
	'ENOTFOUND',
	'ETIMEDOUT',
	'EIO',
	'EMFILE',
	'ENFILE',
	'ENOSPC',
	'EAGAIN',
	'EBUSY',
	'EDQUOT',
	'StorageReadOnly',
]);

function errorField(error: unknown, field: 'name' | 'code'): string | undefined {
	if (!error || typeof error !== 'object') return undefined;
	const value = (error as Record<string, unknown>)[field];
	return typeof value === 'string' ? value : undefined;
}

export function isStorageObjectMissing(error: unknown): boolean {
	return error instanceof Error && MISSING_OBJECT_NAMES.has(error.name);
}

export function isStorageErrorRecoverable(error: unknown): boolean {
	if (error instanceof S3ServiceException) {
		return error.$fault === 'server' || RECOVERABLE_S3_ERROR_NAMES.has(error.name);
	}
	const code = errorField(error, 'code');
	if (code && RECOVERABLE_ERROR_CODES.has(code)) return true;
	return errorField(error, 'name') === 'TimeoutError';
}

export interface IStorageService {
	uploadObject(params: {
		bucket: string;
		key: string;
		body: Uint8Array | Readable;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void>;
	uploadObjectFromFile(params: {
		bucket: string;
		key: string;
		filePath: string;
		contentType?: string;
		contentLength?: number;
		expiresAt?: Date;
	}): Promise<void>;
	deleteObject(bucket: string, key: string): Promise<void>;
	getObjectMetadata(
		bucket: string,
		key: string,
	): Promise<{
		contentLength: number;
		contentType: string;
		etag?: string;
		lastModified?: Date;
	} | null>;
	computeObjectSha256(bucket: string, key: string): Promise<string>;
	readObject(bucket: string, key: string, maxBytes?: number): Promise<Uint8Array>;
	streamObject(params: {bucket: string; key: string; range?: string}): Promise<{
		body: Readable;
		contentLength: number;
		contentRange?: string | null;
		contentType?: string | null;
		etag?: string | null;
		lastModified?: Date | null;
	} | null>;
	writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void>;
	copyObject(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void>;
	getPresignedDownloadURL(params: {
		bucket: string;
		key: string;
		expiresIn?: number;
		responseContentType?: string;
		responseContentDisposition?: string;
	}): Promise<string>;
	getPresignedUploadURL(params: {
		bucket: string;
		key: string;
		contentType?: string;
		contentLength?: number;
		expiresIn?: number;
	}): Promise<string>;
	getPresignedUploadPartURL(params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		contentLength?: number;
		expiresIn?: number;
	}): Promise<string>;
	uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void>;
	deleteAvatar(params: {prefix: string; key: string}): Promise<void>;
	listObjects(params: {bucket: string; prefix: string; maxObjects?: number}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	>;
	createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{
		uploadId: string;
	}>;
	listParts(params: {bucket: string; key: string; uploadId: string}): Promise<
		Array<{
			partNumber: number;
			etag: string;
			size?: number;
		}>
	>;
	completeMultipartUpload(params: {
		bucket: string;
		key: string;
		uploadId: string;
		parts: Array<{
			partNumber: number;
			etag: string;
		}>;
	}): Promise<void>;
	abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void>;
}
