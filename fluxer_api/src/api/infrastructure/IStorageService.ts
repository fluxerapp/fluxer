// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Readable} from 'node:stream';

export interface ProcessedStorageObjectMetadata {
	contentType: string;
	contentLength: number;
	contentHash: string;
	width?: number;
	height?: number;
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
		cacheControl?: string | null;
		contentDisposition?: string | null;
		expires?: Date | null;
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
	copyObjectWithMetadataStripping(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
		sourceLocalPath?: string;
	}): Promise<ProcessedStorageObjectMetadata | null>;
	moveObject(params: {
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
		expiresIn?: number;
	}): Promise<string>;
	purgeBucket(bucket: string): Promise<void>;
	uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void>;
	deleteAvatar(params: {prefix: string; key: string}): Promise<void>;
	listObjects(params: {bucket: string; prefix: string}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	>;
	deleteObjects(params: {
		bucket: string;
		objects: ReadonlyArray<{
			Key: string;
		}>;
	}): Promise<void>;
	createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{
		uploadId: string;
	}>;
	uploadPart(params: {bucket: string; key: string; uploadId: string; partNumber: number; body: Uint8Array}): Promise<{
		etag: string;
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
