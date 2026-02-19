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

import type {Readable} from 'node:stream';

export interface IStorageService {
	uploadObject(params: {
		bucket: string;
		key: string;
		body: Uint8Array;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void>;

	deleteObject(bucket: string, key: string): Promise<void>;

	getObjectMetadata(bucket: string, key: string): Promise<{contentLength: number; contentType: string} | null>;

	readObject(bucket: string, key: string): Promise<Uint8Array>;

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

	copyObjectWithJpegProcessing(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
	}): Promise<{width: number; height: number} | null>;

	moveObject(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void>;

	getPresignedDownloadURL(params: {bucket: string; key: string; expiresIn?: number}): Promise<string>;

	purgeBucket(bucket: string): Promise<void>;

	uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void>;

	deleteAvatar(params: {prefix: string; key: string}): Promise<void>;

	listObjects(params: {bucket: string; prefix: string}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	>;

	deleteObjects(params: {bucket: string; objects: ReadonlyArray<{Key: string}>}): Promise<void>;

	createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{uploadId: string}>;

	uploadPart(params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		body: Uint8Array;
	}): Promise<{etag: string}>;

	completeMultipartUpload(params: {
		bucket: string;
		key: string;
		uploadId: string;
		parts: Array<{partNumber: number; etag: string}>;
	}): Promise<void>;

	abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void>;
}
