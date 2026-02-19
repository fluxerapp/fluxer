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

import crypto from 'node:crypto';
import type {Readable} from 'node:stream';
import {S3ServiceException} from '@aws-sdk/client-s3';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import {vi} from 'vitest';

export interface MockStorageServiceConfig {
	fileData?: Uint8Array | null;
	shouldFail?: boolean;
	shouldFailRead?: boolean;
	shouldFailUpload?: boolean;
	shouldFailDelete?: boolean;
	shouldFailCopy?: boolean;
}

export class MockStorageService implements IStorageService {
	private objects: Map<string, {data: Uint8Array; contentType?: string}> = new Map();
	private multipartUploads: Map<string, {parts: Map<number, Uint8Array>; key: string; bucket: string}> = new Map();
	private deletedObjects: Array<{bucket: string; key: string}> = [];
	private copiedObjects: Array<{
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
	}> = [];

	readonly uploadObjectSpy = vi.fn();
	readonly deleteObjectSpy = vi.fn();
	readonly getObjectMetadataSpy = vi.fn();
	readonly readObjectSpy = vi.fn();
	readonly streamObjectSpy = vi.fn();
	readonly writeObjectToDiskSpy = vi.fn();
	readonly copyObjectSpy = vi.fn();
	readonly copyObjectWithJpegProcessingSpy = vi.fn();
	readonly moveObjectSpy = vi.fn();
	readonly getPresignedDownloadURLSpy = vi.fn();
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

	async uploadObject(params: {
		bucket: string;
		key: string;
		body: Uint8Array;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void> {
		this.uploadObjectSpy(params);
		if (this.config.shouldFail || this.config.shouldFailUpload) {
			throw new Error('Mock storage upload failure');
		}
		this.objects.set(params.key, {data: params.body, contentType: params.contentType});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		this.deleteObjectSpy(bucket, key);
		if (this.config.shouldFail || this.config.shouldFailDelete) {
			throw new Error('Mock storage delete failure');
		}
		this.deletedObjects.push({bucket, key});
		this.objects.delete(key);
	}

	async getObjectMetadata(bucket: string, key: string): Promise<{contentLength: number; contentType: string} | null> {
		this.getObjectMetadataSpy(bucket, key);
		const obj = this.objects.get(key);
		if (!obj) return null;
		return {contentLength: obj.data.length, contentType: obj.contentType ?? 'application/octet-stream'};
	}

	async readObject(bucket: string, key: string): Promise<Uint8Array> {
		this.readObjectSpy(bucket, key);
		if (this.config.shouldFail || this.config.shouldFailRead) {
			throw new Error('Mock storage read failure');
		}
		if (this.config.fileData !== undefined) {
			if (this.config.fileData === null) {
				const error = new S3ServiceException({
					name: 'NoSuchKey',
					$fault: 'client',
					$metadata: {},
					message: `The specified key does not exist: ${key}`,
				});
				throw error;
			}
			return this.config.fileData;
		}
		const obj = this.objects.get(key);
		if (!obj) {
			const error = new S3ServiceException({
				name: 'NoSuchKey',
				$fault: 'client',
				$metadata: {},
				message: `The specified key does not exist: ${key}`,
			});
			throw error;
		}
		return obj.data;
	}

	async streamObject(_params: {bucket: string; key: string; range?: string}): Promise<{
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
		this.streamObjectSpy(_params);
		return null;
	}

	async writeObjectToDisk(_bucket: string, _key: string, _filePath: string): Promise<void> {
		this.writeObjectToDiskSpy(_bucket, _key, _filePath);
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
		const sourceObj = this.objects.get(params.sourceKey);
		if (sourceObj) {
			this.objects.set(params.destinationKey, {
				data: sourceObj.data,
				contentType: params.newContentType ?? sourceObj.contentType,
			});
		}
	}

	async copyObjectWithJpegProcessing(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
	}): Promise<{width: number; height: number} | null> {
		this.copyObjectWithJpegProcessingSpy(params);
		await this.copyObject(params);
		return {width: 100, height: 100};
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

	async getPresignedDownloadURL(_params: {bucket: string; key: string; expiresIn?: number}): Promise<string> {
		this.getPresignedDownloadURLSpy(_params);
		return 'https://presigned.url/test';
	}

	async purgeBucket(_bucket: string): Promise<void> {
		this.purgeBucketSpy(_bucket);
	}

	async uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void> {
		this.uploadAvatarSpy(params);
		await this.uploadObject({bucket: 'cdn', key: `${params.prefix}/${params.key}`, body: params.body});
	}

	async deleteAvatar(params: {prefix: string; key: string}): Promise<void> {
		this.deleteAvatarSpy(params);
		await this.deleteObject('cdn', `${params.prefix}/${params.key}`);
	}

	async listObjects(_params: {
		bucket: string;
		prefix: string;
	}): Promise<ReadonlyArray<{key: string; lastModified?: Date}>> {
		this.listObjectsSpy(_params);
		return [];
	}

	async deleteObjects(_params: {bucket: string; objects: ReadonlyArray<{Key: string}>}): Promise<void> {
		this.deleteObjectsSpy(_params);
	}

	async createMultipartUpload(params: {
		bucket: string;
		key: string;
		contentType?: string;
	}): Promise<{uploadId: string}> {
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
	}): Promise<{etag: string}> {
		this.uploadPartSpy(params);
		const upload = this.multipartUploads.get(params.uploadId);
		if (!upload) {
			throw new Error(`Mock: multipart upload ${params.uploadId} not found`);
		}
		upload.parts.set(params.partNumber, params.body);
		const etag = `"etag-${params.partNumber}"`;
		return {etag};
	}

	async completeMultipartUpload(params: {
		bucket: string;
		key: string;
		uploadId: string;
		parts: Array<{partNumber: number; etag: string}>;
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
		this.objects.set(upload.key, {data: combined});
		this.multipartUploads.delete(params.uploadId);
	}

	async abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		this.abortMultipartUploadSpy(params);
		this.multipartUploads.delete(params.uploadId);
	}

	getDeletedObjects(): Array<{bucket: string; key: string}> {
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

	hasObject(_bucket: string, key: string): boolean {
		return this.objects.has(key);
	}

	reset(): void {
		this.objects.clear();
		this.multipartUploads.clear();
		this.deletedObjects = [];
		this.copiedObjects = [];
		this.config = {};
		this.uploadObjectSpy.mockClear();
		this.deleteObjectSpy.mockClear();
		this.getObjectMetadataSpy.mockClear();
		this.readObjectSpy.mockClear();
		this.streamObjectSpy.mockClear();
		this.writeObjectToDiskSpy.mockClear();
		this.copyObjectSpy.mockClear();
		this.copyObjectWithJpegProcessingSpy.mockClear();
		this.moveObjectSpy.mockClear();
		this.getPresignedDownloadURLSpy.mockClear();
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
