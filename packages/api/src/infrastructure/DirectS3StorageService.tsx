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

import fs from 'node:fs';
import path from 'node:path';
import {PassThrough, pipeline, type Readable} from 'node:stream';
import {promisify} from 'node:util';
import {Config} from '@fluxer/api/src/Config';
import {DirectS3ExpirationManager} from '@fluxer/api/src/infrastructure/DirectS3ExpirationManager';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import {processAndUploadJpeg, streamToBuffer} from '@fluxer/api/src/infrastructure/StorageObjectHelpers';
import {Logger} from '@fluxer/api/src/Logger';
import {S3Errors} from '@fluxer/s3/src/errors/S3Error';
import {generatePresignedUrl} from '@fluxer/s3/src/s3/PresignedUrlGenerator';
import type {S3Service} from '@fluxer/s3/src/s3/S3Service';
import {seconds} from 'itty-time';

const pipelinePromise = promisify(pipeline);

const expirationManagers = new WeakMap<S3Service, DirectS3ExpirationManager>();

function getExpirationManager(s3Service: S3Service): DirectS3ExpirationManager {
	const existing = expirationManagers.get(s3Service);
	if (existing) {
		return existing;
	}
	const manager = new DirectS3ExpirationManager(s3Service);
	expirationManagers.set(s3Service, manager);
	return manager;
}

export class DirectS3StorageService implements IStorageService {
	private readonly s3Service: S3Service;
	private readonly presignedUrlBase: string;
	private readonly expirationManager: DirectS3ExpirationManager;

	constructor(s3Service: S3Service) {
		this.s3Service = s3Service;
		this.presignedUrlBase = Config.s3.presignedUrlBase ?? Config.s3.endpoint;
		this.expirationManager = getExpirationManager(s3Service);
	}

	async uploadObject({
		bucket,
		key,
		body,
		contentType,
		expiresAt,
	}: {
		bucket: string;
		key: string;
		body: Uint8Array;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void> {
		const buffer = Buffer.from(body);
		if (expiresAt) {
			Logger.debug({bucket, key, expiresAt}, 'Uploading S3 object with expiration');
		}
		await this.s3Service.putObject(bucket, key, buffer, {contentType});
		if (expiresAt) {
			try {
				await this.expirationManager.trackExpiration({bucket, key, expiresAt});
			} catch (error) {
				await this.s3Service.deleteObject(bucket, key);
				throw error;
			}
		} else {
			this.expirationManager.clearExpiration(bucket, key);
		}
	}

	async getPresignedDownloadURL({
		bucket,
		key,
		expiresIn = seconds('5 minutes'),
	}: {
		bucket: string;
		key: string;
		expiresIn?: number;
	}): Promise<string> {
		const expired = await this.expirationManager.expireIfNeeded(bucket, key);
		if (expired) {
			throw S3Errors.noSuchKey(key);
		}
		const {accessKeyId, secretAccessKey} = Config.s3;

		if (!accessKeyId || !secretAccessKey) {
			throw new Error('S3 credentials not configured');
		}

		return generatePresignedUrl({
			method: 'GET',
			bucket,
			key,
			expiresIn: Math.floor(expiresIn),
			accessKey: accessKeyId,
			secretKey: secretAccessKey,
			endpoint: this.presignedUrlBase,
			region: Config.s3.region,
		});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		await this.s3Service.deleteObject(bucket, key);
		this.expirationManager.clearExpiration(bucket, key);
	}

	async getObjectMetadata(bucket: string, key: string): Promise<{contentLength: number; contentType: string} | null> {
		const expired = await this.expirationManager.expireIfNeeded(bucket, key);
		if (expired) {
			return null;
		}
		try {
			const metadata = await this.s3Service.headObject(bucket, key);
			return {
				contentLength: metadata.size,
				contentType: metadata.contentType ?? 'application/octet-stream',
			};
		} catch (error) {
			const errorCode =
				error && typeof error === 'object' && 'code' in error ? (error as {code: string}).code : undefined;
			if (errorCode === 'NoSuchKey' || errorCode === 'NotFound') {
				return null;
			}
			throw error;
		}
	}

	async readObject(bucket: string, key: string): Promise<Uint8Array> {
		const expired = await this.expirationManager.expireIfNeeded(bucket, key);
		if (expired) {
			throw S3Errors.noSuchKey(key);
		}
		const {stream} = await this.s3Service.getObject(bucket, key);
		const passThrough = new PassThrough();
		stream.pipe(passThrough);
		return streamToBuffer(passThrough);
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
		const expired = await this.expirationManager.expireIfNeeded(params.bucket, params.key);
		if (expired) {
			return null;
		}
		const rangeOption = params.range
			? (() => {
					const [start, end] = params.range!.split('=')[1].split('-').map(Number);
					return {start, end};
				})()
			: undefined;

		const result = await this.s3Service.getObject(
			params.bucket,
			params.key,
			rangeOption ? {range: rangeOption} : undefined,
		);

		return {
			body: result.stream as Readable,
			contentLength: result.metadata.size,
			contentRange: result.contentRange ?? null,
			contentType: result.metadata.contentType ?? null,
			cacheControl: null,
			contentDisposition: null,
			expires: null,
			etag: result.metadata.etag,
			lastModified: result.metadata.lastModified,
		};
	}

	private async ensureDirectoryExists(dirPath: string): Promise<void> {
		await fs.promises.mkdir(dirPath, {recursive: true});
	}

	async writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		const expired = await this.expirationManager.expireIfNeeded(bucket, key);
		if (expired) {
			throw S3Errors.noSuchKey(key);
		}
		await this.ensureDirectoryExists(path.dirname(filePath));
		const {stream} = await this.s3Service.getObject(bucket, key);
		const writeStream = fs.createWriteStream(filePath);
		try {
			await pipelinePromise(stream, writeStream);
		} catch (error) {
			writeStream.destroy();
			throw error;
		}
	}

	async copyObject({
		sourceBucket,
		sourceKey,
		destinationBucket,
		destinationKey,
		newContentType,
	}: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void> {
		const expired = await this.expirationManager.expireIfNeeded(sourceBucket, sourceKey);
		if (expired) {
			throw S3Errors.noSuchKey(sourceKey);
		}
		await this.s3Service.copyObject(
			sourceBucket,
			sourceKey,
			destinationBucket,
			destinationKey,
			newContentType
				? {
						metadataDirective: 'REPLACE',
						contentType: newContentType,
					}
				: undefined,
		);
		const sourceExpiresAt = this.expirationManager.getExpiration(sourceBucket, sourceKey);
		if (sourceExpiresAt) {
			try {
				await this.expirationManager.trackExpiration({
					bucket: destinationBucket,
					key: destinationKey,
					expiresAt: sourceExpiresAt,
				});
			} catch (error) {
				await this.s3Service.deleteObject(destinationBucket, destinationKey);
				throw error;
			}
		} else {
			this.expirationManager.clearExpiration(destinationBucket, destinationKey);
		}
	}

	async copyObjectWithJpegProcessing({
		sourceBucket,
		sourceKey,
		destinationBucket,
		destinationKey,
		contentType,
	}: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
	}): Promise<{width: number; height: number} | null> {
		const isJpeg = contentType.toLowerCase().includes('jpeg') || contentType.toLowerCase().includes('jpg');
		const sourceExpiresAt = this.expirationManager.getExpiration(sourceBucket, sourceKey);

		if (!isJpeg) {
			await this.copyObject({
				sourceBucket,
				sourceKey,
				destinationBucket,
				destinationKey,
				newContentType: contentType,
			});
			return null;
		}

		try {
			const sourceData = await this.readObject(sourceBucket, sourceKey);
			const result = await processAndUploadJpeg({
				sourceData,
				contentType,
				destination: {bucket: destinationBucket, key: destinationKey},
				uploadObject: async (params) => this.uploadObject(params),
			});
			if (sourceExpiresAt) {
				try {
					await this.expirationManager.trackExpiration({
						bucket: destinationBucket,
						key: destinationKey,
						expiresAt: sourceExpiresAt,
					});
				} catch (error) {
					await this.s3Service.deleteObject(destinationBucket, destinationKey);
					throw error;
				}
			} else {
				this.expirationManager.clearExpiration(destinationBucket, destinationKey);
			}
			return result;
		} catch (error) {
			Logger.error({error}, 'Failed to process JPEG, falling back to simple copy');
			await this.copyObject({
				sourceBucket,
				sourceKey,
				destinationBucket,
				destinationKey,
				newContentType: contentType,
			});
			return null;
		}
	}

	async moveObject({
		sourceBucket,
		sourceKey,
		destinationBucket,
		destinationKey,
		newContentType,
	}: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void> {
		await this.copyObject({
			sourceBucket,
			sourceKey,
			destinationBucket,
			destinationKey,
			newContentType,
		});
		await this.deleteObject(sourceBucket, sourceKey);
	}

	async purgeBucket(bucket: string): Promise<void> {
		const result = await this.s3Service.listObjects(bucket);
		const keys = result.contents.map((obj) => obj.key);
		if (keys.length > 0) {
			const deleteResult = await this.s3Service.deleteObjects(bucket, keys);
			for (const key of deleteResult.deleted) {
				this.expirationManager.clearExpiration(bucket, key);
			}
		}
		this.expirationManager.clearBucket(bucket);
		Logger.debug({bucket}, 'Purged bucket');
	}

	async uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void> {
		const {prefix, key, body} = params;
		await this.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: `${prefix}/${key}`,
			body,
		});
	}

	async deleteAvatar(params: {prefix: string; key: string}): Promise<void> {
		const {prefix, key} = params;
		await this.deleteObject(Config.s3.buckets.cdn, `${prefix}/${key}`);
	}

	async listObjects(params: {bucket: string; prefix: string}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	> {
		const result = await this.s3Service.listObjects(params.bucket, {prefix: params.prefix});
		return result.contents.map((obj) => ({
			key: obj.key,
			lastModified: obj.lastModified,
		}));
	}

	async deleteObjects(params: {bucket: string; objects: ReadonlyArray<{Key: string}>}): Promise<void> {
		const keys = params.objects.map((obj) => obj.Key);
		if (keys.length === 0) {
			return;
		}
		const result = await this.s3Service.deleteObjects(params.bucket, keys);
		for (const key of result.deleted) {
			this.expirationManager.clearExpiration(params.bucket, key);
		}
	}

	async createMultipartUpload(params: {
		bucket: string;
		key: string;
		contentType?: string;
	}): Promise<{uploadId: string}> {
		const result = await this.s3Service.createMultipartUpload(params.bucket, params.key, {
			contentType: params.contentType,
		});
		return {uploadId: result.uploadId};
	}

	async uploadPart(params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		body: Uint8Array;
	}): Promise<{etag: string}> {
		const result = await this.s3Service.uploadPart(
			params.bucket,
			params.key,
			params.uploadId,
			params.partNumber,
			Buffer.from(params.body),
		);
		return {etag: result.etag};
	}

	async completeMultipartUpload(params: {
		bucket: string;
		key: string;
		uploadId: string;
		parts: Array<{partNumber: number; etag: string}>;
	}): Promise<void> {
		await this.s3Service.completeMultipartUpload(params.bucket, params.key, params.uploadId, params.parts);
	}

	async abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		await this.s3Service.abortMultipartUpload(params.bucket, params.key, params.uploadId);
	}
}
