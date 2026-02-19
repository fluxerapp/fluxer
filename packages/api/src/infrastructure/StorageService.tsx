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

import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import {PassThrough, pipeline, Readable} from 'node:stream';
import {promisify} from 'node:util';
import {
	AbortMultipartUploadCommand,
	CompleteMultipartUploadCommand,
	CopyObjectCommand,
	CreateMultipartUploadCommand,
	DeleteObjectCommand,
	DeleteObjectsCommand,
	GetObjectCommand,
	type GetObjectCommandOutput,
	HeadObjectCommand,
	type HeadObjectCommandOutput,
	ListObjectsV2Command,
	PutObjectCommand,
	S3Client,
	S3ServiceException,
	UploadPartCommand,
} from '@aws-sdk/client-s3';
import {getSignedUrl} from '@aws-sdk/s3-request-presigner';
import {Config} from '@fluxer/api/src/Config';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import {processAndUploadJpeg, streamToBuffer} from '@fluxer/api/src/infrastructure/StorageObjectHelpers';
import {Logger} from '@fluxer/api/src/Logger';
import {seconds} from 'itty-time';

const pipelinePromise = promisify(pipeline);

export class StorageService implements IStorageService {
	private readonly s3: S3Client;
	private readonly presignClient: S3Client;

	constructor() {
		const baseInit = {
			endpoint: Config.s3.endpoint,
			region: Config.s3.region,
			credentials: {
				accessKeyId: Config.s3.accessKeyId,
				secretAccessKey: Config.s3.secretAccessKey,
			},
			requestChecksumCalculation: 'WHEN_REQUIRED',
			responseChecksumValidation: 'WHEN_REQUIRED',
		} as const;

		this.s3 = new S3Client({...baseInit, forcePathStyle: true});
		this.presignClient = new S3Client({...baseInit, forcePathStyle: false});
	}

	private getClient(_bucket: string): S3Client {
		return this.s3;
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
		const command = new PutObjectCommand({
			Bucket: bucket,
			Key: key,
			Body: body,
			ContentType: contentType,
			Expires: expiresAt,
		});
		await this.getClient(bucket).send(command);
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
		const command = new GetObjectCommand({
			Bucket: bucket,
			Key: key,
		});
		return getSignedUrl(this.presignClient, command, {expiresIn});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		const command = new DeleteObjectCommand({Bucket: bucket, Key: key});
		await this.getClient(bucket).send(command);
	}

	async getObjectMetadata(bucket: string, key: string): Promise<{contentLength: number; contentType: string} | null> {
		try {
			const command = new HeadObjectCommand({Bucket: bucket, Key: key});
			const response = await this.getClient(bucket).send(command);
			return {
				contentLength: response.ContentLength ?? 0,
				contentType: response.ContentType ?? '',
			};
		} catch (error) {
			if (error instanceof S3ServiceException && error.name === 'NotFound') {
				return null;
			}
			throw error;
		}
	}

	async readObject(bucket: string, key: string): Promise<Uint8Array> {
		const command = new GetObjectCommand({Bucket: bucket, Key: key});
		const {Body} = await this.getClient(bucket).send(command);
		assert(Body != null && Body instanceof Readable);
		const stream = Body instanceof PassThrough ? Body : Body.pipe(new PassThrough());
		return streamToBuffer(stream);
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
		const command = new GetObjectCommand({
			Bucket: params.bucket,
			Key: params.key,
			Range: params.range,
		});
		const response = await this.getClient(params.bucket).send(command);
		assert(response.Body != null && response.Body instanceof Readable);
		const stream = response.Body instanceof PassThrough ? response.Body : response.Body.pipe(new PassThrough());
		return {
			body: stream,
			contentLength: response.ContentLength ?? 0,
			contentRange: response.ContentRange ?? null,
			contentType: response.ContentType ?? null,
			cacheControl: response.CacheControl ?? null,
			contentDisposition: response.ContentDisposition ?? null,
			expires: response.Expires ?? null,
			etag: response.ETag ?? null,
			lastModified: response.LastModified ?? null,
		};
	}

	private async ensureDirectoryExists(dirPath: string): Promise<void> {
		await fs.promises.mkdir(dirPath, {recursive: true});
	}

	async writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		await this.ensureDirectoryExists(path.dirname(filePath));
		const command = new GetObjectCommand({Bucket: bucket, Key: key});
		const {Body} = await this.getClient(bucket).send(command);
		assert(Body != null && Body instanceof Readable);
		const stream = Body instanceof PassThrough ? Body : Body.pipe(new PassThrough());
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
		const command = new CopyObjectCommand({
			Bucket: destinationBucket,
			Key: destinationKey,
			CopySource: `${encodeURIComponent(sourceBucket)}/${sourceKey.split('/').map(encodeURIComponent).join('/')}`,
			ContentType: newContentType,
			MetadataDirective: newContentType ? 'REPLACE' : undefined,
		});
		await this.getClient(destinationBucket).send(command);
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
			return await processAndUploadJpeg({
				sourceData,
				contentType,
				destination: {bucket: destinationBucket, key: destinationKey},
				uploadObject: async (params) => this.uploadObject(params),
			});
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
		const command = new ListObjectsV2Command({Bucket: bucket});
		const {Contents} = await this.s3.send(command);
		if (!Contents) {
			return;
		}
		await Promise.all(Contents.map(({Key}) => Key && this.deleteObject(bucket, Key)));
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

	async getObject(params: {bucket: string; key: string}): Promise<GetObjectCommandOutput> {
		const command = new GetObjectCommand({
			Bucket: params.bucket,
			Key: params.key,
		});
		return await this.getClient(params.bucket).send(command);
	}

	async headObject(params: {bucket: string; key: string}): Promise<HeadObjectCommandOutput> {
		const command = new HeadObjectCommand({
			Bucket: params.bucket,
			Key: params.key,
		});
		return await this.getClient(params.bucket).send(command);
	}

	async listObjects(params: {bucket: string; prefix: string}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	> {
		const allObjects: Array<{key: string; lastModified?: Date}> = [];
		let continuationToken: string | undefined;

		do {
			const command = new ListObjectsV2Command({
				Bucket: params.bucket,
				Prefix: params.prefix,
				ContinuationToken: continuationToken,
			});
			const response = await this.getClient(params.bucket).send(command);

			if (response.Contents) {
				for (const obj of response.Contents) {
					if (obj.Key) {
						allObjects.push({
							key: obj.Key,
							lastModified: obj.LastModified,
						});
					}
				}
			}

			continuationToken = response.IsTruncated ? response.NextContinuationToken : undefined;
		} while (continuationToken);

		return allObjects;
	}

	async deleteObjects(params: {bucket: string; objects: ReadonlyArray<{Key: string}>}): Promise<void> {
		if (params.objects.length === 0) return;

		const command = new DeleteObjectsCommand({
			Bucket: params.bucket,
			Delete: {
				Objects: params.objects as Array<{Key: string}>,
			},
		});
		await this.getClient(params.bucket).send(command);
	}

	async createMultipartUpload(params: {
		bucket: string;
		key: string;
		contentType?: string;
	}): Promise<{uploadId: string}> {
		const command = new CreateMultipartUploadCommand({
			Bucket: params.bucket,
			Key: params.key,
			ContentType: params.contentType,
		});
		const response = await this.getClient(params.bucket).send(command);
		assert(response.UploadId != null, 'Missing UploadId in CreateMultipartUpload response');
		return {uploadId: response.UploadId};
	}

	async uploadPart(params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		body: Uint8Array;
	}): Promise<{etag: string}> {
		const command = new UploadPartCommand({
			Bucket: params.bucket,
			Key: params.key,
			UploadId: params.uploadId,
			PartNumber: params.partNumber,
			Body: params.body,
		});
		const response = await this.getClient(params.bucket).send(command);
		assert(response.ETag != null, 'Missing ETag in UploadPart response');
		return {etag: response.ETag};
	}

	async completeMultipartUpload(params: {
		bucket: string;
		key: string;
		uploadId: string;
		parts: Array<{partNumber: number; etag: string}>;
	}): Promise<void> {
		const command = new CompleteMultipartUploadCommand({
			Bucket: params.bucket,
			Key: params.key,
			UploadId: params.uploadId,
			MultipartUpload: {
				Parts: params.parts.map((part) => ({
					PartNumber: part.partNumber,
					ETag: part.etag,
				})),
			},
		});
		await this.getClient(params.bucket).send(command);
	}

	async abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		const command = new AbortMultipartUploadCommand({
			Bucket: params.bucket,
			Key: params.key,
			UploadId: params.uploadId,
		});
		await this.getClient(params.bucket).send(command);
	}
}
