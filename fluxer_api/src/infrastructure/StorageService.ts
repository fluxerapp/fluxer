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
import {execFile} from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import {PassThrough, pipeline, Readable} from 'node:stream';
import {promisify} from 'node:util';
import {
	CopyObjectCommand,
	CreateBucketCommand,
	DeleteObjectCommand,
	DeleteObjectsCommand,
	GetObjectCommand,
	type GetObjectCommandOutput,
	HeadBucketCommand,
	HeadObjectCommand,
	type HeadObjectCommandOutput,
	ListObjectsV2Command,
	type ListObjectsV2CommandOutput,
	PutBucketPolicyCommand,
	PutObjectCommand,
	S3Client,
	S3ServiceException,
} from '@aws-sdk/client-s3';
import {getSignedUrl} from '@aws-sdk/s3-request-presigner';
import sharp from 'sharp';
import {temporaryFile} from 'tempy';
import {Config} from '~/Config';
import type {IStorageService} from '~/infrastructure/IStorageService';
import {Logger} from '~/Logger';

const pipelinePromise = promisify(pipeline);
const execFilePromise = promisify(execFile);

export class StorageService implements IStorageService {
	private readonly s3: S3Client;
	private readonly presignClient: S3Client;

	constructor() {
		const baseInit = {
			endpoint: Config.s3.endpoint,
			region: 'us-east-1',
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
		expiresIn = 300,
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

	private async streamToBuffer(stream: Readable): Promise<Uint8Array> {
		const chunks: Array<Uint8Array> = [];
		for await (const chunk of stream) {
			chunks.push(new Uint8Array(Buffer.from(chunk)));
		}
		return new Uint8Array(Buffer.concat(chunks.map((chunk) => Buffer.from(chunk))));
	}

	async readObject(bucket: string, key: string): Promise<Uint8Array> {
		const command = new GetObjectCommand({Bucket: bucket, Key: key});
		const {Body} = await this.getClient(bucket).send(command);
		assert(Body != null && Body instanceof Readable);
		const stream = Body instanceof PassThrough ? Body : Body.pipe(new PassThrough());
		return this.streamToBuffer(stream);
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
		await pipelinePromise(stream, writeStream);
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
			CopySource: `${sourceBucket}/${sourceKey}`,
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
			return await this.processAndUploadJpeg({
				sourceData,
				destinationBucket,
				destinationKey,
				contentType,
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

	private async processAndUploadJpeg({
		sourceData,
		destinationBucket,
		destinationKey,
		contentType,
	}: {
		sourceData: Uint8Array;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
	}): Promise<{width: number; height: number} | null> {
		const inputPath = temporaryFile({extension: 'jpg'});
		const outputPath = temporaryFile({extension: 'jpg'});

		try {
			await fs.promises.writeFile(inputPath, sourceData);

			const orientation = await this.getJpegOrientation(inputPath);
			const image = sharp(sourceData);
			const metadata = await image.metadata();

			const processedBuffer = await image
				.rotate(orientation === 6 ? 90 : 0)
				.jpeg({
					quality: 100,
					chromaSubsampling: '4:2:0',
				})
				.toBuffer();

			await fs.promises.writeFile(outputPath, processedBuffer);
			await this.stripJpegMetadata(outputPath);

			const finalBuffer = await fs.promises.readFile(outputPath);
			await this.uploadObject({
				bucket: destinationBucket,
				key: destinationKey,
				body: finalBuffer,
				contentType,
			});

			if (metadata.width && metadata.height) {
				return orientation === 6
					? {width: metadata.height, height: metadata.width}
					: {width: metadata.width, height: metadata.height};
			}

			return null;
		} finally {
			await Promise.all([
				fs.promises.unlink(inputPath).catch(() => {}),
				fs.promises.unlink(outputPath).catch(() => {}),
			]);
		}
	}

	private async getJpegOrientation(filePath: string): Promise<number> {
		const {stdout} = await execFilePromise('exiftool', ['-Orientation#', '-n', '-j', filePath]);
		const [{Orientation = 1}] = JSON.parse(stdout);
		return Orientation;
	}

	private async stripJpegMetadata(filePath: string): Promise<void> {
		await execFilePromise('exiftool', [
			'-all=',
			'-jfif:all=',
			'-JFIFVersion=1.01',
			'-ResolutionUnit=none',
			'-XResolution=1',
			'-YResolution=1',
			'-n',
			'-overwrite_original',
			'-F',
			'-exif:all=',
			'-iptc:all=',
			'-xmp:all=',
			'-icc_profile:all=',
			'-photoshop:all=',
			'-adobe:all=',
			filePath,
		]);
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

	async createBucket(bucket: string, allowPublicAccess = false): Promise<void> {
		try {
			await this.s3.send(new HeadBucketCommand({Bucket: bucket}));
		} catch (error) {
			if (error instanceof S3ServiceException && error.name === 'NotFound') {
				Logger.debug({bucket}, 'Creating bucket');
				await this.s3.send(new CreateBucketCommand({Bucket: bucket}));

				if (allowPublicAccess) {
					const policy = {
						Version: '2012-10-17',
						Statement: [
							{
								Sid: 'PublicReadGetObject',
								Effect: 'Allow',
								Principal: '*',
								Action: 's3:GetObject',
								Resource: `arn:aws:s3:::${bucket}/*`,
							},
						],
					};

					await this.s3.send(
						new PutBucketPolicyCommand({
							Bucket: bucket,
							Policy: JSON.stringify(policy),
						}),
					);

					Logger.debug({bucket}, 'Set public access policy on bucket');
				}
			} else {
				throw error;
			}
		}
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

	async listObjects(params: {bucket: string; prefix: string}): Promise<ListObjectsV2CommandOutput> {
		const command = new ListObjectsV2Command({
			Bucket: params.bucket,
			Prefix: params.prefix,
		});
		return await this.getClient(params.bucket).send(command);
	}

	async deleteObjects(params: {bucket: string; objects: Array<{Key: string}>}): Promise<void> {
		if (params.objects.length === 0) return;

		const command = new DeleteObjectsCommand({
			Bucket: params.bucket,
			Delete: {
				Objects: params.objects,
			},
		});
		await this.getClient(params.bucket).send(command);
	}
}
