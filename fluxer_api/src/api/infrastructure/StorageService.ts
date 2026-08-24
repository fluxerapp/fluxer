// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
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
	ListPartsCommand,
	PutObjectCommand,
	S3Client,
	S3ServiceException,
	UploadPartCommand,
} from '@aws-sdk/client-s3';
import {Upload} from '@aws-sdk/lib-storage';
import {getSignedUrl} from '@aws-sdk/s3-request-presigner';
import type {S3ProviderSettings} from '@fluxer/config/src/S3DownloadsProvider';
import {isSupportedMediaContentType} from '@pkgs/mime_utils/src/ContentTypeUtils';
import {seconds} from 'itty-time';
import {temporaryFile} from 'tempy';
import {Config} from '../Config';
import {Logger} from '../Logger';
import type {IStorageService, ProcessedStorageObjectMetadata} from './IStorageService';
import {processMediaFile} from './StorageObjectHelpers';

const STREAM_UPLOAD_PART_BYTES = 8 * 1024 * 1024;
const STREAM_UPLOAD_CONCURRENCY = 4;

interface PooledS3Options {
	endpoint: string;
	region: string;
	accessKeyId: string;
	secretAccessKey: string;
	forcePathStyle?: boolean;
}

function buildPooledS3Client(options: PooledS3Options): S3Client {
	return new S3Client({
		endpoint: options.endpoint,
		region: options.region,
		forcePathStyle: options.forcePathStyle ?? true,
		credentials: {
			accessKeyId: options.accessKeyId,
			secretAccessKey: options.secretAccessKey,
		},
		requestChecksumCalculation: 'WHEN_REQUIRED',
		responseChecksumValidation: 'WHEN_REQUIRED',
		maxAttempts: 3,
	});
}

const pipelinePromise = promisify(pipeline);

async function streamToUint8Array(body: Readable, maxBytes?: number): Promise<Uint8Array> {
	if (maxBytes !== undefined && maxBytes <= 0) {
		body.destroy();
		return new Uint8Array(0);
	}
	const chunks: Array<Buffer> = [];
	let total = 0;
	for await (const chunk of body) {
		const buf = chunk instanceof Buffer ? chunk : Buffer.from(chunk as Uint8Array);
		if (maxBytes !== undefined && total + buf.length > maxBytes) {
			const remaining = maxBytes - total;
			if (remaining > 0) {
				chunks.push(buf.subarray(0, remaining));
				total += remaining;
			}
			break;
		}
		chunks.push(buf);
		total += buf.length;
		if (maxBytes !== undefined && total >= maxBytes) {
			break;
		}
	}
	const out = new Uint8Array(total);
	let offset = 0;
	for (const c of chunks) {
		out.set(c, offset);
		offset += c.length;
	}
	return out;
}

function extractStreamFromGet(out: GetObjectCommandOutput): Readable {
	const body = out.Body;
	if (body instanceof Readable) {
		return body instanceof PassThrough ? body : body.pipe(new PassThrough());
	}
	throw new Error('Unexpected S3 response body type (not a Node Readable)');
}

export class StorageService implements IStorageService {
	private readonly client: S3Client;
	private readonly presignClient: S3Client;
	private readonly provider: S3ProviderSettings;

	constructor(provider?: S3ProviderSettings) {
		this.provider = provider ?? {
			endpoint: Config.s3.endpoint,
			presignedUrlBase: Config.s3.presignedUrlBase,
			forcePathStyle: Config.s3.forcePathStyle,
			region: Config.s3.region,
			accessKeyId: Config.s3.accessKeyId,
			secretAccessKey: Config.s3.secretAccessKey,
		};
		this.client = buildPooledS3Client({
			endpoint: this.provider.endpoint,
			region: this.provider.region,
			accessKeyId: this.provider.accessKeyId,
			secretAccessKey: this.provider.secretAccessKey,
			forcePathStyle: true,
		});
		this.presignClient = buildPooledS3Client({
			endpoint: this.resolvePresignEndpoint(),
			region: this.provider.region,
			accessKeyId: this.provider.accessKeyId,
			secretAccessKey: this.provider.secretAccessKey,
			forcePathStyle: this.provider.forcePathStyle,
		});
	}

	private resolvePresignEndpoint(): string {
		const fallbackEndpoint = this.provider.endpoint;
		const configuredEndpoint = this.provider.presignedUrlBase;
		if (!configuredEndpoint) {
			return fallbackEndpoint;
		}
		try {
			const endpoint = new URL(configuredEndpoint);
			const uploadsPrefix = `${Config.s3.buckets.uploads}.`;
			if (endpoint.hostname.startsWith(uploadsPrefix)) {
				endpoint.hostname = endpoint.hostname.slice(uploadsPrefix.length);
				endpoint.pathname = '/';
				endpoint.search = '';
				endpoint.hash = '';
			}
			return endpoint.toString();
		} catch {
			return configuredEndpoint;
		}
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
		body: Uint8Array | Readable;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void> {
		if (body instanceof Uint8Array) {
			await this.client.send(
				new PutObjectCommand({
					Bucket: bucket,
					Key: key,
					Body: body,
					ContentType: contentType,
					Expires: expiresAt,
				}),
			);
			return;
		}
		const upload = new Upload({
			client: this.client,
			params: {
				Bucket: bucket,
				Key: key,
				Body: body,
				ContentType: contentType,
				Expires: expiresAt,
			},
			partSize: STREAM_UPLOAD_PART_BYTES,
			queueSize: STREAM_UPLOAD_CONCURRENCY,
			leavePartsOnError: false,
		});
		await upload.done();
	}

	async uploadObjectFromFile({
		bucket,
		key,
		filePath,
		contentType,
		contentLength,
		expiresAt,
	}: {
		bucket: string;
		key: string;
		filePath: string;
		contentType?: string;
		contentLength?: number;
		expiresAt?: Date;
	}): Promise<void> {
		const size = contentLength ?? (await fs.promises.stat(filePath)).size;
		if (size <= STREAM_UPLOAD_PART_BYTES) {
			const body = await fs.promises.readFile(filePath);
			await this.client.send(
				new PutObjectCommand({
					Bucket: bucket,
					Key: key,
					Body: body,
					ContentType: contentType,
					ContentLength: size,
					Expires: expiresAt,
				}),
			);
			return;
		}
		const stream = fs.createReadStream(filePath, {highWaterMark: 1024 * 1024});
		try {
			const upload = new Upload({
				client: this.client,
				params: {
					Bucket: bucket,
					Key: key,
					Body: stream,
					ContentType: contentType,
					Expires: expiresAt,
				},
				partSize: STREAM_UPLOAD_PART_BYTES,
				queueSize: STREAM_UPLOAD_CONCURRENCY,
				leavePartsOnError: false,
			});
			await upload.done();
		} catch (error) {
			stream.destroy();
			throw error;
		}
	}

	async getPresignedDownloadURL({
		bucket,
		key,
		expiresIn = seconds('5 minutes'),
		responseContentType,
		responseContentDisposition,
	}: {
		bucket: string;
		key: string;
		expiresIn?: number;
		responseContentType?: string;
		responseContentDisposition?: string;
	}): Promise<string> {
		const command = new GetObjectCommand({
			Bucket: bucket,
			Key: key,
			ResponseContentType: responseContentType,
			ResponseContentDisposition: responseContentDisposition,
		});
		return getSignedUrl(this.presignClient, command, {expiresIn});
	}

	async getPresignedUploadURL({
		bucket,
		key,
		contentType,
		contentLength,
		expiresIn = seconds('5 minutes'),
	}: {
		bucket: string;
		key: string;
		contentType?: string;
		contentLength?: number;
		expiresIn?: number;
	}): Promise<string> {
		const command = new PutObjectCommand({
			Bucket: bucket,
			Key: key,
			ContentType: contentType,
			ContentLength: contentLength,
		});
		return getSignedUrl(this.presignClient, command, {expiresIn});
	}

	async getPresignedUploadPartURL({
		bucket,
		key,
		uploadId,
		partNumber,
		expiresIn = seconds('1 hour'),
	}: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		expiresIn?: number;
	}): Promise<string> {
		const command = new UploadPartCommand({
			Bucket: bucket,
			Key: key,
			UploadId: uploadId,
			PartNumber: partNumber,
		});
		return getSignedUrl(this.presignClient, command, {expiresIn});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		await this.client.send(new DeleteObjectCommand({Bucket: bucket, Key: key}));
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
		try {
			const out = await this.client.send(new HeadObjectCommand({Bucket: bucket, Key: key}));
			return {
				contentLength: out.ContentLength ?? 0,
				contentType: out.ContentType ?? '',
				...(out.ETag === undefined ? {} : {etag: out.ETag}),
				...(out.LastModified === undefined ? {} : {lastModified: out.LastModified}),
			};
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
	}

	async computeObjectSha256(bucket: string, key: string): Promise<string> {
		const hash = createHash('sha256');
		const out = await this.client.send(new GetObjectCommand({Bucket: bucket, Key: key}));
		const body = extractStreamFromGet(out);
		try {
			for await (const chunk of body) {
				hash.update(chunk as Uint8Array);
			}
		} catch (error) {
			body.destroy();
			throw error;
		}
		return hash.digest('hex');
	}

	async readObject(bucket: string, key: string, maxBytes?: number): Promise<Uint8Array> {
		const out = await this.client.send(new GetObjectCommand({Bucket: bucket, Key: key}));
		return streamToUint8Array(extractStreamFromGet(out), maxBytes);
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
		try {
			const out = await this.client.send(
				new GetObjectCommand({Bucket: params.bucket, Key: params.key, Range: params.range}),
			);
			return {
				body: extractStreamFromGet(out),
				contentLength: out.ContentLength ?? 0,
				contentRange: out.ContentRange ?? null,
				contentType: out.ContentType ?? null,
				cacheControl: out.CacheControl ?? null,
				contentDisposition: out.ContentDisposition ?? null,
				expires: out.Expires ?? null,
				etag: out.ETag ?? null,
				lastModified: out.LastModified ?? null,
			};
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
	}

	private async ensureDirectoryExists(dirPath: string): Promise<void> {
		await fs.promises.mkdir(dirPath, {recursive: true});
	}

	async writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		await this.ensureDirectoryExists(path.dirname(filePath));
		const out = await this.client.send(new GetObjectCommand({Bucket: bucket, Key: key}));
		const stream = extractStreamFromGet(out);
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
		const isSameObject = sourceBucket === destinationBucket && sourceKey === destinationKey;
		if (isSameObject && !newContentType) {
			return;
		}
		await this.client.send(
			new CopyObjectCommand({
				Bucket: destinationBucket,
				Key: destinationKey,
				CopySource: `${encodeURIComponent(sourceBucket)}/${sourceKey.split('/').map(encodeURIComponent).join('/')}`,
				ContentType: newContentType,
				MetadataDirective: newContentType ? 'REPLACE' : undefined,
			}),
		);
	}

	async copyObjectWithMetadataStripping({
		sourceBucket,
		sourceKey,
		destinationBucket,
		destinationKey,
		contentType,
		sourceLocalPath,
	}: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		contentType: string;
		sourceLocalPath?: string;
	}): Promise<ProcessedStorageObjectMetadata | null> {
		if (!isSupportedMediaContentType(contentType)) {
			await this.copyObject({
				sourceBucket,
				sourceKey,
				destinationBucket,
				destinationKey,
				newContentType: contentType,
			});
			return null;
		}
		const downloadedSource = sourceLocalPath ?? temporaryFile();
		const ownsDownload = !sourceLocalPath;
		const processedPath = temporaryFile();
		try {
			if (ownsDownload) {
				await this.writeObjectToDisk(sourceBucket, sourceKey, downloadedSource);
			}
			const processed = await processMediaFile(downloadedSource, processedPath, contentType).catch((error) => {
				Logger.error({error, contentType, sourceBucket, sourceKey}, 'Failed to strip attachment media metadata');
				return null;
			});
			if (!processed) {
				await this.copyObject({
					sourceBucket,
					sourceKey,
					destinationBucket,
					destinationKey,
					newContentType: contentType,
				});
				return null;
			}
			await this.uploadObjectFromFile({
				bucket: destinationBucket,
				key: destinationKey,
				filePath: processed.filePath,
				contentType: processed.contentType,
				contentLength: processed.contentLength,
			});
			return {
				contentType: processed.contentType,
				contentLength: processed.contentLength,
				contentHash: processed.contentHash,
				...(processed.width != null ? {width: processed.width} : {}),
				...(processed.height != null ? {height: processed.height} : {}),
			};
		} finally {
			await fs.promises.unlink(processedPath).catch(() => undefined);
			if (ownsDownload) {
				await fs.promises.unlink(downloadedSource).catch(() => undefined);
			}
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
		const objects = await this.listObjects({bucket, prefix: ''});
		await Promise.all(objects.map(({key}) => this.deleteObject(bucket, key)));
		Logger.debug({bucket, count: objects.length}, 'Purged bucket');
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
		return this.client.send(new GetObjectCommand({Bucket: params.bucket, Key: params.key}));
	}

	async headObject(params: {bucket: string; key: string}): Promise<HeadObjectCommandOutput> {
		return this.client.send(new HeadObjectCommand({Bucket: params.bucket, Key: params.key}));
	}

	async listObjects(params: {bucket: string; prefix: string}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	> {
		const result: Array<{
			key: string;
			lastModified?: Date;
		}> = [];
		let continuationToken: string | undefined;
		do {
			const command = new ListObjectsV2Command({
				Bucket: params.bucket,
				Prefix: params.prefix,
				ContinuationToken: continuationToken,
			});
			const response = await this.client.send(command);
			if (response.Contents) {
				for (const obj of response.Contents) {
					if (obj.Key) {
						result.push({key: obj.Key, lastModified: obj.LastModified});
					}
				}
			}
			continuationToken = response.IsTruncated ? response.NextContinuationToken : undefined;
		} while (continuationToken);
		return result;
	}

	async deleteObjects(params: {
		bucket: string;
		objects: ReadonlyArray<{
			Key: string;
		}>;
	}): Promise<void> {
		if (params.objects.length === 0) return;
		await this.client.send(
			new DeleteObjectsCommand({
				Bucket: params.bucket,
				Delete: {Objects: params.objects as Array<{Key: string}>},
			}),
		);
	}

	async createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{
		uploadId: string;
	}> {
		const command = new CreateMultipartUploadCommand({
			Bucket: params.bucket,
			Key: params.key,
			ContentType: params.contentType,
		});
		const response = await this.client.send(command);
		assert(response.UploadId != null, 'Missing UploadId in CreateMultipartUpload response');
		return {uploadId: response.UploadId};
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
		const command = new UploadPartCommand({
			Bucket: params.bucket,
			Key: params.key,
			UploadId: params.uploadId,
			PartNumber: params.partNumber,
			Body: params.body,
		});
		const response = await this.client.send(command);
		assert(response.ETag != null, 'Missing ETag in UploadPart response');
		return {etag: response.ETag};
	}

	async listParts(params: {bucket: string; key: string; uploadId: string}): Promise<
		Array<{
			partNumber: number;
			etag: string;
			size?: number;
		}>
	> {
		const all: Array<{
			partNumber: number;
			etag: string;
			size?: number;
		}> = [];
		let partNumberMarker: string | undefined;
		do {
			const response = await this.client.send(
				new ListPartsCommand({
					Bucket: params.bucket,
					Key: params.key,
					UploadId: params.uploadId,
					PartNumberMarker: partNumberMarker,
				}),
			);
			for (const part of response.Parts ?? []) {
				if (part.PartNumber == null || part.ETag == null) continue;
				all.push({partNumber: part.PartNumber, etag: part.ETag, size: part.Size});
			}
			partNumberMarker = response.IsTruncated ? response.NextPartNumberMarker : undefined;
		} while (partNumberMarker);
		all.sort((a, b) => a.partNumber - b.partNumber);
		return all;
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
		await this.client.send(command);
	}

	async abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		const command = new AbortMultipartUploadCommand({
			Bucket: params.bucket,
			Key: params.key,
			UploadId: params.uploadId,
		});
		await this.client.send(command);
	}
}
