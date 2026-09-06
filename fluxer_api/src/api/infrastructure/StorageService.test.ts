// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {PassThrough, Readable} from 'node:stream';
import {describe, expect, it} from 'vitest';
import {Config} from '../Config';
import {StorageObjectListingOverflowError} from './IStorageService';
import {StorageService} from './StorageService';

interface CopyObjectTestParams {
	sourceBucket: string;
	sourceKey: string;
	destinationBucket: string;
	destinationKey: string;
	newContentType?: string;
}

interface UploadObjectTestParams {
	bucket: string;
	key: string;
	body: Uint8Array | Readable;
	contentType?: string;
	expiresAt?: Date;
}

interface UploadObjectFromFileTestParams {
	bucket: string;
	key: string;
	filePath: string;
	contentType?: string;
	contentLength?: number;
	expiresAt?: Date;
}

interface WriteObjectToDiskTestParams {
	bucket: string;
	key: string;
	filePath: string;
}

interface S3BucketConfigOverrides {
	cdn?: string;
	uploads?: string;
	downloads?: string;
	reports?: string;
	harvests?: string;
}

interface S3ConfigOverrides {
	endpoint?: string;
	presignedUrlBase?: string;
	forcePathStyle?: boolean;
	region?: string;
	accessKeyId?: string;
	secretAccessKey?: string;
	buckets?: S3BucketConfigOverrides;
}

class TestStorageService extends StorageService {
	readonly copiedObjects: Array<CopyObjectTestParams> = [];
	readonly readObjects: Array<{bucket: string; key: string; maxBytes?: number}> = [];
	readonly uploadedObjects: Array<UploadObjectTestParams> = [];
	readonly uploadedObjectsFromFile: Array<UploadObjectFromFileTestParams> = [];
	readonly writtenObjectsToDisk: Array<WriteObjectToDiskTestParams> = [];

	constructor(private readonly sourceData: Uint8Array) {
		super();
	}

	override async readObject(_bucket: string, _key: string, maxBytes?: number): Promise<Uint8Array> {
		this.readObjects.push(
			maxBytes === undefined ? {bucket: _bucket, key: _key} : {bucket: _bucket, key: _key, maxBytes},
		);
		return maxBytes !== undefined && this.sourceData.length > maxBytes
			? this.sourceData.slice(0, maxBytes)
			: this.sourceData;
	}

	override async copyObject(params: CopyObjectTestParams): Promise<void> {
		this.copiedObjects.push(params);
	}

	override async uploadObject(params: UploadObjectTestParams): Promise<void> {
		this.uploadedObjects.push(params);
	}

	override async uploadObjectFromFile(params: UploadObjectFromFileTestParams): Promise<void> {
		this.uploadedObjectsFromFile.push(params);
	}

	override async writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		this.writtenObjectsToDisk.push({bucket, key, filePath});
		await fs.promises.writeFile(filePath, this.sourceData);
	}
}

async function withS3Config<T>(overrides: S3ConfigOverrides, callback: () => Promise<T>): Promise<T> {
	const original = {
		...Config.s3,
		buckets: {...Config.s3.buckets},
	};
	const {buckets, ...rest} = overrides;
	Object.assign(Config.s3, rest);
	if (buckets) {
		Object.assign(Config.s3.buckets, buckets);
	}
	try {
		return await callback();
	} finally {
		Object.assign(Config.s3, original);
		Object.assign(Config.s3.buckets, original.buckets);
	}
}

describe('StorageService.getPresignedUploadURL', () => {
	it('uses the configured public presign endpoint with path-style bucket addressing', async () => {
		await withS3Config(
			{
				endpoint: 'http://seaweedfs:8333',
				presignedUrlBase: 'https://dev.example.test',
				forcePathStyle: true,
				region: 'us-east-1',
				accessKeyId: 'fluxer',
				secretAccessKey: 'fluxer-secret',
				buckets: {uploads: 'fluxer-uploads'},
			},
			async () => {
				const service = new StorageService();
				const uploadUrl = await service.getPresignedUploadURL({
					bucket: 'fluxer-uploads',
					key: 'stream_previews/1511582191061041152:1511582191061041156:talpa-pentatonic.jpg',
					contentType: 'image/jpeg',
				});
				const url = new URL(uploadUrl);

				expect(url.origin).toBe('https://dev.example.test');
				expect(decodeURIComponent(url.pathname)).toBe(
					'/fluxer-uploads/stream_previews/1511582191061041152:1511582191061041156:talpa-pentatonic.jpg',
				);
			},
		);
	});
});

describe('StorageService.copyObjectWithMetadataStripping', () => {
	it('copies non-media attachments without reading the object through the API', async () => {
		const service = new TestStorageService(new Uint8Array([1, 2, 3]));
		const result = await service.copyObjectWithMetadataStripping({
			sourceBucket: 'uploads',
			sourceKey: 'source.txt',
			destinationBucket: 'cdn',
			destinationKey: 'attachments/source.txt',
			contentType: 'text/plain',
		});
		expect(result).toBeNull();
		expect(service.readObjects).toEqual([]);
		expect(service.uploadedObjects).toEqual([]);
		expect(service.copiedObjects).toEqual([
			{
				sourceBucket: 'uploads',
				sourceKey: 'source.txt',
				destinationBucket: 'cdn',
				destinationKey: 'attachments/source.txt',
				newContentType: 'text/plain',
			},
		]);
	});
	it('falls back to copying the original object when metadata stripping fails', async () => {
		const service = new TestStorageService(new Uint8Array([1, 2, 3]));
		const result = await service.copyObjectWithMetadataStripping({
			sourceBucket: 'uploads',
			sourceKey: 'source.png',
			destinationBucket: 'cdn',
			destinationKey: 'attachments/source.png',
			contentType: 'image/png',
		});
		expect(result).toBeNull();
		expect(service.uploadedObjects).toEqual([]);
		expect(service.copiedObjects).toEqual([
			{
				sourceBucket: 'uploads',
				sourceKey: 'source.png',
				destinationBucket: 'cdn',
				destinationKey: 'attachments/source.png',
				newContentType: 'image/png',
			},
		]);
	});
});

describe('provider selection', () => {
	interface ClientProbe {
		client: {config: {region: () => Promise<string>; endpoint?: () => Promise<{hostname: string}>}};
	}

	it('defaults to the shared S3 configuration', async () => {
		const service = new StorageService() as unknown as ClientProbe;
		expect(await service.client.config.region()).toBe(Config.s3.region);
	});

	it('uses an explicitly supplied provider instead of the shared one', async () => {
		const service = new StorageService({
			endpoint: 'https://downloads.example.net',
			forcePathStyle: false,
			region: 'eu-central-9',
			accessKeyId: 'DL_KEY',
			secretAccessKey: 'DL_SECRET',
		}) as unknown as ClientProbe;
		expect(await service.client.config.region()).toBe('eu-central-9');
		expect(await service.client.config.region()).not.toBe(Config.s3.region);
	});
});

function bodyStream(chunks: Array<Buffer>): PassThrough {
	const stream = new PassThrough();
	const writeChunk = (index: number): void => {
		if (stream.destroyed) {
			return;
		}
		const chunk = chunks[index];
		if (chunk === undefined) {
			stream.end();
			return;
		}
		stream.write(chunk);
		setImmediate(() => writeChunk(index + 1));
	};
	writeChunk(0);
	return stream;
}

function serveGetObject(service: StorageService, out: {Body: PassThrough; ContentLength?: number}): void {
	Object.assign(service, {client: {send: async () => out}});
}

describe('StorageService.readObject', () => {
	it('refuses an object whose declared length is already over the cap', async () => {
		const service = new StorageService();
		const body = bodyStream([Buffer.alloc(64, 1)]);
		serveGetObject(service, {Body: body, ContentLength: 64});

		await expect(service.readObject('fluxer-downloads', 'desktop/stable/manifest.json', 32)).rejects.toThrow(
			/exceeds maximum buffer size of 32 bytes \(got 64 bytes\)/u,
		);
		expect(body.destroyed).toBe(true);
	});

	it('refuses an object that outgrows the cap mid-stream when no length is declared', async () => {
		const service = new StorageService();
		const body = bodyStream([Buffer.alloc(24, 1), Buffer.alloc(24, 2)]);
		serveGetObject(service, {Body: body});

		await expect(service.readObject('fluxer-downloads', 'desktop/stable/manifest.json', 32)).rejects.toThrow(
			/exceeds maximum buffer size of 32 bytes \(got at least 48 bytes\)/u,
		);
		expect(body.destroyed).toBe(true);
	});

	it('returns every byte of an object that exactly fills the cap', async () => {
		const service = new StorageService();
		serveGetObject(service, {Body: bodyStream([Buffer.alloc(16, 7), Buffer.alloc(16, 9)])});

		await expect(service.readObject('fluxer-downloads', 'desktop/stable/manifest.json', 32)).resolves.toEqual(
			new Uint8Array([...Array(16).fill(7), ...Array(16).fill(9)]),
		);
	});

	it('rejects rather than hanging when the object store resets the body mid-transfer', async () => {
		const service = new StorageService();
		const reset = new Error('socket hang up');
		const body = new Readable({read() {}});
		Object.assign(service, {client: {send: async () => ({Body: body})}});
		const read = service.readObject('fluxer-downloads', 'desktop/stable/manifest.json', 1024);
		body.push(Buffer.alloc(8, 1));
		setImmediate(() => body.destroy(reset));

		await expect(read).rejects.toThrow(/socket hang up/u);
	}, 5000);
});

interface ListObjectsPage {
	Contents?: Array<{Key: string; LastModified?: Date}>;
	IsTruncated?: boolean;
	NextContinuationToken?: string;
}

function serveListPages(service: StorageService, pages: Array<ListObjectsPage>): {tokens: Array<string | undefined>} {
	const tokens: Array<string | undefined> = [];
	let index = 0;
	Object.assign(service, {
		client: {
			send: async (command: {input: {ContinuationToken?: string}}) => {
				tokens.push(command.input.ContinuationToken);
				const page = pages[index] ?? {};
				index += 1;
				return page;
			},
		},
	});
	return {tokens};
}

describe('StorageService.listObjects', () => {
	it('returns every page of a truncated listing', async () => {
		const service = new StorageService();
		const {tokens} = serveListPages(service, [
			{
				Contents: [{Key: 'desktop/a.exe'}, {Key: 'desktop/b.exe'}],
				IsTruncated: true,
				NextContinuationToken: 'page-2',
			},
			{Contents: [{Key: 'desktop/c.exe'}], IsTruncated: false},
		]);
		const objects = await service.listObjects({bucket: 'fluxer-downloads', prefix: 'desktop/'});
		expect(objects.map(({key}) => key)).toEqual(['desktop/a.exe', 'desktop/b.exe', 'desktop/c.exe']);
		expect(tokens).toEqual([undefined, 'page-2']);
	});

	it('throws a typed overflow error when the prefix outgrows the requested cap', async () => {
		const service = new StorageService();
		serveListPages(service, [
			{
				Contents: [{Key: 'desktop/a.exe'}, {Key: 'desktop/b.exe'}],
				IsTruncated: true,
				NextContinuationToken: 'page-2',
			},
		]);
		await expect(
			service.listObjects({bucket: 'fluxer-downloads', prefix: 'desktop/', maxObjects: 2}),
		).rejects.toBeInstanceOf(StorageObjectListingOverflowError);
	});

	it('throws rather than returning a partial listing when the continuation token is missing', async () => {
		const service = new StorageService();
		serveListPages(service, [{Contents: [{Key: 'desktop/a.exe'}], IsTruncated: true}]);
		await expect(service.listObjects({bucket: 'fluxer-downloads', prefix: 'desktop/'})).rejects.toThrow(
			/continuation token/u,
		);
	});
});
