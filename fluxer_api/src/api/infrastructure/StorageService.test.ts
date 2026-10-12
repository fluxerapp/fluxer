// SPDX-License-Identifier: AGPL-3.0-or-later

import {PassThrough, Readable} from 'node:stream';
import {Config} from '@app/api/Config';
import {StorageObjectListingOverflowError} from '@app/api/infrastructure/IStorageService';
import {StorageService} from '@app/api/infrastructure/StorageService';
import {describe, expect, it} from 'vitest';

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

	it('gives both clients the configured addressing rather than pinning one to path style', async () => {
		await withS3Config(
			{
				endpoint: 'https://s3.example.test',
				presignedUrlBase: '',
				forcePathStyle: false,
				region: 'eu-central-1',
				accessKeyId: 'fluxer',
				secretAccessKey: 'fluxer-secret',
				buckets: {uploads: 'fluxer-uploads'},
			},
			async () => {
				const service = new StorageService();
				const probe = service as unknown as {
					client: {config: {forcePathStyle?: unknown}};
					presignClient: {config: {forcePathStyle?: unknown}};
				};
				const resolve = async (value: unknown): Promise<unknown> =>
					typeof value === 'function' ? await (value as () => Promise<unknown>)() : value;

				expect(await resolve(probe.client.config.forcePathStyle)).toBe(false);
				expect(await resolve(probe.presignClient.config.forcePathStyle)).toBe(false);
			},
		);
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

function copySourceFor(endpoint: string, forcePathStyle: boolean): Promise<unknown> {
	const service = new StorageService({
		endpoint,
		forcePathStyle,
		region: 'us-east-1',
		accessKeyId: 'fluxer',
		secretAccessKey: 'fluxer-secret',
	});
	let copySource: unknown;
	Object.assign(service, {
		client: {
			send: async (command: {input: {CopySource?: unknown}}) => {
				copySource = command.input.CopySource;
				return {};
			},
		},
	});
	return service
		.copyObject({
			sourceBucket: 'fluxer-uploads',
			sourceKey: 'a b/c+d.pdf',
			destinationBucket: 'fluxer-cdn',
			destinationKey: 'attachments/1/2/c.pdf',
		})
		.then(() => copySource);
}

describe('StorageService.copyObject copy source', () => {
	it.each([
		['http://seaweedfs:8333', true],
		['http://seaweedfs:8333/', true],
		['https://s3.example.test', false],
	])(
		'names only the bucket and key for an endpoint without a path (%s, path style %s)',
		async (endpoint, pathStyle) => {
			await expect(copySourceFor(endpoint, pathStyle)).resolves.toBe('fluxer-uploads/a%20b/c%2Bd.pdf');
		},
	);

	it('puts the endpoint path in front of the bucket with path-style addressing', async () => {
		await expect(copySourceFor('https://s3.example.test/real%20bucket/sub/', true)).resolves.toBe(
			'real%20bucket/sub/fluxer-uploads/a%20b/c%2Bd.pdf',
		);
	});

	it('puts the endpoint path between the bucket and the key with virtual-hosted addressing', async () => {
		await expect(copySourceFor('https://s3.example.test/real%20bucket/sub', false)).resolves.toBe(
			'fluxer-uploads/real%20bucket/sub/a%20b/c%2Bd.pdf',
		);
	});
});
