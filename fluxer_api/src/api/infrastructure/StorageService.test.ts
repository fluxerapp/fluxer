// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {StorageObjectListingOverflowError} from '@app/api/infrastructure/IStorageService';
import {StorageService} from '@app/api/infrastructure/StorageService';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
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
});

function createService(endpoint = 'http://s3.test', forcePathStyle = true): StorageService {
	return new StorageService({
		endpoint,
		forcePathStyle,
		region: 'us-east-1',
		accessKeyId: 'fluxer',
		secretAccessKey: 'fluxer-secret',
	});
}

const OBJECT_URL = 'http://s3.test/fluxer-downloads/desktop/stable/manifest.json';

function serveObject(body: Uint8Array, headers: Record<string, string> = {}): void {
	server.use(http.get(OBJECT_URL, () => new HttpResponse(body, {headers})));
}

describe('StorageService.readObject', () => {
	it('refuses an object whose declared length is already over the cap', async () => {
		serveObject(new Uint8Array(64).fill(1), {'content-length': '64'});
		await expect(createService().readObject('fluxer-downloads', 'desktop/stable/manifest.json', 32)).rejects.toThrow(
			/exceeds maximum buffer size of 32 bytes \(got 64 bytes\)/u,
		);
	});

	it('refuses an object that outgrows the cap when no length is declared', async () => {
		server.use(
			http.get(OBJECT_URL, () => {
				const stream = new ReadableStream<Uint8Array>({
					start(controller) {
						controller.enqueue(new Uint8Array(24).fill(1));
						controller.enqueue(new Uint8Array(24).fill(2));
						controller.close();
					},
				});
				return new HttpResponse(stream);
			}),
		);
		await expect(createService().readObject('fluxer-downloads', 'desktop/stable/manifest.json', 32)).rejects.toThrow(
			/exceeds maximum buffer size of 32 bytes/u,
		);
	});

	it('returns every byte of an object that exactly fills the cap', async () => {
		const bytes = new Uint8Array([...Array(16).fill(7), ...Array(16).fill(9)]);
		serveObject(bytes, {'content-length': '32'});
		await expect(createService().readObject('fluxer-downloads', 'desktop/stable/manifest.json', 32)).resolves.toEqual(
			bytes,
		);
	});
});

function listingXml(keys: Array<string>, nextToken?: string | null): string {
	const contents = keys.map((key) => `<Contents><Key>${key}</Key></Contents>`).join('');
	const truncated = nextToken !== undefined;
	const next = nextToken ? `<NextContinuationToken>${nextToken}</NextContinuationToken>` : '';
	return `<?xml version="1.0" encoding="UTF-8"?><ListBucketResult><Name>fluxer-downloads</Name><Prefix>desktop/</Prefix><IsTruncated>${truncated}</IsTruncated>${next}${contents}</ListBucketResult>`;
}

function serveListing(pages: Array<string>): Array<string | null> {
	const tokens: Array<string | null> = [];
	server.use(
		http.get('http://s3.test/fluxer-downloads', ({request}) => {
			tokens.push(new URL(request.url).searchParams.get('continuation-token'));
			const page = pages[tokens.length - 1] ?? listingXml([]);
			return new HttpResponse(page, {headers: {'content-type': 'application/xml'}});
		}),
	);
	return tokens;
}

describe('StorageService.listObjects', () => {
	it('returns every page of a truncated listing', async () => {
		const tokens = serveListing([
			listingXml(['desktop/a.exe', 'desktop/b.exe'], 'page-2'),
			listingXml(['desktop/c.exe']),
		]);
		const objects = await createService().listObjects({bucket: 'fluxer-downloads', prefix: 'desktop/'});
		expect(objects.map(({key}) => key)).toEqual(['desktop/a.exe', 'desktop/b.exe', 'desktop/c.exe']);
		expect(tokens).toEqual([null, 'page-2']);
	});

	it('throws a typed overflow error when the prefix outgrows the requested cap', async () => {
		serveListing([listingXml(['desktop/a.exe', 'desktop/b.exe'], 'page-2')]);
		await expect(
			createService().listObjects({bucket: 'fluxer-downloads', prefix: 'desktop/', maxObjects: 2}),
		).rejects.toBeInstanceOf(StorageObjectListingOverflowError);
	});

	it('throws rather than returning a partial listing when the continuation token is missing', async () => {
		serveListing([listingXml(['desktop/a.exe'], null)]);
		await expect(createService().listObjects({bucket: 'fluxer-downloads', prefix: 'desktop/'})).rejects.toThrow(
			/continuation token/u,
		);
	});
});

async function copySourceFor(endpoint: string, forcePathStyle: boolean): Promise<string | null> {
	let copySource: string | null = null;
	server.use(
		http.put('*', ({request}) => {
			copySource = request.headers.get('x-amz-copy-source');
			return new HttpResponse(
				'<?xml version="1.0" encoding="UTF-8"?><CopyObjectResult><ETag>"etag"</ETag></CopyObjectResult>',
				{headers: {'content-type': 'application/xml'}},
			);
		}),
	);
	await createService(endpoint, forcePathStyle).copyObject({
		sourceBucket: 'fluxer-uploads',
		sourceKey: 'a b/c+d.pdf',
		destinationBucket: 'fluxer-cdn',
		destinationKey: 'attachments/1/2/c.pdf',
	});
	return copySource;
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
