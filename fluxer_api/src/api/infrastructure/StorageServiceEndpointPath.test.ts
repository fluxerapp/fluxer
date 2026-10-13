// SPDX-License-Identifier: AGPL-3.0-or-later

import {StorageService} from '@app/api/infrastructure/StorageService';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {beforeEach, describe, expect, it} from 'vitest';

const HOST = 's3.single-bucket.test';
const REAL_BUCKET = 'realbucket';

function errorResponse(code: string, status: number): HttpResponse<string> {
	return new HttpResponse(
		`<?xml version="1.0" encoding="UTF-8"?><Error><Code>${code}</Code><Message></Message><RequestId>tx0</RequestId></Error>`,
		{status, headers: {'Content-Type': 'application/xml'}},
	);
}

function splitBucket(path: string): {bucket: string; key: string} {
	const [bucket = '', ...key] = path.split('/');
	return {bucket, key: key.join('/')};
}

function singleBucketStore(unresolvedSourceCode = 'NoSuchBucket') {
	const objects = new Map<string, {body: Uint8Array; contentType: string}>();
	const resolve = (url: URL): {bucket: string; key: string} => {
		const path = decodeURIComponent(url.pathname.slice(1));
		if (url.hostname === HOST) {
			return splitBucket(path);
		}
		return {bucket: url.hostname.slice(0, -HOST.length - 1), key: path};
	};
	server.use(
		http.all(/^https:\/\/([a-z0-9-]+\.)?s3\.single-bucket\.test\//u, async ({request}) => {
			const target = resolve(new URL(request.url));
			if (target.bucket !== REAL_BUCKET) {
				return errorResponse('NoSuchBucket', 404);
			}
			if (request.method === 'GET') {
				const object = objects.get(target.key);
				if (!object) {
					return errorResponse('NoSuchKey', 404);
				}
				return new HttpResponse(object.body, {
					status: 200,
					headers: {'Content-Type': object.contentType, 'Content-Length': String(object.body.length)},
				});
			}
			if (request.method !== 'PUT') {
				return errorResponse('NotImplemented', 501);
			}
			const copySource = request.headers.get('x-amz-copy-source');
			if (!copySource) {
				objects.set(target.key, {
					body: new Uint8Array(await request.arrayBuffer()),
					contentType: request.headers.get('content-type') ?? 'application/octet-stream',
				});
				return new HttpResponse(null, {status: 200, headers: {ETag: '"x"'}});
			}
			const source = splitBucket(decodeURIComponent(copySource.replace(/^\//u, '')));
			if (source.bucket !== REAL_BUCKET) {
				return errorResponse(unresolvedSourceCode, unresolvedSourceCode === 'AccessDenied' ? 403 : 404);
			}
			const object = objects.get(source.key);
			if (!object) {
				return errorResponse('NoSuchKey', 404);
			}
			objects.set(target.key, {...object, contentType: request.headers.get('content-type') ?? object.contentType});
			return new HttpResponse(
				'<?xml version="1.0" encoding="UTF-8"?><CopyObjectResult><ETag>"x"</ETag></CopyObjectResult>',
				{status: 200, headers: {'Content-Type': 'application/xml'}},
			);
		}),
	);
	return objects;
}

function storage(endpoint: string, forcePathStyle: boolean): StorageService {
	return new StorageService({
		endpoint,
		forcePathStyle,
		region: 'us-east-1',
		accessKeyId: 'TEST',
		secretAccessKey: 'TEST',
	});
}

const PDF = new TextEncoder().encode('%PDF-1.7\n1 0 obj << /Type /Catalog >> endobj\n%%EOF\n');

describe('StorageService with every bucket inside one real bucket', () => {
	let objects: ReturnType<typeof singleBucketStore>;

	beforeEach(() => {
		objects = singleBucketStore();
	});

	it('copies a non-media attachment when the endpoint names the bucket with path-style addressing', async () => {
		const service = storage(`https://${HOST}/${REAL_BUCKET}`, true);
		await service.uploadObject({bucket: 'fluxer-uploads', key: 'upload 1', body: PDF});

		await expect(
			service.copyObjectWithMetadataStripping({
				sourceBucket: 'fluxer-uploads',
				sourceKey: 'upload 1',
				destinationBucket: 'fluxer-cdn',
				destinationKey: 'attachments/1/2/file.pdf',
				contentType: 'application/pdf',
			}),
		).resolves.toBeNull();

		expect(objects.get('fluxer-cdn/attachments/1/2/file.pdf')?.contentType).toBe('application/pdf');
		await expect(service.readObject('fluxer-cdn', 'attachments/1/2/file.pdf')).resolves.toEqual(PDF);
	});

	it('copies within a bucket whose virtual host keeps the endpoint path as a key prefix', async () => {
		const service = storage(`https://${HOST}/prefix`, false);
		await service.uploadObject({bucket: REAL_BUCKET, key: 'upload-2', body: PDF});

		await service.copyObject({
			sourceBucket: REAL_BUCKET,
			sourceKey: 'upload-2',
			destinationBucket: REAL_BUCKET,
			destinationKey: 'attachments/1/3/file.pdf',
			newContentType: 'application/pdf',
		});

		expect(objects.get('prefix/attachments/1/3/file.pdf')?.contentType).toBe('application/pdf');
		await expect(service.readObject(REAL_BUCKET, 'attachments/1/3/file.pdf')).resolves.toEqual(PDF);
	});
});

describe.each(['NoSuchBucket', 'AccessDenied'])(
	'StorageService with the bucket in the endpoint hostname, store answers %s',
	(code) => {
		let objects: ReturnType<typeof singleBucketStore>;

		beforeEach(() => {
			objects = singleBucketStore(code);
		});

		it('copies a non-media attachment out of the uploads prefix', async () => {
			const service = storage(`https://${REAL_BUCKET}.${HOST}`, true);
			await service.uploadObject({bucket: 'fluxer-uploads', key: 'upload 1', body: PDF});

			await expect(
				service.copyObjectWithMetadataStripping({
					sourceBucket: 'fluxer-uploads',
					sourceKey: 'upload 1',
					destinationBucket: 'fluxer',
					destinationKey: 'attachments/1/2/file.pdf',
					contentType: 'application/pdf',
				}),
			).resolves.toBeNull();

			expect(objects.get('fluxer/attachments/1/2/file.pdf')).toEqual({body: PDF, contentType: 'application/pdf'});
		});

		it('copies within the cdn prefix', async () => {
			const service = storage(`https://${REAL_BUCKET}.${HOST}`, true);
			await service.uploadObject({bucket: 'fluxer', key: 'emojis/1', body: PDF, contentType: 'image/png'});

			await service.copyObject({
				sourceBucket: 'fluxer',
				sourceKey: 'emojis/1',
				destinationBucket: 'fluxer',
				destinationKey: 'emojis/2',
			});

			expect(objects.get('fluxer/emojis/2')).toEqual({body: PDF, contentType: 'image/png'});
		});

		it('reports a missing source as a missing object', async () => {
			const service = storage(`https://${REAL_BUCKET}.${HOST}`, true);

			await expect(
				service.copyObject({
					sourceBucket: 'fluxer',
					sourceKey: 'emojis/missing',
					destinationBucket: 'fluxer',
					destinationKey: 'emojis/3',
				}),
			).rejects.toMatchObject({name: 'NoSuchKey'});
			expect(objects.has('fluxer/emojis/3')).toBe(false);
		});
	},
);
