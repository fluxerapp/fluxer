// SPDX-License-Identifier: AGPL-3.0-or-later

import {StorageService} from '@app/api/infrastructure/StorageService';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {beforeEach, describe, expect, it} from 'vitest';

const ENDPOINT = 'https://objects.ceph-rgw.test';
const UPLOADS = 'fluxer-uploads';
const CDN = 'fluxer-cdn';

interface StoredObject {
	body: Uint8Array;
	contentType: string;
	cacheControl?: string;
	contentDisposition?: string;
}

const NO_SUCH_KEY = (bucket: string) =>
	new HttpResponse(
		`<?xml version="1.0" encoding="UTF-8"?><Error><Code>NoSuchKey</Code><Message></Message><BucketName>${bucket}</BucketName><RequestId>tx0</RequestId><HostId>ceph</HostId></Error>`,
		{status: 404, headers: {'Content-Type': 'application/xml'}},
	);

function cephWithoutCrossBucketCopy() {
	const objects = new Map<string, StoredObject>();
	const copies: Array<{source: string; destination: string}> = [];
	const locate = (params: {bucket?: string | ReadonlyArray<string>; key?: string | ReadonlyArray<string>}) => {
		const bucket = String(params.bucket);
		const key = Array.isArray(params.key) ? params.key.join('/') : String(params.key);
		return {bucket, key, id: `${bucket}/${key}`};
	};
	server.use(
		http.put(`${ENDPOINT}/:bucket/*`, async ({request, params}) => {
			const target = locate({bucket: params.bucket, key: params[0] as string});
			const copySource = request.headers.get('x-amz-copy-source');
			if (copySource) {
				const decoded = decodeURIComponent(copySource.replace(/^\//u, ''));
				const sourceBucket = decoded.split('/')[0];
				copies.push({source: decoded, destination: target.id});
				if (sourceBucket !== target.bucket) {
					return NO_SUCH_KEY(target.bucket);
				}
				const source = objects.get(decoded);
				if (!source) {
					return NO_SUCH_KEY(target.bucket);
				}
				objects.set(target.id, {
					...source,
					contentType: request.headers.get('content-type') ?? source.contentType,
				});
				return new HttpResponse(
					'<?xml version="1.0" encoding="UTF-8"?><CopyObjectResult><ETag>"x"</ETag></CopyObjectResult>',
					{status: 200, headers: {'Content-Type': 'application/xml'}},
				);
			}
			const body = new Uint8Array(await request.arrayBuffer());
			objects.set(target.id, {
				body,
				contentType: request.headers.get('content-type') ?? 'application/octet-stream',
				...(request.headers.get('cache-control') ? {cacheControl: request.headers.get('cache-control')!} : {}),
				...(request.headers.get('content-disposition')
					? {contentDisposition: request.headers.get('content-disposition')!}
					: {}),
			});
			return new HttpResponse(null, {status: 200, headers: {ETag: '"x"'}});
		}),
		http.get(`${ENDPOINT}/:bucket/*`, ({params}) => {
			const target = locate({bucket: params.bucket, key: params[0] as string});
			const object = objects.get(target.id);
			if (!object) {
				return NO_SUCH_KEY(target.bucket);
			}
			return new HttpResponse(object.body, {
				status: 200,
				headers: {
					'Content-Type': object.contentType,
					'Content-Length': String(object.body.length),
					...(object.cacheControl ? {'Cache-Control': object.cacheControl} : {}),
					...(object.contentDisposition ? {'Content-Disposition': object.contentDisposition} : {}),
				},
			});
		}),
		http.head(`${ENDPOINT}/:bucket/*`, ({params}) => {
			const target = locate({bucket: params.bucket, key: params[0] as string});
			const object = objects.get(target.id);
			if (!object) {
				return new HttpResponse(null, {status: 404});
			}
			return new HttpResponse(null, {
				status: 200,
				headers: {'Content-Type': object.contentType, 'Content-Length': String(object.body.length)},
			});
		}),
	);
	return {objects, copies};
}

function storage(): StorageService {
	return new StorageService({
		endpoint: ENDPOINT,
		forcePathStyle: true,
		region: 'nbg1',
		accessKeyId: 'TEST',
		secretAccessKey: 'TEST',
	});
}

const PDF = new TextEncoder().encode('%PDF-1.7\n1 0 obj << /Type /Catalog >> endobj\n%%EOF\n');

describe('StorageService copies on providers that reject cross-bucket CopyObject', () => {
	let ceph: ReturnType<typeof cephWithoutCrossBucketCopy>;

	beforeEach(() => {
		ceph = cephWithoutCrossBucketCopy();
	});

	it('stores a non-media attachment in the CDN bucket', async () => {
		ceph.objects.set(`${UPLOADS}/upload-1`, {body: PDF, contentType: 'application/octet-stream'});

		await expect(
			storage().copyObjectWithMetadataStripping({
				sourceBucket: UPLOADS,
				sourceKey: 'upload-1',
				destinationBucket: CDN,
				destinationKey: 'attachments/1/2/file.pdf',
				contentType: 'application/pdf',
			}),
		).resolves.toBeNull();

		const stored = ceph.objects.get(`${CDN}/attachments/1/2/file.pdf`);
		expect(stored?.contentType).toBe('application/pdf');
		expect(Buffer.from(stored!.body).equals(Buffer.from(PDF))).toBe(true);
	});

	it('keeps the original file when media processing fails', async () => {
		const brokenHeic = new Uint8Array(4096).fill(7);
		ceph.objects.set(`${UPLOADS}/upload-2`, {body: brokenHeic, contentType: 'application/octet-stream'});

		await expect(
			storage().copyObjectWithMetadataStripping({
				sourceBucket: UPLOADS,
				sourceKey: 'upload-2',
				destinationBucket: CDN,
				destinationKey: 'attachments/1/3/photo.heic',
				contentType: 'image/heic',
			}),
		).resolves.toBeNull();

		const stored = ceph.objects.get(`${CDN}/attachments/1/3/photo.heic`);
		expect(stored?.contentType).toBe('image/heic');
		expect(Buffer.from(stored!.body).equals(Buffer.from(brokenHeic))).toBe(true);
	});

	it('keeps the source headers when no new content type is given', async () => {
		ceph.objects.set(`${CDN}/avatars/1/a.png`, {
			body: PDF,
			contentType: 'image/png',
			cacheControl: 'public, max-age=31536000, immutable',
			contentDisposition: 'inline',
		});

		await storage().copyObject({
			sourceBucket: CDN,
			sourceKey: 'avatars/1/a.png',
			destinationBucket: 'fluxer-reports',
			destinationKey: 'evidence/a.png',
		});

		expect(ceph.objects.get('fluxer-reports/evidence/a.png')).toMatchObject({
			contentType: 'image/png',
			cacheControl: 'public, max-age=31536000, immutable',
			contentDisposition: 'inline',
		});
	});

	it('still fails when the source object does not exist', async () => {
		await expect(
			storage().copyObject({
				sourceBucket: UPLOADS,
				sourceKey: 'missing',
				destinationBucket: CDN,
				destinationKey: 'attachments/1/4/missing.pdf',
				newContentType: 'application/pdf',
			}),
		).rejects.toMatchObject({name: 'NoSuchKey'});
		expect(ceph.objects.has(`${CDN}/attachments/1/4/missing.pdf`)).toBe(false);
	});

	it('does not retry a failed same-bucket copy through the API', async () => {
		await expect(
			storage().copyObject({
				sourceBucket: CDN,
				sourceKey: 'missing',
				destinationBucket: CDN,
				destinationKey: 'other',
				newContentType: 'image/png',
			}),
		).rejects.toMatchObject({name: 'NoSuchKey'});
		expect(ceph.copies).toEqual([{source: `${CDN}/missing`, destination: `${CDN}/other`}]);
	});
});
