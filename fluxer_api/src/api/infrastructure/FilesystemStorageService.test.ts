// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {Readable} from 'node:stream';
import {fileURLToPath} from 'node:url';
import {
	EXTENSION_CONTENT_TYPES,
	FilesystemStorageLayoutError,
	inferContentType,
	isValidStorageBucket,
	isValidStorageKey,
	mapKey,
	mapSegment,
	needsSidecar,
	partFileName,
	writeLayoutSync,
} from '@app/api/infrastructure/FilesystemStorageLayout';
import {FilesystemStorageService} from '@app/api/infrastructure/FilesystemStorageService';
import {type StorageSpaceStats, storageSpace} from '@app/api/infrastructure/FilesystemStorageSpace';
import {
	isStorageErrorRecoverable,
	isStorageObjectMissing,
	StorageFullError,
	StorageObjectRangeNotSatisfiableError,
	StorageReadOnlyError,
} from '@app/api/infrastructure/IStorageService';
import {migrateS3ToFilesystem, type StorageMigrationSource} from '@app/api/migrate/S3FilesystemMigration';
import {sweepFilesystemTree} from '@app/api/worker/tasks/SweepFilesystemStorage';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

interface Vectors {
	segment_mapping: {
		segments: Array<{segment: string; mapped: string}>;
		keys: Array<{key: string; path: string}>;
	};
	invalid_keys: Array<{key: string}>;
	valid_keys: Array<string>;
	invalid_buckets: Array<{bucket: string}>;
	extension_table: {types: Record<string, string>; cases: Array<{key: string; content_type: string | null}>};
	sidecar: Array<{key: string; stored_content_type: string | null; sidecar: boolean}>;
}

const vectors = JSON.parse(
	fs.readFileSync(
		path.resolve(
			path.dirname(fileURLToPath(import.meta.url)),
			'../../../../fluxer_common/src/testdata/filesystem_storage_vectors.json',
		),
		'utf8',
	),
) as Vectors;

const BUCKETS = {cdn: 'cdn', uploads: 'uploads', reports: 'reports', harvests: 'harvests'};
const LONG_CJK_NAME = `${'漢'.repeat(120)}.png`;

async function text(stream: Readable): Promise<string> {
	const chunks: Array<Buffer> = [];
	for await (const chunk of stream) chunks.push(chunk as Buffer);
	return Buffer.concat(chunks).toString('utf8');
}

async function caught(promise: Promise<unknown>): Promise<unknown> {
	try {
		await promise;
	} catch (error) {
		return error;
	}
	throw new Error('expected a rejection');
}

let root: string;
let storage: FilesystemStorageService;

beforeEach(() => {
	root = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-fs-storage-'));
	writeLayoutSync(root);
	storage = new FilesystemStorageService({root, buckets: BUCKETS, readOnly: false});
});

afterEach(() => {
	fs.rmSync(root, {recursive: true, force: true});
});

describe('shared layout vectors', () => {
	it('maps segments and keys the way media-proxy does', () => {
		for (const {segment, mapped} of vectors.segment_mapping.segments) {
			expect(mapSegment(segment)).toBe(mapped);
		}
		for (const {key, path: mappedPath} of vectors.segment_mapping.keys) {
			expect(mapKey(key).path).toBe(mappedPath);
		}
	});

	it('validates keys and buckets', () => {
		for (const {key} of vectors.invalid_keys) expect(isValidStorageKey(key)).toBe(false);
		for (const key of vectors.valid_keys) expect(isValidStorageKey(key)).toBe(true);
		for (const {bucket} of vectors.invalid_buckets) expect(isValidStorageBucket(bucket)).toBe(false);
	});

	it('infers types and decides sidecars with the media-proxy table', () => {
		expect(EXTENSION_CONTENT_TYPES).toEqual(vectors.extension_table.types);
		for (const {key, content_type} of vectors.extension_table.cases) {
			expect(inferContentType(key)).toBe(content_type);
		}
		for (const {key, stored_content_type, sidecar} of vectors.sidecar) {
			expect(needsSidecar(key, stored_content_type, mapKey(key).mapped)).toBe(sidecar);
		}
	});
});

describe('FilesystemStorageService', () => {
	it('refuses an uploads bucket shared with another bucket', () => {
		expect(() => new FilesystemStorageService({root, buckets: {...BUCKETS, uploads: 'cdn'}, readOnly: false})).toThrow(
			FilesystemStorageLayoutError,
		);
		expect(
			() => new FilesystemStorageService({root, buckets: {...BUCKETS, uploads: 'fluxer-geoip'}, readOnly: false}),
		).toThrow(FilesystemStorageLayoutError);
	});

	it('refuses a root without the layout marker', () => {
		const bare = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-fs-bare-'));
		try {
			expect(() => new FilesystemStorageService({root: bare, buckets: BUCKETS, readOnly: false})).toThrow(
				FilesystemStorageLayoutError,
			);
		} finally {
			fs.rmSync(bare, {recursive: true, force: true});
		}
	});

	it('treats invalid keys as missing objects', async () => {
		await storage.uploadObject({bucket: 'cdn', key: 'other/a.png', body: Buffer.from('secret')});
		for (const {key} of vectors.invalid_keys) {
			expect(await storage.getObjectMetadata('uploads', key)).toBeNull();
			expect(await storage.streamObject({bucket: 'uploads', key})).toBeNull();
			expect(isStorageObjectMissing(await caught(storage.readObject('uploads', key)))).toBe(true);
			expect(
				isStorageObjectMissing(
					await caught(
						storage.copyObject({
							sourceBucket: 'uploads',
							sourceKey: key,
							destinationBucket: 'cdn',
							destinationKey: 'x',
						}),
					),
				),
			).toBe(true);
			await storage.deleteObject('uploads', key);
		}
		expect(await storage.getObjectMetadata('uploads', '../cdn/other/a.png')).toBeNull();
		expect(await storage.getObjectMetadata('uploads', 'x'.repeat(4096))).toBeNull();
		expect(await storage.getObjectMetadata('uploads', 'a/b')).toBeNull();
	});

	it('reports a missing object the same way on every throwing path', async () => {
		const missing = [
			await caught(storage.readObject('uploads', 'nope')),
			await caught(storage.computeObjectSha256('uploads', 'nope')),
			await caught(storage.writeObjectToDisk('uploads', 'nope', path.join(root, 'out'))),
			await caught(
				storage.copyObject({sourceBucket: 'uploads', sourceKey: 'nope', destinationBucket: 'cdn', destinationKey: 'x'}),
			),
		];
		for (const error of missing) {
			expect(isStorageObjectMissing(error)).toBe(true);
			expect((error as {code?: string}).code).toBe('NoSuchKey');
		}
	});

	it('keeps old bytes for a reader open across an overwrite', async () => {
		await storage.uploadObject({bucket: 'uploads', key: 'p.jpg', body: Buffer.from('old bytes')});
		const open = await storage.streamObject({bucket: 'uploads', key: 'p.jpg'});
		await storage.uploadObject({bucket: 'uploads', key: 'p.jpg', body: Buffer.from('new bytes!')});
		expect(await text(open!.body)).toBe('old bytes');
		expect(Buffer.from(await storage.readObject('uploads', 'p.jpg')).toString()).toBe('new bytes!');
	});

	it('keeps a copy unchanged when the source is overwritten, and gives report copies a fresh mtime', async () => {
		await storage.uploadObject({bucket: 'uploads', key: 'u1', body: Buffer.from('first'), contentType: 'image/png'});
		const past = new Date(Date.now() - 86_400_000);
		fs.utimesSync(path.join(root, 'uploads', 'u1'), past, past);
		await storage.copyObject({
			sourceBucket: 'uploads',
			sourceKey: 'u1',
			destinationBucket: 'cdn',
			destinationKey: 'attachments/1/2/a.png',
		});
		await storage.copyObject({
			sourceBucket: 'uploads',
			sourceKey: 'u1',
			destinationBucket: 'reports',
			destinationKey: 'reports/9/a.png',
		});
		await storage.uploadObject({bucket: 'uploads', key: 'u1', body: Buffer.from('second')});
		expect(Buffer.from(await storage.readObject('cdn', 'attachments/1/2/a.png')).toString()).toBe('first');
		expect(Buffer.from(await storage.readObject('reports', 'reports/9/a.png')).toString()).toBe('first');
		const report = await storage.getObjectMetadata('reports', 'reports/9/a.png');
		expect(report!.lastModified!.getTime()).toBeGreaterThan(past.getTime() + 60_000);
		expect(report!.contentType).toBe('image/png');
		expect(fs.existsSync(path.join(root, '.meta', 'cdn', 'attachments/1/2/a.png'))).toBe(false);
	});

	it('serves single ranges and rejects unsatisfiable ones', async () => {
		await storage.uploadObject({bucket: 'cdn', key: 'r', body: Buffer.from('0123456789abcdef')});
		const first = await storage.streamObject({bucket: 'cdn', key: 'r', range: 'bytes=0-0'});
		expect([await text(first!.body), first!.contentRange, first!.contentLength]).toEqual(['0', 'bytes 0-0/16', 1]);
		const suffix = await storage.streamObject({bucket: 'cdn', key: 'r', range: 'bytes=-5'});
		expect([await text(suffix!.body), suffix!.contentRange]).toEqual(['bcdef', 'bytes 11-15/16']);
		const open = await storage.streamObject({bucket: 'cdn', key: 'r', range: 'bytes=10-'});
		expect([await text(open!.body), open!.contentRange]).toEqual(['abcdef', 'bytes 10-15/16']);
		await expect(storage.streamObject({bucket: 'cdn', key: 'r', range: 'bytes=16-'})).rejects.toBeInstanceOf(
			StorageObjectRangeNotSatisfiableError,
		);
	});

	it('refuses to buffer an object over the limit with the message callers match', async () => {
		await storage.uploadObject({bucket: 'uploads', key: 'big', body: Buffer.alloc(32)});
		await expect(storage.readObject('uploads', 'big', 16)).rejects.toThrow(/^Stream exceeds maximum buffer size/);
	});

	it('keeps a long CJK file name through upload, list and stream', async () => {
		const key = `attachments/1/2/${LONG_CJK_NAME}`;
		await storage.uploadObject({bucket: 'cdn', key, body: Readable.from([Buffer.from('cjk')])});
		expect(await storage.listObjects({bucket: 'cdn', prefix: 'attachments/1/'})).toEqual([
			{key, lastModified: expect.any(Date)},
		]);
		const streamed = await storage.streamObject({bucket: 'cdn', key});
		expect([await text(streamed!.body), streamed!.contentType]).toEqual(['cjk', 'image/png']);
		await storage.deleteObject('cdn', key);
		expect(await storage.listObjects({bucket: 'cdn', prefix: ''})).toEqual([]);
	});

	it('joins multipart uploads written in the relay part format', async () => {
		const {uploadId} = await storage.createMultipartUpload({bucket: 'uploads', key: 'mp', contentType: 'video/mp4'});
		const dir = path.join(root, '.staging', 'multipart', uploadId);
		fs.writeFileSync(path.join(dir, partFileName(1)), 'hello ');
		fs.writeFileSync(path.join(dir, partFileName(2)), 'world');
		const parts = await storage.listParts({bucket: 'uploads', key: 'mp', uploadId});
		expect(parts.map((part) => [part.partNumber, part.size])).toEqual([
			[1, 6],
			[2, 5],
		]);
		await expect(
			storage.completeMultipartUpload({bucket: 'uploads', key: 'mp', uploadId, parts: parts.slice(0, 1)}),
		).rejects.toMatchObject({name: 'InvalidPart'});
		expect(fs.existsSync(dir)).toBe(false);

		const second = await storage.createMultipartUpload({bucket: 'uploads', key: 'mp', contentType: 'video/mp4'});
		const secondDir = path.join(root, '.staging', 'multipart', second.uploadId);
		fs.writeFileSync(path.join(secondDir, partFileName(1)), 'hello ');
		fs.writeFileSync(path.join(secondDir, partFileName(2)), 'world');
		const secondParts = await storage.listParts({bucket: 'uploads', key: 'mp', uploadId: second.uploadId});
		await storage.completeMultipartUpload({
			bucket: 'uploads',
			key: 'mp',
			uploadId: second.uploadId,
			parts: secondParts,
		});
		expect(Buffer.from(await storage.readObject('uploads', 'mp')).toString()).toBe('hello world');
		expect((await storage.getObjectMetadata('uploads', 'mp'))!.contentType).toBe('video/mp4');
		const late = path.join(root, '.staging', 'tmp', 'late-part');
		fs.writeFileSync(late, 'late');
		expect(() => fs.renameSync(late, path.join(secondDir, partFileName(3)))).toThrow(/ENOENT/);
		expect(fs.readdirSync(path.join(root, '.staging', 'trash'))).toEqual([]);

		await expect(storage.listParts({bucket: 'uploads', key: 'mp', uploadId: 'f'.repeat(32)})).rejects.toMatchObject({
			name: 'NoSuchUpload',
		});
		const third = await storage.createMultipartUpload({bucket: 'uploads', key: 'mp3'});
		await storage.abortMultipartUpload({bucket: 'uploads', key: 'mp3', uploadId: third.uploadId});
		await storage.abortMultipartUpload({bucket: 'uploads', key: 'mp3', uploadId: third.uploadId});
	});

	it('refuses writes, copies and deletes while read-only', async () => {
		await storage.uploadObject({bucket: 'cdn', key: 'kept', body: Buffer.from('kept')});
		const readOnly = new FilesystemStorageService({root, buckets: BUCKETS, readOnly: true});
		await expect(readOnly.uploadObject({bucket: 'cdn', key: 'n', body: Buffer.from('n')})).rejects.toBeInstanceOf(
			StorageReadOnlyError,
		);
		await expect(readOnly.deleteObject('cdn', 'kept')).rejects.toBeInstanceOf(StorageReadOnlyError);
		await expect(
			readOnly.copyObject({sourceBucket: 'cdn', sourceKey: 'kept', destinationBucket: 'cdn', destinationKey: 'c'}),
		).rejects.toBeInstanceOf(StorageReadOnlyError);
		expect(Buffer.from(await readOnly.readObject('cdn', 'kept')).toString()).toBe('kept');
	});
});

function statsFor(freeInodes: number, freeBlocks = 900_000): StorageSpaceStats {
	return {bsize: 4096, blocks: 1_000_000, bavail: freeBlocks, files: 1_000_000, ffree: freeInodes};
}

describe('filesystem storage space', () => {
	it('warns under 10% free and stops writes under 1% or 10,000 free, whichever is fewer', () => {
		expect(storageSpace('/s', statsFor(100_000)).level).toBe('ok');
		expect(storageSpace('/s', statsFor(99_999)).level).toBe('low');
		expect(storageSpace('/s', statsFor(10_000)).level).toBe('low');
		expect(storageSpace('/s', statsFor(9_999)).level).toBe('full');
		expect(storageSpace('/s', {...statsFor(0), files: 100_000_000, ffree: 9_999}).level).toBe('full');
		expect(storageSpace('/s', {...statsFor(0), files: 100_000_000, ffree: 10_000}).level).toBe('low');
		expect(storageSpace('/s', {...statsFor(0), files: 0, ffree: 0}).level).toBe('ok');
		expect(storageSpace('/s', statsFor(500_000, 99_999)).level).toBe('low');
		expect(storageSpace('/s', statsFor(500_000, 9_999)).level).toBe('full');
		expect(storageSpace('/s', statsFor(50_000)).detail).toBe(
			'/s has 50000 of 1000000 inodes free (5.0%) and 3.7 GB of 4.1 GB free (90.0%)',
		);
	});

	it('logs when room runs low, refuses writes when almost full, and lets them through again', async () => {
		const now = vi.spyOn(Date, 'now');
		let clock = 1_000_000;
		now.mockImplementation(() => clock);
		let stats = statsFor(500_000);
		const logger = {info: vi.fn(), warn: vi.fn(), error: vi.fn()} as unknown as LoggerInterface;
		const watched = new FilesystemStorageService({
			root,
			buckets: BUCKETS,
			readOnly: false,
			logger,
			statfs: async () => stats as fs.StatsFs,
		});
		try {
			await watched.uploadObject({bucket: 'cdn', key: 'a', body: Buffer.from('a')});
			stats = statsFor(50_000);
			clock += 31_000;
			await watched.uploadObject({bucket: 'cdn', key: 'b', body: Buffer.from('b')});
			expect(logger.warn).toHaveBeenCalledWith(
				expect.objectContaining({freeInodes: 50_000}),
				expect.stringContaining('Storage is running low'),
			);
			stats = statsFor(5_000);
			clock += 31_000;
			const refused = await caught(watched.uploadObject({bucket: 'cdn', key: 'c', body: Buffer.from('c')}));
			expect(refused).toBeInstanceOf(StorageFullError);
			expect(isStorageErrorRecoverable(refused)).toBe(true);
			expect(logger.error).toHaveBeenCalledTimes(1);
			expect(fs.existsSync(path.join(root, 'cdn', 'c'))).toBe(false);
			await watched.deleteObject('cdn', 'a');
			stats = statsFor(500_000);
			clock += 31_000;
			await watched.uploadObject({bucket: 'cdn', key: 'c', body: Buffer.from('c')});
			expect(logger.info).toHaveBeenCalledWith(expect.anything(), expect.stringContaining('Storage has room again'));
		} finally {
			now.mockRestore();
		}
	});

	it('turns an out of space error from the disk into a storage full error and leaves no staged file', async () => {
		const open = vi
			.spyOn(fs.promises, 'open')
			.mockRejectedValueOnce(Object.assign(new Error('no space'), {code: 'ENOSPC'}));
		try {
			const error = await caught(storage.uploadObject({bucket: 'cdn', key: 'full', body: Buffer.from('x')}));
			expect(error).toBeInstanceOf(StorageFullError);
			expect((error as Error).message).toMatch(/^The upload was refused because storage is almost full\. /);
		} finally {
			open.mockRestore();
		}
		expect(fs.readdirSync(path.join(root, '.staging', 'tmp'))).toEqual([]);
		expect(fs.existsSync(path.join(root, 'cdn', 'full'))).toBe(false);
	});
});

describe('filesystem storage sweep', () => {
	it('expires old uploads only, and leaves the CDN copy of an expired upload', async () => {
		await storage.uploadObject({bucket: 'uploads', key: 'old', body: Buffer.from('old')});
		await storage.uploadObject({bucket: 'uploads', key: 'new', body: Buffer.from('new')});
		await storage.copyObject({
			sourceBucket: 'uploads',
			sourceKey: 'old',
			destinationBucket: 'cdn',
			destinationKey: 'c',
		});
		const past = new Date(Date.now() - 2 * 86_400_000);
		fs.utimesSync(path.join(root, 'uploads', 'old'), past, past);
		const result = await sweepFilesystemTree({root, buckets: BUCKETS, uploadsTtlMs: 86_400_000, now: Date.now()});
		expect(result.expiredUploads).toBe(1);
		expect(await storage.getObjectMetadata('uploads', 'old')).toBeNull();
		expect(await storage.getObjectMetadata('uploads', 'new')).not.toBeNull();
		expect(Buffer.from(await storage.readObject('cdn', 'c')).toString()).toBe('old');
	});

	it('keeps a staged link to an old object while a copy is in flight', async () => {
		await storage.uploadObject({bucket: 'cdn', key: 'old', body: Buffer.from('old')});
		const past = new Date(Date.now() - 7 * 3_600_000);
		fs.utimesSync(path.join(root, 'cdn', 'old'), past, past);
		const staged = path.join(root, '.staging', 'tmp', 'in-flight');
		fs.linkSync(path.join(root, 'cdn', 'old'), staged);
		const result = await sweepFilesystemTree({root, buckets: BUCKETS, uploadsTtlMs: 86_400_000, now: Date.now()});
		expect(result.staleStaging).toBe(0);
		expect(fs.existsSync(staged)).toBe(true);
	});
});

describe('S3 to filesystem import', () => {
	it('copies, skips and verifies', async () => {
		const target = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-fs-import-'));
		const longKey = `attachments/1/2/${LONG_CJK_NAME}`;
		const objects: Record<string, Record<string, {body: string; type?: string; modified: Date}>> = {
			cdn: {
				'a/b': {body: 'object', modified: new Date()},
				'a/b-x': {body: 'sibling', modified: new Date()},
				'a/b/c': {body: 'under an object', modified: new Date()},
				[longKey]: {body: 'long', type: 'image/png', modified: new Date()},
				noext: {body: 'typed', type: 'image/webp', modified: new Date('2024-01-02T03:04:05Z')},
				'.uploads/x': {body: 'weed staging', modified: new Date()},
				'dir/': {body: '', modified: new Date()},
			},
			uploads: {
				fresh: {body: 'fresh', modified: new Date()},
				stale: {body: 'stale', modified: new Date(Date.now() - 3 * 86_400_000)},
			},
			reports: {},
			harvests: {},
		};
		const source: StorageMigrationSource = {
			async *list(bucket) {
				const keys = Object.keys(objects[bucket] ?? {}).sort((a, b) => Buffer.compare(Buffer.from(a), Buffer.from(b)));
				for (const key of keys) {
					const object = objects[bucket]![key]!;
					yield {key, size: Buffer.byteLength(object.body), lastModified: object.modified};
				}
			},
			async read(bucket, key) {
				const object = objects[bucket]![key]!;
				return {
					body: Readable.from([Buffer.from(object.body)]),
					...(object.type ? {contentType: object.type} : {}),
					lastModified: object.modified,
				};
			},
			async hasBucket(bucket) {
				return bucket in objects;
			},
		};
		const options = {root: target, verifyOnly: false, samplePercent: 1, sampleMin: 200, uploadsTtlMs: 86_400_000};
		const output = {info: () => undefined, report: () => undefined};
		try {
			const report = await migrateS3ToFilesystem({options, source, buckets: BUCKETS, output});
			expect(report.verified).toBe(true);
			expect(report.skipped.map(({key, reason}) => [key, reason]).sort()).toEqual(
				[
					['.uploads/x', 'uploads-prefix'],
					['dir/', 'trailing-slash'],
					['a/b/c', 'object-and-prefix'],
					['stale', 'expired-upload'],
				].sort(),
			);
			const imported = new FilesystemStorageService({root: target, buckets: BUCKETS, readOnly: true});
			expect((await imported.getObjectMetadata('cdn', 'noext'))!.contentType).toBe('image/webp');
			expect((await imported.getObjectMetadata('cdn', 'noext'))!.lastModified!.toISOString()).toBe(
				'2024-01-02T03:04:05.000Z',
			);
			expect((await imported.listObjects({bucket: 'cdn', prefix: 'attachments/'})).map(({key}) => key)).toEqual([
				longKey,
			]);
			const again = await migrateS3ToFilesystem({
				options: {...options, verifyOnly: true},
				source,
				buckets: BUCKETS,
				output,
			});
			expect(again.verified).toBe(true);
			objects.cdn!['a/b-x']!.body = 'changed';
			const changed = await migrateS3ToFilesystem({
				options: {...options, verifyOnly: true},
				source,
				buckets: BUCKETS,
				output,
			});
			expect(changed.verified).toBe(false);
		} finally {
			fs.rmSync(target, {recursive: true, force: true});
		}
	});
});
