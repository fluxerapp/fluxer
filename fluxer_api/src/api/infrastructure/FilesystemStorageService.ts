// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import fs from 'node:fs';
import type {FileHandle} from 'node:fs/promises';
import path from 'node:path';
import type {Readable} from 'node:stream';
import {
	assertStorageBuckets,
	checkLayoutSync,
	DEFAULT_CONTENT_TYPE,
	DIRECTORY_MODE,
	decodeSidecar,
	decodeUploadManifest,
	encodeSidecar,
	FILE_MODE,
	FilesystemStorageLayoutError,
	formatEtag,
	inferContentType,
	isValidStorageBucket,
	isValidStorageKey,
	MAX_PART_NUMBER,
	META_DIR,
	MULTIPART_MANIFEST,
	mapKey,
	mapSegment,
	mayBeMappedSegment,
	needsSidecar,
	PART_NAME_PATTERN,
	parseRange,
	partFileName,
	prepareStagingSync,
	STAGING_DIR,
	STAGING_MULTIPART,
	STAGING_TMP,
	STAGING_TRASH,
	type StorageBuckets,
	UPLOAD_ID_PATTERN,
	type UploadManifest,
} from '@app/api/infrastructure/FilesystemStorageLayout';
import {type StorageSpace, storageSpace} from '@app/api/infrastructure/FilesystemStorageSpace';
import {
	type IStorageService,
	StorageFullError,
	StorageObjectListingOverflowError,
	StorageObjectNotFoundError,
	StorageObjectRangeNotSatisfiableError,
	StorageReadOnlyError,
	StorageUploadNotFoundError,
} from '@app/api/infrastructure/IStorageService';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';

const MISSING_CODES = new Set(['ENOENT', 'ENOTDIR', 'ENAMETOOLONG', 'EISDIR', 'ELOOP']);
const READ_FLAGS = fs.constants.O_RDONLY | fs.constants.O_NOFOLLOW;
const CREATE_FLAGS = fs.constants.O_WRONLY | fs.constants.O_CREAT | fs.constants.O_EXCL | fs.constants.O_NOFOLLOW;
const FSYNC_CONCURRENCY = 8;
const COPY_CHUNK_BYTES = 1024 * 1024;
const SPACE_CHECK_MS = 30_000;
const SPACE_REMIND_MS = 60 * 60 * 1000;
const FULL_CODES = new Set(['ENOSPC', 'EDQUOT']);

export interface FilesystemStorageOptions {
	root: string;
	buckets: StorageBuckets;
	readOnly: boolean;
	requireLayout?: boolean;
	logger?: LoggerInterface;
	statfs?: (root: string) => Promise<fs.StatsFs>;
}

interface ResolvedObject {
	bucket: string;
	key: string;
	data: string;
	meta: string;
	mapped: boolean;
}

interface StoredObject {
	handle: FileHandle;
	size: number;
	mtimeNs: bigint;
	mtime: Date;
}

function isMissing(error: unknown): boolean {
	return MISSING_CODES.has((error as NodeJS.ErrnoException | undefined)?.code ?? '');
}

async function ignoreMissing<T>(operation: Promise<T>): Promise<T | null> {
	try {
		return await operation;
	} catch (error) {
		if (isMissing(error)) return null;
		throw error;
	}
}

class Semaphore {
	private available: number;
	private readonly waiters: Array<() => void> = [];

	constructor(size: number) {
		this.available = size;
	}

	async run<T>(task: () => Promise<T>): Promise<T> {
		if (this.available > 0) {
			this.available -= 1;
		} else {
			await new Promise<void>((resolve) => this.waiters.push(resolve));
		}
		try {
			return await task();
		} finally {
			const next = this.waiters.shift();
			if (next) next();
			else this.available += 1;
		}
	}
}

export class FilesystemStorageService implements IStorageService {
	readonly root: string;
	private readonly buckets: StorageBuckets;
	private readonly readOnly: boolean;
	private readonly tmpDir: string;
	private readonly multipartDir: string;
	private readonly trashDir: string;
	private readonly fsyncs = new Semaphore(FSYNC_CONCURRENCY);
	private readonly logger: LoggerInterface | undefined;
	private readonly statfs: (root: string) => Promise<fs.StatsFs>;
	private space: StorageSpace | null = null;
	private spaceCheckedAt = 0;
	private spaceLoggedAt = 0;
	private spaceCheck: Promise<StorageSpace | null> | null = null;

	constructor(options: FilesystemStorageOptions) {
		if (!path.isAbsolute(options.root)) {
			throw new FilesystemStorageLayoutError(`FLUXER_STORAGE_ROOT must be an absolute path, got ${options.root}`);
		}
		assertStorageBuckets(options.buckets);
		if (options.requireLayout !== false) {
			checkLayoutSync(options.root);
		}
		prepareStagingSync(options.root);
		this.root = options.root;
		this.buckets = options.buckets;
		this.readOnly = options.readOnly;
		this.tmpDir = path.join(options.root, STAGING_DIR, STAGING_TMP);
		this.multipartDir = path.join(options.root, STAGING_DIR, STAGING_MULTIPART);
		this.trashDir = path.join(options.root, STAGING_DIR, STAGING_TRASH);
		this.logger = options.logger;
		this.statfs = options.statfs ?? ((root) => fs.promises.statfs(root));
		if (this.logger) this.checkSpace().catch(() => undefined);
	}

	async checkSpace(): Promise<StorageSpace | null> {
		this.spaceCheck ??= this.measureSpace().finally(() => {
			this.spaceCheck = null;
		});
		return this.spaceCheck;
	}

	private async measureSpace(): Promise<StorageSpace | null> {
		let next: StorageSpace;
		try {
			next = storageSpace(this.root, await this.statfs(this.root));
		} catch (error) {
			this.logger?.warn({error, root: this.root}, 'Could not read how much room the storage folder has left');
			this.spaceCheckedAt = Date.now();
			return this.space;
		}
		const previous = this.space?.level ?? 'ok';
		this.space = next;
		this.spaceCheckedAt = Date.now();
		this.logSpace(previous, next);
		return next;
	}

	private logSpace(previous: StorageSpace['level'], next: StorageSpace): void {
		const now = Date.now();
		const remind = next.level !== 'ok' && now - this.spaceLoggedAt >= SPACE_REMIND_MS;
		if (next.level === previous && !remind) return;
		this.spaceLoggedAt = now;
		const fields = {
			root: this.root,
			freeInodes: next.freeInodes,
			totalInodes: next.totalInodes,
			freeBytes: next.freeBytes,
			totalBytes: next.totalBytes,
		};
		if (next.level === 'full') {
			this.logger?.error(
				fields,
				`Storage is almost full, so new uploads are refused until room is freed. ${next.detail}.`,
			);
		} else if (next.level === 'low') {
			this.logger?.warn(
				fields,
				`Storage is running low. ${next.detail}. Each upload uses one inode and some disk space.`,
			);
		} else {
			this.logger?.info(fields, `Storage has room again. ${next.detail}.`);
		}
	}

	private async writing<T>(operation: string, write: () => Promise<T>): Promise<T> {
		this.assertWritable(operation);
		const space = Date.now() - this.spaceCheckedAt >= SPACE_CHECK_MS ? await this.checkSpace() : this.space;
		if (space?.level === 'full') throw new StorageFullError(operation, space.detail);
		try {
			return await write();
		} catch (error) {
			if (error instanceof StorageFullError) throw error;
			if (!FULL_CODES.has((error as NodeJS.ErrnoException | undefined)?.code ?? '')) throw error;
			this.spaceCheckedAt = 0;
			const measured = await this.checkSpace();
			throw new StorageFullError(operation, measured?.detail ?? `${this.root} has no room left`, {cause: error});
		}
	}

	private resolve(bucket: string, key: string): ResolvedObject | null {
		if (!isValidStorageBucket(bucket) || !isValidStorageKey(key)) return null;
		const {path: mapped, mapped: wasMapped} = mapKey(key);
		return {
			bucket,
			key,
			data: path.join(this.root, bucket, mapped),
			meta: path.join(this.root, META_DIR, bucket, mapped),
			mapped: wasMapped,
		};
	}

	private resolveForWrite(bucket: string, key: string, operation: string): ResolvedObject {
		this.assertWritable(operation);
		const target = this.resolve(bucket, key);
		if (!target) {
			throw new FilesystemStorageLayoutError(
				`${JSON.stringify(key)} in ${JSON.stringify(bucket)} is not a storable key`,
			);
		}
		return target;
	}

	private assertWritable(operation: string): void {
		if (this.readOnly) throw new StorageReadOnlyError(operation);
	}

	private tempPath(): string {
		return path.join(this.tmpDir, randomBytes(16).toString('hex'));
	}

	private async fsync(handle: FileHandle): Promise<void> {
		await this.fsyncs.run(() => handle.sync());
	}

	private async syncDir(dir: string): Promise<void> {
		const handle = await fs.promises.open(dir, fs.constants.O_RDONLY);
		try {
			await this.fsync(handle);
		} finally {
			await handle.close();
		}
	}

	private async ensureDir(dir: string): Promise<void> {
		const first = await fs.promises.mkdir(dir, {recursive: true, mode: DIRECTORY_MODE});
		if (first === undefined) return;
		const created: Array<string> = [];
		for (let current = dir; ; current = path.dirname(current)) {
			created.push(current);
			if (current === first || current === path.dirname(current)) break;
		}
		for (const createdDir of created.reverse()) {
			await fs.promises.chmod(createdDir, DIRECTORY_MODE);
			await this.syncDir(path.dirname(createdDir));
		}
	}

	private async stage(fill: (handle: FileHandle) => Promise<void>): Promise<string> {
		const temp = this.tempPath();
		const handle = await fs.promises.open(temp, CREATE_FLAGS, FILE_MODE);
		try {
			await handle.chmod(FILE_MODE);
			await fill(handle);
			await this.fsync(handle);
		} catch (error) {
			await handle.close().catch(() => undefined);
			await fs.promises.unlink(temp).catch(() => undefined);
			throw error;
		}
		await handle.close();
		return temp;
	}

	private async stageCopy(sources: ReadonlyArray<string>): Promise<string> {
		return this.stage(async (handle) => {
			for (const source of sources) {
				const input = await fs.promises.open(source, READ_FLAGS);
				try {
					for await (const chunk of input.createReadStream({highWaterMark: COPY_CHUNK_BYTES, autoClose: false})) {
						await handle.write(chunk as Buffer);
					}
				} finally {
					await input.close();
				}
			}
		});
	}

	private async writeSidecar(target: ResolvedObject, contentType: string | null | undefined): Promise<void> {
		if (!needsSidecar(target.key, contentType, target.mapped)) {
			await ignoreMissing(fs.promises.unlink(target.meta));
			return;
		}
		const body = encodeSidecar(target.key, contentType, target.mapped);
		const temp = await this.stage((handle) => handle.writeFile(body));
		try {
			await this.ensureDir(path.dirname(target.meta));
			await fs.promises.rename(temp, target.meta);
		} catch (error) {
			await fs.promises.unlink(temp).catch(() => undefined);
			throw error;
		}
		await this.syncDir(path.dirname(target.meta));
	}

	private async commit(
		temp: string,
		target: ResolvedObject,
		contentType: string | null | undefined,
		mtime?: Date,
	): Promise<void> {
		try {
			if (mtime) await fs.promises.utimes(temp, mtime, mtime);
			await this.writeSidecar(target, contentType);
			await this.ensureDir(path.dirname(target.data));
			await fs.promises.rename(temp, target.data);
		} catch (error) {
			await fs.promises.unlink(temp).catch(() => undefined);
			throw error;
		}
		await ignoreMissing(fs.promises.unlink(temp));
		await this.syncDir(path.dirname(target.data));
	}

	private async open(target: ResolvedObject | null): Promise<StoredObject | null> {
		if (!target) return null;
		const handle = await ignoreMissing(fs.promises.open(target.data, READ_FLAGS));
		if (!handle) return null;
		try {
			const stat = await handle.stat({bigint: true});
			if (!stat.isFile()) {
				await handle.close();
				return null;
			}
			return {
				handle,
				size: Number(stat.size),
				mtimeNs: stat.mtimeNs,
				mtime: new Date(Number(stat.mtimeMs)),
			};
		} catch (error) {
			await handle.close().catch(() => undefined);
			throw error;
		}
	}

	private async openOrThrow(bucket: string, key: string): Promise<StoredObject> {
		const stored = await this.open(this.resolve(bucket, key));
		if (!stored) throw new StorageObjectNotFoundError(bucket, key);
		return stored;
	}

	private async readSidecar(target: ResolvedObject): Promise<{contentType?: string; key?: string} | null> {
		const raw = await ignoreMissing(fs.promises.readFile(target.meta, {encoding: 'utf8', flag: READ_FLAGS}));
		return raw === null ? null : decodeSidecar(raw);
	}

	private async storedContentType(target: ResolvedObject): Promise<string | null> {
		return (await this.readSidecar(target))?.contentType ?? inferContentType(target.key);
	}

	async uploadObject(params: {
		bucket: string;
		key: string;
		body: Uint8Array | Readable;
		contentType?: string;
		expiresAt?: Date;
	}): Promise<void> {
		await this.writeObject(params.bucket, params.key, params.body, params.contentType);
	}

	async importObject(params: {
		bucket: string;
		key: string;
		body: Readable;
		contentType?: string;
		lastModified?: Date;
	}): Promise<void> {
		await this.writeObject(params.bucket, params.key, params.body, params.contentType, params.lastModified);
	}

	private async writeObject(
		bucket: string,
		key: string,
		body: Uint8Array | Readable,
		contentType: string | undefined,
		mtime?: Date,
	): Promise<void> {
		const target = this.resolveForWrite(bucket, key, 'upload');
		await this.writing('upload', async () => {
			const temp = await this.stage(async (handle) => {
				if (body instanceof Uint8Array) {
					await handle.writeFile(body);
					return;
				}
				for await (const chunk of body) {
					await handle.write(typeof chunk === 'string' ? Buffer.from(chunk) : (chunk as Uint8Array));
				}
			});
			await this.commit(temp, target, contentType, mtime);
		});
	}

	async uploadObjectFromFile(params: {
		bucket: string;
		key: string;
		filePath: string;
		contentType?: string;
		contentLength?: number;
		expiresAt?: Date;
	}): Promise<void> {
		const target = this.resolveForWrite(params.bucket, params.key, 'upload');
		await this.writing('upload', async () => {
			const temp = await this.stageCopy([params.filePath]);
			await this.commit(temp, target, params.contentType);
		});
	}

	async deleteObject(bucket: string, key: string): Promise<void> {
		this.assertWritable('delete');
		const target = this.resolve(bucket, key);
		if (!target) return;
		await ignoreMissing(fs.promises.unlink(target.data));
		await ignoreMissing(fs.promises.unlink(target.meta));
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
		const target = this.resolve(bucket, key);
		if (!target) return null;
		const stat = await ignoreMissing(fs.promises.lstat(target.data, {bigint: true}));
		if (!stat?.isFile()) return null;
		return {
			contentLength: Number(stat.size),
			contentType: (await this.storedContentType(target)) ?? DEFAULT_CONTENT_TYPE,
			etag: formatEtag(stat.size, stat.mtimeNs),
			lastModified: new Date(Number(stat.mtimeMs)),
		};
	}

	async computeObjectSha256(bucket: string, key: string): Promise<string> {
		const stored = await this.openOrThrow(bucket, key);
		const hash = createHash('sha256');
		for await (const chunk of stored.handle.createReadStream({highWaterMark: COPY_CHUNK_BYTES})) {
			hash.update(chunk as Buffer);
		}
		return hash.digest('hex');
	}

	async readObject(bucket: string, key: string, maxBytes?: number): Promise<Uint8Array> {
		const stored = await this.openOrThrow(bucket, key);
		try {
			if (maxBytes !== undefined && stored.size > maxBytes) {
				throw new Error(`Stream exceeds maximum buffer size of ${maxBytes} bytes (got ${stored.size} bytes)`);
			}
			return new Uint8Array(await stored.handle.readFile());
		} finally {
			await stored.handle.close();
		}
	}

	async streamObject(params: {bucket: string; key: string; range?: string}): Promise<{
		body: Readable;
		contentLength: number;
		contentRange?: string | null;
		contentType?: string | null;
		etag?: string | null;
		lastModified?: Date | null;
	} | null> {
		const target = this.resolve(params.bucket, params.key);
		const stored = await this.open(target);
		if (!target || !stored) return null;
		const range = parseRange(params.range, stored.size);
		if (range === 'unsatisfiable') {
			await stored.handle.close();
			throw new StorageObjectRangeNotSatisfiableError(params.bucket, params.key, params.range ?? '');
		}
		let contentType: string | null;
		try {
			contentType = (await this.storedContentType(target)) ?? DEFAULT_CONTENT_TYPE;
		} catch (error) {
			await stored.handle.close();
			throw error;
		}
		const start = range?.start ?? 0;
		const end = range?.end ?? stored.size - 1;
		const body = stored.handle.createReadStream({
			start,
			...(end >= start ? {end} : {}),
			highWaterMark: COPY_CHUNK_BYTES,
		});
		return {
			body,
			contentLength: Math.max(0, end - start + 1),
			contentRange: range ? `bytes ${range.start}-${range.end}/${stored.size}` : null,
			contentType,
			etag: formatEtag(BigInt(stored.size), stored.mtimeNs),
			lastModified: stored.mtime,
		};
	}

	async writeObjectToDisk(bucket: string, key: string, filePath: string): Promise<void> {
		const stored = await this.openOrThrow(bucket, key);
		try {
			await fs.promises.mkdir(path.dirname(filePath), {recursive: true});
			const out = await fs.promises.open(filePath, 'w');
			try {
				for await (const chunk of stored.handle.createReadStream({
					highWaterMark: COPY_CHUNK_BYTES,
					autoClose: false,
				})) {
					await out.write(chunk as Buffer);
				}
			} finally {
				await out.close();
			}
		} finally {
			await stored.handle.close();
		}
	}

	async copyObject(params: {
		sourceBucket: string;
		sourceKey: string;
		destinationBucket: string;
		destinationKey: string;
		newContentType?: string;
	}): Promise<void> {
		this.assertWritable('copy');
		const source = this.resolve(params.sourceBucket, params.sourceKey);
		const sourceStat = source ? await ignoreMissing(fs.promises.lstat(source.data)) : null;
		if (!source || !sourceStat?.isFile()) {
			throw new StorageObjectNotFoundError(params.sourceBucket, params.sourceKey);
		}
		const destination = this.resolveForWrite(params.destinationBucket, params.destinationKey, 'copy');
		const contentType = params.newContentType ?? (await this.storedContentType(source));
		await this.writing('copy', async () => {
			if (source.data === destination.data) {
				if (params.newContentType) await this.writeSidecar(destination, contentType);
				return;
			}
			const temp =
				destination.bucket === this.buckets.reports
					? await this.stageCopy([source.data])
					: await this.stageLink(source);
			await this.commit(temp, destination, contentType);
		});
	}

	private async stageLink(source: ResolvedObject): Promise<string> {
		const temp = this.tempPath();
		try {
			await fs.promises.link(source.data, temp);
		} catch (error) {
			if (isMissing(error)) throw new StorageObjectNotFoundError(source.bucket, source.key);
			throw error;
		}
		return temp;
	}

	async getPresignedDownloadURL(_params: {
		bucket: string;
		key: string;
		expiresIn?: number;
		responseContentType?: string;
		responseContentDisposition?: string;
	}): Promise<string> {
		throw new Error('Presigned URLs are not available with filesystem storage');
	}

	async getPresignedUploadURL(_params: {
		bucket: string;
		key: string;
		contentType?: string;
		contentLength?: number;
		expiresIn?: number;
	}): Promise<string> {
		throw new Error('Presigned URLs are not available with filesystem storage');
	}

	async getPresignedUploadPartURL(_params: {
		bucket: string;
		key: string;
		uploadId: string;
		partNumber: number;
		contentLength?: number;
		expiresIn?: number;
	}): Promise<string> {
		throw new Error('Presigned URLs are not available with filesystem storage');
	}

	async uploadAvatar(params: {prefix: string; key: string; body: Uint8Array}): Promise<void> {
		await this.uploadObject({bucket: this.buckets.cdn, key: `${params.prefix}/${params.key}`, body: params.body});
	}

	async deleteAvatar(params: {prefix: string; key: string}): Promise<void> {
		await this.deleteObject(this.buckets.cdn, `${params.prefix}/${params.key}`);
	}

	async listObjects(params: {bucket: string; prefix: string; maxObjects?: number}): Promise<
		ReadonlyArray<{
			key: string;
			lastModified?: Date;
		}>
	> {
		if (params.maxObjects !== undefined && (!Number.isSafeInteger(params.maxObjects) || params.maxObjects <= 0)) {
			throw new RangeError('maxObjects must be a positive safe integer');
		}
		if (!isValidStorageBucket(params.bucket)) return [];
		const dirSegments = params.prefix.split('/').slice(0, -1);
		if (params.prefix.startsWith('/') || !dirSegments.every((segment) => isValidStorageKey(segment))) {
			return [];
		}
		const base = path.join(this.root, params.bucket, ...dirSegments.map(mapSegment));
		const result: Array<{key: string; lastModified: Date; sortKey: Buffer}> = [];
		const walk = async (dir: string, relative: Array<string>): Promise<void> => {
			const entries = await ignoreMissing(fs.promises.readdir(dir, {withFileTypes: true}));
			if (!entries) return;
			for (const entry of entries) {
				const segments = [...relative, entry.name];
				if (entry.isDirectory()) {
					await walk(path.join(dir, entry.name), segments);
					continue;
				}
				if (!entry.isFile()) continue;
				const key = await this.originalKey(params.bucket, segments);
				if (!key.startsWith(params.prefix)) continue;
				const stat = await ignoreMissing(fs.promises.lstat(path.join(dir, entry.name)));
				if (!stat) continue;
				result.push({key, lastModified: stat.mtime, sortKey: Buffer.from(key, 'utf8')});
				if (params.maxObjects !== undefined && result.length > params.maxObjects) {
					throw new StorageObjectListingOverflowError(params.bucket, params.prefix, params.maxObjects);
				}
			}
		};
		await walk(base, dirSegments.map(mapSegment));
		result.sort((a, b) => Buffer.compare(a.sortKey, b.sortKey));
		return result.map(({key, lastModified}) => ({key, lastModified}));
	}

	private async originalKey(bucket: string, segments: Array<string>): Promise<string> {
		const stored = segments.join('/');
		if (!segments.some(mayBeMappedSegment)) return stored;
		const raw = await ignoreMissing(
			fs.promises.readFile(path.join(this.root, META_DIR, bucket, stored), {encoding: 'utf8', flag: READ_FLAGS}),
		);
		const sidecar = raw === null ? null : decodeSidecar(raw);
		return sidecar?.key && mapKey(sidecar.key).path === stored ? sidecar.key : stored;
	}

	async createMultipartUpload(params: {bucket: string; key: string; contentType?: string}): Promise<{
		uploadId: string;
	}> {
		this.resolveForWrite(params.bucket, params.key, 'multipart upload');
		return this.writing('multipart upload', async () => {
			const uploadId = randomBytes(16).toString('hex');
			const dir = path.join(this.multipartDir, uploadId);
			await fs.promises.mkdir(dir, {mode: DIRECTORY_MODE});
			try {
				await fs.promises.chmod(dir, DIRECTORY_MODE);
				const manifest: UploadManifest = {
					v: 1,
					bucket: params.bucket,
					key: params.key,
					...(params.contentType ? {contentType: params.contentType} : {}),
					created: Date.now(),
				};
				const body = JSON.stringify(manifest);
				const temp = await this.stage((handle) => handle.writeFile(body));
				await fs.promises.rename(temp, path.join(dir, MULTIPART_MANIFEST));
			} catch (error) {
				await fs.promises.rm(dir, {recursive: true, force: true}).catch(() => undefined);
				throw error;
			}
			await this.syncDir(dir);
			await this.syncDir(this.multipartDir);
			return {uploadId};
		});
	}

	private async readManifest(dir: string, uploadId: string): Promise<UploadManifest | null> {
		if (!UPLOAD_ID_PATTERN.test(uploadId)) return null;
		const raw = await ignoreMissing(
			fs.promises.readFile(path.join(dir, MULTIPART_MANIFEST), {encoding: 'utf8', flag: READ_FLAGS}),
		);
		return raw === null ? null : decodeUploadManifest(raw);
	}

	private async manifestFor(params: {bucket: string; key: string; uploadId: string}): Promise<UploadManifest> {
		const manifest = await this.readManifest(path.join(this.multipartDir, params.uploadId), params.uploadId);
		if (!manifest || manifest.bucket !== params.bucket || manifest.key !== params.key) {
			throw new StorageUploadNotFoundError(params.uploadId);
		}
		return manifest;
	}

	private async partsIn(dir: string): Promise<Array<{partNumber: number; etag: string; size: number; file: string}>> {
		const parts: Array<{partNumber: number; etag: string; size: number; file: string}> = [];
		for (const name of await fs.promises.readdir(dir)) {
			const match = PART_NAME_PATTERN.exec(name);
			if (!match) continue;
			const partNumber = Number(match[1]);
			if (partNumber < 1 || partNumber > MAX_PART_NUMBER || partFileName(partNumber) !== name) continue;
			const file = path.join(dir, name);
			const stat = await ignoreMissing(fs.promises.lstat(file, {bigint: true}));
			if (!stat?.isFile()) continue;
			parts.push({partNumber, etag: formatEtag(stat.size, stat.mtimeNs), size: Number(stat.size), file});
		}
		parts.sort((a, b) => a.partNumber - b.partNumber);
		return parts;
	}

	async listParts(params: {bucket: string; key: string; uploadId: string}): Promise<
		Array<{
			partNumber: number;
			etag: string;
			size?: number;
		}>
	> {
		await this.manifestFor(params);
		const parts = await this.partsIn(path.join(this.multipartDir, params.uploadId));
		return parts.map(({partNumber, etag, size}) => ({partNumber, etag, size}));
	}

	private async moveToTrash(uploadId: string): Promise<string | null> {
		const trash = path.join(this.trashDir, `${uploadId}-${randomBytes(8).toString('hex')}`);
		const moved = await ignoreMissing(fs.promises.rename(path.join(this.multipartDir, uploadId), trash));
		return moved === null ? null : trash;
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
		const target = this.resolveForWrite(params.bucket, params.key, 'multipart upload');
		await this.writing('multipart upload', async () => {
			const manifest = await this.manifestFor(params);
			const trash = await this.moveToTrash(params.uploadId);
			if (!trash) throw new StorageUploadNotFoundError(params.uploadId);
			try {
				const onDisk = await this.partsIn(trash);
				const given = [...params.parts].sort((a, b) => a.partNumber - b.partNumber);
				const matches =
					given.length > 0 &&
					given.length === onDisk.length &&
					given.every(
						(part, index) => part.partNumber === onDisk[index]!.partNumber && part.etag === onDisk[index]!.etag,
					);
				if (!matches) {
					const error = new Error(`The parts given for upload ${params.uploadId} do not match the stored parts`);
					error.name = 'InvalidPart';
					throw error;
				}
				const temp = await this.stageCopy(onDisk.map((part) => part.file));
				await this.commit(temp, target, manifest.contentType);
			} finally {
				await fs.promises.rm(trash, {recursive: true, force: true});
			}
		});
	}

	async abortMultipartUpload(params: {bucket: string; key: string; uploadId: string}): Promise<void> {
		this.assertWritable('multipart abort');
		const manifest = await this.readManifest(path.join(this.multipartDir, params.uploadId), params.uploadId);
		if (!manifest) return;
		if (manifest.bucket !== params.bucket || manifest.key !== params.key) {
			throw new StorageUploadNotFoundError(params.uploadId);
		}
		const trash = await this.moveToTrash(params.uploadId);
		if (trash) await fs.promises.rm(trash, {recursive: true, force: true});
	}
}
