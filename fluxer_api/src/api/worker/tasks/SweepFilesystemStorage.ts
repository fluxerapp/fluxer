// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {setTimeout as sleep} from 'node:timers/promises';
import {Config} from '@app/api/Config';
import {
	assertStorageBuckets,
	decodeUploadManifest,
	GEOIP_BUCKET,
	META_DIR,
	MULTIPART_MANIFEST,
	STAGING_DIR,
	STAGING_MULTIPART,
	STAGING_TMP,
	STAGING_TRASH,
	type StorageBuckets,
} from '@app/api/infrastructure/FilesystemStorageLayout';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const STAGING_MAX_AGE_MS = ms('6 hours');
const ORPHAN_SIDECAR_MIN_AGE_MS = ms('1 hour');
const UNFINISHED_UPLOAD_MAX_AGE_MS = ms('1 day');
const OTHER_BUCKET_ENTRIES_PER_RUN = 5000;
const OTHER_BUCKET_PAUSE_EVERY = 500;
const OTHER_BUCKET_PAUSE_MS = 50;

export interface FilesystemSweepOptions {
	root: string;
	buckets: StorageBuckets;
	uploadsTtlMs: number | null;
	now: number;
}

export interface FilesystemSweepResult {
	expiredUploads: number;
	expiredMultipart: number;
	staleStaging: number;
	orphanSidecars: number;
}

async function* walkFiles(dir: string): AsyncGenerator<string> {
	let handle: fs.Dir;
	try {
		handle = await fs.promises.opendir(dir);
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') return;
		throw error;
	}
	for await (const entry of handle) {
		const full = path.join(dir, entry.name);
		if (entry.isDirectory()) {
			yield* walkFiles(full);
		} else if (entry.isFile()) {
			yield full;
		}
	}
}

async function ageOf(file: string, now: number, since: 'mtimeMs' | 'ctimeMs' = 'mtimeMs'): Promise<number | null> {
	try {
		return now - (await fs.promises.lstat(file))[since];
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') return null;
		throw error;
	}
}

async function exists(file: string): Promise<boolean> {
	try {
		await fs.promises.lstat(file);
		return true;
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT' || (error as NodeJS.ErrnoException).code === 'ENOTDIR') {
			return false;
		}
		throw error;
	}
}

async function unlinkQuietly(file: string): Promise<boolean> {
	try {
		await fs.promises.unlink(file);
		return true;
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') return false;
		throw error;
	}
}

async function removeOrphanSidecar(root: string, bucket: string, sidecar: string, now: number): Promise<boolean> {
	const relative = path.relative(path.join(root, META_DIR, bucket), sidecar);
	if (await exists(path.join(root, bucket, relative))) return false;
	const age = await ageOf(sidecar, now);
	if (age === null || age < ORPHAN_SIDECAR_MIN_AGE_MS) return false;
	return unlinkQuietly(sidecar);
}

async function sweepUploads(options: FilesystemSweepOptions, result: FilesystemSweepResult): Promise<void> {
	const {root, buckets, uploadsTtlMs, now} = options;
	if (uploadsTtlMs !== null) {
		const bucketDir = path.join(root, buckets.uploads);
		for await (const file of walkFiles(bucketDir)) {
			const age = await ageOf(file, now);
			if (age === null || age <= uploadsTtlMs) continue;
			if (await unlinkQuietly(file)) {
				result.expiredUploads += 1;
				await unlinkQuietly(path.join(root, META_DIR, buckets.uploads, path.relative(bucketDir, file)));
			}
		}
	}
	for await (const sidecar of walkFiles(path.join(root, META_DIR, buckets.uploads))) {
		if (await removeOrphanSidecar(root, buckets.uploads, sidecar, now)) result.orphanSidecars += 1;
	}
}

async function sweepStaging(options: FilesystemSweepOptions, result: FilesystemSweepResult): Promise<void> {
	const staging = path.join(options.root, STAGING_DIR);
	const multipartMaxAge = options.uploadsTtlMs ?? UNFINISHED_UPLOAD_MAX_AGE_MS;
	for (const name of await fs.promises.readdir(path.join(staging, STAGING_MULTIPART))) {
		const dir = path.join(staging, STAGING_MULTIPART, name);
		let created: number;
		try {
			created =
				decodeUploadManifest(await fs.promises.readFile(path.join(dir, MULTIPART_MANIFEST), 'utf8'))?.created ?? 0;
		} catch {
			created = (await fs.promises.lstat(dir).catch(() => null))?.mtimeMs ?? options.now;
		}
		if (options.now - created <= multipartMaxAge) continue;
		await fs.promises.rm(dir, {recursive: true, force: true});
		result.expiredMultipart += 1;
	}
	for (const sub of [STAGING_TMP, STAGING_TRASH]) {
		for (const name of await fs.promises.readdir(path.join(staging, sub))) {
			const entry = path.join(staging, sub, name);
			const age = await ageOf(entry, options.now, 'ctimeMs');
			if (age === null || age <= STAGING_MAX_AGE_MS) continue;
			await fs.promises.rm(entry, {recursive: true, force: true});
			result.staleStaging += 1;
		}
	}
}

export async function sweepFilesystemTree(options: FilesystemSweepOptions): Promise<FilesystemSweepResult> {
	assertStorageBuckets(options.buckets);
	const result: FilesystemSweepResult = {expiredUploads: 0, expiredMultipart: 0, staleStaging: 0, orphanSidecars: 0};
	await sweepUploads(options, result);
	await sweepStaging(options, result);
	return result;
}

let otherBucketSidecars: AsyncGenerator<{bucket: string; sidecar: string}> | null = null;

async function* otherBucketSidecarWalk(
	root: string,
	buckets: StorageBuckets,
): AsyncGenerator<{bucket: string; sidecar: string}> {
	const others = new Set([buckets.cdn, buckets.reports, buckets.harvests, GEOIP_BUCKET]);
	for (const bucket of others) {
		for await (const sidecar of walkFiles(path.join(root, META_DIR, bucket))) {
			yield {bucket, sidecar};
		}
	}
}

async function sweepOtherBucketSidecars(root: string, buckets: StorageBuckets, now: number): Promise<number> {
	otherBucketSidecars ??= otherBucketSidecarWalk(root, buckets);
	let removed = 0;
	for (let seen = 1; seen <= OTHER_BUCKET_ENTRIES_PER_RUN; seen += 1) {
		const next = await otherBucketSidecars.next();
		if (next.done) {
			otherBucketSidecars = null;
			break;
		}
		if (await removeOrphanSidecar(root, next.value.bucket, next.value.sidecar, now)) removed += 1;
		if (seen % OTHER_BUCKET_PAUSE_EVERY === 0) await sleep(OTHER_BUCKET_PAUSE_MS);
	}
	return removed;
}

const sweepFilesystemStorage: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.storage.backend !== 'filesystem' || Config.storage.readOnly) {
		return;
	}
	const now = Date.now();
	const options: FilesystemSweepOptions = {
		root: Config.storage.root,
		buckets: Config.s3.buckets,
		uploadsTtlMs: Config.storage.uploadsTtlMs,
		now,
	};
	const result = await sweepFilesystemTree(options);
	result.orphanSidecars += await sweepOtherBucketSidecars(options.root, options.buckets, now);
	if (Object.values(result).some((count) => count > 0)) {
		helpers.logger.info({...result}, 'Swept filesystem storage');
	}
};

export default sweepFilesystemStorage;
