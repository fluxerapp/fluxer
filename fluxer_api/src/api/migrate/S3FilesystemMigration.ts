// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import {Readable} from 'node:stream';
import {parseArgs} from 'node:util';
import {Config} from '@app/api/Config';
import {
	DIRECTORY_MODE,
	GEOIP_BUCKET,
	inferContentType,
	isValidStorageKey,
	LAYOUT_FILE,
	META_DIR,
	mapKey,
	STAGING_DIR,
	STAGING_TMP,
	type StorageBuckets,
	writeLayoutSync,
} from '@app/api/infrastructure/FilesystemStorageLayout';
import {FilesystemStorageService} from '@app/api/infrastructure/FilesystemStorageService';
import {type MigrationOutput, MigrationUsageError, positiveInteger} from '@app/api/migrate/MigrationCli';
import {GetObjectCommand, ListObjectsV2Command, S3Client} from '@aws-sdk/client-s3';
import {parseUploadsTtlMs} from '@fluxer/config/src/ConfigLoader';

const STORAGE_MIGRATION_SAMPLE_PERCENT = 1;
const STORAGE_MIGRATION_SAMPLE_MIN = 200;
const STORAGE_MIGRATION_FREE_SPACE_FACTOR = 1.1;

const WEED_MULTIPART_PREFIX = '.uploads/';
const PROGRESS_EVERY = 1000;
const ROOT_ENTRIES_ALLOWED_BEFORE_IMPORT = new Set([LAYOUT_FILE, STAGING_DIR, META_DIR, 'lost+found']);
const PLACEMENT_ERROR_CODES = new Set(['EEXIST', 'ENOTDIR', 'EISDIR', 'ENOTEMPTY']);

export interface S3FilesystemMigrationOptions {
	root: string;
	verifyOnly: boolean;
	samplePercent: number;
	sampleMin: number;
	uploadsTtlMs: number | null;
}

type StorageMigrationSkipReason =
	| 'uploads-prefix'
	| 'trailing-slash'
	| 'expired-upload'
	| 'invalid-segment'
	| 'object-and-prefix';

interface StorageMigrationSkip {
	bucket: string;
	key: string;
	reason: StorageMigrationSkipReason;
}

interface StorageMigrationBucketReport {
	bucket: string;
	objects: number;
	bytes: number;
	mappedNames: number;
	sampled: number;
	mismatches: Array<string>;
}

export interface S3FilesystemMigrationReport {
	root: string;
	verified: boolean;
	buckets: Array<StorageMigrationBucketReport>;
	skipped: Array<StorageMigrationSkip>;
}

export const STORAGE_MIGRATION_USAGE = `usage: MigrateEntrypoint storage [--root <dir>] [--verify-only] [--sample-percent <n>] [--sample-min <n>]

  --root            the folder to write the files into, FLUXER_STORAGE_ROOT by default
  --verify-only     compare the object store with the folder without copying
  --sample-percent  share of objects whose sha256 is compared, ${STORAGE_MIGRATION_SAMPLE_PERCENT} by default
  --sample-min      least number of objects per bucket whose sha256 is compared, ${STORAGE_MIGRATION_SAMPLE_MIN} by default

  Uploads older than FLUXER_STORAGE_UPLOADS_TTL (or FLUXER_SEAWEEDFS_UPLOADS_TTL, 1d by default) are not copied.`;

export function parseS3FilesystemArgs(
	args: ReadonlyArray<string>,
	env: Readonly<Record<string, string | undefined>>,
): S3FilesystemMigrationOptions {
	let parsed: ReturnType<typeof parseStorageArgs>;
	try {
		parsed = parseStorageArgs(args);
	} catch (error) {
		throw new MigrationUsageError(error instanceof Error ? error.message : String(error));
	}
	const {values} = parsed;
	const root = values.root ?? env.FLUXER_STORAGE_ROOT ?? '';
	if (!root.startsWith('/')) {
		throw new MigrationUsageError('--root or FLUXER_STORAGE_ROOT must be an absolute path');
	}
	const samplePercent = positiveInteger('sample-percent', values['sample-percent'], STORAGE_MIGRATION_SAMPLE_PERCENT);
	if (samplePercent > 100) {
		throw new MigrationUsageError('--sample-percent must be at most 100');
	}
	let uploadsTtlMs: number | null;
	try {
		uploadsTtlMs = parseUploadsTtlMs(env.FLUXER_STORAGE_UPLOADS_TTL || env.FLUXER_SEAWEEDFS_UPLOADS_TTL || '1d');
	} catch (error) {
		throw new MigrationUsageError(error instanceof Error ? error.message : String(error));
	}
	return {
		root,
		verifyOnly: values['verify-only'] ?? false,
		samplePercent,
		sampleMin: positiveInteger('sample-min', values['sample-min'], STORAGE_MIGRATION_SAMPLE_MIN),
		uploadsTtlMs,
	};
}

function parseStorageArgs(args: ReadonlyArray<string>) {
	return parseArgs({
		args: [...args],
		strict: true,
		allowPositionals: false,
		options: {
			root: {type: 'string'},
			'verify-only': {type: 'boolean'},
			'sample-percent': {type: 'string'},
			'sample-min': {type: 'string'},
		},
	});
}

interface SourceObject {
	key: string;
	size: number;
	lastModified?: Date;
}

interface BucketPlan {
	bucket: string;
	objects: number;
	bytes: number;
	directories: number;
	mappedNames: number;
	samples: Array<string>;
	conflicts: Set<string>;
}

export interface StorageMigrationSource {
	list(bucket: string): AsyncIterable<SourceObject>;
	read(bucket: string, key: string): Promise<{body: Readable; contentType?: string; lastModified?: Date}>;
	hasBucket(bucket: string): Promise<boolean>;
}

function s3MigrationSource(client: S3Client): StorageMigrationSource {
	return {
		async *list(bucket) {
			let token: string | undefined;
			do {
				const page = await client.send(new ListObjectsV2Command({Bucket: bucket, ContinuationToken: token}));
				for (const object of page.Contents ?? []) {
					if (object.Key === undefined) continue;
					yield {key: object.Key, size: object.Size ?? 0, lastModified: object.LastModified};
				}
				if (page.IsTruncated && !page.NextContinuationToken) {
					throw new Error(`The object listing of ${bucket} was cut short without a continuation token`);
				}
				token = page.IsTruncated ? page.NextContinuationToken : undefined;
			} while (token);
		},
		async read(bucket, key) {
			const out = await client.send(new GetObjectCommand({Bucket: bucket, Key: key}));
			if (!(out.Body instanceof Readable)) {
				throw new Error(`Reading ${bucket}/${key} did not return a stream`);
			}
			return {
				body: out.Body,
				...(out.ContentType ? {contentType: out.ContentType} : {}),
				...(out.LastModified ? {lastModified: out.LastModified} : {}),
			};
		},
		async hasBucket(bucket) {
			try {
				await client.send(new ListObjectsV2Command({Bucket: bucket, MaxKeys: 1}));
				return true;
			} catch (error) {
				if (error instanceof Error && error.name === 'NoSuchBucket') return false;
				throw error;
			}
		},
	};
}

function skipReason(
	bucket: string,
	object: SourceObject,
	buckets: StorageBuckets,
	uploadsTtlMs: number | null,
	now: number,
): StorageMigrationSkipReason | null {
	if (object.key.startsWith(WEED_MULTIPART_PREFIX)) return 'uploads-prefix';
	if (object.key.endsWith('/')) return 'trailing-slash';
	if (!isValidStorageKey(object.key)) return 'invalid-segment';
	if (
		bucket === buckets.uploads &&
		uploadsTtlMs !== null &&
		object.lastModified !== undefined &&
		now - object.lastModified.getTime() > uploadsTtlMs
	) {
		return 'expired-upload';
	}
	return null;
}

function inSample(key: string, percent: number): boolean {
	return createHash('sha256').update(key).digest().readUInt16BE(0) % 100 < percent;
}

async function sha256Of(stream: Readable): Promise<string> {
	const hash = createHash('sha256');
	for await (const chunk of stream) {
		hash.update(chunk as Buffer);
	}
	return hash.digest('hex');
}

async function countFiles(dir: string): Promise<{objects: number; bytes: number}> {
	let objects = 0;
	let bytes = 0;
	let handle: fs.Dir;
	try {
		handle = await fs.promises.opendir(dir);
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') return {objects, bytes};
		throw error;
	}
	for await (const entry of handle) {
		const full = path.join(dir, entry.name);
		if (entry.isDirectory()) {
			const inner = await countFiles(full);
			objects += inner.objects;
			bytes += inner.bytes;
		} else if (entry.isFile()) {
			objects += 1;
			bytes += (await fs.promises.lstat(full)).size;
		}
	}
	return {objects, bytes};
}

async function assertImportTarget(root: string): Promise<void> {
	let entries: Array<string>;
	try {
		entries = await fs.promises.readdir(root);
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') return;
		throw error;
	}
	const unexpected = entries.filter((entry) => !ROOT_ENTRIES_ALLOWED_BEFORE_IMPORT.has(entry));
	if (unexpected.length > 0) {
		throw new Error(`${root} already holds ${unexpected.slice(0, 5).join(', ')}. Import into an empty folder`);
	}
}

async function assertLongNames(root: string): Promise<void> {
	const probe = path.join(root, STAGING_DIR, STAGING_TMP, 'n'.repeat(255));
	try {
		await fs.promises.writeFile(probe, '');
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENAMETOOLONG') {
			throw new Error(`${root} cannot hold file names of 255 bytes`);
		}
		throw error;
	}
	await fs.promises.unlink(probe);
}

async function assertFreeSpace(root: string, bytes: number): Promise<void> {
	const stats = await fs.promises.statfs(root);
	const free = Number(stats.bavail) * Number(stats.bsize);
	const needed = Math.ceil(bytes * STORAGE_MIGRATION_FREE_SPACE_FACTOR);
	if (free < needed) {
		throw new Error(`${root} has ${free} bytes free, the import needs ${needed}`);
	}
}

async function assertFreeInodes(
	root: string,
	files: number,
	directories: number,
	output: MigrationOutput,
): Promise<void> {
	const stats = await fs.promises.statfs(root);
	if (Number(stats.files) === 0) return;
	const free = Number(stats.ffree);
	const fewest = files + directories;
	if (free < fewest) {
		throw new Error(
			`${root} has ${free} free inodes, the import needs at least ${fewest} (${files} files and ${directories} folders)`,
		);
	}
	const most = fewest * 2;
	if (free < most) {
		output.info(
			`${root} has ${free} free inodes. The import needs ${fewest} to ${most}, depending on how many files need a metadata file`,
		);
	}
}

export async function migrateS3ToFilesystem(params: {
	options: S3FilesystemMigrationOptions;
	source: StorageMigrationSource;
	buckets: StorageBuckets;
	output: MigrationOutput;
	now?: number;
}): Promise<S3FilesystemMigrationReport> {
	const {options, source, buckets, output} = params;
	const now = params.now ?? Date.now();
	if (!options.verifyOnly) {
		await assertImportTarget(options.root);
		if ((await fs.promises.mkdir(options.root, {recursive: true})) !== undefined) {
			await fs.promises.chmod(options.root, DIRECTORY_MODE);
		}
	}
	const target = new FilesystemStorageService({
		root: options.root,
		buckets,
		readOnly: false,
		requireLayout: options.verifyOnly,
	});
	const names = [...new Set([buckets.cdn, buckets.uploads, buckets.reports, buckets.harvests])];
	if (await source.hasBucket(GEOIP_BUCKET)) names.push(GEOIP_BUCKET);
	const skipped: Array<StorageMigrationSkip> = [];
	const plans: Array<BucketPlan> = [];
	for (const bucket of names) {
		const plan: BucketPlan = {
			bucket,
			objects: 0,
			bytes: 0,
			directories: 1,
			mappedNames: 0,
			samples: [],
			conflicts: new Set(),
		};
		const parents: Array<string> = [];
		let folders: Array<string> = [];
		for await (const object of source.list(bucket)) {
			const reason = skipReason(bucket, object, buckets, options.uploadsTtlMs, now);
			if (reason) {
				skipped.push({bucket, key: object.key, reason});
				continue;
			}
			while (parents.length > 0 && !object.key.startsWith(parents[parents.length - 1]!)) parents.pop();
			if (parents.some((parent) => object.key.startsWith(`${parent}/`))) {
				plan.conflicts.add(object.key);
				skipped.push({bucket, key: object.key, reason: 'object-and-prefix'});
				continue;
			}
			parents.push(object.key);
			plan.objects += 1;
			plan.bytes += object.size;
			const keyFolders = object.key.split('/').slice(0, -1);
			let shared = 0;
			while (shared < keyFolders.length && keyFolders[shared] === folders[shared]) shared += 1;
			plan.directories += keyFolders.length - shared;
			folders = keyFolders;
			const mapped = mapKey(object.key).mapped;
			if (mapped) plan.mappedNames += 1;
			if (mapped || plan.samples.length < options.sampleMin || inSample(object.key, options.samplePercent)) {
				plan.samples.push(object.key);
			}
		}
		plans.push(plan);
		output.info(`${bucket}: ${plan.objects} objects, ${plan.bytes} bytes`);
	}
	if (!options.verifyOnly) {
		await assertLongNames(options.root);
		await assertFreeSpace(
			options.root,
			plans.reduce((sum, plan) => sum + plan.bytes, 0),
		);
		await assertFreeInodes(
			options.root,
			plans.reduce((sum, plan) => sum + plan.objects, 0),
			plans.reduce((sum, plan) => sum + plan.directories, 0),
			output,
		);
		for (const plan of plans) {
			await copyBucket(plan, source, target, options, buckets, now, skipped, output);
		}
		writeLayoutSync(options.root);
	}
	const reports: Array<StorageMigrationBucketReport> = [];
	for (const plan of plans) {
		reports.push(await verifyBucket(plan, source, target));
	}
	return {
		root: options.root,
		verified: reports.every((report) => report.mismatches.length === 0),
		buckets: reports,
		skipped,
	};
}

async function copyBucket(
	plan: BucketPlan,
	source: StorageMigrationSource,
	target: FilesystemStorageService,
	options: S3FilesystemMigrationOptions,
	buckets: StorageBuckets,
	now: number,
	skipped: Array<StorageMigrationSkip>,
	output: MigrationOutput,
): Promise<void> {
	let copied = 0;
	for await (const object of source.list(plan.bucket)) {
		if (skipReason(plan.bucket, object, buckets, options.uploadsTtlMs, now) || plan.conflicts.has(object.key)) continue;
		const read = await source.read(plan.bucket, object.key);
		const contentType =
			read.contentType === 'application/octet-stream' && inferContentType(object.key) === null
				? undefined
				: read.contentType;
		try {
			await target.importObject({
				bucket: plan.bucket,
				key: object.key,
				body: read.body,
				...(contentType ? {contentType} : {}),
				...((read.lastModified ?? object.lastModified) ? {lastModified: read.lastModified ?? object.lastModified} : {}),
			});
		} catch (error) {
			read.body.destroy();
			if (!PLACEMENT_ERROR_CODES.has((error as NodeJS.ErrnoException).code ?? '')) throw error;
			skipped.push({bucket: plan.bucket, key: object.key, reason: 'object-and-prefix'});
			plan.objects -= 1;
			plan.bytes -= object.size;
			plan.samples = plan.samples.filter((key) => key !== object.key);
			if (mapKey(object.key).mapped) plan.mappedNames -= 1;
			plan.conflicts.add(object.key);
			continue;
		}
		copied += 1;
		if (copied % PROGRESS_EVERY === 0) {
			output.info(`${plan.bucket}: copied ${copied} of ${plan.objects}`);
		}
	}
}

async function verifyBucket(
	plan: BucketPlan,
	source: StorageMigrationSource,
	target: FilesystemStorageService,
): Promise<StorageMigrationBucketReport> {
	const mismatches: Array<string> = [];
	const stored = await countFiles(path.join(target.root, plan.bucket));
	if (stored.objects !== plan.objects) {
		mismatches.push(`${plan.objects} objects in the object store, ${stored.objects} files in the folder`);
	}
	if (stored.bytes !== plan.bytes) {
		mismatches.push(`${plan.bytes} bytes in the object store, ${stored.bytes} bytes in the folder`);
	}
	for (const key of plan.samples) {
		const read = await source.read(plan.bucket, key);
		const expected = await sha256Of(read.body);
		let actual: string;
		try {
			actual = await target.computeObjectSha256(plan.bucket, key);
		} catch (error) {
			mismatches.push(`${key}: ${error instanceof Error ? error.message : String(error)}`);
			continue;
		}
		if (actual !== expected) mismatches.push(`${key}: sha256 differs`);
	}
	return {
		bucket: plan.bucket,
		objects: plan.objects,
		bytes: plan.bytes,
		mappedNames: plan.mappedNames,
		sampled: plan.samples.length,
		mismatches,
	};
}

export async function runS3FilesystemMigration(
	options: S3FilesystemMigrationOptions,
	output: MigrationOutput,
): Promise<S3FilesystemMigrationReport> {
	const client = new S3Client({
		endpoint: Config.s3.endpoint,
		region: Config.s3.region,
		forcePathStyle: Config.s3.forcePathStyle,
		credentials: {accessKeyId: Config.s3.accessKeyId, secretAccessKey: Config.s3.secretAccessKey},
		requestChecksumCalculation: 'WHEN_REQUIRED',
		responseChecksumValidation: 'WHEN_REQUIRED',
		maxAttempts: 3,
	});
	try {
		return await migrateS3ToFilesystem({
			options,
			source: s3MigrationSource(client),
			buckets: Config.s3.buckets,
			output,
		});
	} finally {
		client.destroy();
	}
}
