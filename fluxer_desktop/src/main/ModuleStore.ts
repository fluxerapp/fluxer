// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import {createHash, randomBytes} from 'node:crypto';
import {type Dirent, constants as fsConstants, type Stats} from 'node:fs';
import type {FileHandle} from 'node:fs/promises';
import fs from 'node:fs/promises';
import path from 'node:path';
import {ModuleInstallCoordinator} from '@electron/main/ModuleInstallCoordinator';
import {
	DESKTOP_MODULE_FILE_LIST_NAME,
	DESKTOP_MODULE_MANIFEST_MAX_BYTES,
	type DesktopModuleManifest,
	ModulePackageHashMismatchError,
	parseDesktopModuleManifest,
	verifyAndExtract,
} from '@electron/main/ModulePackage';
import {compareModuleVersions, parseModuleVersion} from '@electron/main/ModuleVersion';
import {isDesktopModuleName} from '@fluxer/desktop_ipc/src/ModuleContract';

const MODULE_STORE_DIRECTORY_NAME = 'modules';
export const MODULE_STATE_FILE_NAME = 'state.json';
export const MODULE_STATE_VERSION = 3;
const MODULE_DOWNLOAD_DIRECTORY_NAME = 'download';
const MODULE_DOWNLOAD_INCOMING_DIRECTORY_NAME = 'incoming';
const MODULE_STORE_TREE_DIRECTORY_NAME = 'store';
const MODULE_PACKAGE_FILE_EXTENSION = '.br';
const MODULE_PARTIAL_PACKAGE_FILE_EXTENSION = '.partial';
export const MODULE_INCOMING_STORE_PREFIX = '.incoming-';
const MODULE_REPLACED_STORE_PREFIX = '.replaced-';
export const MODULE_PACKAGE_MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;
export const MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD = 2;

const MODULE_STATE_MAX_BYTES = 4 * 1024 * 1024;
const MODULE_MANIFEST_HIGH_WATER_MAX_ENTRIES = 16;
const MODULE_FEED_COMPONENT_PATTERN = /^[a-z][a-z0-9_-]{0,63}$/u;
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;
const DOWNLOAD_FILE_MODE = 0o644;
const STATE_FILE_MODE = 0o600;
const INSTALLED_FILE_HASH_CHUNK_BYTES = 1024 * 1024;
const INSTALLED_FILE_CHECK_CONCURRENCY = 32;
const TREE_FSYNC_CONCURRENCY = 4;
export const MODULE_FILE_STAMPS_NAME = '.installed-files.json';
const MODULE_FILE_STAMPS_VERSION = 1;
const MODULE_FILE_STAMPS_MODE = 0o600;
const MODULE_FILE_STAMPS_MAX_BYTES = 16 * 1024 * 1024;
const MISSING_INSTALLATION_ERROR_CODES: ReadonlySet<string> = new Set(['ENOENT', 'ENOTDIR', 'ELOOP']);
const MODULE_PACKAGE_DIGEST_MISMATCH_PREFIX = 'package sha256 mismatch:';
const MODULE_LAUNCH_ATTEMPT = Symbol('fluxer.desktop.moduleLaunchAttempt');
const WINDOWS_TRANSIENT_FS_ERROR_CODES: ReadonlySet<string> = new Set(['EPERM', 'EACCES', 'EBUSY']);
const WINDOWS_TRANSIENT_FS_RETRY_DELAYS_MS: ReadonlyArray<number> = [25, 50, 100, 200, 400, 800];

class ModuleStoreError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleStoreError';
	}
}

class ModuleStoreIOError extends ModuleStoreError {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleStoreIOError';
	}
}

class ModuleStoreStateCorruptError extends ModuleStoreError {
	public constructor(statePath: string, detail: string, options?: ErrorOptions) {
		super(`module state at ${statePath} is corrupt: ${detail}`, options);
		this.name = 'ModuleStoreStateCorruptError';
	}
}

class ModuleStoreStateUnsupportedVersionError extends ModuleStoreError {
	public constructor(statePath: string, version: unknown) {
		super(`module state at ${statePath} has unsupported state_version ${String(version)}`);
		this.name = 'ModuleStoreStateUnsupportedVersionError';
	}
}

export class ModuleManifestRollbackError extends ModuleStoreError {
	public constructor(feed: ModuleManifestFeedIdentity, candidate: number, retained: number) {
		super(
			`module manifest metadata_version ${candidate} for ${moduleManifestFeedKey(feed)} is below retained version ${retained}`,
		);
		this.name = 'ModuleManifestRollbackError';
	}
}

export class ModuleManifestEquivocationError extends ModuleStoreError {
	public constructor(feed: ModuleManifestFeedIdentity, metadataVersion: number) {
		super(
			`module manifest metadata_version ${metadataVersion} for ${moduleManifestFeedKey(feed)} has a different digest`,
		);
		this.name = 'ModuleManifestEquivocationError';
	}
}

class ModuleManifestHighWaterCapacityError extends ModuleStoreError {
	public constructor() {
		super(`module manifest high-water state cannot exceed ${MODULE_MANIFEST_HIGH_WATER_MAX_ENTRIES} feeds`);
		this.name = 'ModuleManifestHighWaterCapacityError';
	}
}

export class ModuleStoreDownloadHashMismatchError extends ModuleStoreError {
	public readonly module: string;
	public readonly expectedSha256: string;
	public readonly actualSha256: string;

	public constructor(moduleName: string, expectedSha256: string, actualSha256: string) {
		super(`module ${moduleName} package sha256 mismatch: expected ${expectedSha256}, got ${actualSha256}`);
		this.name = 'ModuleStoreDownloadHashMismatchError';
		this.module = moduleName;
		this.expectedSha256 = expectedSha256;
		this.actualSha256 = actualSha256;
	}
}

export class ModuleStoreInstallationMissingError extends ModuleStoreError {
	public readonly module: string;
	public readonly sha256: string;
	public readonly directory: string;

	public constructor(moduleName: string, sha256: string, directory: string) {
		super(`module ${moduleName} ${sha256} is not installed at ${directory}`);
		this.name = 'ModuleStoreInstallationMissingError';
		this.module = moduleName;
		this.sha256 = sha256;
		this.directory = directory;
	}
}

export class ModuleStoreGarbageCollectionOrderError extends ModuleStoreError {
	public constructor() {
		super('module garbage collection ran before a successful launch');
		this.name = 'ModuleStoreGarbageCollectionOrderError';
	}
}

export interface LinuxModuleSecurityMinimum {
	readonly version: string;
	readonly requiredModules: ReadonlyArray<string>;
}

export interface ModuleManifestFeedIdentity {
	readonly releaseChannel: string;
	readonly platform: string;
	readonly arch: string;
}

export interface ModuleManifestFeedObservation {
	readonly feed: ModuleManifestFeedIdentity;
	readonly metadataVersion: number;
	readonly manifestSha256: string;
}

interface ModuleManifestHighWater extends ModuleManifestFeedIdentity {
	readonly metadataVersion: number;
	readonly manifestSha256: string;
}

interface ModuleStoreState {
	readonly state_version: number;
	readonly shell_version: string;
	readonly release_channel: string;
	readonly committed: Readonly<Record<string, string>>;
	readonly previous: Readonly<Record<string, string>>;
	readonly rejected: Readonly<Record<string, string>>;
	readonly floor: Readonly<Record<string, string>>;
	readonly linux_security_minimum: LinuxModuleSecurityMinimum | null;
	readonly manifest_high_water: ReadonlyArray<ModuleManifestHighWater>;
	readonly last_manifest_etag: string | null;
	readonly last_manifest_fetch: string | null;
	readonly boot_attempt: number;
}

const ModuleStoreStateFileStatus = Object.freeze({
	MISSING: 'missing',
	LOADED: 'loaded',
} as const);

type ModuleStoreStateFile =
	| {readonly status: typeof ModuleStoreStateFileStatus.MISSING}
	| {
			readonly status: typeof ModuleStoreStateFileStatus.LOADED;
			readonly state: ModuleStoreState;
	  };

interface ModuleInstallation {
	readonly module: string;
	readonly sha256: string;
	readonly directory: string;
	readonly manifest: DesktopModuleManifest;
}

interface InstalledModuleFile {
	readonly relativePath: string;
	readonly absolutePath: string;
}

interface ValidInstalledModule {
	readonly manifest: DesktopModuleManifest;
	readonly files: ReadonlyArray<InstalledModuleFile>;
}

interface ModuleBootAttempt {
	readonly rolledBack: boolean;
	readonly bootAttempt: number;
	readonly committed: Readonly<Record<string, string>>;
}

export interface ModuleLaunchAttempt {
	readonly [MODULE_LAUNCH_ATTEMPT]: true;
	readonly committed: Readonly<Record<string, string>>;
}

interface ModuleGarbageCollectionResult {
	readonly removedDirectories: ReadonlyArray<string>;
	readonly removedPackages: ReadonlyArray<string>;
}

export interface ModulePackageDownload {
	readonly offset: number;
	readonly chunks: AsyncIterable<Uint8Array> | Iterable<Uint8Array>;
}

export type ModulePackageDownloader = (resumeFrom: number) => ModulePackageDownload | Promise<ModulePackageDownload>;

export function getModuleStoreRoot(userDataPath: string): string {
	return path.join(userDataPath, MODULE_STORE_DIRECTORY_NAME);
}

export function getModuleStoreTreeRoot(userDataPath: string): string {
	return path.join(getModuleStoreRoot(userDataPath), MODULE_STORE_TREE_DIRECTORY_NAME);
}

function assertModuleName(moduleName: string): void {
	if (!isDesktopModuleName(moduleName)) {
		throw new ModuleStoreError(`unsupported module name: ${moduleName}`);
	}
}

function assertSha256(sha256: string): void {
	if (!SHA256_PATTERN.test(sha256)) {
		throw new ModuleStoreError(`not a sha256 digest: ${sha256}`);
	}
}

function isPathInsideRoot(root: string, candidate: string): boolean {
	const relative = path.relative(root, candidate);
	return relative.length > 0 && !relative.startsWith('..') && !path.isAbsolute(relative);
}

async function createDirectory(directory: string): Promise<void> {
	try {
		await fs.mkdir(directory, {recursive: true});
	} catch (error) {
		throw new ModuleStoreIOError(`failed to create ${directory}`, {cause: error});
	}
}

function isWindowsTransientFsError(error: unknown): boolean {
	return (
		process.platform === 'win32' &&
		typeof error === 'object' &&
		error !== null &&
		'code' in error &&
		typeof error.code === 'string' &&
		WINDOWS_TRANSIENT_FS_ERROR_CODES.has(error.code)
	);
}

async function renameWithRetry(source: string, target: string): Promise<void> {
	for (let attempt = 0; ; attempt += 1) {
		try {
			await fs.rename(source, target);
			return;
		} catch (error) {
			const delay = WINDOWS_TRANSIENT_FS_RETRY_DELAYS_MS[attempt];
			if (delay === undefined || !isWindowsTransientFsError(error)) {
				throw error;
			}
			await new Promise((resolve) => setTimeout(resolve, delay));
		}
	}
}

function removeOptions(): {recursive: true; force: true; maxRetries: number} {
	return {
		recursive: true,
		force: true,
		maxRetries: process.platform === 'win32' ? WINDOWS_TRANSIENT_FS_RETRY_DELAYS_MS.length : 0,
	};
}

async function removeTree(target: string): Promise<void> {
	try {
		await fs.rm(target, removeOptions());
	} catch (error) {
		throw new ModuleStoreIOError(`failed to remove ${target}`, {cause: error});
	}
}

async function renameAside(directory: string, aside: string): Promise<boolean> {
	try {
		await renameWithRetry(directory, aside);
		return true;
	} catch (error) {
		if (typeof error === 'object' && error !== null && 'code' in error && error.code === 'ENOENT') {
			return false;
		}
		throw new ModuleStoreIOError(`failed to set aside ${directory}`, {cause: error});
	}
}

function isPackageDigestMismatch(error: unknown): boolean {
	return (
		error instanceof ModulePackageHashMismatchError && error.message.startsWith(MODULE_PACKAGE_DIGEST_MISMATCH_PREFIX)
	);
}

async function readDirectoryEntries(directory: string): Promise<ReadonlyArray<Dirent>> {
	try {
		return await fs.readdir(directory, {withFileTypes: true});
	} catch {
		return [];
	}
}

async function fsyncFile(filePath: string): Promise<void> {
	let handle: FileHandle;
	try {
		handle = await fs.open(filePath, 'r+');
	} catch (error) {
		throw new ModuleStoreIOError(`failed to open ${filePath} for fsync`, {cause: error});
	}
	try {
		await handle.sync();
	} catch (error) {
		throw new ModuleStoreIOError(`failed to fsync ${filePath}`, {cause: error});
	} finally {
		await handle.close();
	}
}

async function fsyncDirectory(directory: string): Promise<void> {
	if (process.platform === 'win32') {
		return;
	}
	let handle: FileHandle;
	try {
		handle = await fs.open(directory, 'r');
	} catch (error) {
		throw new ModuleStoreIOError(`failed to open ${directory} for fsync`, {cause: error});
	}
	try {
		await handle.sync();
	} catch (error) {
		throw new ModuleStoreIOError(`failed to fsync ${directory}`, {cause: error});
	} finally {
		await handle.close();
	}
}

export async function writeFileAtomically(target: string, contents: Buffer | string, mode: number): Promise<void> {
	const directory = path.dirname(target);
	const temporary = path.join(directory, `${path.basename(target)}.${randomBytes(8).toString('hex')}.tmp`);
	let handle: FileHandle;
	try {
		handle = await fs.open(temporary, 'wx', mode);
	} catch (error) {
		throw new ModuleStoreIOError(`failed to create ${temporary}`, {cause: error});
	}
	try {
		await handle.writeFile(contents, 'utf8');
		await handle.sync();
	} catch (error) {
		await handle.close();
		await fs.rm(temporary, {force: true});
		throw new ModuleStoreIOError(`failed to write ${temporary}`, {cause: error});
	}
	await handle.close();
	try {
		await renameWithRetry(temporary, target);
	} catch (error) {
		await fs.rm(temporary, {force: true});
		throw new ModuleStoreIOError(`failed to publish ${target}`, {cause: error});
	}
	await fsyncDirectory(directory);
}

async function openPartialPackage(partial: string): Promise<FileHandle> {
	try {
		return await fs.open(partial, 'r+');
	} catch (error) {
		if (!isMissingStateFileError(error)) {
			throw new ModuleStoreIOError(`failed to open ${partial}`, {cause: error});
		}
	}
	try {
		return await fs.open(partial, 'wx', DOWNLOAD_FILE_MODE);
	} catch (error) {
		throw new ModuleStoreIOError(`failed to create ${partial}`, {cause: error});
	}
}

async function hashPartialPackage(handle: FileHandle, digest: ReturnType<typeof createHash>): Promise<number> {
	const stats = await handle.stat();
	const chunk = Buffer.allocUnsafe(INSTALLED_FILE_HASH_CHUNK_BYTES);
	let offset = 0;
	while (offset < stats.size) {
		const {bytesRead} = await handle.read(chunk, 0, Math.min(chunk.byteLength, stats.size - offset), offset);
		if (bytesRead === 0) {
			break;
		}
		digest.update(chunk.subarray(0, bytesRead));
		offset += bytesRead;
	}
	if (offset !== stats.size) {
		await handle.truncate(offset);
	}
	return offset;
}

async function fsyncTree(directory: string): Promise<void> {
	const files: Array<string> = [];
	const directories: Array<string> = [];
	const walk = async (current: string): Promise<void> => {
		for (const entry of await readDirectoryEntries(current)) {
			const child = path.join(current, entry.name);
			if (entry.isDirectory()) {
				await walk(child);
			} else if (entry.isFile()) {
				files.push(child);
			}
		}
		directories.push(current);
	};
	await walk(directory);
	await forEachConcurrently(files, fsyncFile, TREE_FSYNC_CONCURRENCY);
	await forEachConcurrently(directories, fsyncDirectory, TREE_FSYNC_CONCURRENCY);
}

async function makeInstallationDurable(directory: string, stamps: InstalledFileStamps | null): Promise<void> {
	await fsyncTree(directory);
	if (stamps != null) {
		await writeInstalledFileStamps(directory, stamps.manifestSha256, stamps.files);
	}
}

async function modifiedAt(target: string): Promise<number | null> {
	try {
		const stats = await fs.stat(target);
		return stats.mtimeMs;
	} catch {
		return null;
	}
}

async function isRegularFile(target: string): Promise<boolean> {
	try {
		const stats = await fs.stat(target);
		return stats.isFile();
	} catch {
		return false;
	}
}

function isMissingInstallationError(error: unknown): boolean {
	return (
		typeof error === 'object' &&
		error !== null &&
		'code' in error &&
		typeof error.code === 'string' &&
		MISSING_INSTALLATION_ERROR_CODES.has(error.code)
	);
}

function readOnlyNoFollowFlags(): number {
	const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
	return fsConstants.O_RDONLY | noFollow;
}

async function installationPathStats(target: string): Promise<Stats | null> {
	try {
		return await fs.lstat(target);
	} catch (error) {
		if (isMissingInstallationError(error)) {
			return null;
		}
		throw new ModuleStoreIOError(`failed to inspect installed module path ${target}`, {cause: error});
	}
}

async function readInstalledManifestBytes(target: string): Promise<Buffer | null> {
	let handle: FileHandle;
	try {
		handle = await fs.open(target, readOnlyNoFollowFlags());
	} catch (error) {
		if (isMissingInstallationError(error)) {
			return null;
		}
		throw new ModuleStoreIOError(`failed to open installed module file ${target}`, {cause: error});
	}
	try {
		const stats = await handle.stat();
		if (
			!stats.isFile() ||
			!Number.isSafeInteger(stats.size) ||
			stats.size < 0 ||
			stats.size > DESKTOP_MODULE_MANIFEST_MAX_BYTES
		) {
			return null;
		}
		const bytes = Buffer.alloc(stats.size);
		let offset = 0;
		while (offset < stats.size) {
			const {bytesRead} = await handle.read(bytes, offset, stats.size - offset, offset);
			if (bytesRead === 0) {
				return null;
			}
			offset += bytesRead;
		}
		const trailing = Buffer.allocUnsafe(1);
		const {bytesRead: trailingBytes} = await handle.read(trailing, 0, trailing.length, stats.size);
		return trailingBytes === 0 ? bytes : null;
	} catch (error) {
		throw new ModuleStoreIOError(`failed to read installed module file ${target}`, {cause: error});
	} finally {
		await handle.close();
	}
}

async function installedFileMatches(
	target: string,
	expectedBytes: number,
	expectedSha256: string,
): Promise<Stats | null> {
	let handle: FileHandle;
	try {
		handle = await fs.open(target, readOnlyNoFollowFlags());
	} catch (error) {
		if (isMissingInstallationError(error)) {
			return null;
		}
		throw new ModuleStoreIOError(`failed to open installed module file ${target}`, {cause: error});
	}
	try {
		const stats = await handle.stat();
		if (!stats.isFile() || !Number.isSafeInteger(stats.size) || stats.size < 0 || stats.size !== expectedBytes) {
			return null;
		}
		const digest = createHash('sha256');
		const chunk = Buffer.allocUnsafe(Math.min(INSTALLED_FILE_HASH_CHUNK_BYTES, Math.max(1, expectedBytes)));
		let offset = 0;
		while (offset < expectedBytes) {
			const length = Math.min(chunk.byteLength, expectedBytes - offset);
			const {bytesRead} = await handle.read(chunk, 0, length, offset);
			if (bytesRead === 0) {
				return null;
			}
			digest.update(chunk.subarray(0, bytesRead));
			offset += bytesRead;
		}
		const trailing = Buffer.allocUnsafe(1);
		const {bytesRead: trailingBytes} = await handle.read(trailing, 0, trailing.byteLength, expectedBytes);
		return trailingBytes === 0 && digest.digest('hex') === expectedSha256 ? stats : null;
	} catch (error) {
		throw new ModuleStoreIOError(`failed to verify installed module file ${target}`, {cause: error});
	} finally {
		await handle.close();
	}
}

type InstalledFileStamp = readonly [size: number, mtimeMs: number, ctimeMs: number, ino: number];

interface InstalledFileStamps {
	readonly manifestSha256: string;
	readonly files: ReadonlyMap<string, InstalledFileStamp>;
}

function fileStampOf(stats: Stats): InstalledFileStamp {
	return [stats.size, stats.mtimeMs, stats.ctimeMs, stats.ino];
}

function fileStampMatches(stamp: InstalledFileStamp | undefined, stats: Stats, expectedBytes: number): boolean {
	return (
		stamp !== undefined &&
		stats.isFile() &&
		stats.size === expectedBytes &&
		stamp[0] === stats.size &&
		stamp[1] === stats.mtimeMs &&
		stamp[2] === stats.ctimeMs &&
		stamp[3] === stats.ino
	);
}

function manifestDeclaresStampFile(manifest: DesktopModuleManifest): boolean {
	const reserved = MODULE_FILE_STAMPS_NAME.toLowerCase();
	return manifest.files.some((file) => file.path.toLowerCase() === reserved);
}

function serializeInstalledFileStamps(manifestSha256: string, files: ReadonlyMap<string, InstalledFileStamp>): string {
	return JSON.stringify({
		version: MODULE_FILE_STAMPS_VERSION,
		manifest_sha256: manifestSha256,
		files: Object.fromEntries(files),
	});
}

function isInstalledFileStamp(value: unknown): value is InstalledFileStamp {
	return (
		Array.isArray(value) &&
		value.length === 4 &&
		value.every((component) => typeof component === 'number' && Number.isFinite(component))
	);
}

async function readInstalledFileStamps(directory: string): Promise<InstalledFileStamps | null> {
	let raw: string;
	try {
		const target = path.join(directory, MODULE_FILE_STAMPS_NAME);
		const stats = await fs.lstat(target);
		if (!stats.isFile() || stats.size > MODULE_FILE_STAMPS_MAX_BYTES) {
			return null;
		}
		raw = await fs.readFile(target, 'utf8');
	} catch {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		return null;
	}
	if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
		return null;
	}
	const record = parsed as Record<string, unknown>;
	const manifestSha256 = record['manifest_sha256'];
	const rawFiles = record['files'];
	if (
		record['version'] !== MODULE_FILE_STAMPS_VERSION ||
		typeof manifestSha256 !== 'string' ||
		!SHA256_PATTERN.test(manifestSha256) ||
		typeof rawFiles !== 'object' ||
		rawFiles === null ||
		Array.isArray(rawFiles)
	) {
		return null;
	}
	const files = new Map<string, InstalledFileStamp>();
	for (const [relativePath, stamp] of Object.entries(rawFiles as Record<string, unknown>)) {
		if (isInstalledFileStamp(stamp)) {
			files.set(relativePath, stamp);
		}
	}
	return {manifestSha256, files};
}

async function writeInstalledFileStamps(
	directory: string,
	manifestSha256: string,
	files: ReadonlyMap<string, InstalledFileStamp>,
): Promise<void> {
	await writeFileAtomically(
		path.join(directory, MODULE_FILE_STAMPS_NAME),
		serializeInstalledFileStamps(manifestSha256, files),
		MODULE_FILE_STAMPS_MODE,
	);
}

async function stampExtractedModule(
	directory: string,
	manifest: DesktopModuleManifest,
): Promise<InstalledFileStamps | null> {
	if (manifestDeclaresStampFile(manifest)) {
		return null;
	}
	const manifestBytes = await readInstalledManifestBytes(path.join(directory, DESKTOP_MODULE_FILE_LIST_NAME));
	if (manifestBytes == null) {
		return null;
	}
	const files = new Map<string, InstalledFileStamp>();
	await forEachConcurrently(manifest.files, async (file) => {
		const stats = await fs.lstat(path.resolve(directory, file.path));
		files.set(file.path, fileStampOf(stats));
	});
	return {manifestSha256: createHash('sha256').update(manifestBytes).digest('hex'), files};
}

async function forEachConcurrently<T>(
	items: ReadonlyArray<T>,
	operation: (item: T) => Promise<void>,
	concurrency: number = INSTALLED_FILE_CHECK_CONCURRENCY,
): Promise<void> {
	let next = 0;
	const worker = async (): Promise<void> => {
		while (next < items.length) {
			const item = items[next];
			next += 1;
			await operation(item);
		}
	};
	await Promise.all(Array.from({length: Math.min(concurrency, items.length)}, worker));
}

async function installedFileStats(target: string): Promise<Stats | null> {
	try {
		return await fs.lstat(target);
	} catch (error) {
		if (isMissingInstallationError(error)) {
			return null;
		}
		throw new ModuleStoreIOError(`failed to inspect installed module file ${target}`, {cause: error});
	}
}

async function readValidInstalledModule(
	directory: string,
	moduleName: string,
	pendingStamps: InstalledFileStamps | null = null,
): Promise<ValidInstalledModule | null> {
	const directoryStats = await installationPathStats(directory);
	if (directoryStats == null || !directoryStats.isDirectory() || directoryStats.isSymbolicLink()) {
		return null;
	}
	const bytes = await readInstalledManifestBytes(path.join(directory, DESKTOP_MODULE_FILE_LIST_NAME));
	if (bytes == null) {
		return null;
	}
	let manifest: DesktopModuleManifest;
	try {
		manifest = parseDesktopModuleManifest(bytes);
	} catch {
		return null;
	}
	if (manifest.module !== moduleName) {
		return null;
	}
	const validatedDirectories = new Set<string>([directory]);
	const files: Array<InstalledModuleFile> = [];
	for (const file of manifest.files) {
		const segments = file.path.split('/');
		let current = directory;
		for (let index = 0; index < segments.length - 1; index += 1) {
			current = path.join(current, segments[index]);
			if (validatedDirectories.has(current)) {
				continue;
			}
			const parentStats = await installationPathStats(current);
			if (parentStats == null || !parentStats.isDirectory() || parentStats.isSymbolicLink()) {
				return null;
			}
			validatedDirectories.add(current);
		}
		const absolutePath = path.resolve(directory, file.path);
		if (!isPathInsideRoot(directory, absolutePath)) {
			return null;
		}
		files.push({relativePath: file.path, absolutePath});
	}
	const stampable = !manifestDeclaresStampFile(manifest);
	const manifestSha256 = createHash('sha256').update(bytes).digest('hex');
	const persisted = stampable ? (pendingStamps ?? (await readInstalledFileStamps(directory))) : null;
	const trusted = persisted != null && persisted.manifestSha256 === manifestSha256 ? persisted.files : null;
	const stamps = new Map<string, InstalledFileStamp>();
	let restamped = trusted == null;
	let intact = true;
	await forEachConcurrently(manifest.files, async (file) => {
		if (!intact) {
			return;
		}
		const absolutePath = path.resolve(directory, file.path);
		const stamp = trusted?.get(file.path);
		const stats = await installedFileStats(absolutePath);
		if (stats != null && fileStampMatches(stamp, stats, file.bytes)) {
			stamps.set(file.path, stamp as InstalledFileStamp);
			return;
		}
		const verified = await installedFileMatches(absolutePath, file.bytes, file.sha256);
		if (verified == null) {
			intact = false;
			return;
		}
		restamped = true;
		stamps.set(file.path, fileStampOf(verified));
	});
	if (!intact) {
		return null;
	}
	if (stampable && restamped && pendingStamps == null) {
		await writeInstalledFileStamps(directory, manifestSha256, stamps).catch(() => undefined);
	}
	return {manifest, files};
}

export function sameModuleMap(
	left: Readonly<Record<string, string>>,
	right: Readonly<Record<string, string>>,
): boolean {
	const leftKeys = Object.keys(left);
	if (leftKeys.length !== Object.keys(right).length) {
		return false;
	}
	return leftKeys.every((key) => left[key] === right[key]);
}

function sortModuleMap(value: Readonly<Record<string, string>>): Record<string, string> {
	const sorted: Record<string, string> = {};
	for (const key of Object.keys(value).sort()) {
		sorted[key] = value[key];
	}
	return sorted;
}

function moduleManifestFeedKey(feed: ModuleManifestFeedIdentity): string {
	return `${feed.releaseChannel}/${feed.platform}/${feed.arch}`;
}

function assertManifestFeedIdentity(feed: ModuleManifestFeedIdentity): void {
	for (const [name, value] of Object.entries(feed)) {
		if (!MODULE_FEED_COMPONENT_PATTERN.test(value)) {
			throw new ModuleStoreError(`invalid module manifest feed ${name}: ${value}`);
		}
	}
}

function sortManifestHighWater(
	entries: ReadonlyArray<ModuleManifestHighWater>,
): ReadonlyArray<ModuleManifestHighWater> {
	return Object.freeze(
		[...entries]
			.sort((left, right) => moduleManifestFeedKey(left).localeCompare(moduleManifestFeedKey(right)))
			.map((entry) => Object.freeze({...entry})),
	);
}

function parseManifestHighWater(value: unknown): ReadonlyArray<ModuleManifestHighWater> | null {
	if (!Array.isArray(value) || value.length > MODULE_MANIFEST_HIGH_WATER_MAX_ENTRIES) {
		return null;
	}
	const entries: Array<ModuleManifestHighWater> = [];
	const feeds = new Set<string>();
	for (const rawEntry of value) {
		if (typeof rawEntry !== 'object' || rawEntry === null || Array.isArray(rawEntry)) {
			return null;
		}
		const record = rawEntry as Record<string, unknown>;
		const releaseChannel = record['release_channel'];
		const platform = record['platform'];
		const arch = record['arch'];
		const metadataVersion = record['metadata_version'];
		const manifestSha256 = record['manifest_sha256'];
		if (
			typeof releaseChannel !== 'string' ||
			typeof platform !== 'string' ||
			typeof arch !== 'string' ||
			!MODULE_FEED_COMPONENT_PATTERN.test(releaseChannel) ||
			!MODULE_FEED_COMPONENT_PATTERN.test(platform) ||
			!MODULE_FEED_COMPONENT_PATTERN.test(arch) ||
			typeof metadataVersion !== 'number' ||
			!Number.isSafeInteger(metadataVersion) ||
			metadataVersion < 0 ||
			typeof manifestSha256 !== 'string' ||
			!SHA256_PATTERN.test(manifestSha256)
		) {
			return null;
		}
		const entry = {releaseChannel, platform, arch, metadataVersion, manifestSha256};
		const feedKey = moduleManifestFeedKey(entry);
		if (feeds.has(feedKey)) {
			return null;
		}
		feeds.add(feedKey);
		entries.push(entry);
	}
	return sortManifestHighWater(entries);
}

function assertManifestObservation(observation: ModuleManifestFeedObservation): void {
	assertManifestFeedIdentity(observation.feed);
	if (!Number.isSafeInteger(observation.metadataVersion) || observation.metadataVersion < 0) {
		throw new ModuleStoreError(`invalid module manifest metadata_version: ${observation.metadataVersion}`);
	}
	assertSha256(observation.manifestSha256);
}

function retainedManifestHighWater(
	entries: ReadonlyArray<ModuleManifestHighWater>,
	feed: ModuleManifestFeedIdentity,
): ModuleManifestHighWater | null {
	const feedKey = moduleManifestFeedKey(feed);
	return entries.find((entry) => moduleManifestFeedKey(entry) === feedKey) ?? null;
}

function requireManifestFresh(
	entries: ReadonlyArray<ModuleManifestHighWater>,
	observation: ModuleManifestFeedObservation,
): void {
	assertManifestObservation(observation);
	const retained = retainedManifestHighWater(entries, observation.feed);
	if (retained == null) {
		return;
	}
	if (observation.metadataVersion < retained.metadataVersion) {
		throw new ModuleManifestRollbackError(observation.feed, observation.metadataVersion, retained.metadataVersion);
	}
	if (
		observation.metadataVersion === retained.metadataVersion &&
		observation.manifestSha256 !== retained.manifestSha256
	) {
		throw new ModuleManifestEquivocationError(observation.feed, observation.metadataVersion);
	}
}

function advanceManifestHighWater(
	entries: ReadonlyArray<ModuleManifestHighWater>,
	observation: ModuleManifestFeedObservation,
): ReadonlyArray<ModuleManifestHighWater> {
	requireManifestFresh(entries, observation);
	const feedKey = moduleManifestFeedKey(observation.feed);
	const retained = retainedManifestHighWater(entries, observation.feed);
	if (
		retained != null &&
		retained.metadataVersion === observation.metadataVersion &&
		retained.manifestSha256 === observation.manifestSha256
	) {
		return entries;
	}
	if (retained == null && entries.length >= MODULE_MANIFEST_HIGH_WATER_MAX_ENTRIES) {
		throw new ModuleManifestHighWaterCapacityError();
	}
	return sortManifestHighWater([
		...entries.filter((entry) => moduleManifestFeedKey(entry) !== feedKey),
		{
			...observation.feed,
			metadataVersion: observation.metadataVersion,
			manifestSha256: observation.manifestSha256,
		},
	]);
}

function parseModuleMap(value: unknown): Record<string, string> | null {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		return null;
	}
	const parsed: Record<string, string> = {};
	for (const [moduleName, sha256] of Object.entries(value as Record<string, unknown>)) {
		if (!isDesktopModuleName(moduleName)) {
			return null;
		}
		if (typeof sha256 !== 'string' || !SHA256_PATTERN.test(sha256)) {
			return null;
		}
		parsed[moduleName] = sha256;
	}
	return sortModuleMap(parsed);
}

function retainRejected(
	rejected: Readonly<Record<string, string>>,
	current: Readonly<Record<string, string>>,
): Record<string, string> {
	const retained: Record<string, string> = {};
	for (const [moduleName, sha256] of Object.entries(rejected)) {
		if (current[moduleName] === undefined || current[moduleName] === sha256) {
			retained[moduleName] = sha256;
		}
	}
	return retained;
}

function parseLinuxSecurityMinimum(value: unknown): LinuxModuleSecurityMinimum | null | undefined {
	if (value === undefined || value === null) {
		return null;
	}
	if (typeof value !== 'object' || Array.isArray(value)) {
		return undefined;
	}
	const record = value as Record<string, unknown>;
	const version = record['version'];
	const rawRequiredModules = record['required_modules'];
	if (typeof version !== 'string' || !Array.isArray(rawRequiredModules)) {
		return undefined;
	}
	try {
		parseModuleVersion(version, 'persisted Linux security minimum');
	} catch {
		return undefined;
	}
	const requiredModules: Array<string> = [];
	const seen = new Set<string>();
	for (const moduleName of rawRequiredModules) {
		if (!isDesktopModuleName(moduleName) || seen.has(moduleName)) {
			return undefined;
		}
		seen.add(moduleName);
		requiredModules.push(moduleName);
	}
	return {version, requiredModules: Object.freeze(requiredModules.sort())};
}

function advanceLinuxSecurityMinimum(
	current: LinuxModuleSecurityMinimum | null,
	candidate: LinuxModuleSecurityMinimum,
): LinuxModuleSecurityMinimum {
	const version = parseModuleVersion(candidate.version, 'Linux security minimum');
	const requiredModules = Array.from(new Set(candidate.requiredModules)).sort();
	if (requiredModules.length !== candidate.requiredModules.length) {
		throw new ModuleStoreError('Linux security minimum contains duplicate required modules');
	}
	for (const moduleName of requiredModules) {
		assertModuleName(moduleName);
	}
	if (current != null) {
		const comparison = compareModuleVersions(
			version,
			parseModuleVersion(current.version, 'persisted Linux security minimum'),
		);
		if (comparison < 0) {
			throw new ModuleStoreError(
				`refusing to lower Linux security minimum from ${current.version} to ${candidate.version}`,
			);
		}
		if (comparison === 0 && requiredModules.join('\0') !== current.requiredModules.join('\0')) {
			throw new ModuleStoreError(`Linux security minimum ${candidate.version} changed its required modules`);
		}
	}
	return {version: candidate.version, requiredModules: Object.freeze(requiredModules)};
}

function parseOptionalString(value: unknown): string | null | undefined {
	if (value === null) {
		return null;
	}
	return typeof value === 'string' ? value : undefined;
}

function isMissingStateFileError(error: unknown): boolean {
	return (
		typeof error === 'object' &&
		error !== null &&
		'code' in error &&
		typeof error.code === 'string' &&
		error.code === 'ENOENT'
	);
}

function parseModuleStoreState(bytes: Buffer, statePath: string): ModuleStoreState {
	let parsed: unknown;
	try {
		parsed = JSON.parse(bytes.toString('utf8'));
	} catch (error) {
		throw new ModuleStoreStateCorruptError(statePath, 'state is not valid JSON', {cause: error});
	}
	if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
		throw new ModuleStoreStateCorruptError(statePath, 'state root must be an object');
	}
	const record = parsed as Record<string, unknown>;
	const stateVersion = record['state_version'];
	if (stateVersion !== MODULE_STATE_VERSION) {
		throw new ModuleStoreStateUnsupportedVersionError(statePath, stateVersion);
	}
	const shellVersion = record['shell_version'];
	const releaseChannel = record['release_channel'];
	if (
		typeof shellVersion !== 'string' ||
		shellVersion.length === 0 ||
		typeof releaseChannel !== 'string' ||
		releaseChannel.length === 0
	) {
		throw new ModuleStoreStateCorruptError(statePath, 'shell_version and release_channel are required');
	}
	const committed = parseModuleMap(record['committed']);
	const previous = parseModuleMap(record['previous']);
	const rejected = record['rejected'] === undefined ? {} : parseModuleMap(record['rejected']);
	const floor = parseModuleMap(record['floor']);
	const rawLinuxSecurityMinimum = record['linux_security_minimum'];
	if (rawLinuxSecurityMinimum === undefined) {
		throw new ModuleStoreStateCorruptError(statePath, 'linux_security_minimum is required');
	}
	const linuxSecurityMinimum = parseLinuxSecurityMinimum(rawLinuxSecurityMinimum);
	if (
		committed === null ||
		previous === null ||
		rejected === null ||
		floor === null ||
		linuxSecurityMinimum === undefined
	) {
		throw new ModuleStoreStateCorruptError(statePath, 'module maps or Linux security minimum are invalid');
	}
	const manifestHighWater = parseManifestHighWater(record['manifest_high_water']);
	if (manifestHighWater === null) {
		throw new ModuleStoreStateCorruptError(statePath, 'manifest_high_water is invalid');
	}
	const lastManifestEtag = parseOptionalString(record['last_manifest_etag']);
	const lastManifestFetch = parseOptionalString(record['last_manifest_fetch']);
	if (lastManifestEtag === undefined || lastManifestFetch === undefined) {
		throw new ModuleStoreStateCorruptError(statePath, 'manifest fetch metadata is invalid');
	}
	const bootAttempt = record['boot_attempt'];
	if (typeof bootAttempt !== 'number' || !Number.isSafeInteger(bootAttempt) || bootAttempt < 0) {
		throw new ModuleStoreStateCorruptError(statePath, 'boot_attempt is invalid');
	}
	return {
		state_version: MODULE_STATE_VERSION,
		shell_version: shellVersion,
		release_channel: releaseChannel,
		committed,
		previous,
		rejected,
		floor,
		linux_security_minimum: linuxSecurityMinimum,
		manifest_high_water: manifestHighWater,
		last_manifest_etag: lastManifestEtag,
		last_manifest_fetch: lastManifestFetch,
		boot_attempt: bootAttempt,
	};
}

function createInitialModuleStoreState(shellVersion: string, releaseChannel: string): ModuleStoreState {
	return {
		state_version: MODULE_STATE_VERSION,
		shell_version: shellVersion,
		release_channel: releaseChannel,
		committed: {},
		previous: {},
		rejected: {},
		floor: {},
		linux_security_minimum: null,
		manifest_high_water: [],
		last_manifest_etag: null,
		last_manifest_fetch: null,
		boot_attempt: 0,
	};
}

function serializeModuleStoreState(state: ModuleStoreState): string {
	return `${JSON.stringify(
		{
			state_version: state.state_version,
			shell_version: state.shell_version,
			release_channel: state.release_channel,
			committed: sortModuleMap(state.committed),
			previous: sortModuleMap(state.previous),
			rejected: sortModuleMap(state.rejected),
			floor: sortModuleMap(state.floor),
			linux_security_minimum:
				state.linux_security_minimum == null
					? null
					: {
							version: state.linux_security_minimum.version,
							required_modules: state.linux_security_minimum.requiredModules,
						},
			manifest_high_water: state.manifest_high_water.map((entry) => ({
				release_channel: entry.releaseChannel,
				platform: entry.platform,
				arch: entry.arch,
				metadata_version: entry.metadataVersion,
				manifest_sha256: entry.manifestSha256,
			})),
			last_manifest_etag: state.last_manifest_etag,
			last_manifest_fetch: state.last_manifest_fetch,
			boot_attempt: state.boot_attempt,
		},
		null,
		'\t',
	)}\n`;
}

export class ModuleStore {
	public readonly root: string;
	public readonly statePath: string;
	public readonly downloadRoot: string;
	public readonly incomingDownloadRoot: string;
	public readonly storeRoot: string;
	private state: ModuleStoreState;
	private convergingShellVersion: string | null = null;
	private launchSucceeded = false;
	private activeLaunchAttempt: ModuleLaunchAttempt | null = null;
	private readonly installCoordinator = new ModuleInstallCoordinator<ModuleInstallation>();
	private readonly installationsAwaitingCommit = new Set<string>();
	private readonly installationsAwaitingDurability = new Map<string, InstalledFileStamps | null>();
	private durabilityWork: Promise<void> = Promise.resolve();
	private readonly packagesInUse = new Set<string>();
	private stateWrites: Promise<void> = Promise.resolve();
	private storeTreeWrites: Promise<void> = Promise.resolve();

	private constructor(root: string, state: ModuleStoreState) {
		this.root = root;
		this.statePath = path.join(root, MODULE_STATE_FILE_NAME);
		this.downloadRoot = path.join(root, MODULE_DOWNLOAD_DIRECTORY_NAME);
		this.incomingDownloadRoot = path.join(this.downloadRoot, MODULE_DOWNLOAD_INCOMING_DIRECTORY_NAME);
		this.storeRoot = path.join(root, MODULE_STORE_TREE_DIRECTORY_NAME);
		this.state = state;
	}

	public static async open({
		root,
		shellVersion,
		releaseChannel,
	}: {
		readonly root: string;
		readonly shellVersion: string;
		readonly releaseChannel: string;
	}): Promise<ModuleStore> {
		const resolved = path.resolve(root);
		await createDirectory(resolved);
		await createDirectory(path.join(resolved, MODULE_DOWNLOAD_DIRECTORY_NAME, MODULE_DOWNLOAD_INCOMING_DIRECTORY_NAME));
		await createDirectory(path.join(resolved, MODULE_STORE_TREE_DIRECTORY_NAME));
		const stateFile = await ModuleStore.readState(path.join(resolved, MODULE_STATE_FILE_NAME));
		if (stateFile.status === ModuleStoreStateFileStatus.MISSING) {
			const store = new ModuleStore(resolved, createInitialModuleStoreState(shellVersion, releaseChannel));
			await store.writeState(store.state);
			return store;
		}
		const store = new ModuleStore(resolved, stateFile.state);
		if (stateFile.state.shell_version !== shellVersion) {
			store.convergingShellVersion = shellVersion;
		}
		if (stateFile.state.release_channel !== releaseChannel) {
			await store.writeState({...store.state, release_channel: releaseChannel});
		}
		return store;
	}

	private static async readState(statePath: string): Promise<ModuleStoreStateFile> {
		let handle: FileHandle;
		try {
			handle = await fs.open(statePath, readOnlyNoFollowFlags());
		} catch (error) {
			if (isMissingStateFileError(error)) {
				return {status: ModuleStoreStateFileStatus.MISSING};
			}
			throw new ModuleStoreIOError(`failed to open module state at ${statePath}`, {cause: error});
		}
		try {
			const stats = await handle.stat();
			if (!stats.isFile()) {
				throw new ModuleStoreStateCorruptError(statePath, 'state path is not a regular file');
			}
			if (!Number.isSafeInteger(stats.size) || stats.size < 0 || stats.size > MODULE_STATE_MAX_BYTES) {
				throw new ModuleStoreStateCorruptError(statePath, `state exceeds ${MODULE_STATE_MAX_BYTES} bytes`);
			}
			const buffer = Buffer.allocUnsafe(MODULE_STATE_MAX_BYTES + 1);
			let offset = 0;
			while (offset < buffer.byteLength) {
				const {bytesRead} = await handle.read(buffer, offset, buffer.byteLength - offset, offset);
				if (bytesRead === 0) {
					break;
				}
				offset += bytesRead;
			}
			if (offset > MODULE_STATE_MAX_BYTES) {
				throw new ModuleStoreStateCorruptError(statePath, `state exceeds ${MODULE_STATE_MAX_BYTES} bytes`);
			}
			const state = parseModuleStoreState(buffer.subarray(0, offset), statePath);
			return {status: ModuleStoreStateFileStatus.LOADED, state};
		} catch (error) {
			if (error instanceof ModuleStoreError) {
				throw error;
			}
			throw new ModuleStoreIOError(`failed to read module state at ${statePath}`, {cause: error});
		} finally {
			await handle.close();
		}
	}

	public get shellVersionChanged(): boolean {
		return this.convergingShellVersion != null;
	}

	public async recordShellVersionConverged(): Promise<void> {
		const shellVersion = this.convergingShellVersion;
		if (shellVersion == null) {
			return;
		}
		await this.withStateLock(async () => {
			if (this.state.shell_version !== shellVersion) {
				await this.writeState({...this.state, shell_version: shellVersion});
			}
		});
	}

	public getState(): ModuleStoreState {
		return this.state;
	}

	public getCommitted(): Readonly<Record<string, string>> {
		return this.state.committed;
	}

	public getPackagePath(sha256: string): string {
		assertSha256(sha256);
		return path.join(this.downloadRoot, `${sha256}${MODULE_PACKAGE_FILE_EXTENSION}`);
	}

	public getModuleDirectory(moduleName: string, sha256: string): string {
		assertModuleName(moduleName);
		assertSha256(sha256);
		return path.join(this.storeRoot, moduleName, sha256);
	}

	public async isInstalled(moduleName: string, sha256: string): Promise<boolean> {
		return (await this.getInstalledManifest(moduleName, sha256)) != null;
	}

	public async getInstalledManifest(moduleName: string, sha256: string): Promise<DesktopModuleManifest | null> {
		assertModuleName(moduleName);
		assertSha256(sha256);
		return (
			(await this.readValidInstallation(this.getModuleDirectory(moduleName, sha256), moduleName))?.manifest ?? null
		);
	}

	public async installModule({
		module: moduleName,
		sha256,
		download,
	}: {
		readonly module: string;
		readonly sha256: string;
		readonly download: ModulePackageDownloader;
	}): Promise<ModuleInstallation> {
		assertModuleName(moduleName);
		assertSha256(sha256);
		return this.installCoordinator.run(moduleName, sha256, () =>
			this.installModuleExclusively(moduleName, sha256, download),
		);
	}

	private async installModuleExclusively(
		moduleName: string,
		sha256: string,
		download: ModulePackageDownloader,
	): Promise<ModuleInstallation> {
		const packagePath = this.getPackagePath(sha256);
		this.packagesInUse.add(sha256);
		try {
			const cached = await isRegularFile(packagePath);
			if (!cached) {
				await this.downloadPackage(moduleName, sha256, packagePath, download);
			}
			return await this.withStoreTreeLock(() => this.publishInstallation(moduleName, sha256, packagePath, cached));
		} finally {
			this.packagesInUse.delete(sha256);
		}
	}

	private async publishInstallation(
		moduleName: string,
		sha256: string,
		packagePath: string,
		cached: boolean,
	): Promise<ModuleInstallation> {
		const directory = this.getModuleDirectory(moduleName, sha256);
		const installed = await this.readValidInstallation(directory, moduleName);
		if (installed != null) {
			this.installationsAwaitingCommit.add(`${moduleName}/${sha256}`);
			return {module: moduleName, sha256, directory, manifest: installed.manifest};
		}
		const incoming = path.join(this.storeRoot, `${MODULE_INCOMING_STORE_PREFIX}${sha256}`);
		await removeTree(incoming);
		const aside = path.join(
			this.storeRoot,
			`${MODULE_REPLACED_STORE_PREFIX}${sha256}-${randomBytes(8).toString('hex')}`,
		);
		let replaced = false;
		let manifest: DesktopModuleManifest;
		let stamps: InstalledFileStamps | null = null;
		try {
			manifest = await verifyAndExtract({
				packedPath: packagePath,
				expectedSha256: sha256,
				destinationDir: incoming,
			});
			if (manifest.module !== moduleName) {
				throw new ModuleStoreError(`package ${sha256} declares module ${manifest.module}, expected ${moduleName}`);
			}
			stamps = await stampExtractedModule(incoming, manifest).catch(() => null);
			await createDirectory(path.dirname(directory));
			replaced = await renameAside(directory, aside);
			try {
				await renameWithRetry(incoming, directory);
			} catch (error) {
				if (replaced) {
					replaced = false;
					await renameWithRetry(aside, directory).catch(() => undefined);
				}
				throw new ModuleStoreIOError(`failed to publish ${directory}`, {cause: error});
			}
		} catch (error) {
			await removeTree(incoming);
			if (cached && isPackageDigestMismatch(error)) {
				await fs.rm(packagePath, {force: true});
			}
			throw error;
		}
		await fsyncDirectory(path.dirname(directory));
		this.installationsAwaitingCommit.add(`${moduleName}/${sha256}`);
		this.installationsAwaitingDurability.set(directory, stamps);
		if (this.launchSucceeded) {
			void this.settleInstalledModules();
		}
		if (replaced) {
			await removeTree(aside).catch(() => undefined);
		}
		return {module: moduleName, sha256, directory, manifest};
	}

	public settleInstalledModules(): Promise<void> {
		const settle = async (): Promise<void> => {
			for (const [directory, stamps] of [...this.installationsAwaitingDurability]) {
				await makeInstallationDurable(directory, stamps).catch(() => undefined);
				if (this.installationsAwaitingDurability.get(directory) === stamps) {
					this.installationsAwaitingDurability.delete(directory);
				}
			}
		};
		this.durabilityWork = this.durabilityWork.then(settle, settle);
		return this.durabilityWork;
	}

	private readValidInstallation(directory: string, moduleName: string): Promise<ValidInstalledModule | null> {
		return readValidInstalledModule(directory, moduleName, this.installationsAwaitingDurability.get(directory) ?? null);
	}

	public getPartialPackagePath(sha256: string): string {
		assertSha256(sha256);
		return path.join(this.incomingDownloadRoot, `${sha256}${MODULE_PARTIAL_PACKAGE_FILE_EXTENSION}`);
	}

	private async downloadPackage(
		moduleName: string,
		sha256: string,
		packagePath: string,
		download: ModulePackageDownloader,
	): Promise<void> {
		await createDirectory(this.incomingDownloadRoot);
		const partial = this.getPartialPackagePath(sha256);
		const handle = await openPartialPackage(partial);
		let digest = createHash('sha256');
		let position: number;
		try {
			position = await hashPartialPackage(handle, digest);
		} catch (error) {
			await handle.close();
			await fs.rm(partial, {force: true});
			throw new ModuleStoreIOError(`failed to read ${partial}`, {cause: error});
		}
		try {
			const stream = await download(position);
			if (stream.offset !== position) {
				if (stream.offset !== 0) {
					throw new ModuleStoreError(
						`package download for ${moduleName} resumed at byte ${stream.offset}, expected ${position}`,
					);
				}
				await handle.truncate(0);
				digest = createHash('sha256');
				position = 0;
			}
			for await (const chunk of stream.chunks) {
				digest.update(chunk);
				let written = 0;
				while (written < chunk.byteLength) {
					const {bytesWritten} = await handle.write(chunk, written, chunk.byteLength - written, position);
					written += bytesWritten;
					position += bytesWritten;
				}
			}
			await handle.sync();
		} catch (error) {
			await handle.close();
			throw error instanceof ModuleStoreError
				? error
				: new ModuleStoreIOError(`failed to download module ${moduleName}`, {cause: error});
		}
		await handle.close();
		const actual = digest.digest('hex');
		if (actual !== sha256) {
			await fs.rm(partial, {force: true});
			throw new ModuleStoreDownloadHashMismatchError(moduleName, sha256, actual);
		}
		try {
			await renameWithRetry(partial, packagePath);
		} catch (error) {
			throw new ModuleStoreIOError(`failed to publish ${packagePath}`, {cause: error});
		}
		await fsyncDirectory(this.downloadRoot);
	}

	public async commit(next: Readonly<Record<string, string>>): Promise<ModuleStoreState> {
		return await this.withStateLock(async () => {
			const committed = await this.resolveInstalled(next);
			return await this.writeState({...this.state, committed, previous: this.state.committed});
		});
	}

	public async mergeCommitted(entries: Readonly<Record<string, string>>): Promise<ModuleStoreState> {
		return await this.withStateLock(async () => {
			const added = await this.resolveInstalled(entries);
			const committed = {...this.state.committed, ...added};
			if (sameModuleMap(this.state.committed, committed)) {
				return this.state;
			}
			return await this.writeState({...this.state, committed});
		});
	}

	public async activateMergedForRendererReload(
		installed: Readonly<Record<string, string>>,
		dropped: ReadonlySet<string> = new Set(),
	): Promise<ModuleLaunchAttempt | null> {
		return await this.withStateLock(async () => {
			const merged: Record<string, string> = {...this.state.committed, ...installed};
			for (const moduleName of dropped) {
				if (installed[moduleName] === undefined) {
					delete merged[moduleName];
				}
			}
			const committed = await this.resolveInstalled(merged);
			if (sameModuleMap(this.state.committed, committed)) {
				return null;
			}
			const previous = this.state.committed;
			const activated = await this.writeState({
				...this.state,
				committed,
				previous,
				boot_attempt: 1,
			});
			return this.createLaunchAttempt(activated.committed);
		});
	}

	private async resolveInstalled(next: Readonly<Record<string, string>>): Promise<Record<string, string>> {
		const resolved: Record<string, string> = {};
		for (const moduleName of Object.keys(next).sort()) {
			const sha256 = next[moduleName];
			assertModuleName(moduleName);
			assertSha256(sha256);
			const directory = this.getModuleDirectory(moduleName, sha256);
			if ((await this.readValidInstallation(directory, moduleName)) == null) {
				throw new ModuleStoreInstallationMissingError(moduleName, sha256, directory);
			}
			resolved[moduleName] = sha256;
		}
		return resolved;
	}

	public requireManifestFresh(observation: ModuleManifestFeedObservation): void {
		requireManifestFresh(this.state.manifest_high_water, observation);
	}

	public async recordManifestFetch({
		etag,
		fetchedAt,
		manifest,
		floor,
		linuxSecurityMinimum,
		advertised,
	}: {
		readonly etag: string | null;
		readonly fetchedAt: string;
		readonly manifest: ModuleManifestFeedObservation;
		readonly floor?: Readonly<Record<string, string>>;
		readonly linuxSecurityMinimum?: LinuxModuleSecurityMinimum;
		readonly advertised?: Readonly<Record<string, string>>;
	}): Promise<ModuleStoreState> {
		return await this.withStateLock(async () => {
			const manifestHighWater = advanceManifestHighWater(this.state.manifest_high_water, manifest);
			return await this.writeState({
				...this.state,
				rejected: advertised == null ? this.state.rejected : retainRejected(this.state.rejected, advertised),
				last_manifest_etag: etag,
				last_manifest_fetch: fetchedAt,
				floor: floor == null ? this.state.floor : sortModuleMap(floor),
				linux_security_minimum:
					linuxSecurityMinimum == null
						? this.state.linux_security_minimum
						: advanceLinuxSecurityMinimum(this.state.linux_security_minimum, linuxSecurityMinimum),
				manifest_high_water: manifestHighWater,
			});
		});
	}

	public async beginBootAttempt(): Promise<ModuleBootAttempt> {
		return await this.withStateLock(async () => {
			if (this.state.boot_attempt < MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD) {
				return {rolledBack: false, bootAttempt: this.state.boot_attempt, committed: this.state.committed};
			}
			const revertedCommitted =
				Object.keys(this.state.previous).length === 0 ? this.state.committed : sortModuleMap(this.state.previous);
			const rolledBack = !sameModuleMap(this.state.committed, revertedCommitted);
			const rejected: Record<string, string> = {...this.state.rejected};
			if (rolledBack) {
				for (const [moduleName, sha256] of Object.entries(this.state.committed)) {
					if (revertedCommitted[moduleName] !== sha256) {
						rejected[moduleName] = sha256;
					}
				}
			}
			const reverted = await this.writeState({
				...this.state,
				committed: revertedCommitted,
				rejected,
				boot_attempt: 0,
			});
			return {rolledBack, bootAttempt: reverted.boot_attempt, committed: reverted.committed};
		});
	}

	public async recordLaunchAttempt(): Promise<ModuleLaunchAttempt> {
		return await this.withStateLock(async () => {
			const next = await this.writeState({...this.state, boot_attempt: this.state.boot_attempt + 1});
			return this.createLaunchAttempt(next.committed);
		});
	}

	public async markLaunchAttemptSucceeded(attempt: ModuleLaunchAttempt): Promise<boolean> {
		return await this.withStateLock(async () => {
			if (this.activeLaunchAttempt !== attempt) {
				return false;
			}
			if (this.state.boot_attempt !== 0) {
				await this.writeState({...this.state, boot_attempt: 0});
			}
			this.activeLaunchAttempt = null;
			this.launchSucceeded = true;
			void this.settleInstalledModules();
			return true;
		});
	}

	public isRejected(moduleName: string, sha256: string): boolean {
		return this.state.rejected[moduleName] === sha256;
	}

	public async buildModuleIndex(
		committed: Readonly<Record<string, string>> = this.state.committed,
	): Promise<Map<string, string>> {
		const index = new Map<string, string>();
		const owners = new Map<string, string>();
		for (const moduleName of Object.keys(committed).sort()) {
			const sha256 = committed[moduleName];
			const directory = this.getModuleDirectory(moduleName, sha256);
			const installed = await this.readValidInstallation(directory, moduleName);
			if (installed == null) {
				throw new ModuleStoreInstallationMissingError(moduleName, sha256, directory);
			}
			for (const file of installed.files) {
				const existingOwner = owners.get(file.relativePath);
				if (existingOwner != null) {
					throw new ModuleStoreError(
						`modules ${existingOwner} and ${moduleName} both declare served path ${file.relativePath}`,
					);
				}
				owners.set(file.relativePath, moduleName);
				index.set(file.relativePath, file.absolutePath);
			}
		}
		return index;
	}

	public async collectGarbage({
		now = Date.now(),
	}: {
		readonly now?: number;
	} = {}): Promise<ModuleGarbageCollectionResult> {
		if (!this.launchSucceeded) {
			throw new ModuleStoreGarbageCollectionOrderError();
		}
		return await this.withStoreTreeLock(() => this.collectGarbageExclusively(now));
	}

	private async collectGarbageExclusively(now: number): Promise<ModuleGarbageCollectionResult> {
		const keep = new Set<string>(this.installationsAwaitingCommit);
		for (const map of [this.state.committed, this.state.previous]) {
			for (const [moduleName, sha256] of Object.entries(map)) {
				keep.add(`${moduleName}/${sha256}`);
			}
		}
		const removedDirectories = await this.removeUnkeptInstallations(keep);
		const stalePackages = await this.removeStalePackages(now);
		const staleIncoming = await this.removeStaleIncomingDownloads(now);
		return {removedDirectories, removedPackages: [...stalePackages, ...staleIncoming]};
	}

	private async removeUnkeptInstallations(keep: ReadonlySet<string>): Promise<Array<string>> {
		const removedDirectories: Array<string> = [];
		for (const entry of await readDirectoryEntries(this.storeRoot)) {
			const child = path.join(this.storeRoot, entry.name);
			if (!entry.isDirectory() || !isDesktopModuleName(entry.name)) {
				await removeTree(child);
				removedDirectories.push(child);
				continue;
			}
			let retained = 0;
			for (const version of await readDirectoryEntries(child)) {
				const versionPath = path.join(child, version.name);
				if (version.isDirectory() && SHA256_PATTERN.test(version.name) && keep.has(`${entry.name}/${version.name}`)) {
					retained += 1;
					continue;
				}
				await removeTree(versionPath);
				removedDirectories.push(versionPath);
			}
			if (retained === 0) {
				await removeTree(child);
				removedDirectories.push(child);
			}
		}
		return removedDirectories;
	}

	private async removeStalePackages(now: number): Promise<Array<string>> {
		const removedPackages: Array<string> = [];
		for (const entry of await readDirectoryEntries(this.downloadRoot)) {
			if (entry.name === MODULE_DOWNLOAD_INCOMING_DIRECTORY_NAME) {
				continue;
			}
			const child = path.join(this.downloadRoot, entry.name);
			const packageSha256 = entry.name.slice(0, -MODULE_PACKAGE_FILE_EXTENSION.length);
			const named =
				entry.isFile() && entry.name.endsWith(MODULE_PACKAGE_FILE_EXTENSION) && SHA256_PATTERN.test(packageSha256);
			if (!named) {
				await removeTree(child);
				removedPackages.push(child);
				continue;
			}
			if (this.packagesInUse.has(packageSha256)) {
				continue;
			}
			const modified = await modifiedAt(child);
			if (modified != null && now - modified > MODULE_PACKAGE_MAX_AGE_MS) {
				await removeTree(child);
				removedPackages.push(child);
			}
		}
		return removedPackages;
	}

	private async removeStaleIncomingDownloads(now: number): Promise<Array<string>> {
		const removedPackages: Array<string> = [];
		for (const entry of await readDirectoryEntries(this.incomingDownloadRoot)) {
			if (
				entry.name.endsWith(MODULE_PARTIAL_PACKAGE_FILE_EXTENSION) &&
				this.packagesInUse.has(entry.name.slice(0, -MODULE_PARTIAL_PACKAGE_FILE_EXTENSION.length))
			) {
				continue;
			}
			const child = path.join(this.incomingDownloadRoot, entry.name);
			const modified = await modifiedAt(child);
			if (modified != null && now - modified > MODULE_PACKAGE_MAX_AGE_MS) {
				await removeTree(child);
				removedPackages.push(child);
			}
		}
		return removedPackages;
	}

	private withStateLock<T>(operation: () => Promise<T>): Promise<T> {
		const started = this.stateWrites.then(operation, operation);
		this.stateWrites = started.then(
			() => undefined,
			() => undefined,
		);
		return started;
	}

	private withStoreTreeLock<T>(operation: () => Promise<T>): Promise<T> {
		const started = this.storeTreeWrites.then(operation, operation);
		this.storeTreeWrites = started.then(
			() => undefined,
			() => undefined,
		);
		return started;
	}

	private createLaunchAttempt(committed: Readonly<Record<string, string>>): ModuleLaunchAttempt {
		const launchAttempt = Object.freeze({
			[MODULE_LAUNCH_ATTEMPT]: true as const,
			committed: Object.freeze({...committed}),
		});
		this.activeLaunchAttempt = launchAttempt;
		this.launchSucceeded = false;
		return launchAttempt;
	}

	private async writeState(next: ModuleStoreState): Promise<ModuleStoreState> {
		await writeFileAtomically(this.statePath, serializeModuleStoreState(next), STATE_FILE_MODE);
		this.state = {
			...next,
			committed: sortModuleMap(next.committed),
			previous: sortModuleMap(next.previous),
			rejected: sortModuleMap(next.rejected),
			floor: sortModuleMap(next.floor),
			linux_security_minimum:
				next.linux_security_minimum == null
					? null
					: {
							version: next.linux_security_minimum.version,
							requiredModules: Object.freeze([...next.linux_security_minimum.requiredModules]),
						},
			manifest_high_water: sortManifestHighWater(next.manifest_high_water),
		};
		for (const [moduleName, sha256] of Object.entries(this.state.committed)) {
			this.installationsAwaitingCommit.delete(`${moduleName}/${sha256}`);
		}
		return this.state;
	}
}
