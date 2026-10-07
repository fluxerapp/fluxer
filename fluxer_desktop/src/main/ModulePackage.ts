// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {createReadStream} from 'node:fs';
import type {FileHandle} from 'node:fs/promises';
import fs from 'node:fs/promises';
import path from 'node:path';
import zlib from 'node:zlib';
import {ModuleVersionMalformedError, parseModuleVersion} from '@electron/main/ModuleVersion';
import {isDesktopModuleName} from '@fluxer/desktop_ipc/src/ModuleContract';

const TAR_BLOCK_BYTES = 512;
const TAR_NAME_OFFSET = 0;
const TAR_NAME_BYTES = 100;
const TAR_SIZE_OFFSET = 124;
const TAR_SIZE_BYTES = 12;
const TAR_CHECKSUM_OFFSET = 148;
const TAR_CHECKSUM_BYTES = 8;
const TAR_TYPEFLAG_OFFSET = 156;
const TAR_MAGIC_OFFSET = 257;
const TAR_MAGIC_BYTES = 5;
const TAR_MAGIC = 'ustar';
const TAR_PREFIX_OFFSET = 345;
const TAR_PREFIX_BYTES = 155;
const TAR_CHECKSUM_FILL = 0x20;
const TAR_REGULAR_TYPEFLAG = '0';
const TAR_LEGACY_REGULAR_TYPEFLAG = '\0';
const TAR_HARDLINK_TYPEFLAG = '1';
const TAR_SYMLINK_TYPEFLAG = '2';

const COPY_CHUNK_BYTES = 1024 * 1024;
export const DESKTOP_MODULE_MANIFEST_MAX_BYTES = 16 * 1024 * 1024;
const MAX_MODULE_FILES = 100_000;
const MAX_MODULE_BYTES = 2 * 1024 * 1024 * 1024;
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;
const MODULE_BUILD_VERSION_PATTERN = /^\d+\.\d+\.\d+$/u;

export const DESKTOP_MODULE_FILE_LIST_NAME = 'module.json';
const DESKTOP_MODULE_FILES_PREFIX = 'files/';

export interface DesktopSharedAssetFile {
	readonly path: string;
	readonly sha256: string;
	readonly bytes: number;
}

export interface DesktopModuleManifest {
	readonly module: string;
	readonly build_version: string;
	readonly release_channel: string;
	readonly source_sha: string;
	readonly files: ReadonlyArray<DesktopSharedAssetFile>;
}

class ModulePackageError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModulePackageError';
	}
}

export class ModulePackageHashMismatchError extends ModulePackageError {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModulePackageHashMismatchError';
	}
}

export class ModulePackageMalformedError extends ModulePackageError {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModulePackageMalformedError';
	}
}

export class ModulePackageIOError extends ModulePackageError {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModulePackageIOError';
	}
}

interface ModulePackageArchiveHeader {
	readonly name: string;
	readonly size: number;
}

class ArchiveByteReader {
	private readonly source: AsyncIterator<Buffer | Uint8Array>;
	private readonly pending: Array<Buffer> = [];
	private buffered = 0;
	private exhausted = false;

	public constructor(source: AsyncIterable<Buffer | Uint8Array>) {
		this.source = source[Symbol.asyncIterator]();
	}

	public async read(length: number): Promise<Buffer | null> {
		if (length === 0) {
			return Buffer.alloc(0);
		}
		while (this.buffered < length) {
			if (this.exhausted) {
				return null;
			}
			const next = await this.pull();
			if (next === null) {
				this.exhausted = true;
				return null;
			}
			if (next.length > 0) {
				this.pending.push(next);
				this.buffered += next.length;
			}
		}
		return this.take(length);
	}

	private async pull(): Promise<Buffer | null> {
		let result: IteratorResult<Buffer | Uint8Array>;
		try {
			result = await this.source.next();
		} catch (error) {
			if (error instanceof ModulePackageError) {
				throw error;
			}
			throw new ModulePackageMalformedError('failed to decompress module package', {cause: error});
		}
		if (result.done === true) {
			return null;
		}
		return Buffer.isBuffer(result.value) ? result.value : Buffer.from(result.value);
	}

	private take(length: number): Buffer {
		const out = Buffer.allocUnsafe(length);
		let copied = 0;
		while (copied < length) {
			const chunk = this.pending[0];
			if (chunk === undefined) {
				throw new ModulePackageMalformedError('module package reader ran out of buffered bytes');
			}
			const wanted = length - copied;
			if (chunk.length <= wanted) {
				chunk.copy(out, copied);
				copied += chunk.length;
				this.pending.shift();
			} else {
				chunk.copy(out, copied, 0, wanted);
				this.pending[0] = chunk.subarray(wanted);
				copied += wanted;
			}
		}
		this.buffered -= length;
		return out;
	}
}

function readCString(header: Buffer, offset: number, length: number): string {
	const field = header.subarray(offset, offset + length);
	const end = field.indexOf(0);
	return field.subarray(0, end === -1 ? field.length : end).toString('utf8');
}

function parseOctalField(header: Buffer, offset: number, length: number, label: string): number {
	const raw = header.subarray(offset, offset + length).toString('latin1');
	const trimmed = raw.replaceAll('\0', '').trim();
	if (trimmed.length === 0) {
		return 0;
	}
	if (!/^[0-7]+$/u.test(trimmed)) {
		throw new ModulePackageMalformedError(`unsupported ${label} field in archive header`);
	}
	const value = Number.parseInt(trimmed, 8);
	if (!Number.isSafeInteger(value) || value < 0) {
		throw new ModulePackageMalformedError(`unsupported ${label} field in archive header`);
	}
	return value;
}

function isZeroBlock(block: Buffer): boolean {
	return block.every((byte) => byte === 0);
}

function parseUstarHeader(header: Buffer, offset: number): ModulePackageArchiveHeader {
	const stored = parseOctalField(header, TAR_CHECKSUM_OFFSET, TAR_CHECKSUM_BYTES, 'checksum');
	const canonical = Buffer.from(header);
	canonical.fill(TAR_CHECKSUM_FILL, TAR_CHECKSUM_OFFSET, TAR_CHECKSUM_OFFSET + TAR_CHECKSUM_BYTES);
	let sum = 0;
	for (const byte of canonical) {
		sum += byte;
	}
	if (stored !== sum) {
		throw new ModulePackageMalformedError(`checksum mismatch at offset ${offset}`);
	}
	if (header.subarray(TAR_MAGIC_OFFSET, TAR_MAGIC_OFFSET + TAR_MAGIC_BYTES).toString('latin1') !== TAR_MAGIC) {
		throw new ModulePackageMalformedError(`not a ustar header at offset ${offset}`);
	}
	const typeflag = String.fromCharCode(header[TAR_TYPEFLAG_OFFSET] ?? 0);
	if (typeflag === TAR_HARDLINK_TYPEFLAG || typeflag === TAR_SYMLINK_TYPEFLAG) {
		throw new ModulePackageMalformedError(`links are not allowed in archive at offset ${offset}`);
	}
	if (typeflag !== TAR_REGULAR_TYPEFLAG && typeflag !== TAR_LEGACY_REGULAR_TYPEFLAG) {
		throw new ModulePackageMalformedError(`unsupported archive member type ${typeflag} at offset ${offset}`);
	}
	const name = readCString(header, TAR_NAME_OFFSET, TAR_NAME_BYTES);
	const prefix = readCString(header, TAR_PREFIX_OFFSET, TAR_PREFIX_BYTES);
	const size = parseOctalField(header, TAR_SIZE_OFFSET, TAR_SIZE_BYTES, 'size');
	return {name: prefix.length > 0 ? `${prefix}/${name}` : name, size};
}

function paddingFor(size: number): number {
	const remainder = size % TAR_BLOCK_BYTES;
	return remainder === 0 ? 0 : TAR_BLOCK_BYTES - remainder;
}

function assertSafeModuleRelativePath(relative: string): void {
	if (relative.length === 0) {
		throw new ModulePackageMalformedError('unsafe path in archive: <empty>');
	}
	if (relative.includes('\0') || relative.includes('\\')) {
		throw new ModulePackageMalformedError(`unsafe path in archive: ${relative}`);
	}
	if (path.posix.isAbsolute(relative) || path.win32.isAbsolute(relative)) {
		throw new ModulePackageMalformedError(`unsafe path in archive: ${relative}`);
	}
	for (const segment of relative.split('/')) {
		if (segment.length === 0 || segment === '.' || segment === '..') {
			throw new ModulePackageMalformedError(`unsafe path in archive: ${relative}`);
		}
	}
	if (path.posix.normalize(relative) !== relative) {
		throw new ModulePackageMalformedError(`unsafe path in archive: ${relative}`);
	}
}

function isPathInsideRoot(root: string, candidate: string): boolean {
	const relative = path.relative(root, candidate);
	return relative !== '' && !relative.startsWith('..') && !path.isAbsolute(relative);
}

function moduleFilePathCollisionKey(relative: string): string {
	return relative.normalize('NFC').toLowerCase().normalize('NFC');
}

function requireString(record: Record<string, unknown>, key: string): string {
	const value = record[key];
	if (typeof value !== 'string' || value.length === 0) {
		throw new ModulePackageMalformedError(`module.json ${key} must be a non-empty string`);
	}
	return value;
}

function requireBuildVersion(record: Record<string, unknown>): string {
	const source = requireString(record, 'build_version');
	try {
		parseModuleVersion(source, 'module.json build_version');
	} catch (error) {
		if (error instanceof ModuleVersionMalformedError) {
			throw new ModulePackageMalformedError(error.message, {cause: error});
		}
		throw error;
	}
	if (!MODULE_BUILD_VERSION_PATTERN.test(source)) {
		throw new ModulePackageMalformedError(
			`module.json build_version must contain exactly three numeric components: ${source}`,
		);
	}
	return source;
}

export function parseDesktopModuleManifest(bytes: Buffer): DesktopModuleManifest {
	let parsed: unknown;
	try {
		parsed = JSON.parse(bytes.toString('utf8'));
	} catch (error) {
		throw new ModulePackageMalformedError('module.json is not valid JSON', {cause: error});
	}
	if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
		throw new ModulePackageMalformedError('module.json must be an object');
	}
	const record = parsed as Record<string, unknown>;
	const moduleName = requireString(record, 'module');
	if (!isDesktopModuleName(moduleName)) {
		throw new ModulePackageMalformedError(`module.json declares an unsupported module name: ${moduleName}`);
	}
	const buildVersion = requireBuildVersion(record);
	const releaseChannel = requireString(record, 'release_channel');
	const sourceSha = requireString(record, 'source_sha');
	const rawFiles = record['files'];
	if (!Array.isArray(rawFiles)) {
		throw new ModulePackageMalformedError('module.json files must be an array');
	}
	if (rawFiles.length > MAX_MODULE_FILES) {
		throw new ModulePackageMalformedError(`module.json declares more than ${MAX_MODULE_FILES} files`);
	}
	const files: Array<DesktopSharedAssetFile> = [];
	const declared = new Set<string>();
	const collisions = new Map<string, string>();
	let total = 0;
	for (const rawFile of rawFiles) {
		if (typeof rawFile !== 'object' || rawFile === null || Array.isArray(rawFile)) {
			throw new ModulePackageMalformedError('module.json files must contain objects');
		}
		const entry = rawFile as Record<string, unknown>;
		const relative = requireString(entry, 'path');
		assertSafeModuleRelativePath(relative);
		const collisionKey = moduleFilePathCollisionKey(relative);
		if (collisionKey === DESKTOP_MODULE_FILE_LIST_NAME) {
			throw new ModulePackageMalformedError(`module.json declares the reserved path ${relative}`);
		}
		const sha256 = requireString(entry, 'sha256');
		if (!SHA256_PATTERN.test(sha256)) {
			throw new ModulePackageMalformedError(`module.json declares an invalid sha256 for ${relative}`);
		}
		const bytes = entry['bytes'];
		if (typeof bytes !== 'number' || !Number.isSafeInteger(bytes) || bytes < 0) {
			throw new ModulePackageMalformedError(`module.json declares invalid bytes for ${relative}`);
		}
		if (declared.has(relative)) {
			throw new ModulePackageMalformedError(`module.json declares ${relative} twice`);
		}
		const colliding = collisions.get(collisionKey);
		if (colliding !== undefined) {
			throw new ModulePackageMalformedError(`module.json declares colliding paths ${colliding} and ${relative}`);
		}
		declared.add(relative);
		collisions.set(collisionKey, relative);
		total += bytes;
		if (total > MAX_MODULE_BYTES) {
			throw new ModulePackageMalformedError(`module.json declares more than ${MAX_MODULE_BYTES} bytes`);
		}
		files.push({path: relative, sha256, bytes});
	}
	for (const [collisionKey, relative] of collisions) {
		let cursor = collisionKey;
		for (;;) {
			const separator = cursor.lastIndexOf('/');
			if (separator === -1) {
				break;
			}
			cursor = cursor.slice(0, separator);
			const ancestor = collisions.get(cursor);
			if (ancestor !== undefined) {
				throw new ModulePackageMalformedError(`module.json declares colliding paths ${ancestor} and ${relative}`);
			}
		}
	}
	return {
		module: moduleName,
		build_version: buildVersion,
		release_channel: releaseChannel,
		source_sha: sourceSha,
		files,
	};
}

async function hashFile(filePath: string): Promise<string> {
	const digest = createHash('sha256');
	try {
		for await (const chunk of createReadStream(filePath)) {
			digest.update(chunk as Buffer);
		}
	} catch (error) {
		throw new ModulePackageIOError(`failed to read module package ${filePath}`, {cause: error});
	}
	return digest.digest('hex');
}

async function readMember(reader: ArchiveByteReader, size: number, name: string): Promise<Buffer> {
	const body = await reader.read(size);
	if (body === null) {
		throw new ModulePackageMalformedError(`archive ended inside ${name}`);
	}
	const padding = paddingFor(size);
	if (padding > 0 && (await reader.read(padding)) === null) {
		throw new ModulePackageMalformedError(`archive ended inside ${name}`);
	}
	return body;
}

async function writeMember(
	reader: ArchiveByteReader,
	target: string,
	file: DesktopSharedAssetFile,
	name: string,
): Promise<void> {
	const digest = createHash('sha256');
	let handle: FileHandle;
	try {
		handle = await fs.open(target, 'wx', 0o644);
	} catch (error) {
		throw new ModulePackageIOError(`failed to create ${target}`, {cause: error});
	}
	try {
		let remaining = file.bytes;
		while (remaining > 0) {
			const chunk = await reader.read(Math.min(remaining, COPY_CHUNK_BYTES));
			if (chunk === null) {
				throw new ModulePackageMalformedError(`archive ended inside ${name}`);
			}
			digest.update(chunk);
			try {
				await handle.writeFile(chunk);
			} catch (error) {
				throw new ModulePackageIOError(`failed to write ${target}`, {cause: error});
			}
			remaining -= chunk.length;
		}
	} finally {
		await handle.close();
	}
	const padding = paddingFor(file.bytes);
	if (padding > 0 && (await reader.read(padding)) === null) {
		throw new ModulePackageMalformedError(`archive ended inside ${name}`);
	}
	if (digest.digest('hex') !== file.sha256) {
		throw new ModulePackageHashMismatchError(`hash mismatch: ${file.path}`);
	}
}

async function extractArchive(
	source: AsyncIterable<Buffer | Uint8Array>,
	destinationRoot: string,
): Promise<DesktopModuleManifest> {
	const reader = new ArchiveByteReader(source);
	const declared = new Map<string, DesktopSharedAssetFile>();
	const seen = new Set<string>();
	const createdDirectories = new Set<string>();
	let manifest: DesktopModuleManifest | null = null;
	let offset = 0;

	while (true) {
		const block = await reader.read(TAR_BLOCK_BYTES);
		if (block === null) {
			throw new ModulePackageMalformedError('archive ended without a terminator block');
		}
		if (isZeroBlock(block)) {
			break;
		}
		const header = parseUstarHeader(block, offset);
		offset += TAR_BLOCK_BYTES + header.size + paddingFor(header.size);

		if (manifest === null) {
			if (header.name !== DESKTOP_MODULE_FILE_LIST_NAME) {
				throw new ModulePackageMalformedError('module.json missing from archive root');
			}
			if (header.size > DESKTOP_MODULE_MANIFEST_MAX_BYTES) {
				throw new ModulePackageMalformedError(`module.json is larger than ${DESKTOP_MODULE_MANIFEST_MAX_BYTES} bytes`);
			}
			const body = await readMember(reader, header.size, DESKTOP_MODULE_FILE_LIST_NAME);
			manifest = parseDesktopModuleManifest(body);
			for (const file of manifest.files) {
				declared.set(`${DESKTOP_MODULE_FILES_PREFIX}${file.path}`, file);
			}
			const manifestPath = path.join(destinationRoot, DESKTOP_MODULE_FILE_LIST_NAME);
			try {
				await fs.writeFile(manifestPath, body, {flag: 'wx', mode: 0o644});
			} catch (error) {
				throw new ModulePackageIOError(`failed to write ${manifestPath}`, {cause: error});
			}
			continue;
		}

		if (header.name === DESKTOP_MODULE_FILE_LIST_NAME) {
			throw new ModulePackageMalformedError('duplicate module.json in archive');
		}
		if (!header.name.startsWith(DESKTOP_MODULE_FILES_PREFIX)) {
			throw new ModulePackageMalformedError(`path must start with files/: ${header.name}`);
		}
		const file = declared.get(header.name);
		if (file === undefined) {
			throw new ModulePackageMalformedError(`unexpected file in archive: not in manifest: ${header.name}`);
		}
		if (seen.has(header.name)) {
			throw new ModulePackageMalformedError(`duplicate file in archive: ${header.name}`);
		}
		seen.add(header.name);
		if (header.size !== file.bytes) {
			throw new ModulePackageHashMismatchError(`size mismatch: ${file.path}`);
		}
		assertSafeModuleRelativePath(file.path);
		const target = path.resolve(destinationRoot, file.path);
		if (!isPathInsideRoot(destinationRoot, target)) {
			throw new ModulePackageMalformedError(`unsafe path in archive: ${header.name}`);
		}
		const parent = path.dirname(target);
		if (!createdDirectories.has(parent)) {
			try {
				await fs.mkdir(parent, {recursive: true});
			} catch (error) {
				throw new ModulePackageIOError(`failed to create ${parent}`, {cause: error});
			}
			createdDirectories.add(parent);
		}
		await writeMember(reader, target, file, header.name);
	}

	if (manifest === null) {
		throw new ModulePackageMalformedError('module.json missing from archive root');
	}
	for (const file of manifest.files) {
		if (!seen.has(`${DESKTOP_MODULE_FILES_PREFIX}${file.path}`)) {
			throw new ModulePackageMalformedError(`missing file: ${file.path}`);
		}
	}
	return manifest;
}

export async function verifyAndExtract({
	packedPath,
	expectedSha256,
	destinationDir,
}: {
	readonly packedPath: string;
	readonly expectedSha256: string;
	readonly destinationDir: string;
}): Promise<DesktopModuleManifest> {
	if (!SHA256_PATTERN.test(expectedSha256)) {
		throw new ModulePackageHashMismatchError(`expected package sha256 is not a sha256 digest: ${expectedSha256}`);
	}
	const actualSha256 = await hashFile(packedPath);
	if (actualSha256 !== expectedSha256) {
		throw new ModulePackageHashMismatchError(
			`package sha256 mismatch: expected ${expectedSha256}, got ${actualSha256}`,
		);
	}
	const destinationRoot = path.resolve(destinationDir);
	try {
		await fs.mkdir(destinationRoot, {recursive: true});
	} catch (error) {
		throw new ModulePackageIOError(`failed to create ${destinationRoot}`, {cause: error});
	}
	const packed = createReadStream(packedPath);
	const decompressor = zlib.createBrotliDecompress();
	packed.on('error', (error) => {
		decompressor.destroy(new ModulePackageIOError(`failed to read module package ${packedPath}`, {cause: error}));
	});
	packed.pipe(decompressor);
	try {
		return await extractArchive(decompressor, destinationRoot);
	} finally {
		packed.destroy();
		decompressor.destroy();
	}
}
