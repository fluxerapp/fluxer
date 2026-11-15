// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';

export const LAYOUT_FILE = '.layout';
const LAYOUT_VERSION = 1;
const LAYOUT_CONTENTS = JSON.stringify({v: LAYOUT_VERSION});
export const META_DIR = '.meta';
export const STAGING_DIR = '.staging';
export const STAGING_TMP = 'tmp';
export const STAGING_MULTIPART = 'multipart';
export const STAGING_TRASH = 'trash';
const STAGING_SUBDIRS = [STAGING_TMP, STAGING_MULTIPART, STAGING_TRASH] as const;
export const MULTIPART_MANIFEST = 'upload.json';
export const UPLOAD_ID_PATTERN = /^[0-9a-f]{32}$/;
export const PART_NAME_PATTERN = /^part-(\d{5})$/;
export const MAX_PART_NUMBER = 10000;
export const GEOIP_BUCKET = 'fluxer-geoip';
export const DIRECTORY_MODE = 0o2770;
export const FILE_MODE = 0o660;
export const DEFAULT_CONTENT_TYPE = 'application/octet-stream';

const MAX_SEGMENT_BYTES = 255;
const MAX_EXTENSION_BYTES = 16;
const HASH_HEX_CHARS = 16;
const MAPPED_SEGMENT_PATTERN = /~[0-9a-f]{16}(\.[^.]*)?$/;

export const EXTENSION_CONTENT_TYPES: Readonly<Record<string, string>> = {
	'3gp': 'video/3gpp',
	aac: 'audio/aac',
	aif: 'audio/aiff',
	aiff: 'audio/aiff',
	apng: 'image/apng',
	avi: 'video/x-msvideo',
	avif: 'image/avif',
	bmp: 'image/bmp',
	css: 'text/css; charset=utf-8',
	flac: 'audio/flac',
	flv: 'video/x-flv',
	gif: 'image/gif',
	heic: 'image/heic',
	heif: 'image/heif',
	jpeg: 'image/jpeg',
	jpg: 'image/jpeg',
	jxl: 'image/jxl',
	m4a: 'audio/mp4',
	m4b: 'audio/mp4',
	m4v: 'video/mp4',
	mkv: 'video/x-matroska',
	mov: 'video/quicktime',
	mp3: 'audio/mpeg',
	mp4: 'video/mp4',
	mpeg: 'video/mpeg',
	mpg: 'video/mpeg',
	oga: 'audio/ogg',
	ogg: 'audio/ogg',
	ogv: 'video/ogg',
	opus: 'audio/ogg',
	png: 'image/png',
	svg: 'image/svg+xml',
	tif: 'image/tiff',
	tiff: 'image/tiff',
	ts: 'video/mp2t',
	wav: 'audio/wav',
	weba: 'audio/webm',
	webm: 'video/webm',
	webp: 'image/webp',
	wmv: 'video/x-ms-wmv',
};

export interface Sidecar {
	contentType?: string;
	key?: string;
}

export interface UploadManifest {
	v: number;
	bucket: string;
	key: string;
	contentType?: string;
	created: number;
}

export interface ByteRange {
	start: number;
	end: number;
}

export class FilesystemStorageLayoutError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'FilesystemStorageLayoutError';
	}
}

function isValidSegment(segment: string): boolean {
	return segment !== '' && segment !== '.' && segment !== '..' && !segment.includes('\0');
}

export function isValidStorageKey(key: string): boolean {
	if (key === '' || key.startsWith('/')) {
		return false;
	}
	return key.split('/').every(isValidSegment);
}

export function isValidStorageBucket(bucket: string): boolean {
	return isValidSegment(bucket) && !bucket.startsWith('.') && !bucket.includes('/');
}

function utf8Prefix(bytes: Buffer, limit: number): Buffer {
	let cut = Math.min(limit, bytes.length);
	while (cut > 0 && cut < bytes.length && (bytes[cut]! & 0xc0) === 0x80) {
		cut -= 1;
	}
	return bytes.subarray(0, cut);
}

export function mapSegment(segment: string): string {
	const bytes = Buffer.from(segment, 'utf8');
	if (bytes.length <= MAX_SEGMENT_BYTES) {
		return segment;
	}
	const dot = segment.lastIndexOf('.');
	const candidate = dot > 0 ? segment.slice(dot) : '';
	const ext = Buffer.byteLength(candidate, 'utf8') <= MAX_EXTENSION_BYTES ? candidate : '';
	const extBytes = Buffer.byteLength(ext, 'utf8');
	const head = utf8Prefix(bytes, MAX_SEGMENT_BYTES - 1 - MAX_EXTENSION_BYTES - extBytes).toString('utf8');
	const hash = createHash('sha256').update(bytes).digest('hex').slice(0, HASH_HEX_CHARS);
	return `${head}~${hash}${ext}`;
}

export function mapKey(key: string): {path: string; mapped: boolean} {
	let mapped = false;
	const segments = key.split('/').map((segment) => {
		const out = mapSegment(segment);
		if (out !== segment) mapped = true;
		return out;
	});
	return {path: segments.join('/'), mapped};
}

export function mayBeMappedSegment(segment: string): boolean {
	return MAPPED_SEGMENT_PATTERN.test(segment);
}

export function inferContentType(key: string): string | null {
	const dot = key.lastIndexOf('.');
	if (dot < 0) return null;
	const ext = key.slice(dot + 1).replace(/[A-Z]/g, (letter) => letter.toLowerCase());
	return Object.hasOwn(EXTENSION_CONTENT_TYPES, ext) ? EXTENSION_CONTENT_TYPES[ext]! : null;
}

export function needsSidecar(key: string, contentType: string | null | undefined, mapped: boolean): boolean {
	if (mapped) return true;
	if (!contentType) return false;
	return inferContentType(key) !== contentType;
}

export function encodeSidecar(key: string, contentType: string | null | undefined, mapped: boolean): string {
	return JSON.stringify({
		v: LAYOUT_VERSION,
		...(contentType ? {contentType} : {}),
		...(mapped ? {key} : {}),
	});
}

export function decodeSidecar(raw: string): Sidecar | null {
	try {
		const parsed = JSON.parse(raw) as {v?: unknown; contentType?: unknown; key?: unknown};
		if (parsed.v !== LAYOUT_VERSION) return null;
		return {
			...(typeof parsed.contentType === 'string' ? {contentType: parsed.contentType} : {}),
			...(typeof parsed.key === 'string' ? {key: parsed.key} : {}),
		};
	} catch {
		return null;
	}
}

export function decodeUploadManifest(raw: string): UploadManifest | null {
	try {
		const parsed = JSON.parse(raw) as Partial<UploadManifest>;
		if (parsed.v !== LAYOUT_VERSION || typeof parsed.bucket !== 'string' || typeof parsed.key !== 'string') {
			return null;
		}
		return {
			v: parsed.v,
			bucket: parsed.bucket,
			key: parsed.key,
			...(typeof parsed.contentType === 'string' ? {contentType: parsed.contentType} : {}),
			created: typeof parsed.created === 'number' ? parsed.created : 0,
		};
	} catch {
		return null;
	}
}

export function partFileName(partNumber: number): string {
	return `part-${String(partNumber).padStart(5, '0')}`;
}

function objectPath(root: string, bucket: string, key: string): string {
	return path.join(root, bucket, mapKey(key).path);
}

export function filesystemObjectPath(root: string, bucket: string, key: string): string | null {
	if (!isValidStorageBucket(bucket) || !isValidStorageKey(key)) return null;
	return objectPath(root, bucket, key);
}

export function formatEtag(size: bigint, mtimeNs: bigint): string {
	return `"${size.toString(16)}-${mtimeNs.toString(16)}"`;
}

export function parseRange(header: string | undefined, size: number): ByteRange | 'unsatisfiable' | null {
	if (header === undefined) return null;
	const match = /^bytes=(\d*)-(\d*)$/.exec(header.trim());
	if (!match) return null;
	const [, rawStart = '', rawEnd = ''] = match;
	if (rawStart === '' && rawEnd === '') return null;
	if (rawStart === '') {
		const suffix = Number(rawEnd);
		if (suffix === 0 || size === 0) return 'unsatisfiable';
		return {start: Math.max(0, size - suffix), end: size - 1};
	}
	const start = Number(rawStart);
	const end = rawEnd === '' ? size - 1 : Number(rawEnd);
	if (rawEnd !== '' && end < start) return null;
	if (start >= size) return 'unsatisfiable';
	return {start, end: Math.min(end, size - 1)};
}

export function checkLayoutSync(root: string): void {
	if (!path.isAbsolute(root)) {
		throw new FilesystemStorageLayoutError(`FLUXER_STORAGE_ROOT must be an absolute path, got ${root}`);
	}
	let raw: string;
	try {
		raw = fs.readFileSync(path.join(root, LAYOUT_FILE), 'utf8');
	} catch (error) {
		throw new FilesystemStorageLayoutError(
			`${path.join(root, LAYOUT_FILE)} is missing. The storage folder was not set up for this release (${String(error)})`,
		);
	}
	let version: unknown;
	try {
		version = (JSON.parse(raw) as {v?: unknown}).v;
	} catch {
		version = undefined;
	}
	if (version !== LAYOUT_VERSION) {
		throw new FilesystemStorageLayoutError(
			`${path.join(root, LAYOUT_FILE)} holds layout ${JSON.stringify(raw.trim())}, this release reads ${LAYOUT_CONTENTS}`,
		);
	}
}

export function prepareStagingSync(root: string): void {
	const staging = path.join(root, STAGING_DIR);
	for (const dir of [staging, ...STAGING_SUBDIRS.map((sub) => path.join(staging, sub))]) {
		try {
			fs.mkdirSync(dir, {mode: DIRECTORY_MODE});
			fs.chmodSync(dir, DIRECTORY_MODE);
		} catch (error) {
			if ((error as NodeJS.ErrnoException).code !== 'EEXIST') throw error;
		}
	}
	const rootDev = fs.statSync(root).dev;
	for (const sub of STAGING_SUBDIRS) {
		if (fs.statSync(path.join(staging, sub)).dev !== rootDev) {
			throw new FilesystemStorageLayoutError(
				`${path.join(staging, sub)} is on a different filesystem from ${root}, so files cannot be renamed into place`,
			);
		}
	}
}

export function writeLayoutSync(root: string): void {
	fs.mkdirSync(root, {recursive: true, mode: DIRECTORY_MODE});
	const target = path.join(root, LAYOUT_FILE);
	const temp = `${target}.tmp-${process.pid}`;
	const fd = fs.openSync(temp, 'w', FILE_MODE);
	try {
		fs.fchmodSync(fd, FILE_MODE);
		fs.writeSync(fd, LAYOUT_CONTENTS);
		fs.fsyncSync(fd);
	} finally {
		fs.closeSync(fd);
	}
	fs.renameSync(temp, target);
	const dir = fs.openSync(root, 'r');
	try {
		fs.fsyncSync(dir);
	} finally {
		fs.closeSync(dir);
	}
}

export interface StorageBuckets {
	cdn: string;
	uploads: string;
	reports: string;
	harvests: string;
}

export function assertStorageBuckets(buckets: StorageBuckets): void {
	for (const [name, bucket] of Object.entries(buckets)) {
		if (!isValidStorageBucket(bucket)) {
			throw new FilesystemStorageLayoutError(
				`The ${name} bucket name ${JSON.stringify(bucket)} cannot be a folder name`,
			);
		}
	}
	const others: Array<[string, string]> = [
		['cdn', buckets.cdn],
		['reports', buckets.reports],
		['harvests', buckets.harvests],
		['geoip', GEOIP_BUCKET],
	];
	for (const [name, bucket] of others) {
		if (bucket === buckets.uploads) {
			throw new FilesystemStorageLayoutError(
				`The uploads bucket and the ${name} bucket are both ${JSON.stringify(bucket)}. Expired uploads are deleted, so they need separate buckets`,
			);
		}
	}
}
