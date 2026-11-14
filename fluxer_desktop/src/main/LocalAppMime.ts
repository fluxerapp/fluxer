// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';

const OCTET_STREAM_CONTENT_TYPE = 'application/octet-stream';

const CONTENT_TYPES_BY_EXTENSION: ReadonlyMap<string, string> = new Map([
	['.html', 'text/html; charset=utf-8'],
	['.htm', 'text/html; charset=utf-8'],
	['.js', 'application/javascript; charset=utf-8'],
	['.mjs', 'application/javascript; charset=utf-8'],
	['.css', 'text/css; charset=utf-8'],
	['.json', 'application/json; charset=utf-8'],
	['.png', 'image/png'],
	['.jpg', 'image/jpeg'],
	['.jpeg', 'image/jpeg'],
	['.gif', 'image/gif'],
	['.webp', 'image/webp'],
	['.avif', 'image/avif'],
	['.svg', 'image/svg+xml'],
	['.ico', 'image/x-icon'],
	['.woff', 'font/woff'],
	['.woff2', 'font/woff2'],
	['.ttf', 'font/ttf'],
	['.otf', 'font/otf'],
	['.mp3', 'audio/mpeg'],
	['.mp4', 'video/mp4'],
	['.webm', 'video/webm'],
	['.ogg', 'audio/ogg'],
	['.txt', 'text/plain; charset=utf-8'],
	['.xml', 'application/xml; charset=utf-8'],
	['.webmanifest', 'application/manifest+json'],
	['.map', 'application/json'],
	['.onnx', OCTET_STREAM_CONTENT_TYPE],
	['.wasm', 'application/wasm'],
]);

const NO_STORE_CACHE_CONTROL = 'no-store';
const IMMUTABLE_CACHE_CONTROL = 'public, max-age=31536000, immutable';
const REVALIDATED_CACHE_CONTROL = 'public, max-age=3600, must-revalidate';

const REWRITTEN_FILE_NAMES: ReadonlySet<string> = new Set(['index.html', 'manifest.json', 'browserconfig.xml']);
const HASHED_ASSET_DIRECTORY = 'assets';
const HASHED_ASSET_STEM_SEPARATORS: ReadonlyArray<string> = Object.freeze(['.', '-']);
const CONTENT_HASH_MIN_LENGTH = 8;
const CONTENT_HASH_PATTERN = /^[0-9a-f]+$/iu;
const PATH_SEPARATORS = /[/\\]+/u;

function localAppExtension(filePath: string): string {
	return path.extname(filePath).toLowerCase();
}

export function localAppContentType(filePath: string): string {
	return CONTENT_TYPES_BY_EXTENSION.get(localAppExtension(filePath)) ?? OCTET_STREAM_CONTENT_TYPE;
}

export function isServedLocalAppExtension(filePath: string): boolean {
	return CONTENT_TYPES_BY_EXTENSION.has(localAppExtension(filePath));
}

function isContentHash(value: string): boolean {
	return value.length >= CONTENT_HASH_MIN_LENGTH && CONTENT_HASH_PATTERN.test(value);
}

function isHashedAssetFileName(fileName: string): boolean {
	const lastDot = fileName.lastIndexOf('.');
	if (lastDot === -1) {
		return false;
	}
	const stem = fileName.slice(0, lastDot);
	if (isContentHash(stem)) {
		return true;
	}
	const separatorPattern = new RegExp(`[${HASHED_ASSET_STEM_SEPARATORS.join('')}]`);
	return stem.split(separatorPattern).some(isContentHash);
}

function isHashedAssetPath(filePath: string): boolean {
	const segments = filePath.split(PATH_SEPARATORS);
	const fileName = segments[segments.length - 1];
	if (fileName == null) {
		return false;
	}
	if (!segments.slice(0, -1).includes(HASHED_ASSET_DIRECTORY)) {
		return false;
	}
	return isHashedAssetFileName(fileName);
}

export function localAppCacheControl(filePath: string): string {
	if (REWRITTEN_FILE_NAMES.has(path.basename(filePath))) {
		return NO_STORE_CACHE_CONTROL;
	}
	if (isHashedAssetPath(filePath)) {
		return IMMUTABLE_CACHE_CONTROL;
	}
	return REVALIDATED_CACHE_CONTROL;
}
