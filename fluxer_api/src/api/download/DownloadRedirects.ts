// SPDX-License-Identifier: AGPL-3.0-or-later

import {posix} from 'node:path';

export const PKGS_BASE_URL = 'https://pkgs.fluxer.com';
export const DOWNLOAD_PREFIX = '/dl';
export const DESKTOP_REDIRECT_PREFIX = `${DOWNLOAD_PREFIX}/desktop`;

export const DESKTOP_COORDINATE_DOCUMENTS = new Map<string, string>([['latest', 'latest.json']]);

const DESKTOP_PATH_PREFIX = 'desktop/';
const PLATFORM_ARCH_PATH = /^(desktop\/(?:stable|canary)\/(?:win32|darwin|linux))-(x64|arm64)(\/.+)$/u;
const COORDINATE_DOCUMENT_PATH = /^(desktop\/(?:stable|canary)\/(?:win32|darwin|linux)\/(?:x64|arm64))\/([a-z]+)$/u;
const MUTABLE_REDIRECT_CACHE_CONTROL = 'no-store';
const VERSIONED_REDIRECT_CACHE_CONTROL = 'public, max-age=31536000';
const SCOPE_SEGMENT_INDEX = 4;

interface DownloadRedirect {
	location: string;
	cacheControl: string;
}

function isReleaseFeedFilename(filename: string): boolean {
	return (
		filename === 'manifest.json' ||
		filename === 'latest.json' ||
		filename === 'version.json' ||
		filename.endsWith('.yml') ||
		filename.endsWith('.yaml') ||
		filename.startsWith('RELEASES') ||
		(filename.startsWith('releases') && filename.endsWith('.json')) ||
		(filename.startsWith('assets') && filename.endsWith('.json'))
	);
}

function normalizePlatformArchPath(path: string): string {
	const match = path.match(PLATFORM_ARCH_PATH);
	return match ? `${match[1]}/${match[2]}${match[3]}` : path;
}

function resolveCoordinateDocument(path: string): string {
	const match = path.match(COORDINATE_DOCUMENT_PATH);
	const document = match ? DESKTOP_COORDINATE_DOCUMENTS.get(match[2]) : undefined;
	return match && document ? `${match[1]}/${document}` : path;
}

export function resolveDownloadObjectPath(requestPath: string): string | null {
	if (!requestPath.startsWith(DOWNLOAD_PREFIX)) {
		return null;
	}
	const normalized = posix.normalize(requestPath.slice(DOWNLOAD_PREFIX.length).replace(/^\/+/u, ''));
	if (normalized.length === 0 || normalized.startsWith('/') || normalized.startsWith('..')) {
		return null;
	}
	for (const segment of normalized.split('/')) {
		if (segment.length === 0 || segment === '.' || segment === '..' || segment.includes('\0')) {
			return null;
		}
	}
	const objectPath = resolveCoordinateDocument(normalizePlatformArchPath(normalized));
	return objectPath.startsWith(DESKTOP_PATH_PREFIX) ? objectPath : null;
}

export function downloadRedirectCacheControl(objectPath: string): string {
	const segments = objectPath.split('/');
	if (segments[SCOPE_SEGMENT_INDEX] === 'latest') {
		return MUTABLE_REDIRECT_CACHE_CONTROL;
	}
	return isReleaseFeedFilename(segments[segments.length - 1])
		? MUTABLE_REDIRECT_CACHE_CONTROL
		: VERSIONED_REDIRECT_CACHE_CONTROL;
}

export function resolveDownloadRedirect(requestPath: string): DownloadRedirect | null {
	const objectPath = resolveDownloadObjectPath(requestPath);
	if (!objectPath) {
		return null;
	}
	return {
		location: `${PKGS_BASE_URL}/${objectPath}`,
		cacheControl: downloadRedirectCacheControl(objectPath),
	};
}
