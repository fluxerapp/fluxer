// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';

const EXTENSION_PATTERN = /\.[A-Za-z0-9]{1,8}$/u;
const ASSET_HASH_PATTERN = /^[0-9a-f]{8}$/u;
const TRAILING_SLASHES_PATTERN = /\/+$/u;
const ANIMATED_PREFIX = 'a_';
const MIN_PATH_SEGMENTS = 2;

function mediaBase(): URL | null {
	return URL.parse(Config.endpoints.media);
}

function decodePathname(pathname: string): string {
	try {
		return decodeURIComponent(pathname);
	} catch {
		return pathname;
	}
}

function animationVariants(stem: string): Array<string> {
	if (stem.startsWith(ANIMATED_PREFIX)) {
		const bare = stem.slice(ANIMATED_PREFIX.length);
		return ASSET_HASH_PATTERN.test(bare) ? [stem, bare] : [stem];
	}
	return ASSET_HASH_PATTERN.test(stem) ? [stem, `${ANIMATED_PREFIX}${stem}`] : [stem];
}

export function canonicalizePurgeUrl(url: string): Array<string> {
	const base = mediaBase();
	const parsed = URL.parse(url);
	if (base === null || parsed === null || parsed.origin !== base.origin) {
		return [];
	}
	const basePath = base.pathname.replace(TRAILING_SLASHES_PATTERN, '');
	if (!parsed.pathname.startsWith(`${basePath}/`)) {
		return [];
	}
	const segments = decodePathname(parsed.pathname.slice(basePath.length))
		.split('/')
		.filter((segment) => segment !== '');
	if (segments.length < MIN_PATH_SEGMENTS) {
		return [];
	}
	const stem = segments[segments.length - 1]!.replace(EXTENSION_PATTERN, '');
	if (stem === '') {
		return [];
	}
	const directory = segments.slice(0, -1).join('/');
	return animationVariants(stem).map((variant) => `${base.host}${basePath}/${directory}/${variant}`);
}
