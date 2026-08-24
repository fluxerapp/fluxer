// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MiddlewareHandler} from 'hono';

const CACHEABLE_CONTENT_TYPES = [
	'text/css',
	'application/javascript',
	'font/',
	'image/',
	'video/',
	'audio/',
	'application/font-woff2',
];

function shouldCache(contentType: string): boolean {
	return CACHEABLE_CONTENT_TYPES.some((type) => contentType.startsWith(type));
}

export interface CacheHeadersOptions {
	staticCacheControl?: string;
	defaultCacheControl?: string;
}

export function cacheHeaders(options: CacheHeadersOptions = {}): MiddlewareHandler {
	const {staticCacheControl = 'public, max-age=31536000', defaultCacheControl = 'no-cache'} = options;
	return async (c, next) => {
		await next();
		const existingCacheControl = c.res.headers.get('Cache-Control');
		if (existingCacheControl) {
			return;
		}
		const contentType = c.res.headers.get('Content-Type') || '';
		const cacheHeader = shouldCache(contentType) ? staticCacheControl : defaultCacheControl;
		c.res.headers.set('Cache-Control', cacheHeader);
	};
}
