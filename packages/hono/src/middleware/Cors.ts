// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Context, MiddlewareHandler} from 'hono';

export interface CorsOptions {
	enabled?: boolean;
	origins?: Array<string> | '*';
	methods?: Array<string>;
	allowedHeaders?: Array<string>;
	exposedHeaders?: Array<string>;
	credentials?: boolean;
	maxAge?: number;
}

const DEFAULT_METHODS = ['GET', 'HEAD', 'PUT', 'PATCH', 'POST', 'DELETE', 'OPTIONS'];
const DEFAULT_HEADERS = ['Content-Type', 'Authorization', 'X-Requested-With', 'Accept-Language', 'X-Request-ID'];

function appendOriginVary(c: Context): void {
	const vary = c.res.headers.get('Vary');
	const fields = vary?.split(',').map((field) => field.trim().toLowerCase()) ?? [];
	if (!fields.includes('*') && !fields.includes('origin')) {
		c.header('Vary', 'Origin', {append: Boolean(vary)});
	}
}

export function cors(options: CorsOptions = {}): MiddlewareHandler {
	const {
		enabled = true,
		origins = '*',
		methods = DEFAULT_METHODS,
		allowedHeaders = DEFAULT_HEADERS,
		exposedHeaders = [],
		credentials = false,
		maxAge = 86400,
	} = options;
	return async (c, next) => {
		if (!enabled) {
			await next();
			return;
		}
		const requestOrigin = c.req.header('origin');
		const applyCorsHeaders = () => {
			if (origins === '*') {
				c.header('Access-Control-Allow-Origin', '*');
			} else if (Array.isArray(origins) && requestOrigin && origins.includes(requestOrigin)) {
				c.header('Access-Control-Allow-Origin', requestOrigin);
			}
			if (credentials) {
				c.header('Access-Control-Allow-Credentials', 'true');
			}
			if (exposedHeaders.length > 0) {
				c.header('Access-Control-Expose-Headers', exposedHeaders.join(', '));
			}
		};
		applyCorsHeaders();
		if (c.req.method === 'OPTIONS') {
			c.header('Access-Control-Allow-Methods', methods.join(', '));
			c.header('Access-Control-Allow-Headers', allowedHeaders.join(', '));
			if (maxAge !== undefined) {
				c.header('Access-Control-Max-Age', maxAge.toString());
			}
			if (origins !== '*') {
				appendOriginVary(c);
			}
			return c.body(null, 204);
		}
		await next();
		applyCorsHeaders();
		if (origins !== '*') {
			appendOriginVary(c);
		}
		return;
	};
}
