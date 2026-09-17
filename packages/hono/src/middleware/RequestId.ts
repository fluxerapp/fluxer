// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {Headers} from '@fluxer/constants/src/Headers';
import type {MiddlewareHandler} from 'hono';

export type RequestIdGenerator = () => string;

export interface RequestIdOptions {
	headerName?: string;
	generator?: RequestIdGenerator;
	setResponseHeader?: boolean;
}

const REQUEST_ID_KEY = 'requestId';

export function requestId(options: RequestIdOptions = {}): MiddlewareHandler {
	const {headerName = Headers.X_REQUEST_ID, generator = randomUUID, setResponseHeader = true} = options;
	return async (c, next) => {
		const existingId = c.req.header(headerName);
		const id = existingId || generator();
		c.set(REQUEST_ID_KEY, id);
		await next();
		if (setResponseHeader) {
			c.res.headers.set(headerName, id);
		}
	};
}
