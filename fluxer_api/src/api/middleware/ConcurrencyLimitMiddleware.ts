// SPDX-License-Identifier: AGPL-3.0-or-later

import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '../types/HonoEnv';
import {normalizeRequestPath} from '../utils/RequestPathUtils';

const PROBE_PATHS = new Set(['/_health', '/_healthz', '/_metrics']);
const OVERLOAD_RETRY_AFTER_SECONDS = 1;

interface ConcurrencyLimitOptions {
	maxInflightRequests: number;
}

export function ConcurrencyLimitMiddleware({maxInflightRequests}: ConcurrencyLimitOptions) {
	let inflight = 0;
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (PROBE_PATHS.has(normalizeRequestPath(ctx.req.path))) {
			await next();
			return;
		}
		inflight += 1;
		try {
			if (inflight > maxInflightRequests) {
				throw new ServiceUnavailableError({
					headers: {'Retry-After': String(OVERLOAD_RETRY_AFTER_SECONDS)},
				});
			}
			await next();
		} finally {
			inflight -= 1;
		}
	});
}
