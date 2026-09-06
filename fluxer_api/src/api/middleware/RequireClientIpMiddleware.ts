// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {createMiddleware} from 'hono/factory';
import {Config} from '../Config';
import {Logger} from '../Logger';
import type {HonoEnv} from '../types/HonoEnv';
import {getRequestClientIp} from '../utils/RequestClientIp';
import {stripApiPrefix} from '../utils/RequestPathUtils';

interface RequireClientIpOptions {
	exemptPaths?: Array<string>;
}

const defaultExemptPaths: Array<string> = [
	'/_health',
	'/webhooks/livekit',
	'/test',
	'/connections/bluesky/client-metadata.json',
	'/connections/bluesky/jwks.json',
];

export function RequireClientIpMiddleware({exemptPaths = defaultExemptPaths}: RequireClientIpOptions = {}) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (Config.dev.testModeEnabled) {
			await next();
			return;
		}
		const path = stripApiPrefix(ctx.req.path);
		if (exemptPaths.some((prefix) => path === prefix || path.startsWith(prefix))) {
			await next();
			return;
		}
		if (getRequestClientIp(ctx) === null) {
			Logger.warn({path}, 'Rejected request without a resolvable client IP');
			throw new ForbiddenError({code: APIErrorCodes.FORBIDDEN});
		}
		await next();
	});
}
