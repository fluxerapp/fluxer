// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {resolveClientIpHeaderName} from '@fluxer/ip_utils/src/ClientIp';
import {createMiddleware} from 'hono/factory';
import {Config} from '../Config';
import {Logger} from '../Logger';
import type {HonoEnv} from '../types/HonoEnv';
import {stripApiPrefix} from '../utils/RequestPathUtils';

interface RequireClientIpOptions {
	exemptPaths?: Array<string>;
	requiredHeaders?: Array<string>;
}

const defaultExemptPaths: Array<string> = [
	'/_health',
	'/webhooks/livekit',
	'/test',
	'/connections/bluesky/client-metadata.json',
	'/connections/bluesky/jwks.json',
];

export function RequireClientIpMiddleware({
	exemptPaths = defaultExemptPaths,
	requiredHeaders = [resolveClientIpHeaderName(Config.proxy.client_ip_header)],
}: RequireClientIpOptions = {}) {
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
		const hasRequiredHeader = requiredHeaders.some((header) => {
			const value = ctx.req.header(header);
			return value != null && value.trim() !== '';
		});
		if (!hasRequiredHeader) {
			Logger.warn({path}, 'Rejected request without required proxy headers');
			throw new ForbiddenError({code: APIErrorCodes.FORBIDDEN});
		}
		await next();
	});
}
