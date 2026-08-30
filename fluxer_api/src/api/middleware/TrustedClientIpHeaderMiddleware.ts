// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {createMiddleware} from 'hono/factory';
import type {ILogger} from '../ILogger';
import type {HonoEnv} from '../types/HonoEnv';
import {resolveClientIpWithOptions} from '../utils/RequestClientIp';
import {stripApiPrefix} from '../utils/RequestPathUtils';

interface TrustedClientIpHeaderOptions {
	enabled: boolean;
	logger: ILogger;
	trustClientIpHeader: boolean;
	clientIpHeaderName: string;
	exemptPaths?: Array<string>;
}

const defaultExemptPaths: Array<string> = [
	'/_health',
	'/webhooks/livekit',
	'/test',
	'/connections/bluesky/client-metadata.json',
	'/connections/bluesky/jwks.json',
];

export function TrustedClientIpHeaderMiddleware({
	enabled,
	logger,
	trustClientIpHeader,
	clientIpHeaderName,
	exemptPaths = defaultExemptPaths,
}: TrustedClientIpHeaderOptions) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (!enabled || !trustClientIpHeader) {
			await next();
			return;
		}
		const path = stripApiPrefix(ctx.req.path);
		if (exemptPaths.some((prefix) => path === prefix || path.startsWith(prefix))) {
			await next();
			return;
		}
		const clientIpHeaderValue = ctx.req.header(clientIpHeaderName)?.trim();
		if (!clientIpHeaderValue) {
			await next();
			return;
		}
		if (!resolveClientIpWithOptions(ctx, {trustClientIpHeader, clientIpHeaderName})) {
			logger.warn({path, clientIpHeaderName}, 'Rejected request with invalid client IP header');
			throw new ForbiddenError({code: APIErrorCodes.FORBIDDEN});
		}
		await next();
	});
}
