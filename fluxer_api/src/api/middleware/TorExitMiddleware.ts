// SPDX-License-Identifier: AGPL-3.0-or-later

import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {createMiddleware} from 'hono/factory';
import {Config} from '../Config';
import type {HonoEnv} from '../types/HonoEnv';
import {torExitListCache} from './TorExitListCache';

export const TorExitMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const clientIp = extractClientIp(ctx.req.raw, {
		trustClientIpHeader: Config.proxy.trust_client_ip_header,
		clientIpHeaderName: Config.proxy.client_ip_header,
	});
	if (clientIp && torExitListCache.isTorExit(clientIp)) {
		throw new IpBannedError({
			ipAddress: clientIp,
			kind: 'permanent',
		});
	}
	await next();
});
