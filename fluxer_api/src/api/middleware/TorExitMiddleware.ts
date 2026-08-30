// SPDX-License-Identifier: AGPL-3.0-or-later

import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';
import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '../types/HonoEnv';
import {getRequestClientIp} from '../utils/RequestClientIp';
import {torExitListCache} from './TorExitListCache';

export const TorExitMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const clientIp = getRequestClientIp(ctx);
	if (clientIp && torExitListCache.isTorExit(clientIp)) {
		throw new IpBannedError({
			ipAddress: clientIp,
			kind: 'permanent',
		});
	}
	await next();
});
