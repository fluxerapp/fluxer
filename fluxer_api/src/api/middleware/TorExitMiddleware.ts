// SPDX-License-Identifier: AGPL-3.0-or-later

import {torExitListCache} from '@app/api/middleware/TorExitListCache';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';
import {createMiddleware} from 'hono/factory';

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
