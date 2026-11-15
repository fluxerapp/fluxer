// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {createMiddleware} from 'hono/factory';

export function routeAvailableWhen(isAvailable: () => boolean) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (Config.instance.selfHosted && !isAvailable()) return AppNotFoundHandler(ctx);
		return next();
	});
}

export const HostedOnlyRoute = routeAvailableWhen(() => false);
