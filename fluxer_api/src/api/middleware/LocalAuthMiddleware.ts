// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {SsoRequiredError} from '@fluxer/errors/src/domains/auth/SsoRequiredError';
import {createMiddleware} from 'hono/factory';

export const LocalAuthMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const ssoService = ctx.get('ssoService');
	if (await ssoService.isEnforced()) {
		throw new SsoRequiredError();
	}
	await next();
});
