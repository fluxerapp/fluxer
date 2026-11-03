// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {SsoRequiredError, SsoRequiredReasons} from '@fluxer/errors/src/domains/auth/SsoRequiredError';
import {createMiddleware} from 'hono/factory';

export const LocalAuthMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const ssoService = ctx.get('ssoService');
	if (await ssoService.isEnforced()) {
		throw new SsoRequiredError(SsoRequiredReasons.SIGN_IN);
	}
	await next();
});

export const MfaTicketAuthMiddleware = createMiddleware<
	HonoEnv,
	string,
	{in: {json: {ticket: string}}; out: {json: {ticket: string}}}
>(async (ctx, next) => {
	const ssoService = ctx.get('ssoService');
	if ((await ssoService.isEnforced()) && !(await ssoService.isPendingLinkTicket(ctx.req.valid('json').ticket))) {
		throw new SsoRequiredError(SsoRequiredReasons.SIGN_IN);
	}
	await next();
});
