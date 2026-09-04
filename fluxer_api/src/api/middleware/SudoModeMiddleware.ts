// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMiddleware} from 'hono/factory';
import {getSudoModeService} from '../auth/services/SudoModeService';
import type {HonoEnv} from '../types/HonoEnv';

export const SUDO_MODE_HEADER = 'X-Fluxer-Sudo-Mode-JWT';
export const SudoModeMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const user = ctx.get('user');
	ctx.set('sudoModeValid', false);
	ctx.set('sudoModeToken', null);
	if (!user) {
		await next();
		return;
	}
	if (user.isBot) {
		ctx.set('sudoModeValid', true);
		await next();
		return;
	}
	const tokenToVerify = ctx.req.header(SUDO_MODE_HEADER);
	if (!tokenToVerify) {
		await next();
		return;
	}
	const sudoModeService = getSudoModeService();
	const isValid = await sudoModeService.verifySudoToken(tokenToVerify, user.id);
	if (isValid) {
		ctx.set('sudoModeValid', true);
		ctx.set('sudoModeToken', tokenToVerify);
	}
	await next();
});
