// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CreatePrivateChannelRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {Context, MiddlewareHandler, Next} from 'hono';
import {createMiddleware} from 'hono/factory';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoEnv} from '../types/HonoEnv';
import {verifyCaptchaToken} from './CaptchaMiddleware';
import {RateLimitMiddleware} from './RateLimitMiddleware';

const groupDmCreateRateLimit = RateLimitMiddleware(RateLimitConfigs.USER_GROUP_DM_CREATE);
const groupDmRecipientAddRateLimit = RateLimitMiddleware(RateLimitConfigs.USER_GROUP_DM_RECIPIENT_ADD);

function getCreatePrivateChannelRequest(ctx: Context<HonoEnv>): CreatePrivateChannelRequest {
	const req = ctx.req as unknown as {
		valid(target: 'json'): CreatePrivateChannelRequest;
	};
	return req.valid('json');
}

async function enforceRateLimitAndCaptcha(ctx: Context<HonoEnv>, next: Next, rateLimit: MiddlewareHandler<HonoEnv>) {
	return await rateLimit(ctx, async () => {
		await verifyCaptchaToken(ctx);
		await next();
	});
}

export const GroupDmCreateProtectionMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const data = getCreatePrivateChannelRequest(ctx);
	if (data.recipients === undefined) {
		await next();
		return;
	}
	return await enforceRateLimitAndCaptcha(ctx, next, groupDmCreateRateLimit);
});

export const GroupDmRecipientAddProtectionMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	return await enforceRateLimitAndCaptcha(ctx, next, groupDmRecipientAddRateLimit);
});
