/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {timingSafeEqual} from 'node:crypto';
import {createMiddleware} from 'hono/factory';
import type {HonoApp, HonoEnv} from '~/App';
import {Config} from '~/Config';
import {UnauthorizedError} from '~/Errors';
import {RpcRequest} from '~/rpc/RpcModel';
import {Validator} from '~/Validator';

const InternalNetworkRequired = createMiddleware<HonoEnv>(async (ctx, next) => {
	const authHeader = ctx.req.header('Authorization');
	const expectedAuth = `Bearer ${Config.gateway.rpcSecret}`;
	if (!authHeader) {
		throw new UnauthorizedError();
	}
	const authBuffer = Buffer.from(authHeader, 'utf8');
	const expectedBuffer = Buffer.from(expectedAuth, 'utf8');
	if (authBuffer.length !== expectedBuffer.length || !timingSafeEqual(authBuffer, expectedBuffer)) {
		throw new UnauthorizedError();
	}
	await next();
});

export const RpcController = (app: HonoApp) => {
	app.post('/_rpc', InternalNetworkRequired, Validator('json', RpcRequest), async (ctx) => {
		return ctx.json(
			await ctx
				.get('rpcService')
				.handleRpcRequest({request: ctx.req.valid('json'), requestCache: ctx.get('requestCache')}),
		);
	});
};
