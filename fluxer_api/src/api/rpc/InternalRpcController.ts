// SPDX-License-Identifier: AGPL-3.0-or-later

import {timingSafeEqual} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';

const INTERNAL_RPC_AUTH_HEADER = 'x-fluxer-rpc-auth';

function isValidInternalRpcToken(providedToken: string): boolean {
	const expectedToken = Config.internal.gatewayRpcAuthToken;
	const expectedBuffer = Buffer.from(expectedToken);
	const providedBuffer = Buffer.from(providedToken);
	if (expectedBuffer.length === 0 || providedBuffer.length === 0 || expectedBuffer.length !== providedBuffer.length) {
		return false;
	}
	return timingSafeEqual(expectedBuffer, providedBuffer);
}

export function InternalRpcController(app: HonoApp): void {
	app.post('/internal/rpc', Validator('json', RpcRequest), async (ctx) => {
		const providedToken = ctx.req.header(INTERNAL_RPC_AUTH_HEADER) ?? '';
		if (!isValidInternalRpcToken(providedToken)) {
			throw new UnauthorizedError();
		}
		const request = ctx.req.valid('json');
		const rpcService = ctx.get('rpcService');
		const requestCache = ctx.get('requestCache');
		const response = await rpcService.handleRpcRequest({request, requestCache});
		return ctx.json(response);
	});
}
