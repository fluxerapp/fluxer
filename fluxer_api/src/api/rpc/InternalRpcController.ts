// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {constantTimeEquals} from '@app/api/utils/SecretHash';
import {Validator} from '@app/api/Validator';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';

const INTERNAL_RPC_AUTH_HEADER = 'x-fluxer-rpc-auth';

function isValidInternalRpcToken(providedToken: string): boolean {
	const expectedToken = Config.internal.gatewayRpcAuthToken;
	return expectedToken.length > 0 && constantTimeEquals(expectedToken, providedToken);
}

export function InternalRpcController(app: HonoApp): void {
	app.post(
		'/internal/rpc',
		async (ctx, next) => {
			if (!isValidInternalRpcToken(ctx.req.header(INTERNAL_RPC_AUTH_HEADER) ?? '')) {
				throw new UnauthorizedError();
			}
			await next();
		},
		Validator('json', RpcRequest),
		async (ctx) => {
			const request = ctx.req.valid('json');
			const rpcService = ctx.get('rpcService');
			const requestCache = ctx.get('requestCache');
			const response = await rpcService.handleRpcRequest({request, requestCache});
			return ctx.json(response);
		},
	);
}
