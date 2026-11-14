// SPDX-License-Identifier: AGPL-3.0-or-later

import {createOriginHandoff, redeemOriginHandoff} from '@app/api/auth/services/OriginHandoffService';
import {Config} from '@app/api/Config';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {InvalidApiOriginError} from '@fluxer/errors/src/domains/core/InvalidApiOriginError';
import {
	ORIGIN_HANDOFF_MAX_PAYLOAD_LENGTH,
	OriginHandoffCreateRequest,
	OriginHandoffCreateResponse,
	OriginHandoffRedeemRequest,
	OriginHandoffRedeemResponse,
} from '@fluxer/schema/src/domains/auth/OriginHandoffSchemas';
import {bodyLimit} from 'hono/body-limit';

const ORIGIN_HANDOFF_CREATE_MAX_BODY_BYTES = ORIGIN_HANDOFF_MAX_PAYLOAD_LENGTH + 1024;

export function OriginHandoffController(app: HonoApp) {
	app.post(
		'/auth/origin-handoff',
		RateLimitMiddleware(RateLimitConfigs.AUTH_ORIGIN_HANDOFF_CREATE),
		LoginRequired,
		DefaultUserOnly,
		bodyLimit({
			maxSize: ORIGIN_HANDOFF_CREATE_MAX_BODY_BYTES,
			onError: () => {
				throw new FileSizeTooLargeError(ORIGIN_HANDOFF_CREATE_MAX_BODY_BYTES);
			},
		}),
		Validator('json', OriginHandoffCreateRequest),
		OpenAPI({
			operationId: 'create_origin_handoff',
			summary: 'Create origin handoff',
			responseSchema: OriginHandoffCreateResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Auth'],
			description:
				'Store encrypted client state for up to two minutes so another first-party web origin can redeem it once. The receiving origin must present the nonce whose SHA-256 digest is sent here.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const handoffId = await createOriginHandoff(ctx.get('cacheService'), {
				userId: ctx.get('user').id,
				nonceHash: body.nonce_hash,
				payload: body.payload,
			});
			const response: OriginHandoffCreateResponse = {handoff_id: handoffId};
			return ctx.json(response);
		},
	);
	app.post(
		'/auth/origin-handoff/redeem',
		RateLimitMiddleware(RateLimitConfigs.AUTH_ORIGIN_HANDOFF_REDEEM),
		Validator('json', OriginHandoffRedeemRequest),
		OpenAPI({
			operationId: 'redeem_origin_handoff',
			summary: 'Redeem origin handoff',
			responseSchema: OriginHandoffRedeemResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Return the encrypted client state stored by create origin handoff and delete it in the same step. A wrong nonce also consumes the handoff. On the official instance the request must come from a first-party web origin.',
		}),
		async (ctx) => {
			if (!Config.instance.selfHosted) {
				const origin = ctx.req.header('origin');
				if (origin === undefined || !Config.endpoints.webAppOrigins.includes(origin)) {
					throw new InvalidApiOriginError();
				}
			}
			const body = ctx.req.valid('json');
			const payload = await redeemOriginHandoff(ctx.get('cacheService'), {
				handoffId: body.handoff_id,
				nonce: body.nonce,
			});
			const response: OriginHandoffRedeemResponse = {payload};
			return ctx.json(response);
		},
	);
}
