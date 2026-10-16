// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	cancelPasskeyBridge,
	completePasskeyBridge,
	getPasskeyBridgeOptions,
	redeemPasskeyBridgeLogin,
	redeemPasskeyBridgeSudo,
	startPasskeyBridgeLogin,
	startPasskeyBridgeSudo,
} from '@app/api/auth/services/PasskeyBridgeService';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {LocalAuthMiddleware} from '@app/api/middleware/LocalAuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	PasskeyBridgeCeremonyIdParam,
	PasskeyBridgeCompleteRequest,
	PasskeyBridgeFinishResponse,
	PasskeyBridgeLoginRedeemResponse,
	PasskeyBridgeLoginStartRequest,
	PasskeyBridgeOptionsResponse,
	PasskeyBridgeRedeemRequest,
	PasskeyBridgeStartResponse,
	PasskeyBridgeSudoRedeemResponse,
	PasskeyBridgeSudoStartRequest,
} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';

export function PasskeyBridgeController(app: HonoApp) {
	app.post(
		'/auth/passkey-bridge',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_PASSKEY_BRIDGE_START),
		Validator('json', PasskeyBridgeLoginStartRequest),
		OpenAPI({
			operationId: 'start_passkey_bridge_login',
			summary: 'Start passkey bridge sign in',
			responseSchema: PasskeyBridgeStartResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Start a sign in or two-factor ceremony for a passkey that belongs to the paired first-party origin. Only available on the official instance from the new origin.',
		}),
		async (ctx) => {
			return ctx.json(
				await startPasskeyBridgeLogin(ctx.get('apiContext'), ctx.req.header('origin'), ctx.req.valid('json')),
			);
		},
	);
	app.post(
		'/users/@me/passkey-bridge',
		RateLimitMiddleware(RateLimitConfigs.USER_PASSKEY_BRIDGE_START),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', PasskeyBridgeSudoStartRequest),
		OpenAPI({
			operationId: 'start_passkey_bridge_sudo',
			summary: 'Start passkey bridge sudo verification',
			responseSchema: PasskeyBridgeStartResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Start a sudo verification ceremony for a passkey that belongs to the paired first-party origin. Only available on the official instance from the new origin.',
		}),
		async (ctx) => {
			return ctx.json(
				await startPasskeyBridgeSudo(
					ctx.get('apiContext'),
					ctx.req.header('origin'),
					ctx.get('user').id,
					ctx.req.valid('json'),
				),
			);
		},
	);
	app.post(
		'/auth/passkey-bridge/:ceremony_id/options',
		RateLimitMiddleware(RateLimitConfigs.AUTH_PASSKEY_BRIDGE_CEREMONY),
		Validator('param', PasskeyBridgeCeremonyIdParam),
		OpenAPI({
			operationId: 'get_passkey_bridge_options',
			summary: 'Get passkey bridge options',
			responseSchema: PasskeyBridgeOptionsResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Issue WebAuthn authentication options for a pending passkey bridge ceremony. The request must come from the origin that runs the ceremony.',
		}),
		async (ctx) => {
			const {ceremony_id} = ctx.req.valid('param');
			return ctx.json(await getPasskeyBridgeOptions(ctx.get('apiContext'), ceremony_id, ctx.req.header('origin')));
		},
	);
	app.post(
		'/auth/passkey-bridge/:ceremony_id/complete',
		RateLimitMiddleware(RateLimitConfigs.AUTH_PASSKEY_BRIDGE_CEREMONY),
		Validator('param', PasskeyBridgeCeremonyIdParam),
		Validator('json', PasskeyBridgeCompleteRequest),
		OpenAPI({
			operationId: 'complete_passkey_bridge',
			summary: 'Complete passkey bridge',
			responseSchema: PasskeyBridgeFinishResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Verify the WebAuthn response for a pending passkey bridge ceremony. A failed verification leaves the ceremony pending so it can be retried.',
		}),
		async (ctx) => {
			const {ceremony_id} = ctx.req.valid('param');
			return ctx.json(
				await completePasskeyBridge(
					ctx.get('apiContext'),
					ceremony_id,
					ctx.req.header('origin'),
					ctx.req.valid('json'),
				),
			);
		},
	);
	app.post(
		'/auth/passkey-bridge/:ceremony_id/cancel',
		RateLimitMiddleware(RateLimitConfigs.AUTH_PASSKEY_BRIDGE_CEREMONY),
		Validator('param', PasskeyBridgeCeremonyIdParam),
		OpenAPI({
			operationId: 'cancel_passkey_bridge',
			summary: 'Cancel passkey bridge',
			responseSchema: PasskeyBridgeFinishResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description: 'Cancel a passkey bridge ceremony that has not completed.',
		}),
		async (ctx) => {
			const {ceremony_id} = ctx.req.valid('param');
			return ctx.json(await cancelPasskeyBridge(ctx.get('apiContext'), ceremony_id, ctx.req.header('origin')));
		},
	);
	app.post(
		'/auth/passkey-bridge/:ceremony_id/redeem',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_PASSKEY_BRIDGE_REDEEM),
		Validator('param', PasskeyBridgeCeremonyIdParam),
		Validator('json', PasskeyBridgeRedeemRequest),
		OpenAPI({
			operationId: 'redeem_passkey_bridge_login',
			summary: 'Redeem passkey bridge sign in',
			responseSchema: PasskeyBridgeLoginRedeemResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Redeem a finished sign in or two-factor passkey bridge ceremony once. Requires the nonce kept by the starting page and the completion code handed back when the ceremony finished.',
		}),
		async (ctx) => {
			const {ceremony_id} = ctx.req.valid('param');
			return ctx.json(
				await redeemPasskeyBridgeLogin(
					ctx.get('apiContext'),
					ceremony_id,
					ctx.req.header('origin'),
					ctx.req.valid('json'),
					ctx.req.raw,
				),
			);
		},
	);
	app.post(
		'/users/@me/passkey-bridge/:ceremony_id/redeem',
		RateLimitMiddleware(RateLimitConfigs.USER_PASSKEY_BRIDGE_REDEEM),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', PasskeyBridgeCeremonyIdParam),
		Validator('json', PasskeyBridgeRedeemRequest),
		OpenAPI({
			operationId: 'redeem_passkey_bridge_sudo',
			summary: 'Redeem passkey bridge sudo verification',
			responseSchema: PasskeyBridgeSudoRedeemResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Redeem a finished sudo passkey bridge ceremony once for a sudo mode token. Requires the nonce kept by the starting page and the completion code handed back when the ceremony finished.',
		}),
		async (ctx) => {
			const {ceremony_id} = ctx.req.valid('param');
			return ctx.json(
				await redeemPasskeyBridgeSudo(
					ctx.get('apiContext'),
					ceremony_id,
					ctx.req.header('origin'),
					ctx.req.valid('json'),
					ctx.get('user').id,
					ctx.get('authSession'),
				),
			);
		},
	);
}
