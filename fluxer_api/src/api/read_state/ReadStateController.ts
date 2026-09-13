// SPDX-License-Identifier: AGPL-3.0-or-later

import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	ReadStateAckBulkRequest,
	ReadStateAckRequest,
	ReadStateAckResponse,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {Hono} from 'hono';

export function ReadStateController(app: Hono<HonoEnv>): void {
	app.post(
		'/read-states/ack',
		RateLimitMiddleware(RateLimitConfigs.READ_STATE_ACK_BULK),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'ack_read_states',
			summary: 'Acknowledge read states',
			description:
				'Applies one or more read-state acknowledgements and returns the authoritative read states after the write.',
			responseSchema: ReadStateAckResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Read States'],
		}),
		Validator('json', ReadStateAckRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('readStateRequestService').ackReadStates({
					userId: ctx.get('user').id,
					data: ctx.req.valid('json'),
				}),
			);
		},
	);
	app.post(
		'/read-states/ack-bulk',
		RateLimitMiddleware(RateLimitConfigs.READ_STATE_ACK_BULK),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'ack_bulk_messages',
			summary: 'Mark channels as read',
			description: 'Marks multiple channels as read for the authenticated user in bulk.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Read States'],
		}),
		Validator('json', ReadStateAckBulkRequest),
		async (ctx) => {
			await ctx.get('readStateRequestService').bulkAckMessages({
				userId: ctx.get('user').id,
				data: ctx.req.valid('json'),
			});
			return ctx.body(null, 204);
		},
	);
}
