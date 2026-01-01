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

import type {Hono} from 'hono';
import {z} from 'zod';
import type {HonoEnv} from '~/App';
import {createChannelID, createMessageID} from '~/BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type} from '~/Schema';
import {Validator} from '~/Validator';

export function ReadStateController(app: Hono<HonoEnv>): void {
	app.post(
		'/read-states/ack-bulk',
		RateLimitMiddleware(RateLimitConfigs.READ_STATE_ACK_BULK),
		LoginRequired,
		DefaultUserOnly,
		Validator(
			'json',
			z.object({
				read_states: z
					.array(z.object({channel_id: Int64Type, message_id: Int64Type}))
					.min(1)
					.max(100),
			}),
		),
		async (ctx) => {
			await ctx.get('readStateService').bulkAckMessages({
				userId: ctx.get('user').id,
				readStates: ctx.req.valid('json').read_states.map((rs) => ({
					channelId: createChannelID(rs.channel_id),
					messageId: createMessageID(rs.message_id),
				})),
			});
			return ctx.body(null, 204);
		},
	);
}
