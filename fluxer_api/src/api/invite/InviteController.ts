// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelIdParam, GuildIdParam, InviteCodeParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	ChannelInviteCreateRequest,
	InviteMetadataResponseSchema,
	InviteResponseSchema,
} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {z} from 'zod';
import {createChannelID, createGuildID, createInviteCode} from '../BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {Validator} from '../Validator';

export function InviteController(app: HonoApp) {
	app.get(
		'/invites/:invite_code',
		RateLimitMiddleware(RateLimitConfigs.INVITE_GET),
		Validator('param', InviteCodeParam),
		OpenAPI({
			operationId: 'get_invite',
			summary: 'Get invite information',
			description:
				'Fetches detailed information about an invite using its code, including the guild or channel it belongs to and metadata such as expiration and usage limits. This endpoint does not require authentication and does not consume the invite.',
			responseSchema: InviteResponseSchema,
			statusCode: 200,
			security: [],
			tags: ['Invites'],
		}),
		async (ctx) => {
			const inviteCode = createInviteCode(ctx.req.valid('param').invite_code);
			const inviteRequestService = ctx.get('inviteRequestService');
			const requestCache = ctx.get('requestCache');
			return ctx.json(await inviteRequestService.getInvite({inviteCode, requestCache}));
		},
	);
	app.post(
		'/invites/:invite_code',
		RateLimitMiddleware(RateLimitConfigs.INVITE_ACCEPT),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', InviteCodeParam),
		OpenAPI({
			operationId: 'accept_invite',
			summary: 'Accept invite',
			description:
				'Accepts an invite using its code, adding the authenticated user to the corresponding guild or other entity. The invite usage count is incremented, and if it reaches its maximum usage limit or expiration, the invite is automatically revoked. Returns the accepted invite details.',
			responseSchema: InviteResponseSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Invites'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const inviteCode = createInviteCode(ctx.req.valid('param').invite_code);
			const inviteRequestService = ctx.get('inviteRequestService');
			const requestCache = ctx.get('requestCache');
			const invite = await inviteRequestService.acceptInvite({userId, inviteCode, requestCache});
			return ctx.json(invite);
		},
	);
	app.delete(
		'/invites/:invite_code',
		RateLimitMiddleware(RateLimitConfigs.INVITE_DELETE),
		LoginRequired,
		Validator('param', InviteCodeParam),
		OpenAPI({
			operationId: 'delete_invite',
			summary: 'Delete invite',
			description:
				'Permanently deletes an invite by its code, preventing any further usage. The authenticated user must have permission to manage invites for the guild or channel associated with the invite. This action can be logged in the audit log if an X-Audit-Log-Reason header is provided.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Invites'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const inviteCode = createInviteCode(ctx.req.valid('param').invite_code);
			const inviteRequestService = ctx.get('inviteRequestService');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await inviteRequestService.deleteInvite({userId, inviteCode, auditLogReason});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/invites',
		RateLimitMiddleware(RateLimitConfigs.INVITE_CREATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', ChannelInviteCreateRequest),
		OpenAPI({
			operationId: 'create_channel_invite',
			summary: 'Create channel invite',
			description:
				'Creates a new invite for the specified channel with optional parameters such as maximum age, maximum uses, and temporary membership settings. The authenticated user must have permission to create invites for the channel. Returns the created invite with full metadata including usage statistics.',
			responseSchema: InviteMetadataResponseSchema,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Invites'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const inviteRequestService = ctx.get('inviteRequestService');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const requestCache = ctx.get('requestCache');
			return ctx.json(
				await inviteRequestService.createChannelInvite({
					inviterId: userId,
					channelId,
					requestCache,
					data: ctx.req.valid('json'),
					auditLogReason,
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/invites',
		RateLimitMiddleware(RateLimitConfigs.INVITE_LIST_CHANNEL),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'list_channel_invites',
			summary: 'List channel invites',
			description:
				'Retrieves all currently active invites for the specified channel, including invite codes, creators, expiration times, and usage statistics. The authenticated user must have permission to manage invites for the channel. Returns an array of invite metadata objects.',
			responseSchema: z.array(InviteMetadataResponseSchema),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Invites'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const inviteRequestService = ctx.get('inviteRequestService');
			const requestCache = ctx.get('requestCache');
			return ctx.json(await inviteRequestService.listChannelInvites({userId, channelId, requestCache}));
		},
	);
	app.get(
		'/guilds/:guild_id/invites',
		RateLimitMiddleware(RateLimitConfigs.INVITE_LIST_GUILD),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_invites',
			summary: 'List guild invites',
			description:
				'Retrieves all currently active invites across all channels in the specified guild, including invite codes, creators, expiration times, and usage statistics. The authenticated user must have permission to manage invites for the guild. Returns an array of invite metadata objects.',
			responseSchema: z.array(InviteMetadataResponseSchema),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Invites'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const inviteRequestService = ctx.get('inviteRequestService');
			const requestCache = ctx.get('requestCache');
			return ctx.json(await inviteRequestService.listGuildInvites({userId, guildId, requestCache}));
		},
	);
}
