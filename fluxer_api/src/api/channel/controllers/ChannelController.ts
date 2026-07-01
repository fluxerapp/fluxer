// SPDX-License-Identifier: AGPL-3.0-or-later

import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	ChannelUpdateRequest,
	DeleteChannelQuery,
	PermissionOverwriteCreateRequest,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {
	ChannelResponse,
	ChannelSlowmodeStateResponse,
	RtcRegionResponse,
} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {
	ChannelIdOverwriteIdParam,
	ChannelIdParam,
	ChannelIdUserIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {Context} from 'hono';
import {z} from 'zod';
import {requireSudoMode} from '../../auth/services/SudoVerificationService';
import {createChannelID, createUserID} from '../../BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '../../middleware/AuthMiddleware';
import {GroupDmRecipientAddProtectionMiddleware} from '../../middleware/GroupDmProtectionMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '../../middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import {CLIENT_FEATURES_HEADER, parseClientFeaturesHeader} from '../../utils/featureUtils';
import {Validator} from '../../Validator';

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export function ChannelController(app: HonoApp) {
	app.get(
		'/channels/:channel_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'get_channel',
			summary: 'Fetch a channel',
			description:
				'Retrieves the channel object including metadata, member list, and settings. Requires the user to be a member of the channel with view permissions.',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			const channelRequestService = ctx.get('channelRequestService');
			return ctx.json(
				await channelRequestService.getChannelResponse({
					userId,
					channelId,
					requestCache,
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/slowmode',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'get_channel_slowmode_state',
			summary: 'Fetch slowmode state',
			description:
				'Returns the current slowmode rate-limit state for the calling user in this channel, including the configured interval and the time at which they are next allowed to send a message. Lets clients restore slowmode countdowns across devices without relying on local persistence.',
			responseSchema: ChannelSlowmodeStateResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const channelRequestService = ctx.get('channelRequestService');
			return ctx.json(await channelRequestService.getSlowmodeState({user, channelId}));
		},
	);
	app.get(
		'/channels/:channel_id/rtc-regions',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'list_rtc_regions',
			summary: 'List RTC regions',
			description:
				'Returns available voice and video calling regions for the channel, used to optimise connection quality. Requires membership with call permissions.',
			responseSchema: z.array(RtcRegionResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const channelRequestService = ctx.get('channelRequestService');
			return ctx.json(await channelRequestService.listRtcRegions({userId, channelId}));
		},
	);
	app.patch(
		'/channels/:channel_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdParam, {
			post: async (result, ctx: Context<HonoEnv>) => {
				if (!result.success) {
					return undefined;
				}
				const channelId = createChannelID(result.data.channel_id);
				const existing = await ctx.get('channelService').channelData.operations.getChannel({
					userId: ctx.get('user').id,
					channelId,
				});
				ctx.set('channelUpdateType', existing.type);
				return undefined;
			},
		}),
		Validator('json', ChannelUpdateRequest, {
			pre: async (raw: unknown, ctx: Context<HonoEnv>) => {
				const channelType = ctx.get('channelUpdateType');
				if (channelType === undefined) {
					throw new Error('Missing channel type for update validation');
				}
				const body = isPlainObject(raw) ? raw : {};
				return {...body, type: channelType};
			},
		}),
		OpenAPI({
			operationId: 'update_channel',
			summary: 'Update channel settings',
			description:
				'Modifies channel properties such as name, description, topic, nsfw flag, and slowmode. Requires management permissions in the channel.',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const data = ctx.req.valid('json');
			const clientFeatures = parseClientFeaturesHeader(ctx.req.header(CLIENT_FEATURES_HEADER));
			const requestCache = ctx.get('requestCache');
			const channelRequestService = ctx.get('channelRequestService');
			return ctx.json(
				await channelRequestService.updateChannel({
					userId,
					channelId,
					data,
					clientFeatures,
					requestCache,
				}),
			);
		},
	);
	app.delete(
		'/channels/:channel_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_DELETE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', DeleteChannelQuery),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'delete_channel',
			summary: 'Delete a channel',
			description:
				"Permanently removes a channel and all its content. Only server administrators or the channel owner can delete channels. When `delete_messages` is set on a group DM, the caller's authored messages in the group are deleted before leaving and sudo mode verification is required.",
			requestSchema: SudoVerificationSchema,
			requestBodyRequired: false,
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const userId = user.id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {silent, delete_messages} = ctx.req.valid('query');
			const body = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const channelRequestService = ctx.get('channelRequestService');
			await ctx.get('channelService').channelData.operations.getChannel({userId, channelId});
			if (delete_messages) {
				await requireSudoMode(ctx, user, body);
				await ctx.get('channelService').userMessageDeletion.deleteUserMessagesInScope(userId, {
					channelIds: [channelId],
				});
			}
			await channelRequestService.deleteChannel({userId, channelId, requestCache, silent});
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/channels/:channel_id/recipients/:user_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdUserIdParam),
		GroupDmRecipientAddProtectionMiddleware,
		OpenAPI({
			operationId: 'add_group_dm_recipient',
			summary: 'Add recipient to group DM',
			description:
				'Adds a user to a group direct message channel. The requesting user must be a member of the group DM. Requires CAPTCHA verification.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const recipientId = createUserID(ctx.req.valid('param').user_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').groupDms.addRecipientToChannel({
				userId,
				channelId,
				recipientId,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/recipients/:user_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdUserIdParam),
		Validator('query', DeleteChannelQuery),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'remove_group_dm_recipient',
			summary: 'Remove recipient from group DM',
			description:
				'Removes a user from a group direct message channel. The requesting user must be a member with appropriate permissions. When the caller removes themself with `delete_messages`, their authored messages in the group are deleted before leaving and sudo mode verification is required.',
			requestSchema: SudoVerificationSchema,
			requestBodyRequired: false,
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const recipientId = createUserID(ctx.req.valid('param').user_id);
			const {silent, delete_messages} = ctx.req.valid('query');
			const body = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			if (delete_messages && recipientId === userId) {
				await ctx.get('channelService').channelData.operations.getChannel({userId, channelId});
				await requireSudoMode(ctx, ctx.get('user'), body);
				await ctx.get('channelService').userMessageDeletion.deleteUserMessagesInScope(userId, {
					channelIds: [channelId],
				});
			}
			await ctx
				.get('channelService')
				.groupDms.removeRecipientFromChannel({userId, channelId, recipientId, requestCache, silent});
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/channels/:channel_id/permissions/:overwrite_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdOverwriteIdParam),
		Validator('json', PermissionOverwriteCreateRequest),
		OpenAPI({
			operationId: 'set_channel_permission_overwrite',
			summary: 'Set permission overwrite for channel',
			description:
				'Creates or updates permission overrides for a role or user in the channel. Allows fine-grained control over who can view, send messages, or manage the channel.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const overwriteId = ctx.req.valid('param').overwrite_id;
			const data = ctx.req.valid('json');
			const clientFeatures = parseClientFeaturesHeader(ctx.req.header(CLIENT_FEATURES_HEADER));
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').channelData.operations.setChannelPermissionOverwrite({
				userId,
				channelId,
				overwriteId,
				overwrite: {
					type: data.type,
					allow_: data.allow ? data.allow : 0n,
					deny_: data.deny ? data.deny : 0n,
				},
				clientFeatures,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/permissions/:overwrite_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdOverwriteIdParam),
		OpenAPI({
			operationId: 'delete_channel_permission_overwrite',
			summary: 'Delete permission overwrite',
			description:
				'Removes a permission override from a role or user in the channel, restoring default permissions. Requires channel management rights.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const overwriteId = ctx.req.valid('param').overwrite_id;
			const requestCache = ctx.get('requestCache');
			await ctx
				.get('channelService')
				.channelData.operations.deleteChannelPermissionOverwrite({userId, channelId, overwriteId, requestCache});
			return ctx.body(null, 204);
		},
	);
}
