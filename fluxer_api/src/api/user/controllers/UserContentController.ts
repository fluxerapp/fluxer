// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import {HarvestIdParam, MessageIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {MessageListResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {
	HarvestCreationResponseSchema,
	HarvestDownloadUrlResponse,
	HarvestStatusResponseSchema,
	HarvestStatusResponseSchemaNullable,
} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import {
	HarvestSelfDataRequest,
	MarkMentionsReadRequest,
	SaveMessageRequest,
	UserMentionsQueryRequest,
	UserSavedMessagesQueryRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {SavedMessageEntryListResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createChannelID, createMessageID} from '../../BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function UserContentController(app: HonoApp) {
	app.get(
		'/users/@me/mentions',
		RateLimitMiddleware(RateLimitConfigs.USER_MENTIONS_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('query', UserMentionsQueryRequest),
		OpenAPI({
			operationId: 'list_mentions_for_current_user',
			summary: 'List mentions for current user',
			responseSchema: MessageListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves messages where the current user was mentioned. Supports filtering by role mentions, everyone mentions, and specific guilds. Returns paginated list of messages.',
		}),
		async (ctx) => {
			const {limit, roles, everyone, guilds, before} = ctx.req.valid('query');
			const response = await ctx.get('userContentRequestService').listMentions({
				userId: ctx.get('user').id,
				limit,
				everyone,
				roles,
				guilds,
				before: before ? createMessageID(before) : undefined,
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/users/@me/mentions/read',
		RateLimitMiddleware(RateLimitConfigs.USER_MENTIONS_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', MarkMentionsReadRequest),
		OpenAPI({
			operationId: 'mark_mentions_read',
			summary: 'Mark mentions read',
			requestSchema: MarkMentionsReadRequest,
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"Removes multiple messages from the current user's recent mention history. Does not delete the original messages.",
		}),
		async (ctx) => {
			await ctx.get('userContentRequestService').markMentionsRead({
				userId: ctx.get('user').id,
				messageIds: ctx.req.valid('json').message_ids.map((messageId) => createMessageID(messageId)),
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/users/@me/mentions/:message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_MENTIONS_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', MessageIdParam),
		OpenAPI({
			operationId: 'delete_mention',
			summary: 'Delete mention',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"Removes a mention from the current user's mention history. Does not delete the original message, only removes it from the user's personal mention list.",
		}),
		async (ctx) => {
			await ctx.get('userContentRequestService').deleteMention({
				userId: ctx.get('user').id,
				messageId: createMessageID(ctx.req.valid('param').message_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/users/@me/saved-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('query', UserSavedMessagesQueryRequest),
		OpenAPI({
			operationId: 'list_saved_messages',
			summary: 'List saved messages',
			responseSchema: SavedMessageEntryListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves all messages saved by the current user. Messages are saved privately for easy reference. Returns paginated list of saved messages with metadata.',
		}),
		async (ctx) => {
			const response = await ctx.get('userContentRequestService').listSavedMessages({
				userId: ctx.get('user').id,
				limit: ctx.req.valid('query').limit,
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response, 200);
		},
	);
	app.post(
		'/users/@me/saved-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', SaveMessageRequest),
		OpenAPI({
			operationId: 'save_message',
			summary: 'Save message',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Saves a message for the current user. Saved messages can be accessed later from the saved messages list. Messages are saved privately.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('json');
			await ctx.get('userContentRequestService').saveMessage({
				userId: ctx.get('user').id,
				channelId: createChannelID(channel_id),
				messageId: createMessageID(message_id),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/users/@me/saved-messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', MessageIdParam),
		OpenAPI({
			operationId: 'unsave_message',
			summary: 'Unsave message',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"Removes a message from the current user's saved messages. Does not delete the original message, only removes it from the user's saved collection.",
		}),
		async (ctx) => {
			await ctx.get('userContentRequestService').unsaveMessage({
				userId: ctx.get('user').id,
				messageId: createMessageID(ctx.req.valid('param').message_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/harvest',
		RateLimitMiddleware(RateLimitConfigs.USER_DATA_HARVEST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'request_data_harvest',
			summary: 'Request data harvest',
			responseSchema: HarvestCreationResponseSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Requests a data harvest of all user data and content. Initiates an asynchronous process to compile and prepare all data for download in a portable format. Returns harvest ID and status.',
		}),
		async (ctx) => {
			const result = await ctx.get('userContentRequestService').requestHarvest({userId: ctx.get('user').id});
			return ctx.json(result, 200);
		},
	);
	app.post(
		'/users/@me/harvest/filtered',
		RateLimitMiddleware(RateLimitConfigs.USER_DATA_HARVEST),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', HarvestSelfDataRequest),
		OpenAPI({
			operationId: 'request_filtered_data_harvest',
			summary: 'Request filtered data harvest',
			responseSchema: HarvestCreationResponseSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Requests a data harvest with the same optional scope and date-range filters offered by the bulk-delete endpoint. Settings, memberships, and other non-message data are always included; the filter only constrains which messages end up in the archive. Returns harvest ID and status.',
		}),
		async (ctx) => {
			const filter = ctx.req.valid('json');
			const result = await ctx
				.get('userContentRequestService')
				.requestFilteredHarvest({userId: ctx.get('user').id, filter});
			return ctx.json(result, 200);
		},
	);
	app.get(
		'/users/@me/harvest/latest',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_LATEST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_latest_data_harvest',
			summary: 'Get latest data harvest',
			responseSchema: HarvestStatusResponseSchemaNullable,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves the status of the most recent data harvest request. Returns null if no harvest has been requested yet. Shows progress and estimated completion time.',
		}),
		async (ctx) => {
			const harvest = await ctx.get('userContentRequestService').getLatestHarvest({userId: ctx.get('user').id});
			return ctx.json(harvest, 200);
		},
	);
	app.get(
		'/users/@me/harvest/:harvestId',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_STATUS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', HarvestIdParam),
		OpenAPI({
			operationId: 'get_data_harvest_status',
			summary: 'Get data harvest status',
			responseSchema: HarvestStatusResponseSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves detailed status information for a specific data harvest. Shows progress, completion status, and other metadata about the harvest request.',
		}),
		async (ctx) => {
			const {harvestId} = ctx.req.valid('param');
			const harvest = await ctx.get('userContentRequestService').getHarvestStatus({
				userId: ctx.get('user').id,
				harvestId,
			});
			return ctx.json(harvest, 200);
		},
	);
	app.get(
		'/harvest-downloads/:harvestId',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_DOWNLOAD_FILE),
		Validator('param', HarvestIdParam),
		OpenAPI({
			operationId: 'download_data_harvest_archive',
			summary: 'Download data harvest archive',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['Users'],
			description:
				'Streams a completed data harvest archive. Authorised by a signed, expiring token rather than a session, so the link works from the harvest completion email. Only active when presigned harvest downloads are disabled.',
		}),
		async (ctx) => {
			const {harvestId} = ctx.req.valid('param');
			const token = ctx.req.query('token');
			if (!token) {
				return ctx.text('Not Found', 404);
			}
			const result = await ctx.get('userContentRequestService').streamHarvestDownload({
				harvestId,
				token,
				range: ctx.req.header('range') ?? undefined,
				storageService: ctx.get('storageService'),
			});
			if (!result) {
				return ctx.text('Not Found', 404);
			}
			const headers = new Headers();
			headers.set('Content-Type', result.contentType ?? 'application/zip');
			headers.set('Content-Disposition', `attachment; filename="${encodeURIComponent(result.filename)}"`);
			headers.set('Content-Length', String(result.contentLength));
			headers.set('Cache-Control', 'private, no-store');
			headers.set('Accept-Ranges', 'bytes');
			if (result.contentRange) {
				headers.set('Content-Range', result.contentRange);
			}
			return new Response(Readable.toWeb(result.body) as ReadableStream, {
				status: result.contentRange ? 206 : 200,
				headers,
			});
		},
	);
	app.get(
		'/users/@me/harvest/:harvestId/download',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_DOWNLOAD),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', HarvestIdParam),
		OpenAPI({
			operationId: 'get_data_harvest_download_url',
			summary: 'Get data harvest download URL',
			responseSchema: HarvestDownloadUrlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves the download URL for a completed data harvest. The URL is temporary and expires after a set time. Can only be accessed for completed harvests.',
		}),
		async (ctx) => {
			const {harvestId} = ctx.req.valid('param');
			const result = await ctx.get('userContentRequestService').getHarvestDownloadUrl({
				userId: ctx.get('user').id,
				harvestId,
				storageService: ctx.get('storageService'),
			});
			return ctx.json(result, 200);
		},
	);
}
