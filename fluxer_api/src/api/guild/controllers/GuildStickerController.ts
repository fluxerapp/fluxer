// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GuildIdParam,
	GuildIdStickerIdParam,
	StickerIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {PurgeQuery} from '@fluxer/schema/src/domains/common/CommonQuerySchemas';
import {
	GuildStickerBulkCreateResponse,
	GuildStickerMetadataResponse,
	GuildStickerResponse,
	GuildStickerWithUserListResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {
	GuildStickerBulkCreateRequest,
	GuildStickerCloneRequest,
	GuildStickerCreateRequest,
	GuildStickerUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {createGuildID, createStickerID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function GuildStickerController(app: HonoApp) {
	app.post(
		'/guilds/:guild_id/stickers',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildStickerCreateRequest),
		OpenAPI({
			operationId: 'create_guild_sticker',
			summary: 'Create guild sticker',
			responseSchema: GuildStickerResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Create guild sticker. Requires manage_emojis permission. Uploads a new sticker with name, description, and tags.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {name, description, tags, image} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const sticker = await ctx
				.get('guildService')
				.content.createSticker({user, guildId, name, description, tags, image}, auditLogReason);
			return ctx.json(sticker);
		},
	);
	app.post(
		'/guilds/:guild_id/stickers/bulk',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_BULK_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildStickerBulkCreateRequest),
		OpenAPI({
			operationId: 'bulk_create_guild_stickers',
			summary: 'Bulk create guild stickers',
			responseSchema: GuildStickerBulkCreateResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Bulk create guild stickers. Requires manage_emojis permission. Creates multiple stickers in a single request for efficiency.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {stickers} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const result = await ctx
				.get('guildService')
				.content.bulkCreateStickers({user, guildId, stickers}, auditLogReason);
			return ctx.json(result);
		},
	);
	app.post(
		'/guilds/:guild_id/stickers/clone',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_CLONE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildStickerCloneRequest),
		OpenAPI({
			operationId: 'clone_guild_sticker',
			summary: 'Clone guild sticker',
			responseSchema: GuildStickerResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Clone an existing sticker into this guild by referencing its id. Copies the source image server-side, so the client does not need to re-upload it. Requires manage_emojis permission in the target guild, and the source guild must permit cloning.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {source_sticker_id} = ctx.req.valid('json');
			const sourceStickerId = createStickerID(source_sticker_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const sticker = await ctx
				.get('guildService')
				.content.cloneSticker({user, guildId, sourceStickerId}, auditLogReason);
			return ctx.json(sticker);
		},
	);
	app.get(
		'/guilds/:guild_id/stickers',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKERS_LIST),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_stickers',
			summary: 'List guild stickers',
			responseSchema: GuildStickerWithUserListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'List guild stickers. Returns all custom stickers for the guild including metadata about creators, descriptions, and tags.',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').content.getStickers({userId, guildId, requestCache}));
		},
	);
	app.patch(
		'/guilds/:guild_id/stickers/:sticker_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_UPDATE),
		LoginRequired,
		Validator('param', GuildIdStickerIdParam),
		Validator('json', GuildStickerUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_sticker',
			summary: 'Update guild sticker',
			responseSchema: GuildStickerResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Update guild sticker. Requires manage_emojis permission. Updates sticker name, description, or tags.',
		}),
		async (ctx) => {
			const {guild_id, sticker_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const stickerId = createStickerID(sticker_id);
			const {name, description, tags} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx
					.get('guildService')
					.content.updateSticker({userId, guildId, stickerId, name, description, tags}, auditLogReason),
			);
		},
	);
	app.delete(
		'/guilds/:guild_id/stickers/:sticker_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_DELETE),
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_DELETE_DAILY),
		LoginRequired,
		Validator('param', GuildIdStickerIdParam),
		Validator('query', PurgeQuery),
		OpenAPI({
			operationId: 'delete_guild_sticker',
			summary: 'Delete guild sticker',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Delete guild sticker. Requires manage_emojis permission. Removes a sticker from the guild; optionally purges all references.',
		}),
		async (ctx) => {
			const {guild_id, sticker_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const stickerId = createStickerID(sticker_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {purge = false} = ctx.req.valid('query');
			await ctx.get('guildService').content.deleteSticker({userId, guildId, stickerId, purge}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/stickers/:sticker_id/metadata',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_METADATA),
		LoginRequired,
		Validator('param', StickerIdParam),
		OpenAPI({
			operationId: 'get_sticker_metadata',
			summary: 'Get sticker metadata',
			description:
				'Lookup minimal metadata for a custom sticker by id, including whether the source guild allows the in-app one-click clone shortcut. Does not require membership in the source guild.',
			responseSchema: GuildStickerMetadataResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Stickers'],
		}),
		async (ctx) => {
			const stickerId = createStickerID(ctx.req.valid('param').sticker_id);
			return ctx.json(await ctx.get('guildService').getStickerMetadata(stickerId));
		},
	);
}
