// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmojiIdParam, GuildIdEmojiIdParam, GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {PurgeQuery} from '@fluxer/schema/src/domains/common/CommonQuerySchemas';
import {
	GuildEmojiBulkCreateResponse,
	GuildEmojiMetadataResponse,
	GuildEmojiResponse,
	GuildEmojiWithUserListResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {
	GuildEmojiBulkCreateRequest,
	GuildEmojiCloneRequest,
	GuildEmojiCreateRequest,
	GuildEmojiUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {createEmojiID, createGuildID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function GuildEmojiController(app: HonoApp) {
	app.post(
		'/guilds/:guild_id/emojis',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildEmojiCreateRequest),
		OpenAPI({
			operationId: 'create_guild_emoji',
			summary: 'Create guild emoji',
			responseSchema: GuildEmojiResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Create guild emoji. Requires manage_emojis permission. Uploads and registers a new custom emoji for the guild.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {name, image} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const emoji = await ctx.get('guildService').content.createEmoji({user, guildId, name, image}, auditLogReason);
			return ctx.json(emoji);
		},
	);
	app.post(
		'/guilds/:guild_id/emojis/bulk',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_BULK_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildEmojiBulkCreateRequest),
		OpenAPI({
			operationId: 'bulk_create_guild_emojis',
			summary: 'Bulk create guild emojis',
			responseSchema: GuildEmojiBulkCreateResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Bulk create guild emojis. Requires manage_emojis permission. Creates multiple emojis in a single request for efficiency.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {emojis} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const result = await ctx.get('guildService').content.bulkCreateEmojis({user, guildId, emojis}, auditLogReason);
			return ctx.json(result);
		},
	);
	app.post(
		'/guilds/:guild_id/emojis/clone',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_CLONE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildEmojiCloneRequest),
		OpenAPI({
			operationId: 'clone_guild_emoji',
			summary: 'Clone guild emoji',
			responseSchema: GuildEmojiResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Clone an existing emoji into this guild by referencing its id. Copies the source image server-side, so the client does not need to re-upload it. Requires manage_emojis permission in the target guild, and the source guild must permit cloning.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {source_emoji_id} = ctx.req.valid('json');
			const sourceEmojiId = createEmojiID(source_emoji_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const emoji = await ctx.get('guildService').content.cloneEmoji({user, guildId, sourceEmojiId}, auditLogReason);
			return ctx.json(emoji);
		},
	);
	app.get(
		'/guilds/:guild_id/emojis',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJIS_LIST),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_emojis',
			summary: 'List guild emojis',
			responseSchema: GuildEmojiWithUserListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'List guild emojis. Returns all custom emojis for the guild including metadata about creators and timestamps.',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').content.getEmojis({userId, guildId, requestCache}));
		},
	);
	app.patch(
		'/guilds/:guild_id/emojis/:emoji_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_UPDATE),
		LoginRequired,
		Validator('param', GuildIdEmojiIdParam),
		Validator('json', GuildEmojiUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_emoji',
			summary: 'Update guild emoji',
			responseSchema: GuildEmojiResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Update guild emoji. Requires manage_emojis permission. Renames or updates properties of an existing emoji.',
		}),
		async (ctx) => {
			const {guild_id, emoji_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const emojiId = createEmojiID(emoji_id);
			const {name} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const emoji = await ctx.get('guildService').content.updateEmoji({userId, guildId, emojiId, name}, auditLogReason);
			return ctx.json(emoji);
		},
	);
	app.delete(
		'/guilds/:guild_id/emojis/:emoji_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_DELETE),
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_DELETE_DAILY),
		LoginRequired,
		Validator('param', GuildIdEmojiIdParam),
		Validator('query', PurgeQuery),
		OpenAPI({
			operationId: 'delete_guild_emoji',
			summary: 'Delete guild emoji',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Delete guild emoji. Requires manage_emojis permission. Removes a custom emoji from the guild; optionally purges all references.',
		}),
		async (ctx) => {
			const {guild_id, emoji_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const emojiId = createEmojiID(emoji_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {purge = false} = ctx.req.valid('query');
			await ctx.get('guildService').content.deleteEmoji({userId, guildId, emojiId, purge}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/emojis/:emoji_id/metadata',
		RateLimitMiddleware(RateLimitConfigs.GUILD_EMOJI_METADATA),
		LoginRequired,
		Validator('param', EmojiIdParam),
		OpenAPI({
			operationId: 'get_emoji_metadata',
			summary: 'Get emoji metadata',
			description:
				'Lookup minimal metadata for a custom emoji by id, including whether the source guild allows the in-app one-click clone shortcut. Does not require membership in the source guild.',
			responseSchema: GuildEmojiMetadataResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Emojis'],
		}),
		async (ctx) => {
			const emojiId = createEmojiID(ctx.req.valid('param').emoji_id);
			return ctx.json(await ctx.get('guildService').getEmojiMetadata(emojiId));
		},
	);
}
