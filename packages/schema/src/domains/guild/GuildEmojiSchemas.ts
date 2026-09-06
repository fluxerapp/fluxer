// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_GUILD_STICKER_TAGS} from '@fluxer/constants/src/LimitConstants';
import {type UserPartial, UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildEmojiResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this emoji'),
	name: z.string().describe('The name of the emoji'),
	animated: z.boolean().describe('Whether this emoji is animated'),
	nsfw: z.boolean().describe('Deprecated; always false. Retained for compatibility with older clients'),
});

export type GuildEmojiResponse = z.infer<typeof GuildEmojiResponse>;

export const GuildEmojiWithUserResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this emoji'),
	name: z.string().describe('The name of the emoji'),
	animated: z.boolean().describe('Whether this emoji is animated'),
	nsfw: z.boolean().describe('Deprecated; always false. Retained for compatibility with older clients'),
	user: z.lazy(() => UserPartialResponse).describe('The user who uploaded this emoji'),
});

export type GuildEmojiWithUserResponse = z.infer<typeof GuildEmojiWithUserResponse>;

export const GuildStickerResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this sticker'),
	name: z.string().describe('The name of the sticker'),
	description: z.string().describe('The description of the sticker'),
	tags: z.array(z.string()).max(MAX_GUILD_STICKER_TAGS).describe('Autocomplete/suggestion tags for the sticker'),
	animated: z.boolean().describe('Whether this sticker is animated'),
	nsfw: z.boolean().describe('Deprecated; always false. Retained for compatibility with older clients'),
});

export type GuildStickerResponse = z.infer<typeof GuildStickerResponse>;

export const GuildStickerWithUserResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this sticker'),
	name: z.string().describe('The name of the sticker'),
	description: z.string().describe('The description of the sticker'),
	tags: z.array(z.string()).max(MAX_GUILD_STICKER_TAGS).describe('Autocomplete/suggestion tags for the sticker'),
	animated: z.boolean().describe('Whether this sticker is animated'),
	nsfw: z.boolean().describe('Deprecated; always false. Retained for compatibility with older clients'),
	user: z.lazy(() => UserPartialResponse).describe('The user who uploaded this sticker'),
});

export type GuildStickerWithUserResponse = z.infer<typeof GuildStickerWithUserResponse>;

export const GuildEmojiBulkCreateResponse = z.object({
	success: z.array(GuildEmojiResponse).describe('Successfully created emojis'),
	failed: z
		.array(
			z.object({
				name: z.string().describe('The name of the emoji that failed to create'),
				error: z.string().describe('The error message explaining why the emoji failed to create'),
			}),
		)
		.describe('Emojis that failed to create'),
});

export type GuildEmojiBulkCreateResponse = z.infer<typeof GuildEmojiBulkCreateResponse>;

export const GuildStickerBulkCreateResponse = z.object({
	success: z.array(GuildStickerResponse).describe('Successfully created stickers'),
	failed: z
		.array(
			z.object({
				name: z.string().describe('The name of the sticker that failed to create'),
				error: z.string().describe('The error message explaining why the sticker failed to create'),
			}),
		)
		.describe('Stickers that failed to create'),
});

export type GuildStickerBulkCreateResponse = z.infer<typeof GuildStickerBulkCreateResponse>;

export const GuildEmojiWithUserListResponse = z.array(GuildEmojiWithUserResponse);

export type GuildEmojiWithUserListResponse = z.infer<typeof GuildEmojiWithUserListResponse>;

export const GuildStickerWithUserListResponse = z.array(GuildStickerWithUserResponse);
export const GuildEmojiMetadataResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this emoji'),
	guild_id: SnowflakeStringType.describe('The guild this emoji belongs to'),
	name: z.string().describe('The name of the emoji'),
	animated: z.boolean().describe('Whether this emoji is animated'),
	allow_cloning: z.boolean().describe('Whether the source guild allows non-members to use the in-app clone shortcut'),
});

export type GuildEmojiMetadataResponse = z.infer<typeof GuildEmojiMetadataResponse>;

export const GuildStickerMetadataResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this sticker'),
	guild_id: SnowflakeStringType.describe('The guild this sticker belongs to'),
	name: z.string().describe('The name of the sticker'),
	animated: z.boolean().describe('Whether this sticker is animated'),
	allow_cloning: z.boolean().describe('Whether the source guild allows non-members to use the in-app clone shortcut'),
});

export type GuildStickerMetadataResponse = z.infer<typeof GuildStickerMetadataResponse>;
export type GuildStickerWithUserListResponse = z.infer<typeof GuildStickerWithUserListResponse>;

export interface GuildEmoji {
	readonly id: string;
	readonly name: string;
	readonly animated: boolean;
	readonly user?: UserPartial;
}

export interface GuildEmojiWithUser extends GuildEmoji {
	readonly user: UserPartial;
}

export interface GuildSticker {
	readonly id: string;
	readonly name: string;
	readonly description: string;
	readonly tags: Array<string>;
	readonly animated: boolean;
	readonly user?: UserPartial;
}

export interface GuildStickerWithUser extends GuildSticker {
	readonly user: UserPartial;
}
