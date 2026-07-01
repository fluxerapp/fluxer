// SPDX-License-Identifier: AGPL-3.0-or-later

import {GifMediaFormat} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {createStringType, SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const FavoriteMemeBase = z.object({
	name: createStringType(1, 100).describe('Display name for the meme'),
	alt_text: createStringType(0, 500).nullish().describe('Alternative text description for accessibility'),
	tags: z
		.array(createStringType(1, 30))
		.nullish()
		.default([])
		.transform((tags) => (tags || []).filter((t) => t.trim().length > 0))
		.describe('Tags for categorizing and searching the meme'),
});
export const CreateFavoriteMemeBodySchema = FavoriteMemeBase.extend({
	attachment_id: SnowflakeType.nullish().describe('ID of the message attachment to save as a meme'),
	embed_index: z.number().int().min(0).nullish().describe('Index of the message embed to save as a meme'),
}).refine((data) => data.attachment_id !== undefined || data.embed_index !== undefined, {
	message: 'Either attachment_id or embed_index must be provided',
});

export type CreateFavoriteMemeBodySchema = z.infer<typeof CreateFavoriteMemeBodySchema>;

export const CreateFavoriteMemeFromUrlBodySchema = FavoriteMemeBase.extend({
	url: z.url().describe('URL of the image or video to save as a favorite meme'),
	gif_slug: createStringType(1, 300)
		.nullish()
		.describe('Provider-issued slug or slug-id token for the GIF, when sourced from a provider'),
	gif_provider: createStringType(1, 32)
		.nullish()
		.describe('Stable name of the GIF provider that issued gif_slug. New provider GIFs are sourced from KLIPY.'),
	media: z
		.record(z.string(), GifMediaFormat)
		.nullish()
		.describe(
			'Optional provider-issued format-name → media descriptor map captured by the client at favorite-time (mirrors GifResponse.media). Only persisted for gif-sourced memes; ignored otherwise.',
		),
})
	.omit({name: true})
	.extend({
		name: createStringType(1, 100).nullish().describe('Display name for the meme'),
	});

export type CreateFavoriteMemeFromUrlBodySchema = z.infer<typeof CreateFavoriteMemeFromUrlBodySchema>;

export const UpdateFavoriteMemeBodySchema = FavoriteMemeBase.partial()
	.omit({tags: true})
	.extend({
		tags: z
			.array(createStringType(1, 30))
			.nullish()
			.transform((tags) => (tags ? tags.filter((t) => t.trim().length > 0) : undefined))
			.describe('New tags for categorizing and searching the meme'),
	});

export type UpdateFavoriteMemeBodySchema = z.infer<typeof UpdateFavoriteMemeBodySchema>;

export const FavoriteMemeResponse = z.object({
	id: SnowflakeStringType.describe('Unique identifier for the favorite meme'),
	user_id: SnowflakeStringType.describe('ID of the user who owns this favorite meme'),
	name: z.string().describe('Display name of the meme'),
	alt_text: z.string().nullish().describe('Alternative text description for accessibility'),
	tags: z.array(z.string()).describe('Tags for categorizing and searching the meme'),
	attachment_id: SnowflakeStringType.describe('ID of the attachment storing the meme'),
	filename: z.string().describe('Original filename of the meme'),
	content_type: z.string().describe('MIME type of the meme file'),
	content_hash: z.string().nullish().describe('Hash of the file content for deduplication'),
	size: z.number().describe('File size in bytes'),
	width: z.number().int().nullish().describe('Width of the image or video in pixels'),
	height: z.number().int().nullish().describe('Height of the image or video in pixels'),
	duration: z.number().nullish().describe('Duration of the video in seconds'),
	url: z.string().describe('CDN URL to access the meme'),
	is_gifv: z.boolean().default(false).describe('Whether the meme is a video converted from GIF'),
	gif_slug: z.string().nullish().describe('Provider-issued slug for the GIF this meme was sourced from, if any'),
	gif_provider: z
		.string()
		.nullish()
		.describe(
			'Stable name of the GIF provider that issued gif_slug, if any. Legacy records may contain older provider names.',
		),
	media: z
		.record(z.string(), GifMediaFormat)
		.nullish()
		.describe(
			'Provider-issued format-name → media descriptor map for gif-sourced memes (mirrors GifResponse.media). Null on memes uploaded as plain attachments.',
		),
	placeholder: z
		.string()
		.nullish()
		.describe(
			'Compact thumbhash placeholder produced by the media proxy at favorite-time. Clients render it as a low-res preview while the full media loads. Null when the proxy did not emit one.',
		),
});

export type FavoriteMemeResponse = z.infer<typeof FavoriteMemeResponse>;

export const FavoriteMemeListResponse = z.array(FavoriteMemeResponse);

export type FavoriteMemeListResponse = z.infer<typeof FavoriteMemeListResponse>;
