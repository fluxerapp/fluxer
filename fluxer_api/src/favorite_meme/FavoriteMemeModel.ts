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

import {userIdToChannelId} from '~/BrandedTypes';
import {MAX_FAVORITE_MEME_TAGS} from '~/Constants';
import {makeAttachmentCdnUrl} from '~/channel/services/message/MessageHelpers';
import type {FavoriteMeme} from '~/Models';
import {createStringType, Int64Type, z} from '~/Schema';

export const FavoriteMemeResponse = z.object({
	id: z.string(),
	user_id: z.string(),
	name: z.string(),
	alt_text: z.string().nullish(),
	tags: z.array(z.string()),
	attachment_id: z.string(),
	filename: z.string(),
	content_type: z.string(),
	content_hash: z.string().nullish(),
	size: z.number(),
	width: z.number().int().nullish(),
	height: z.number().int().nullish(),
	duration: z.number().nullish(),
	url: z.string(),
	is_gifv: z.boolean().default(false),
	tenor_id: z.string().nullish(),
});
export type FavoriteMemeResponse = z.infer<typeof FavoriteMemeResponse>;

export const CreateFavoriteMemeBodySchema = z
	.object({
		attachment_id: Int64Type.nullish(),
		embed_index: z.number().int().min(0).nullish(),
		name: createStringType(1, 100),
		alt_text: createStringType(0, 500).nullish(),
		tags: z
			.array(createStringType(1, 30))
			.max(MAX_FAVORITE_MEME_TAGS, `Maximum ${MAX_FAVORITE_MEME_TAGS} tags allowed`)
			.nullish()
			.default([])
			.transform((tags) => (tags || []).filter((t) => t.trim().length > 0)),
	})
	.refine((data) => data.attachment_id !== undefined || data.embed_index !== undefined, {
		message: 'Either attachment_id or embed_index must be provided',
	});
export type CreateFavoriteMemeBodySchema = z.infer<typeof CreateFavoriteMemeBodySchema>;

export const CreateFavoriteMemeFromUrlBodySchema = z.object({
	url: z.url(),
	name: createStringType(1, 100).nullish(),
	alt_text: createStringType(0, 500).nullish(),
	tags: z
		.array(createStringType(1, 30))
		.max(MAX_FAVORITE_MEME_TAGS, `Maximum ${MAX_FAVORITE_MEME_TAGS} tags allowed`)
		.nullish()
		.default([])
		.transform((tags) => (tags || []).filter((t) => t.trim().length > 0)),
	tenor_id: createStringType(1, 100).nullish(),
});
export type CreateFavoriteMemeFromUrlBodySchema = z.infer<typeof CreateFavoriteMemeFromUrlBodySchema>;

export const UpdateFavoriteMemeBodySchema = z.object({
	name: createStringType(1, 100).nullish(),
	alt_text: createStringType(0, 500).nullish().or(z.null()),
	tags: z
		.array(createStringType(1, 30))
		.max(MAX_FAVORITE_MEME_TAGS, `Maximum ${MAX_FAVORITE_MEME_TAGS} tags allowed`)
		.nullish()
		.transform((tags) => (tags ? tags.filter((t) => t.trim().length > 0) : undefined)),
});
export type UpdateFavoriteMemeBodySchema = z.infer<typeof UpdateFavoriteMemeBodySchema>;

export const mapFavoriteMemeToResponse = (meme: FavoriteMeme): FavoriteMemeResponse => {
	const url = makeAttachmentCdnUrl(userIdToChannelId(meme.userId), meme.attachmentId, meme.filename);
	return {
		id: meme.id.toString(),
		user_id: meme.userId.toString(),
		name: meme.name,
		alt_text: meme.altText ?? null,
		tags: meme.tags || [],
		attachment_id: meme.attachmentId.toString(),
		filename: meme.filename,
		content_type: meme.contentType,
		content_hash: meme.contentHash ?? null,
		size: Number(meme.size),
		width: meme.width ?? null,
		height: meme.height ?? null,
		duration: meme.duration ?? null,
		url,
		is_gifv: meme.isGifv ?? false,
		tenor_id: meme.tenorId ?? null,
	};
};
