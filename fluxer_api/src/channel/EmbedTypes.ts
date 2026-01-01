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

import {AttachmentURLType, ColorType, createStringType, DateTimeType, URLType, z} from '~/Schema';

export const RichEmbedAuthorRequest = z.object({
	name: createStringType(),
	url: URLType.nullish(),
	icon_url: URLType.nullish(),
});
export type RichEmbedAuthorRequest = z.infer<typeof RichEmbedAuthorRequest>;

export const RichEmbedMediaRequest = z.object({
	url: AttachmentURLType,
	description: createStringType(1, 4096).nullish(),
});
export type RichEmbedMediaRequest = z.infer<typeof RichEmbedMediaRequest>;

export interface RichEmbedMediaWithMetadata extends RichEmbedMediaRequest {
	_attachmentMetadata?: {
		width: number | null;
		height: number | null;
		content_type: string;
		content_hash: string | null;
		placeholder: string | null;
		flags: number;
		duration: number | null;
		nsfw: boolean | null;
	};
}

export const RichEmbedFooterRequest = z.object({
	text: createStringType(1, 2048),
	icon_url: URLType.nullish(),
});
export type RichEmbedFooterRequest = z.infer<typeof RichEmbedFooterRequest>;

const RichEmbedFieldRequest = z.object({
	name: createStringType(),
	value: createStringType(1, 1024),
	inline: z.boolean().default(false),
});

export const RichEmbedRequest = z.object({
	url: URLType.nullish(),
	title: createStringType().nullish(),
	color: ColorType.nullish(),
	timestamp: DateTimeType.nullish(),
	description: createStringType(1, 4096).nullish(),
	author: RichEmbedAuthorRequest.nullish(),
	image: RichEmbedMediaRequest.nullish(),
	thumbnail: RichEmbedMediaRequest.nullish(),
	footer: RichEmbedFooterRequest.nullish(),
	fields: z.array(RichEmbedFieldRequest).max(25).nullish(),
});
export type RichEmbedRequest = z.infer<typeof RichEmbedRequest>;

const EmbedAuthorResponse = z.object({
	name: z.string(),
	url: z.url().nullish(),
	icon_url: z.url().nullish(),
	proxy_icon_url: z.url().nullish(),
});

const EmbedFooterResponse = z.object({
	text: z.string(),
	icon_url: z.url().nullish(),
	proxy_icon_url: z.url().nullish(),
});

const EmbedMediaResponse = z.object({
	url: z.string(),
	proxy_url: z.url().nullish(),
	content_type: z.string().nullish(),
	content_hash: z.string().nullish(),
	width: z.number().int().nullish(),
	height: z.number().int().nullish(),
	description: z.string().nullish(),
	placeholder: z.string().nullish(),
	duration: z.number().int().nullish(),
	flags: z.number().int(),
});

const EmbedFieldResponse = z.object({
	name: z.string(),
	value: z.string(),
	inline: z.boolean(),
});
export type EmbedFieldResponse = z.infer<typeof EmbedFieldResponse>;

export const MessageEmbedResponse = z.object({
	type: z.string(),
	url: z.url().nullish(),
	title: z.string().nullish(),
	color: z.number().int().nullish(),
	timestamp: z.iso.datetime().nullish(),
	description: z.string().nullish(),
	author: EmbedAuthorResponse.nullish(),
	image: EmbedMediaResponse.nullish(),
	thumbnail: EmbedMediaResponse.nullish(),
	footer: EmbedFooterResponse.nullish(),
	fields: z.array(EmbedFieldResponse).nullish(),
	provider: EmbedAuthorResponse.nullish(),
	video: EmbedMediaResponse.nullish(),
	audio: EmbedMediaResponse.nullish(),
	nsfw: z.boolean().nullish(),
});
export type MessageEmbedResponse = z.infer<typeof MessageEmbedResponse>;
