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

import {MAX_ATTACHMENTS_PER_MESSAGE, MAX_EMBEDS_PER_MESSAGE} from '~/Constants';
import {
	type AttachmentRequestData,
	ClientAttachmentReferenceRequest,
	ClientAttachmentRequest,
} from '~/channel/AttachmentDTOs';
import {createStringType, createUnboundedStringType, Int32Type, Int64Type, z} from '~/Schema';
import {UserPartialResponse} from '~/user/UserModel';
import {MessageEmbedResponse, RichEmbedRequest} from './EmbedTypes';

const AUTHOR_TYPES = ['user', 'bot', 'webhook'] as const;
const MESSAGE_CONTENT_TYPES = [
	'image',
	'sound',
	'video',
	'file',
	'sticker',
	'embed',
	'link',
	'poll',
	'snapshot',
] as const;
const EMBED_TYPES = ['image', 'video', 'sound', 'article'] as const;
const SORT_FIELDS = ['timestamp', 'relevance'] as const;
const SORT_ORDERS = ['asc', 'desc'] as const;

const MessageAttachmentResponse = z.object({
	id: z.string(),
	filename: z.string(),
	title: z.string().nullish(),
	description: z.string().nullish(),
	content_type: z.string().nullish(),
	content_hash: z.string().nullish(),
	size: z.number().int(),
	url: z.string().nullish(),
	proxy_url: z.string().nullish(),
	width: z.number().int().nullish(),
	height: z.number().int().nullish(),
	placeholder: z.string().nullish(),
	flags: z.number().int(),
	nsfw: z.boolean().nullish(),
	duration: z.number().int().nullish(),
	expires_at: z.string().nullish(),
	expired: z.boolean().nullish(),
});
export type MessageAttachmentResponse = z.infer<typeof MessageAttachmentResponse>;

const MessageReferenceResponse = z.object({
	channel_id: z.string(),
	message_id: z.string(),
	guild_id: z.string().nullish(),
	type: z.number().int(),
});
export type MessageReferenceResponse = z.infer<typeof MessageReferenceResponse>;

const MessageReactionResponse = z.object({
	emoji: z.object({
		id: z.string().nullish(),
		name: z.string(),
		animated: z.boolean().nullish(),
	}),
	count: z.number().int(),
	me: z.literal(true).nullish(),
});
export type MessageReactionResponse = z.infer<typeof MessageReactionResponse>;

const MessageStickerResponse = z.object({
	id: z.string(),
	name: z.string(),
	format_type: z.number().int(),
});
export type MessageStickerResponse = z.infer<typeof MessageStickerResponse>;

const MessageSnapshotResponse = z.object({
	content: z.string().nullish(),
	timestamp: z.iso.datetime(),
	edited_timestamp: z.iso.datetime().nullish(),
	mentions: z.array(z.string()).nullish(),
	mention_roles: z.array(z.string()).nullish(),
	embeds: z.array(MessageEmbedResponse).nullish(),
	attachments: z.array(MessageAttachmentResponse).nullish(),
	stickers: z.array(MessageStickerResponse).nullish(),
	type: z.number().int(),
	flags: z.number().int(),
});

const MessageCallResponse = z.object({
	participants: z.array(z.string()),
	ended_timestamp: z.iso.datetime().nullish(),
});

const BaseMessageResponse = z.object({
	id: z.string(),
	channel_id: z.string(),
	author: z.lazy(() => UserPartialResponse),
	webhook_id: z.string().nullish(),
	type: z.number().int(),
	flags: z.number().int(),
	content: z.string(),
	timestamp: z.iso.datetime(),
	edited_timestamp: z.iso.datetime().nullish(),
	pinned: z.boolean(),
	mention_everyone: z.boolean(),
	tts: z.boolean().optional(),
	mentions: z.array(z.lazy(() => UserPartialResponse)).nullish(),
	mention_roles: z.array(z.string()).nullish(),
	embeds: z.array(MessageEmbedResponse).nullish(),
	attachments: z.array(MessageAttachmentResponse).nullish(),
	stickers: z.array(MessageStickerResponse).nullish(),
	reactions: z.array(MessageReactionResponse).nullish(),
	message_reference: MessageReferenceResponse.nullish(),
	message_snapshots: z.array(MessageSnapshotResponse).nullish(),
	nonce: z.string().nullish(),
	call: MessageCallResponse.nullish(),
});
export interface MessageResponse extends z.infer<typeof BaseMessageResponse> {
	referenced_message?: MessageResponse | null;
}

const MessageResponseSchema = z.object({
	...BaseMessageResponse.shape,
	referenced_message: BaseMessageResponse.nullish(),
});

const MessageReferenceRequest = z
	.object({
		message_id: Int64Type,
		channel_id: Int64Type.optional(),
		guild_id: Int64Type.optional(),
		type: z.number().int().optional(),
	})
	.refine(
		(data) => {
			if (data.type === 1) {
				return data.channel_id !== undefined && data.message_id !== undefined;
			}
			return true;
		},
		{
			message: 'Forward message reference must include channel_id and message_id',
		},
	);

export const ALLOWED_MENTIONS_PARSE = ['users', 'roles', 'everyone'] as const;

export const AllowedMentionsRequest = z.object({
	parse: z.array(z.enum(ALLOWED_MENTIONS_PARSE)).optional(),
	users: z.array(Int64Type).max(100).optional(),
	roles: z.array(Int64Type).max(100).optional(),
	replied_user: z.boolean().optional(),
});
export type AllowedMentionsRequest = z.infer<typeof AllowedMentionsRequest>;

export const MessageAttachmentRequest = ClientAttachmentRequest;
export type MessageAttachmentRequest = z.infer<typeof ClientAttachmentRequest>;

export const MessageUpdateAttachmentRequest = ClientAttachmentReferenceRequest;
export type MessageUpdateAttachmentRequest = z.infer<typeof MessageUpdateAttachmentRequest>;

const MessageRequestSchema = z
	.object({
		content: createUnboundedStringType().nullish(),
		embeds: z.array(RichEmbedRequest).max(MAX_EMBEDS_PER_MESSAGE),
		attachments: z.array(ClientAttachmentRequest).max(MAX_ATTACHMENTS_PER_MESSAGE),
		message_reference: MessageReferenceRequest.nullish(),
		allowed_mentions: AllowedMentionsRequest.nullish(),
		flags: Int32Type.default(0),
		nonce: createStringType(1, 32),
		favorite_meme_id: Int64Type.nullish(),
		sticker_ids: z.array(Int64Type).max(3).nullish(),
		tts: z.boolean().optional(),
	})
	.partial();

export const MessageRequest = MessageRequestSchema;

interface BaseMessageRequestType extends Omit<z.infer<typeof MessageRequestSchema>, 'attachments'> {}
export interface MessageRequest extends BaseMessageRequestType {
	attachments?: Array<AttachmentRequestData>;
}

const MessageUpdateRequestSchema = MessageRequestSchema.pick({
	content: true,
	embeds: true,
	allowed_mentions: true,
}).extend({
	flags: Int32Type.optional(),
	attachments: z.array(MessageUpdateAttachmentRequest).max(MAX_ATTACHMENTS_PER_MESSAGE).optional(),
});

export const MessageUpdateRequest = MessageUpdateRequestSchema;

interface BaseMessageUpdateRequestType extends Omit<z.infer<typeof MessageUpdateRequestSchema>, 'attachments'> {}
export interface MessageUpdateRequest extends BaseMessageUpdateRequestType {
	attachments?: Array<AttachmentRequestData>;
}

export const ChannelPinResponse = z.object({
	message: MessageResponseSchema.omit({referenced_message: true, reactions: true}),
	pinned_at: z.iso.datetime(),
});
export type ChannelPinResponse = z.infer<typeof ChannelPinResponse>;

export const MessageSearchScope = z.enum([
	'current',
	'open_dms',
	'all_dms',
	'all_guilds',
	'all',
	'open_dms_and_all_guilds',
]);
export type MessageSearchScope = z.infer<typeof MessageSearchScope>;

export const MessageSearchRequest = z.object({
	hits_per_page: z.number().int().min(1).max(25).default(25),
	page: z.number().int().min(1).max(Number.MAX_SAFE_INTEGER).default(1),
	max_id: Int64Type.optional(),
	min_id: Int64Type.optional(),

	content: createStringType(1, 1024).optional(),
	contents: z.array(createStringType(1, 1024)).max(100).optional(),

	channel_id: z.array(Int64Type).max(500).optional(),
	exclude_channel_id: z.array(Int64Type).max(500).optional(),

	author_type: z.array(z.enum(AUTHOR_TYPES)).optional(),
	exclude_author_type: z.array(z.enum(AUTHOR_TYPES)).optional(),
	author_id: z.array(Int64Type).optional(),
	exclude_author_id: z.array(Int64Type).optional(),

	mentions: z.array(Int64Type).optional(),
	exclude_mentions: z.array(Int64Type).optional(),
	mention_everyone: z.boolean().optional(),

	pinned: z.boolean().optional(),

	has: z.array(z.enum(MESSAGE_CONTENT_TYPES)).optional(),
	exclude_has: z.array(z.enum(MESSAGE_CONTENT_TYPES)).optional(),

	embed_type: z.array(z.enum(EMBED_TYPES)).optional(),
	exclude_embed_type: z.array(z.enum(EMBED_TYPES)).optional(),
	embed_provider: z.array(createStringType(1, 256)).optional(),
	exclude_embed_provider: z.array(createStringType(1, 256)).optional(),

	link_hostname: z.array(createStringType(1, 255)).optional(),
	exclude_link_hostname: z.array(createStringType(1, 255)).optional(),

	attachment_filename: z.array(createStringType(1, 1024)).optional(),
	exclude_attachment_filename: z.array(createStringType(1, 1024)).optional(),
	attachment_extension: z.array(createStringType(1, 32)).optional(),
	exclude_attachment_extension: z.array(createStringType(1, 32)).optional(),

	sort_by: z.enum(SORT_FIELDS).default('timestamp'),
	sort_order: z.enum(SORT_ORDERS).default('desc'),

	include_nsfw: z.boolean().default(false),
	scope: MessageSearchScope.optional(),
});
export type MessageSearchRequest = z.infer<typeof MessageSearchRequest>;

export const MessageSearchResponse = z.object({
	messages: z.array(MessageResponseSchema.omit({referenced_message: true})),
	total: z.number().int(),
	hits_per_page: z.number().int(),
	page: z.number().int(),
});
export type MessageSearchResponse = z.infer<typeof MessageSearchResponse>;
