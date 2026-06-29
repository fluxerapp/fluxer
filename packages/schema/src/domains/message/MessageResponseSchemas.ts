// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MessageAttachmentFlags,
	MessageAttachmentFlagsDescriptions,
	MessageFlags,
	MessageFlagsDescriptions,
} from '@fluxer/constants/src/ChannelConstants';
import {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {type MessageEmbed, MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {type UserPartial, UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {MessageReferenceTypeSchema, MessageTypeSchema} from '@fluxer/schema/src/primitives/MessageValidators';
import {createBitflagInt32Type, Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const MessageAttachmentResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this attachment'),
	filename: z.string().describe('The name of the attached file'),
	title: z.string().nullish().describe('The title of the attachment'),
	description: z.string().nullish().describe('The description of the attachment'),
	content_type: z.string().nullish().describe('The MIME type of the attachment'),
	content_hash: z.string().nullish().describe('The hash of the attachment content'),
	size: Int32Type.describe('The size of the attachment in bytes'),
	url: z.string().nullish().describe('The URL of the attachment'),
	proxy_url: z.string().nullish().describe('The proxied URL of the attachment'),
	width: Int32Type.nullish().describe('The width of the attachment in pixels (for images/videos)'),
	height: Int32Type.nullish().describe('The height of the attachment in pixels (for images/videos)'),
	placeholder: z.string().nullish().describe('The base64 encoded placeholder image for lazy loading'),
	flags: createBitflagInt32Type(
		MessageAttachmentFlags,
		MessageAttachmentFlagsDescriptions,
		'The bitwise flags for this attachment',
		'MessageAttachmentFlags',
	),
	nsfw: z.boolean().nullish().describe('Whether the attachment is flagged as NSFW'),
	duration: Int32Type.nullish().describe('The duration of the media in seconds'),
	waveform: z.string().nullish().describe('The base64 encoded audio waveform data'),
	expires_at: z.string().nullish().describe('The ISO 8601 timestamp when the attachment URL expires'),
	expired: z.boolean().nullish().describe('Whether the attachment URL has expired'),
});

export type MessageAttachmentResponse = z.infer<typeof MessageAttachmentResponse>;

const MessageReferenceResponse = z.object({
	channel_id: SnowflakeStringType.describe('The ID of the channel containing the referenced message'),
	message_id: SnowflakeStringType.describe('The ID of the referenced message'),
	guild_id: SnowflakeStringType.nullish().describe('The ID of the guild containing the referenced message'),
	type: MessageReferenceTypeSchema,
});

const ReactionEmojiResponse = z.object({
	id: SnowflakeStringType.nullish().describe('The ID of the custom emoji (null for Unicode emojis)'),
	name: z.string().describe('The name of the emoji (or Unicode character for standard emojis)'),
	animated: z.boolean().nullish().describe('Whether the emoji is animated'),
});

export const MessageReactionResponse = z.object({
	emoji: ReactionEmojiResponse.describe('The emoji used for the reaction'),
	count: Int32Type.describe('The total number of times this reaction has been used'),
	me: z.boolean().nullish().describe('Whether the current user has reacted with this emoji'),
});

export type MessageReactionResponse = z.infer<typeof MessageReactionResponse>;

export const MessageStickerResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier of the sticker'),
	name: z.string().describe('The name of the sticker'),
	animated: z.boolean().describe('Whether the sticker is animated'),
	nsfw: z.boolean().optional().describe('Whether this sticker is classified as NSFW'),
});

export type MessageStickerResponse = z.infer<typeof MessageStickerResponse>;

export const MessageChannelMentionResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the mentioned channel'),
	name: z.string().describe('The name of the mentioned channel'),
	type: Int32Type.describe('The type of the mentioned channel'),
});

export type MessageChannelMentionResponse = z.infer<typeof MessageChannelMentionResponse>;

export const MessageSnapshotResponse = z.object({
	content: z.string().nullish().describe('The text content of the snapshot'),
	timestamp: z.iso.datetime().describe('The ISO 8601 timestamp of when the original message was created'),
	edited_timestamp: z.iso
		.datetime()
		.nullish()
		.describe('The ISO 8601 timestamp of when the original message was last edited'),
	mentions: z.array(z.string()).nullish().describe('The user IDs mentioned in the snapshot'),
	mention_roles: z.array(z.string()).nullish().describe('The role IDs mentioned in the snapshot'),
	mention_channels: z
		.array(MessageChannelMentionResponse)
		.nullish()
		.describe('Channels mentioned in the snapshot that are visible to @everyone'),
	embeds: z.array(MessageEmbedResponse).nullish().describe('The embeds included in the snapshot'),
	attachments: z.array(MessageAttachmentResponse).nullish().describe('The attachments included in the snapshot'),
	stickers: z.array(MessageStickerResponse).nullish().describe('The stickers included in the snapshot'),
	type: MessageTypeSchema,
	flags: createBitflagInt32Type(
		MessageFlags,
		MessageFlagsDescriptions,
		'The bitwise flags of the original message',
		'MessageFlags',
	),
});

export type MessageSnapshotResponse = z.infer<typeof MessageSnapshotResponse>;

const MessageCallResponse = z.object({
	participants: z.array(z.string()).describe('The user IDs of participants in the call'),
	ended_timestamp: z.iso.datetime().nullish().describe('The ISO 8601 timestamp of when the call ended'),
});

const MessageBaseResponseSchema = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this message'),
	channel_id: SnowflakeStringType.describe('The ID of the channel this message was sent in'),
	author: z.lazy(() => UserPartialResponse).describe('The author of the message'),
	webhook_id: SnowflakeStringType.nullish().describe('The ID of the webhook that sent this message'),
	type: MessageTypeSchema,
	flags: createBitflagInt32Type(
		MessageFlags,
		MessageFlagsDescriptions,
		'The bitwise flags for this message',
		'MessageFlags',
	),
	content: z.string().describe('The text content of the message'),
	timestamp: z.iso.datetime().describe('The ISO 8601 timestamp of when the message was created'),
	edited_timestamp: z.iso.datetime().nullish().describe('The ISO 8601 timestamp of when the message was last edited'),
	pinned: z.boolean().describe('Whether the message is pinned'),
	mention_everyone: z.boolean().describe('Whether the message mentions @everyone'),
	tts: z.boolean().describe('Whether the message was sent as text-to-speech'),
	mentions: z.array(z.lazy(() => UserPartialResponse)).describe('The users mentioned in the message'),
	mention_roles: z.array(z.string()).describe('The role IDs mentioned in the message'),
	mention_channels: z
		.array(MessageChannelMentionResponse)
		.nullish()
		.describe('Channels mentioned in the message that are visible to @everyone'),
	users: z
		.array(z.lazy(() => UserPartialResponse))
		.nullish()
		.describe(
			'Users referenced from non-notifying content, embed, and snapshot text, included for client-side resolution',
		),
	embeds: z.array(MessageEmbedResponse).nullish().describe('The embeds attached to the message'),
	attachments: z.array(MessageAttachmentResponse).nullish().describe('The files attached to the message'),
	stickers: z.array(MessageStickerResponse).nullish().describe('The stickers sent with the message'),
	nsfw_emojis: z
		.array(SnowflakeStringType)
		.optional()
		.describe('IDs of custom emojis in this message that are classified as NSFW'),
	reactions: z.array(MessageReactionResponse).nullish().describe('The reactions on the message'),
	message_reference: MessageReferenceResponse.nullish().describe('Reference data for replies or forwards'),
	message_snapshots: z.array(MessageSnapshotResponse).nullish().describe('Snapshots of forwarded messages'),
	nonce: z.string().nullish().describe('A client-provided value for message deduplication'),
	call: MessageCallResponse.nullish().describe('Call information if this message represents a call'),
});

type MessageBaseResponse = z.infer<typeof MessageBaseResponseSchema>;

export interface MessageResponse extends MessageBaseResponse {
	referenced_message?: MessageResponse | null;
}

export const MessageResponseSchema = MessageBaseResponseSchema.extend({
	referenced_message: MessageBaseResponseSchema.nullish().describe(
		'The message that this message is replying to or forwarding',
	),
});
const ChannelPinMessageResponse = MessageResponseSchema.omit({
	referenced_message: true,
	reactions: true,
}).describe('The pinned message');

export const ChannelPinResponse = z.object({
	message: ChannelPinMessageResponse,
	pinned_at: z.iso.datetime().describe('The ISO 8601 timestamp of when the message was pinned'),
});

export type ChannelPinResponse = z.infer<typeof ChannelPinResponse>;

export const ChannelPinsResponse = z.object({
	items: z.array(ChannelPinResponse).describe('Pinned messages in this channel'),
	has_more: z.boolean().describe('Whether more pins can be fetched with pagination'),
});

export type ChannelPinsResponse = z.infer<typeof ChannelPinsResponse>;

export const ReactionUsersListResponse = z.array(z.lazy(() => UserPartialResponse));

export type ReactionUsersListResponse = z.infer<typeof ReactionUsersListResponse>;

export const ReactionUsersPageResponse = z.object({
	items: z.array(z.lazy(() => UserPartialResponse)).describe('Users who reacted with the requested emoji'),
	has_more: z.boolean().describe('Whether more reaction users can be fetched'),
	next_after: SnowflakeStringType.nullable().describe('Cursor for the next page, or null when there are no more users'),
});

export type ReactionUsersPageResponse = z.infer<typeof ReactionUsersPageResponse>;

export const MessageSearchResultsResponse = z.object({
	messages: z
		.array(MessageResponseSchema.omit({referenced_message: true}))
		.describe('The messages matching the search query'),
	channels: z.array(z.lazy(() => ChannelResponse)).describe('Serialized channels referenced by the returned messages'),
	total: Int32Type.describe('The total number of messages matching the search'),
	hits_per_page: Int32Type.describe('The maximum number of messages returned per page'),
	page: Int32Type.describe('The current page number'),
	cursor: z.array(z.string()).optional().describe('Opaque cursor for fetching the next page of results'),
});

export type MessageSearchResultsResponse = z.infer<typeof MessageSearchResultsResponse>;

const MessageSearchIndexingResponse = z.object({
	indexing: z.literal(true).describe('Indicates that one or more channels are being indexed'),
});

export const MessageSearchResponse = z.union([MessageSearchResultsResponse, MessageSearchIndexingResponse]);

export type MessageSearchResponse = z.infer<typeof MessageSearchResponse>;

export const MessageListResponse = z.array(MessageResponseSchema);

export type MessageListResponse = z.infer<typeof MessageListResponse>;

export const BulkMessageFetchResponse = z.object({
	channels: z.array(
		z.object({
			channel_id: SnowflakeStringType.describe('The ID of the channel whose messages were fetched'),
			messages: MessageListResponse.describe('Messages fetched for this channel'),
		}),
	),
});

export type BulkMessageFetchResponse = z.infer<typeof BulkMessageFetchResponse>;

export interface MessageReference {
	readonly message_id: string;
	readonly channel_id: string;
	readonly guild_id?: string;
	readonly type?: number;
	readonly attachment_ids?: ReadonlyArray<string>;
	readonly embed_indices?: ReadonlyArray<number>;
}

export interface ReactionEmoji {
	readonly id?: string | null;
	readonly name: string;
	readonly animated?: boolean;
	readonly url?: string | null;
}

export interface MessageReaction {
	readonly emoji: ReactionEmoji;
	readonly count: number;
	readonly me?: true;
	readonly me_burst?: boolean;
	readonly count_details?: {
		readonly burst: number;
		readonly normal: number;
	};
}

export interface MessageAttachment {
	readonly id: string;
	readonly filename: string;
	readonly title?: string;
	readonly description?: string;
	readonly content_type?: string;
	readonly size: number;
	readonly url: string | null;
	readonly proxy_url: string | null;
	readonly width?: number;
	readonly height?: number;
	readonly placeholder?: string;
	readonly flags: number;
	readonly duration?: number;
	readonly waveform?: string;
	readonly content_hash?: string | null;
	readonly nsfw?: boolean;
	readonly expires_at?: string | null;
	readonly expired?: boolean;
}

export interface MessageCall {
	readonly participants: ReadonlyArray<string>;
	readonly ended_timestamp?: string | null;
}

export interface MessageSnapshot {
	readonly type: number;
	readonly content: string;
	readonly flags?: number;
	readonly edited_timestamp?: string;
	readonly mentions?: ReadonlyArray<string>;
	readonly mention_roles?: ReadonlyArray<string>;
	readonly mention_channels?: ReadonlyArray<ChannelMention>;
	readonly embeds?: ReadonlyArray<MessageEmbed>;
	readonly attachments?: ReadonlyArray<MessageAttachment>;
	readonly stickers?: ReadonlyArray<MessageStickerItem>;
	readonly timestamp: string;
}

export interface MessageStickerItem {
	readonly id: string;
	readonly name: string;
	readonly animated: boolean;
	readonly nsfw?: boolean;
}

export interface AllowedMentions {
	readonly parse?: ReadonlyArray<'roles' | 'users' | 'everyone'>;
	readonly roles?: ReadonlyArray<string>;
	readonly users?: ReadonlyArray<string>;
	readonly replied_user?: boolean;
}

export interface ChannelMention {
	readonly id: string;
	readonly type: number;
	readonly name: string;
}

export interface MessageMention extends UserPartial {
	readonly member?: Omit<GuildMemberData, 'user'>;
}

export interface Message {
	readonly id: string;
	readonly channel_id: string;
	readonly guild_id?: string;
	readonly author: UserPartial;
	readonly member?: Omit<GuildMemberData, 'user'>;
	readonly webhook_id?: string;
	readonly type: number;
	readonly flags: number;
	readonly pinned: boolean;
	readonly tts: boolean;
	readonly mention_everyone: boolean;
	readonly content: string;
	readonly timestamp: string;
	readonly edited_timestamp?: string;
	readonly mentions: ReadonlyArray<MessageMention>;
	readonly mention_roles: ReadonlyArray<string>;
	readonly mention_channels?: ReadonlyArray<ChannelMention>;
	readonly users?: ReadonlyArray<UserPartial>;
	readonly embeds?: ReadonlyArray<MessageEmbed>;
	readonly attachments?: ReadonlyArray<MessageAttachment>;
	readonly stickers?: ReadonlyArray<MessageStickerItem>;
	readonly nsfw_emojis?: ReadonlyArray<string>;
	readonly reactions?: ReadonlyArray<MessageReaction>;
	readonly message_reference?: MessageReference;
	readonly referenced_message?: Message | null;
	readonly message_snapshots?: ReadonlyArray<MessageSnapshot>;
	readonly call?: MessageCall | null;
	readonly state?: string;
	readonly nonce?: string;
	readonly blocked?: boolean;
	readonly _allowedMentions?: AllowedMentions;
	readonly _favoriteMemeId?: string;
}
