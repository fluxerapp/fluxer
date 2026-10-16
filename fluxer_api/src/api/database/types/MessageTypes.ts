// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AttachmentID,
	ChannelID,
	EmojiID,
	GuildID,
	MessageID,
	RoleID,
	StickerID,
	UserID,
	WebhookID,
} from '@app/api/BrandedTypes';

export interface MessageAttachment {
	attachment_id: AttachmentID;
	filename: string;
	size: bigint;
	title: string | null;
	description: string | null;
	width: number | null;
	height: number | null;
	content_type: string;
	content_hash: string | null;
	placeholder: string | null;
	flags: number;
	duration: number | null;
	nsfw: boolean | null;
	waveform: string | null;
}

export interface MessageEmbedAuthor {
	name: string | null;
	url: string | null;
	icon_url: string | null;
}

export interface MessageEmbedProvider {
	name: string | null;
	url: string | null;
}

export interface MessageEmbedFooter {
	text: string | null;
	icon_url: string | null;
}

export interface MessageEmbedMedia {
	url: string | null;
	width: number | null;
	height: number | null;
	description: string | null;
	content_type: string | null;
	content_hash: string | null;
	placeholder: string | null;
	flags: number;
	duration: number | null;
}

export interface MessageEmbedField {
	name: string | null;
	value: string | null;
	inline: boolean;
}

interface MessageEmbedBase {
	type: string | null;
	title: string | null;
	description: string | null;
	url: string | null;
	timestamp: Date | null;
	color: number | null;
	author: MessageEmbedAuthor | null;
	provider: MessageEmbedProvider | null;
	thumbnail: MessageEmbedMedia | null;
	image: MessageEmbedMedia | null;
	video: MessageEmbedMedia | null;
	audio?: MessageEmbedMedia | null;
	footer: MessageEmbedFooter | null;
	fields: Array<MessageEmbedField> | null;
	html?: string | null;
	html_width?: number | null;
	html_height?: number | null;
	nsfw: boolean | null;
}

export interface MessageEmbedChild extends MessageEmbedBase {}

export interface MessageEmbed extends MessageEmbedBase {
	children?: Array<MessageEmbedChild> | null;
}

export interface MessageStickerItem {
	sticker_id: StickerID;
	name: string;
	animated?: boolean;
}

export interface MessageReference {
	channel_id: ChannelID;
	message_id: MessageID | null;
	guild_id: GuildID | null;
	type: number;
}

export interface MessageSnapshot {
	content: string | null;
	timestamp: Date;
	edited_timestamp: Date | null;
	mention_users: Set<UserID> | null;
	mention_roles: Set<RoleID> | null;
	mention_channels: Set<ChannelID> | null;
	attachments: Array<MessageAttachment> | null;
	embeds: Array<MessageEmbed> | null;
	sticker_items: Array<MessageStickerItem> | null;
	type: number;
	flags: number;
}

export interface MessageCall {
	participant_ids: Set<UserID> | null;
	ended_timestamp: Date | null;
}

export interface MessageRow {
	channel_id: ChannelID;
	bucket: number;
	message_id: MessageID;
	author_id: UserID | null;
	type: number;
	webhook_id: WebhookID | null;
	webhook_name: string | null;
	webhook_avatar_hash: string | null;
	content: string | null;
	edited_timestamp: Date | null;
	pinned_timestamp: Date | null;
	flags: number;
	mention_everyone: boolean;
	mention_users: Set<UserID> | null;
	mention_roles: Set<RoleID> | null;
	mention_channels: Set<ChannelID> | null;
	attachments: Array<MessageAttachment> | null;
	embeds: Array<MessageEmbed> | null;
	sticker_items: Array<MessageStickerItem> | null;
	message_reference: MessageReference | null;
	message_snapshots: Array<MessageSnapshot> | null;
	call: MessageCall | null;
	has_reaction: boolean | null;
	version: number;
}

export const MESSAGE_COLUMNS = [
	'channel_id',
	'bucket',
	'message_id',
	'author_id',
	'type',
	'webhook_id',
	'webhook_name',
	'webhook_avatar_hash',
	'content',
	'edited_timestamp',
	'pinned_timestamp',
	'flags',
	'mention_everyone',
	'mention_users',
	'mention_roles',
	'mention_channels',
	'attachments',
	'embeds',
	'sticker_items',
	'message_reference',
	'message_snapshots',
	'call',
	'has_reaction',
	'version',
] as const satisfies ReadonlyArray<keyof MessageRow>;

export interface ChannelPinRow {
	channel_id: ChannelID;
	message_id: MessageID;
	pinned_timestamp: Date;
}

export interface MessageReactionRow {
	channel_id: ChannelID;
	bucket: number;
	message_id: MessageID;
	user_id: UserID;
	emoji_id: EmojiID;
	emoji_name: string;
	emoji_animated: boolean;
	created_at: Date | null;
}

export interface AttachmentLookupRow {
	channel_id: ChannelID;
	attachment_id: AttachmentID;
	filename: string;
	message_id: MessageID;
}

export const ATTACHMENT_LOOKUP_COLUMNS = [
	'channel_id',
	'attachment_id',
	'filename',
	'message_id',
] as const satisfies ReadonlyArray<keyof AttachmentLookupRow>;
export const CHANNEL_PIN_COLUMNS = ['channel_id', 'message_id', 'pinned_timestamp'] as const satisfies ReadonlyArray<
	keyof ChannelPinRow
>;
export const MESSAGE_REACTION_COLUMNS = [
	'channel_id',
	'bucket',
	'message_id',
	'user_id',
	'emoji_id',
	'emoji_name',
	'emoji_animated',
	'created_at',
] as const satisfies ReadonlyArray<keyof MessageReactionRow>;

export interface MessageByAuthorRow {
	author_id: UserID;
	channel_id: ChannelID;
	message_id: MessageID;
}

export const MESSAGE_BY_AUTHOR_COLUMNS = ['author_id', 'channel_id', 'message_id'] as const satisfies ReadonlyArray<
	keyof MessageByAuthorRow
>;

export interface ChannelStateRow {
	channel_id: ChannelID;
	created_bucket: number;
	has_messages: boolean;
	last_message_id: MessageID | null;
	last_message_bucket: number | null;
	updated_at: Date;
}

export const CHANNEL_STATE_COLUMNS = [
	'channel_id',
	'created_bucket',
	'has_messages',
	'last_message_id',
	'last_message_bucket',
	'updated_at',
] as const satisfies ReadonlyArray<keyof ChannelStateRow>;

export interface ChannelMessageBucketRow {
	channel_id: ChannelID;
	bucket: number;
	updated_at: Date;
}

export const CHANNEL_MESSAGE_BUCKET_COLUMNS = ['channel_id', 'bucket', 'updated_at'] as const satisfies ReadonlyArray<
	keyof ChannelMessageBucketRow
>;

export interface ChannelEmptyBucketRow {
	channel_id: ChannelID;
	bucket: number;
	updated_at: Date;
}

export const CHANNEL_EMPTY_BUCKET_COLUMNS = ['channel_id', 'bucket', 'updated_at'] as const satisfies ReadonlyArray<
	keyof ChannelEmptyBucketRow
>;
