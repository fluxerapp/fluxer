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
} from '~/BrandedTypes';

type Nullish<T> = T | null;

export interface MessageAttachment {
	attachment_id: AttachmentID;
	filename: string;
	size: bigint;
	title: Nullish<string>;
	description: Nullish<string>;
	width: Nullish<number>;
	height: Nullish<number>;
	content_type: string;
	content_hash: Nullish<string>;
	placeholder: Nullish<string>;
	flags: number;
	duration: Nullish<number>;
	nsfw: Nullish<boolean>;
}

export interface MessageEmbedAuthor {
	name: Nullish<string>;
	url: Nullish<string>;
	icon_url: Nullish<string>;
}

export interface MessageEmbedProvider {
	name: Nullish<string>;
	url: Nullish<string>;
}

export interface MessageEmbedFooter {
	text: Nullish<string>;
	icon_url: Nullish<string>;
}

export interface MessageEmbedMedia {
	url: Nullish<string>;
	width: Nullish<number>;
	height: Nullish<number>;
	description: Nullish<string>;
	content_type: Nullish<string>;
	content_hash: Nullish<string>;
	placeholder: Nullish<string>;
	flags: number;
	duration: Nullish<number>;
}

export interface MessageEmbedField {
	name: Nullish<string>;
	value: Nullish<string>;
	inline: boolean;
}

export interface MessageEmbed {
	type: Nullish<string>;
	title: Nullish<string>;
	description: Nullish<string>;
	url: Nullish<string>;
	timestamp: Nullish<Date>;
	color: Nullish<number>;
	author: Nullish<MessageEmbedAuthor>;
	provider: Nullish<MessageEmbedProvider>;
	thumbnail: Nullish<MessageEmbedMedia>;
	image: Nullish<MessageEmbedMedia>;
	video: Nullish<MessageEmbedMedia>;
	footer: Nullish<MessageEmbedFooter>;
	fields: Nullish<Array<MessageEmbedField>>;
	nsfw: Nullish<boolean>;
}

export interface MessageStickerItem {
	sticker_id: StickerID;
	name: string;
	format_type: number;
}

export interface MessageReference {
	channel_id: ChannelID;
	message_id: MessageID;
	guild_id: Nullish<GuildID>;
	type: number;
}

export interface MessageSnapshot {
	content: Nullish<string>;
	timestamp: Date;
	edited_timestamp: Nullish<Date>;
	mention_users: Nullish<Set<UserID>>;
	mention_roles: Nullish<Set<RoleID>>;
	mention_channels: Nullish<Set<ChannelID>>;
	attachments: Nullish<Array<MessageAttachment>>;
	embeds: Nullish<Array<MessageEmbed>>;
	sticker_items: Nullish<Array<MessageStickerItem>>;
	type: number;
	flags: number;
}

export interface MessageCall {
	participant_ids: Nullish<Set<UserID>>;
	ended_timestamp: Nullish<Date>;
}

export interface MessageRow {
	channel_id: ChannelID;
	bucket: number;
	message_id: MessageID;
	author_id: Nullish<UserID>;
	type: number;
	webhook_id: Nullish<WebhookID>;
	webhook_name: Nullish<string>;
	webhook_avatar_hash: Nullish<string>;
	content: Nullish<string>;
	edited_timestamp: Nullish<Date>;
	pinned_timestamp: Nullish<Date>;
	flags: number;
	mention_everyone: boolean;
	mention_users: Nullish<Set<UserID>>;
	mention_roles: Nullish<Set<RoleID>>;
	mention_channels: Nullish<Set<ChannelID>>;
	attachments: Nullish<Array<MessageAttachment>>;
	embeds: Nullish<Array<MessageEmbed>>;
	sticker_items: Nullish<Array<MessageStickerItem>>;
	message_reference: Nullish<MessageReference>;
	message_snapshots: Nullish<Array<MessageSnapshot>>;
	call: Nullish<MessageCall>;
	has_reaction: Nullish<boolean>;
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
	last_message_id: Nullish<MessageID>;
	last_message_bucket: Nullish<number>;
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
