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

import type {MessageRow} from '~/database/CassandraTypes';
import type {ChannelID, MessageID, RoleID, UserID, WebhookID} from '../BrandedTypes';
import {Attachment} from './Attachment';
import {CallInfo} from './CallInfo';
import {Embed} from './Embed';
import {MessageRef} from './MessageRef';
import {MessageSnapshot} from './MessageSnapshot';
import {StickerItem} from './StickerItem';

export class Message {
	readonly channelId: ChannelID;
	readonly bucket: number;
	readonly id: MessageID;
	readonly authorId: UserID | null;
	readonly type: number;
	readonly webhookId: WebhookID | null;
	readonly webhookName: string | null;
	readonly webhookAvatarHash: string | null;
	readonly content: string | null;
	readonly editedTimestamp: Date | null;
	readonly pinnedTimestamp: Date | null;
	readonly flags: number;
	readonly mentionEveryone: boolean;
	readonly mentionedUserIds: Set<UserID>;
	readonly mentionedRoleIds: Set<RoleID>;
	readonly mentionedChannelIds: Set<ChannelID>;
	readonly attachments: Array<Attachment>;
	readonly embeds: Array<Embed>;
	readonly stickers: Array<StickerItem>;
	readonly reference: MessageRef | null;
	readonly messageSnapshots: Array<MessageSnapshot>;
	readonly call: CallInfo | null;
	readonly hasReaction: boolean | null;
	readonly version: number;

	constructor(row: MessageRow) {
		this.channelId = row.channel_id;
		this.bucket = row.bucket;
		this.id = row.message_id;
		this.authorId = row.author_id ?? null;
		this.type = row.type;
		this.webhookId = row.webhook_id ?? null;
		this.webhookName = row.webhook_name ?? null;
		this.webhookAvatarHash = row.webhook_avatar_hash ?? null;
		this.content = row.content ?? null;
		this.editedTimestamp = row.edited_timestamp ?? null;
		this.pinnedTimestamp = row.pinned_timestamp ?? null;
		this.flags = row.flags ?? 0;
		this.mentionEveryone = row.mention_everyone ?? false;
		this.mentionedUserIds = row.mention_users ?? new Set();
		this.mentionedRoleIds = row.mention_roles ?? new Set();
		this.mentionedChannelIds = row.mention_channels ?? new Set();
		this.attachments = (row.attachments ?? []).map((att) => new Attachment(att));
		this.embeds = (row.embeds ?? []).map((embed) => new Embed(embed));
		this.stickers = (row.sticker_items ?? []).map((sticker) => new StickerItem(sticker));
		this.reference = row.message_reference ? new MessageRef(row.message_reference) : null;
		this.messageSnapshots = (row.message_snapshots ?? []).map((snapshot) => new MessageSnapshot(snapshot));
		this.call = row.call ? new CallInfo(row.call) : null;
		this.hasReaction = row.has_reaction ?? null;
		this.version = row.version;
	}

	toRow(): MessageRow {
		return {
			channel_id: this.channelId,
			bucket: this.bucket,
			message_id: this.id,
			author_id: this.authorId,
			type: this.type,
			webhook_id: this.webhookId,
			webhook_name: this.webhookName,
			webhook_avatar_hash: this.webhookAvatarHash,
			content: this.content,
			edited_timestamp: this.editedTimestamp,
			pinned_timestamp: this.pinnedTimestamp,
			flags: this.flags,
			mention_everyone: this.mentionEveryone,
			mention_users: this.mentionedUserIds.size > 0 ? this.mentionedUserIds : null,
			mention_roles: this.mentionedRoleIds.size > 0 ? this.mentionedRoleIds : null,
			mention_channels: this.mentionedChannelIds.size > 0 ? this.mentionedChannelIds : null,
			attachments: this.attachments.length > 0 ? this.attachments.map((att) => att.toMessageAttachment()) : null,
			embeds: this.embeds.length > 0 ? this.embeds.map((embed) => embed.toMessageEmbed()) : null,
			sticker_items: this.stickers.length > 0 ? this.stickers.map((sticker) => sticker.toMessageStickerItem()) : null,
			message_reference: this.reference?.toMessageReference() ?? null,
			message_snapshots:
				this.messageSnapshots.length > 0 ? this.messageSnapshots.map((snapshot) => snapshot.toMessageSnapshot()) : null,
			call: this.call?.toMessageCall() ?? null,
			has_reaction: this.hasReaction ?? null,
			version: this.version,
		};
	}
}
