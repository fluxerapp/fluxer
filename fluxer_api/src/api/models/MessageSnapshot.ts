// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, RoleID, UserID} from '@app/api/BrandedTypes';
import type {MessageSnapshot as CassandraMessageSnapshot} from '@app/api/database/types/MessageTypes';
import {Attachment} from '@app/api/models/Attachment';
import {Embed} from '@app/api/models/Embed';
import {StickerItem} from '@app/api/models/StickerItem';
import type {MessageTypeValue} from '@fluxer/constants/src/ChannelConstants';

export class MessageSnapshot {
	readonly content: string | null;
	readonly timestamp: Date;
	readonly editedTimestamp: Date | null;
	readonly mentionedUserIds: Set<UserID>;
	readonly mentionedRoleIds: Set<RoleID>;
	readonly mentionedChannelIds: Set<ChannelID>;
	readonly attachments: Array<Attachment>;
	readonly embeds: Array<Embed>;
	readonly stickers: Array<StickerItem>;
	readonly type: MessageTypeValue;
	readonly flags: number;

	constructor(snapshot: CassandraMessageSnapshot) {
		this.content = snapshot.content ?? null;
		this.timestamp = new Date(snapshot.timestamp);
		this.editedTimestamp = snapshot.edited_timestamp ? new Date(snapshot.edited_timestamp) : null;
		this.mentionedUserIds = snapshot.mention_users ?? new Set();
		this.mentionedRoleIds = snapshot.mention_roles ?? new Set();
		this.mentionedChannelIds = snapshot.mention_channels ?? new Set();
		this.attachments = (snapshot.attachments ?? []).map((att) => new Attachment(att));
		this.embeds = (snapshot.embeds ?? []).map((embed) => new Embed(embed));
		this.stickers = (snapshot.sticker_items ?? []).map((sticker) => new StickerItem(sticker));
		this.type = snapshot.type as MessageTypeValue;
		this.flags = snapshot.flags;
	}

	toMessageSnapshot(): CassandraMessageSnapshot {
		return {
			content: this.content,
			timestamp: this.timestamp,
			edited_timestamp: this.editedTimestamp,
			mention_users: this.mentionedUserIds.size > 0 ? this.mentionedUserIds : null,
			mention_roles: this.mentionedRoleIds.size > 0 ? this.mentionedRoleIds : null,
			mention_channels: this.mentionedChannelIds.size > 0 ? this.mentionedChannelIds : null,
			attachments: this.attachments.length > 0 ? this.attachments.map((att) => att.toMessageAttachment()) : null,
			embeds: this.embeds.length > 0 ? this.embeds.map((embed) => embed.toMessageEmbed()) : null,
			sticker_items: this.stickers.length > 0 ? this.stickers.map((sticker) => sticker.toMessageStickerItem()) : null,
			type: this.type,
			flags: this.flags,
		};
	}
}
