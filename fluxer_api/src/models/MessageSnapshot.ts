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

import type {MessageSnapshot as CassandraMessageSnapshot} from '~/database/CassandraTypes';
import type {ChannelID, RoleID, UserID} from '../BrandedTypes';
import {Attachment} from './Attachment';
import {Embed} from './Embed';
import {StickerItem} from './StickerItem';

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
	readonly type: number;
	readonly flags: number;

	constructor(snapshot: CassandraMessageSnapshot) {
		this.content = snapshot.content ?? null;
		this.timestamp = snapshot.timestamp;
		this.editedTimestamp = snapshot.edited_timestamp ?? null;
		this.mentionedUserIds = snapshot.mention_users ?? new Set();
		this.mentionedRoleIds = snapshot.mention_roles ?? new Set();
		this.mentionedChannelIds = snapshot.mention_channels ?? new Set();
		this.attachments = (snapshot.attachments ?? []).map((att) => new Attachment(att));
		this.embeds = (snapshot.embeds ?? []).map((embed) => new Embed(embed));
		this.stickers = (snapshot.sticker_items ?? []).map((sticker) => new StickerItem(sticker));
		this.type = snapshot.type;
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
