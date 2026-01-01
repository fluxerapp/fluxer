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

import type {ChannelRow, PermissionOverwrite} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, MessageID, RoleID, UserID} from '../BrandedTypes';
import {ChannelPermissionOverwrite} from './ChannelPermissionOverwrite';

export class Channel {
	readonly id: ChannelID;
	readonly guildId: GuildID | null;
	readonly type: number;
	readonly name: string | null;
	readonly topic: string | null;
	readonly iconHash: string | null;
	readonly url: string | null;
	readonly parentId: ChannelID | null;
	readonly position: number;
	readonly ownerId: UserID | null;
	readonly recipientIds: Set<UserID>;
	readonly isNsfw: boolean;
	readonly rateLimitPerUser: number;
	readonly bitrate: number | null;
	readonly userLimit: number | null;
	readonly rtcRegion: string | null;
	readonly lastMessageId: MessageID | null;
	readonly lastPinTimestamp: Date | null;
	readonly permissionOverwrites: Map<RoleID | UserID, ChannelPermissionOverwrite>;
	readonly nicknames: Map<string, string>;
	readonly isSoftDeleted: boolean;
	readonly indexedAt: Date | null;
	readonly version: number;

	constructor(row: ChannelRow) {
		this.id = row.channel_id;
		this.guildId = row.guild_id ?? null;
		this.type = row.type;
		this.name = row.name ?? null;
		this.topic = row.topic ?? null;
		this.iconHash = row.icon_hash ?? null;
		this.url = row.url ?? null;
		this.parentId = row.parent_id ?? null;
		this.position = row.position ?? 0;
		this.ownerId = row.owner_id ?? null;
		this.recipientIds = row.recipient_ids ?? new Set();
		this.isNsfw = row.nsfw ?? false;
		this.rateLimitPerUser = row.rate_limit_per_user ?? 0;
		this.bitrate = row.bitrate ?? 0;
		this.userLimit = row.user_limit ?? 0;
		this.rtcRegion = row.rtc_region ?? null;
		this.lastMessageId = row.last_message_id ?? null;
		this.lastPinTimestamp = row.last_pin_timestamp ?? null;
		this.permissionOverwrites = new Map();
		if (row.permission_overwrites) {
			for (const [id, overwrite] of row.permission_overwrites) {
				this.permissionOverwrites.set(id, new ChannelPermissionOverwrite(overwrite));
			}
		}
		this.nicknames = row.nicks ?? new Map();
		this.isSoftDeleted = row.soft_deleted;
		this.indexedAt = row.indexed_at ?? null;
		this.version = row.version;
	}

	toRow(): ChannelRow {
		const permOverwritesMap: Map<UserID | RoleID, PermissionOverwrite> | null =
			this.permissionOverwrites.size > 0
				? new Map(
						Array.from(this.permissionOverwrites.entries()).map(([id, overwrite]) => [
							id,
							overwrite.toPermissionOverwrite(),
						]),
					)
				: null;

		return {
			channel_id: this.id,
			guild_id: this.guildId,
			type: this.type,
			name: this.name,
			topic: this.topic,
			icon_hash: this.iconHash,
			url: this.url,
			parent_id: this.parentId,
			position: this.position,
			owner_id: this.ownerId,
			recipient_ids: this.recipientIds.size > 0 ? this.recipientIds : null,
			nsfw: this.isNsfw,
			rate_limit_per_user: this.rateLimitPerUser,
			bitrate: this.bitrate,
			user_limit: this.userLimit,
			rtc_region: this.rtcRegion,
			last_message_id: this.lastMessageId,
			last_pin_timestamp: this.lastPinTimestamp,
			permission_overwrites: permOverwritesMap,
			nicks: this.nicknames.size > 0 ? this.nicknames : null,
			soft_deleted: this.isSoftDeleted,
			indexed_at: this.indexedAt,
			version: this.version,
		};
	}
}
