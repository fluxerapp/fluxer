// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelType, ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT} from '@fluxer/constants/src/LimitConstants';
import type {ThreadState} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelID, GuildID, MessageID, RoleID, UserID} from '../BrandedTypes';
import type {ChannelRow, PermissionOverwrite} from '../database/types/ChannelTypes';
import {ChannelPermissionOverwrite} from './ChannelPermissionOverwrite';

export class Channel {
	readonly id: ChannelID;
	readonly guildId: GuildID | null;
	readonly type: ChannelType;
	readonly name: string | null;
	readonly topic: string | null;
	readonly iconHash: string | null;
	readonly url: string | null;
	readonly parentId: ChannelID | null;
	readonly position: number;
	readonly ownerId: UserID | null;
	readonly recipientIds: Set<UserID>;
	readonly isNsfw: boolean;
	readonly nsfwOverride: boolean | null;
	readonly contentWarningLevel: number;
	readonly contentWarningText: string | null;
	readonly rateLimitPerUser: number;
	readonly bitrate: number | null;
	readonly userLimit: number | null;
	readonly voiceConnectionLimit: number | null;
	readonly rtcRegion: string | null;
	readonly lastMessageId: MessageID | null;
	readonly lastPinTimestamp: Date | null;
	readonly permissionOverwrites: Map<RoleID | UserID, ChannelPermissionOverwrite>;
	readonly nicknames: Map<string, string>;
	readonly isSoftDeleted: boolean;
	readonly indexedAt: Date | null;
	readonly version: number;
	readonly threadParentChannelId: ChannelID | null;
	readonly threadCreatorUsername: string | null;
	readonly threadState: ThreadState | null;
	readonly threadExpiresAt: Date | null;
	readonly threadSourceMessageId: MessageID | null;
	readonly threadMessageCount: number;
	readonly threadTotalMessageSent: number;
	readonly threadMemberCountActual: number;
	readonly threadArchived: boolean;
	readonly threadLocked: boolean;
	readonly threadAutoArchiveDuration: number | null;
	readonly threadArchiveTimestamp: Date | null;

	get isThread(): boolean {
		return this.type === ChannelTypes.GUILD_THREAD;
	}

	constructor(row: ChannelRow) {
		this.id = row.channel_id;
		this.guildId = row.guild_id ?? null;
		this.type = row.type as ChannelType;
		this.name = row.name ?? null;
		this.topic = row.topic ?? null;
		this.iconHash = row.icon_hash ?? null;
		this.url = row.url ?? null;
		this.parentId = row.parent_id ?? null;
		this.position = row.position ?? 0;
		this.ownerId = row.owner_id ?? null;
		this.recipientIds = row.recipient_ids ?? new Set();
		this.isNsfw = row.nsfw ?? false;
		this.nsfwOverride = row.nsfw ?? null;
		this.contentWarningLevel = row.content_warning_level ?? 0;
		this.contentWarningText = row.content_warning_text ?? null;
		this.rateLimitPerUser = row.rate_limit_per_user ?? 0;
		this.bitrate = row.bitrate ?? 0;
		this.userLimit = row.user_limit ?? 0;
		this.voiceConnectionLimit =
			row.voice_connection_limit ??
			(this.type === ChannelTypes.GUILD_VOICE ? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT : null);
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
		this.threadParentChannelId = row.thread_parent_channel_id ?? null;
		this.threadCreatorUsername = row.thread_creator_username ?? null;
		this.threadState = (row.thread_state ?? null) as ThreadState | null;
		this.threadExpiresAt = row.thread_expires_at ?? null;
		this.threadSourceMessageId = row.thread_source_message_id ?? null;
		this.threadMessageCount = row.thread_message_count ?? 0;
		this.threadTotalMessageSent = row.thread_total_message_sent ?? 0;
		this.threadMemberCountActual = row.thread_member_count_actual ?? 0;
		this.threadArchived = row.thread_archived ?? false;
		this.threadLocked = row.thread_locked ?? false;
		this.threadAutoArchiveDuration = row.thread_auto_archive_duration ?? null;
		this.threadArchiveTimestamp = row.thread_archive_timestamp ?? null;
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
			nsfw: this.nsfwOverride,
			content_warning_level: this.contentWarningLevel,
			content_warning_text: this.contentWarningText,
			rate_limit_per_user: this.rateLimitPerUser,
			bitrate: this.bitrate,
			user_limit: this.userLimit,
			voice_connection_limit: this.voiceConnectionLimit,
			rtc_region: this.rtcRegion,
			last_message_id: this.lastMessageId,
			last_pin_timestamp: this.lastPinTimestamp,
			permission_overwrites: permOverwritesMap,
			nicks: this.nicknames.size > 0 ? this.nicknames : null,
			soft_deleted: this.isSoftDeleted,
			indexed_at: this.indexedAt,
			version: this.version,
			thread_parent_channel_id: this.threadParentChannelId,
			thread_creator_username: this.threadCreatorUsername,
			thread_state: this.threadState,
			thread_expires_at: this.threadExpiresAt,
			thread_source_message_id: this.threadSourceMessageId,
			thread_message_count: this.threadMessageCount,
			thread_total_message_sent: this.threadTotalMessageSent,
			thread_member_count_actual: this.threadMemberCountActual,
			thread_archived: this.threadArchived,
			thread_locked: this.threadLocked,
			thread_auto_archive_duration: this.threadAutoArchiveDuration,
			thread_archive_timestamp: this.threadArchiveTimestamp,
		};
	}
}
