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

import type {UserSettingsRow} from '~/database/CassandraTypes';
import type {GuildID, UserID} from '../BrandedTypes';
import {Config} from '../Config';
import {
	FriendSourceFlags,
	GroupDmAddPermissionFlags,
	IncomingCallFlags,
	RenderSpoilers,
	StickerAnimationOptions,
	ThemeTypes,
	UserExplicitContentFilterTypes,
} from '../Constants';
import {UserCustomStatus} from './UserCustomStatus';
import {UserGuildFolder} from './UserGuildFolder';

export class UserSettings {
	readonly userId: UserID;
	readonly locale: string;
	readonly theme: string;
	readonly status: string;
	readonly statusResetsAt: Date | null;
	readonly statusResetsTo: string | null;
	readonly customStatus: UserCustomStatus | null;
	readonly developerMode: boolean;
	readonly compactMessageDisplay: boolean;
	readonly animateEmoji: boolean;
	readonly animateStickers: number;
	readonly gifAutoPlay: boolean;
	readonly renderEmbeds: boolean;
	readonly renderReactions: boolean;
	readonly renderSpoilers: number;
	readonly inlineAttachmentMedia: boolean;
	readonly inlineEmbedMedia: boolean;
	readonly explicitContentFilter: number;
	readonly friendSourceFlags: number;
	readonly incomingCallFlags: number;
	readonly groupDmAddPermissionFlags: number;
	readonly defaultGuildsRestricted: boolean;
	readonly restrictedGuilds: Set<GuildID>;
	readonly guildPositions: Array<GuildID>;
	readonly guildFolders: Array<UserGuildFolder>;
	readonly afkTimeout: number;
	readonly timeFormat: number;
	readonly version: number;

	constructor(row: UserSettingsRow) {
		this.userId = row.user_id;
		this.locale = row.locale;
		this.theme = row.theme;
		this.status = row.status;
		this.statusResetsAt = row.status_resets_at ?? null;
		this.statusResetsTo = row.status_resets_to ?? null;
		this.customStatus = row.custom_status ? new UserCustomStatus(row.custom_status) : null;
		this.developerMode = row.developer_mode ?? false;
		this.compactMessageDisplay = row.message_display_compact ?? false;
		this.animateEmoji = row.animate_emoji ?? false;
		this.animateStickers = row.animate_stickers ?? 0;
		this.gifAutoPlay = row.gif_auto_play ?? false;
		this.renderEmbeds = row.render_embeds ?? false;
		this.renderReactions = row.render_reactions ?? false;
		this.renderSpoilers = row.render_spoilers ?? 0;
		this.inlineAttachmentMedia = row.inline_attachment_media ?? false;
		this.inlineEmbedMedia = row.inline_embed_media ?? false;
		this.explicitContentFilter = row.explicit_content_filter ?? 0;
		this.friendSourceFlags = row.friend_source_flags ?? 0;
		this.incomingCallFlags = row.incoming_call_flags ?? 0;
		this.groupDmAddPermissionFlags = row.group_dm_add_permission_flags ?? 0;
		this.defaultGuildsRestricted = row.default_guilds_restricted ?? false;
		this.restrictedGuilds = row.restricted_guilds ?? new Set();
		this.guildPositions = row.guild_positions ?? [];
		this.guildFolders = (row.guild_folders ?? []).map((folder) => new UserGuildFolder(folder));
		this.afkTimeout = row.afk_timeout ?? 600;
		this.timeFormat = row.time_format ?? 0;
		this.version = row.version;
	}

	toRow(): UserSettingsRow {
		return {
			user_id: this.userId,
			locale: this.locale,
			theme: this.theme,
			status: this.status,
			status_resets_at: this.statusResetsAt,
			status_resets_to: this.statusResetsTo,
			custom_status: this.customStatus?.toCustomStatus() ?? null,
			developer_mode: this.developerMode,
			message_display_compact: this.compactMessageDisplay,
			animate_emoji: this.animateEmoji,
			animate_stickers: this.animateStickers,
			gif_auto_play: this.gifAutoPlay,
			render_embeds: this.renderEmbeds,
			render_reactions: this.renderReactions,
			render_spoilers: this.renderSpoilers,
			inline_attachment_media: this.inlineAttachmentMedia,
			inline_embed_media: this.inlineEmbedMedia,
			explicit_content_filter: this.explicitContentFilter,
			friend_source_flags: this.friendSourceFlags,
			incoming_call_flags: this.incomingCallFlags,
			group_dm_add_permission_flags: this.groupDmAddPermissionFlags,
			default_guilds_restricted: this.defaultGuildsRestricted,
			restricted_guilds: this.restrictedGuilds.size > 0 ? this.restrictedGuilds : null,
			guild_positions: this.guildPositions.length > 0 ? this.guildPositions : null,
			guild_folders: this.guildFolders.length > 0 ? this.guildFolders.map((folder) => folder.toGuildFolder()) : null,
			afk_timeout: this.afkTimeout,
			time_format: this.timeFormat,
			version: this.version,
		};
	}

	static getDefaultUserSettings({
		userId,
		locale,
		isAdult,
	}: {
		userId: UserID;
		locale: string;
		isAdult: boolean;
	}): UserSettingsRow {
		let explicitContentFilter: number = UserExplicitContentFilterTypes.NON_FRIENDS;
		if (isAdult) {
			explicitContentFilter = UserExplicitContentFilterTypes.FRIENDS_AND_NON_FRIENDS;
		}
		let friendSourceFlags: number = FriendSourceFlags.MUTUAL_FRIENDS | FriendSourceFlags.MUTUAL_GUILDS;
		if (isAdult) {
			friendSourceFlags |= FriendSourceFlags.NO_RELATION;
		}

		return {
			user_id: userId,
			locale,
			theme: ThemeTypes.SYSTEM,
			status: 'online',
			status_resets_at: null,
			status_resets_to: null,
			custom_status: null,
			developer_mode: Config.nodeEnv === 'development',
			message_display_compact: false,
			animate_emoji: true,
			animate_stickers: StickerAnimationOptions.ALWAYS_ANIMATE,
			gif_auto_play: true,
			render_embeds: true,
			render_reactions: true,
			render_spoilers: RenderSpoilers.ON_CLICK,
			inline_attachment_media: true,
			inline_embed_media: true,
			explicit_content_filter: explicitContentFilter,
			friend_source_flags: friendSourceFlags,
			incoming_call_flags: IncomingCallFlags.FRIENDS_ONLY,
			group_dm_add_permission_flags: GroupDmAddPermissionFlags.FRIENDS_ONLY,
			default_guilds_restricted: false,
			restricted_guilds: new Set(),
			guild_positions: [],
			guild_folders: [],
			afk_timeout: 600,
			time_format: 0,
			version: 1,
		};
	}
}
