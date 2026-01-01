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

import type {Channel, Guild, GuildEmoji, GuildSticker} from '~/Models';
import {toIdString, toSortedIdArray} from './IdUtils';

export function serializeGuildForAudit(guild: Guild): Record<string, unknown> {
	return {
		guild_id: guild.id.toString(),
		name: guild.name,
		owner_id: guild.ownerId.toString(),
		vanity_url_code: guild.vanityUrlCode ?? null,
		icon_hash: guild.iconHash ?? null,
		banner_hash: guild.bannerHash ?? null,
		banner_width: guild.bannerWidth ?? null,
		banner_height: guild.bannerHeight ?? null,
		splash_hash: guild.splashHash ?? null,
		splash_width: guild.splashWidth ?? null,
		splash_height: guild.splashHeight ?? null,
		splash_card_alignment: guild.splashCardAlignment,
		embed_splash_hash: guild.embedSplashHash ?? null,
		embed_splash_width: guild.embedSplashWidth ?? null,
		embed_splash_height: guild.embedSplashHeight ?? null,
		features: toSortedIdArray(guild.features),
		verification_level: guild.verificationLevel,
		mfa_level: guild.mfaLevel,
		nsfw_level: guild.nsfwLevel,
		explicit_content_filter: guild.explicitContentFilter,
		default_message_notifications: guild.defaultMessageNotifications,
		system_channel_id: toIdString(guild.systemChannelId),
		system_channel_flags: guild.systemChannelFlags,
		rules_channel_id: toIdString(guild.rulesChannelId),
		afk_channel_id: toIdString(guild.afkChannelId),
		afk_timeout: guild.afkTimeout,
		disabled_operations: guild.disabledOperations,
		member_count: guild.memberCount,
	};
}

export function serializeChannelForAudit(channel: Channel): Record<string, unknown> {
	return {
		channel_id: channel.id.toString(),
		type: channel.type,
		name: channel.name ?? null,
		topic: channel.topic ?? null,
		parent_id: toIdString(channel.parentId),
		position: channel.position,
		nsfw: channel.isNsfw,
		rate_limit_per_user: channel.rateLimitPerUser,
		user_limit: channel.userLimit,
		bitrate: channel.bitrate,
		rtc_region: channel.rtcRegion ?? null,
		permission_overwrite_count: channel.permissionOverwrites ? channel.permissionOverwrites.size : 0,
	};
}

export function serializeEmojiForAudit(emoji: GuildEmoji): Record<string, unknown> {
	return {
		emoji_id: emoji.id.toString(),
		name: emoji.name,
		animated: emoji.isAnimated,
		creator_id: emoji.creatorId.toString(),
	};
}

export function serializeStickerForAudit(sticker: GuildSticker): Record<string, unknown> {
	return {
		sticker_id: sticker.id.toString(),
		name: sticker.name,
		description: sticker.description,
		format_type: sticker.formatType,
		creator_id: sticker.creatorId.toString(),
	};
}
