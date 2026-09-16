// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/api/models/Channel';
import type {Guild} from '@app/api/models/Guild';
import type {GuildEmoji} from '@app/api/models/GuildEmoji';
import type {GuildSticker} from '@app/api/models/GuildSticker';
import {toIdString, toSortedIdArray} from '@app/api/utils/IdUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

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
		nsfw: guild.nsfw,
		content_warning_level: guild.contentWarningLevel,
		content_warning_text: guild.contentWarningText,
		explicit_content_filter: guild.explicitContentFilter,
		default_message_notifications: guild.defaultMessageNotifications,
		system_channel_id: toIdString(guild.systemChannelId),
		system_channel_flags: guild.systemChannelFlags,
		rules_channel_id: toIdString(guild.rulesChannelId),
		afk_channel_id: toIdString(guild.afkChannelId),
		afk_timeout: guild.afkTimeout,
		disabled_operations: guild.disabledOperations,
		member_count: guild.memberCount,
		message_history_cutoff: guild.messageHistoryCutoff ? guild.messageHistoryCutoff.toISOString() : null,
	};
}

export function serializeChannelForAudit(channel: Channel): Record<string, unknown> {
	return {
		channel_id: channel.id.toString(),
		type: channel.type,
		name: channel.name ?? null,
		topic: channel.topic ?? null,
		...(channel.type === ChannelTypes.GUILD_LINK ? {url: channel.url ?? null} : {}),
		parent_id: toIdString(channel.parentId),
		position: channel.position,
		nsfw: channel.nsfwOverride,
		content_warning_level: channel.contentWarningLevel,
		content_warning_text: channel.contentWarningText,
		rate_limit_per_user: channel.rateLimitPerUser,
		user_limit: channel.userLimit,
		voice_connection_limit: channel.voiceConnectionLimit,
		bitrate: channel.bitrate,
		rtc_region: channel.rtcRegion ?? null,
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
		animated: sticker.animated,
		creator_id: sticker.creatorId.toString(),
	};
}
