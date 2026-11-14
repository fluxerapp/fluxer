// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/api/models/Channel';
import type {Guild} from '@app/api/models/Guild';
import type {GuildEmoji} from '@app/api/models/GuildEmoji';
import type {GuildSelfAssignableRoleSection} from '@app/api/models/GuildSelfAssignableRoleSection';
import type {GuildSticker} from '@app/api/models/GuildSticker';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {SelfAssignableRoleSectionAuditKey} from '@fluxer/constants/src/GuildConstants';

function serializeSelfAssignableRoleSectionsForAudit(
	sections: ReadonlyArray<GuildSelfAssignableRoleSection>,
): Record<SelfAssignableRoleSectionAuditKey, unknown> {
	return {
		self_assignable_role_ids: sections.flatMap((section) => section.roles.map((role) => role.roleId.toString())),
		self_assignable_role_descriptions: sections.flatMap((section) => section.roles.map((role) => role.description)),
		self_assignable_role_section_contents: sections.map((section) => section.content),
		self_assignable_role_section_max_roles: sections.map((section) => section.maxRoles ?? 0),
		self_assignable_required_role_ids: sections.flatMap((section) =>
			section.requiredRoleIds.map((roleId) => roleId.toString()),
		),
	};
}

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
		features: [...guild.features].sort(),
		verification_level: guild.verificationLevel,
		mfa_level: guild.mfaLevel,
		nsfw_level: guild.nsfwLevel,
		nsfw: guild.nsfw,
		content_warning_level: guild.contentWarningLevel,
		content_warning_text: guild.contentWarningText,
		explicit_content_filter: guild.explicitContentFilter,
		default_message_notifications: guild.defaultMessageNotifications,
		system_channel_id: guild.systemChannelId?.toString() ?? null,
		system_channel_flags: guild.systemChannelFlags,
		rules_channel_id: guild.rulesChannelId?.toString() ?? null,
		afk_channel_id: guild.afkChannelId?.toString() ?? null,
		afk_timeout: guild.afkTimeout,
		disabled_operations: guild.disabledOperations,
		member_count: guild.memberCount,
		message_history_cutoff: guild.messageHistoryCutoff ? guild.messageHistoryCutoff.toISOString() : null,
		...serializeSelfAssignableRoleSectionsForAudit(guild.selfAssignableRoleSections),
	};
}

export function serializeChannelForAudit(channel: Channel): Record<string, unknown> {
	return {
		channel_id: channel.id.toString(),
		type: channel.type,
		name: channel.name ?? null,
		topic: channel.topic ?? null,
		...(channel.type === ChannelTypes.GUILD_LINK ? {url: channel.url ?? null} : {}),
		parent_id: channel.parentId?.toString() ?? null,
		position: channel.position,
		nsfw: channel.nsfwOverride,
		content_warning_level: channel.contentWarningLevel,
		content_warning_text: channel.contentWarningText,
		rate_limit_per_user: channel.rateLimitPerUser,
		user_limit: channel.userLimit,
		voice_connection_limit: channel.voiceConnectionLimit,
		bitrate: channel.bitrate,
		rtc_region: channel.rtcRegion ?? null,
		rtc_p2p: channel.rtcP2p,
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
