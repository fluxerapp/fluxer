// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	GuildEmojiResponse,
	GuildEmojiWithUserResponse,
	GuildStickerResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildBanResponse, GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildPartialResponse, GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {z} from 'zod';
import {
	stripGuildBannerForFeatures,
	stripGuildIconForFeatures,
	stripGuildSplashForFeatures,
} from '../infrastructure/AssetEntitlementUtils';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Guild} from '../models/Guild';
import type {GuildBan} from '../models/GuildBan';
import type {GuildEmoji} from '../models/GuildEmoji';
import type {GuildMember} from '../models/GuildMember';
import type {GuildRole} from '../models/GuildRole';
import type {GuildSticker} from '../models/GuildSticker';
import {getCachedUserPartialResponse, getCachedUserPartialResponses} from '../user/UserCacheHelpers';
import {mapGuildFeatures} from './GuildFeatureUtils';

export function mapGuildToPartialResponse(guild: Guild): z.infer<typeof GuildPartialResponse> {
	const guildId = guild.id.toString();
	const iconHash = stripGuildIconForFeatures(guild.iconHash, guild.features);
	const bannerHash = stripGuildBannerForFeatures(guild.bannerHash, guild.features);
	const splashHash = stripGuildSplashForFeatures(guild.splashHash, guild.features);
	const embedSplashHash = stripGuildSplashForFeatures(guild.embedSplashHash, guild.features);
	return {
		id: guildId,
		name: guild.name,
		icon: iconHash,
		banner: bannerHash,
		banner_width: bannerHash ? guild.bannerWidth : null,
		banner_height: bannerHash ? guild.bannerHeight : null,
		splash: splashHash,
		splash_width: splashHash ? guild.splashWidth : null,
		splash_height: splashHash ? guild.splashHeight : null,
		embed_splash: embedSplashHash,
		embed_splash_width: embedSplashHash ? guild.embedSplashWidth : null,
		embed_splash_height: embedSplashHash ? guild.embedSplashHeight : null,
		splash_card_alignment: guild.splashCardAlignment,
		features: mapGuildFeatures(guild.features),
	};
}

export function mapGuildToGuildResponse(
	guild: Guild,
	options?: {
		permissions?: bigint | null;
	},
): z.infer<typeof GuildResponse> {
	const iconHash = stripGuildIconForFeatures(guild.iconHash, guild.features);
	const bannerHash = stripGuildBannerForFeatures(guild.bannerHash, guild.features);
	const splashHash = stripGuildSplashForFeatures(guild.splashHash, guild.features);
	const embedSplashHash = stripGuildSplashForFeatures(guild.embedSplashHash, guild.features);
	return {
		id: guild.id.toString(),
		name: guild.name,
		icon: iconHash,
		banner: bannerHash,
		banner_width: bannerHash ? guild.bannerWidth : null,
		banner_height: bannerHash ? guild.bannerHeight : null,
		splash: splashHash,
		splash_width: splashHash ? guild.splashWidth : null,
		splash_height: splashHash ? guild.splashHeight : null,
		embed_splash: embedSplashHash,
		embed_splash_width: embedSplashHash ? guild.embedSplashWidth : null,
		embed_splash_height: embedSplashHash ? guild.embedSplashHeight : null,
		splash_card_alignment: guild.splashCardAlignment,
		vanity_url_code: guild.vanityUrlCode,
		owner_id: guild.ownerId.toString(),
		system_channel_id: guild.systemChannelId ? guild.systemChannelId.toString() : null,
		system_channel_flags: guild.systemChannelFlags,
		rules_channel_id: guild.rulesChannelId ? guild.rulesChannelId.toString() : null,
		afk_channel_id: guild.afkChannelId ? guild.afkChannelId.toString() : null,
		afk_timeout: guild.afkTimeout,
		features: mapGuildFeatures(guild.features),
		verification_level: guild.verificationLevel,
		mfa_level: guild.mfaLevel,
		nsfw_level: guild.nsfwLevel,
		nsfw: guild.nsfw,
		content_warning_level: guild.contentWarningLevel as 0 | 1,
		content_warning_text: guild.contentWarningText,
		explicit_content_filter: guild.explicitContentFilter,
		default_message_notifications: guild.defaultMessageNotifications,
		disabled_operations: guild.disabledOperations,
		message_history_cutoff: guild.messageHistoryCutoff ? guild.messageHistoryCutoff.toISOString() : null,
		permissions: options?.permissions != null ? options.permissions.toString() : undefined,
	};
}

export function mapGuildRoleToResponse(role: GuildRole): z.infer<typeof GuildRoleResponse> {
	return {
		id: role.id.toString(),
		name: role.name,
		color: role.color,
		position: role.position,
		hoist_position: role.hoistPosition,
		permissions: role.permissions.toString(),
		hoist: role.isHoisted,
		mentionable: role.isMentionable,
	};
}

export function mapGuildEmojiToResponse(emoji: GuildEmoji): z.infer<typeof GuildEmojiResponse> {
	const id = emoji.id.toString();
	return {
		id,
		name: emoji.name,
		animated: emoji.isAnimated,
		nsfw: false,
	};
}

export function mapGuildStickerToResponse(sticker: GuildSticker): z.infer<typeof GuildStickerResponse> {
	const id = sticker.id.toString();
	return {
		id,
		name: sticker.name,
		description: sticker.description ?? '',
		tags: sticker.tags,
		animated: sticker.animated,
		nsfw: false,
	};
}

function mapMemberWithUser(
	member: GuildMember,
	userPartial: z.infer<typeof UserPartialResponse>,
): z.infer<typeof GuildMemberResponse> {
	const now = Date.now();
	const isTimedOut = member.communicationDisabledUntil != null && member.communicationDisabledUntil.getTime() > now;
	return {
		user: userPartial,
		nick: member.nickname,
		avatar: member.isPremiumSanitized ? null : member.avatarHash,
		banner: member.isPremiumSanitized ? null : member.bannerHash,
		accent_color: member.accentColor,
		roles: Array.from(member.roleIds).map((id) => id.toString()),
		joined_at: member.joinedAt.toISOString(),
		mute: isTimedOut ? true : member.isMute,
		deaf: member.isDeaf,
		communication_disabled_until: member.communicationDisabledUntil?.toISOString() ?? null,
		profile_flags: member.profileFlags || undefined,
		mention_flags: member.mentionFlags || undefined,
	};
}

export function isGuildMemberTimedOut(member?: z.infer<typeof GuildMemberResponse> | null): boolean {
	if (!member?.communication_disabled_until) {
		return false;
	}
	const timestamp = Date.parse(member.communication_disabled_until);
	return !Number.isNaN(timestamp) && timestamp > Date.now();
}

export async function mapGuildMemberToResponse(
	member: GuildMember,
	userCacheService: Pick<UserCacheService, 'getUserPartialResponse'>,
	requestCache: RequestCache,
): Promise<z.infer<typeof GuildMemberResponse>> {
	const userPartial = await getCachedUserPartialResponse({userId: member.userId, userCacheService, requestCache});
	return mapMemberWithUser(member, userPartial);
}

function mapEmojiWithUser(
	emoji: GuildEmoji,
	userPartial: z.infer<typeof UserPartialResponse>,
): z.infer<typeof GuildEmojiWithUserResponse> {
	const id = emoji.id.toString();
	return {
		id,
		name: emoji.name,
		animated: emoji.isAnimated,
		nsfw: false,
		user: userPartial,
	};
}

export async function mapGuildEmojisWithUsersToResponse(
	emojis: Array<GuildEmoji>,
	userCacheService: Pick<UserCacheService, 'getUserPartialResponses'>,
	requestCache: RequestCache,
): Promise<Array<z.infer<typeof GuildEmojiWithUserResponse>>> {
	const userIds = [...new Set(emojis.map((emoji) => emoji.creatorId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return emojis
		.filter((emoji) => userPartials.has(emoji.creatorId))
		.map((emoji) => mapEmojiWithUser(emoji, userPartials.get(emoji.creatorId)!));
}

function mapStickerWithUser(
	sticker: GuildSticker,
	userPartial: z.infer<typeof UserPartialResponse>,
): z.infer<typeof GuildStickerWithUserResponse> {
	return {
		id: sticker.id.toString(),
		name: sticker.name,
		description: sticker.description ?? '',
		tags: sticker.tags,
		animated: sticker.animated,
		nsfw: false,
		user: userPartial,
	};
}

export async function mapGuildStickersWithUsersToResponse(
	stickers: Array<GuildSticker>,
	userCacheService: Pick<UserCacheService, 'getUserPartialResponses'>,
	requestCache: RequestCache,
): Promise<Array<z.infer<typeof GuildStickerWithUserResponse>>> {
	const userIds = [...new Set(stickers.map((sticker) => sticker.creatorId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return stickers
		.filter((sticker) => userPartials.has(sticker.creatorId))
		.map((sticker) => mapStickerWithUser(sticker, userPartials.get(sticker.creatorId)!));
}

function mapBanWithUser(
	ban: GuildBan,
	userPartial: z.infer<typeof UserPartialResponse>,
): z.infer<typeof GuildBanResponse> {
	return {
		user: userPartial,
		reason: ban.reason,
		moderator_id: ban.moderatorId.toString(),
		banned_at: ban.bannedAt.toISOString(),
		expires_at: ban.expiresAt ? ban.expiresAt.toISOString() : null,
	};
}

export async function mapGuildBansToResponse(
	bans: Array<GuildBan>,
	userCacheService: Pick<UserCacheService, 'getUserPartialResponses'>,
	requestCache: RequestCache,
): Promise<Array<z.infer<typeof GuildBanResponse>>> {
	const userIds = [...new Set(bans.map((ban) => ban.userId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return bans
		.filter((ban) => userPartials.has(ban.userId))
		.map((ban) => mapBanWithUser(ban, userPartials.get(ban.userId)!));
}
