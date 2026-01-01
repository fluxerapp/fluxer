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

import {createGuildID, type UserID} from '~/BrandedTypes';
import {PUBLIC_USER_FLAGS, SuspiciousActivityFlags, UserFlags} from '~/Constants';
import type {
	BetaCode,
	GuildChannelOverride,
	GuildMember,
	MuteConfiguration,
	Relationship,
	User,
	UserGuildSettings,
	UserSettings,
} from '~/Models';
import {isUserAdult} from '~/utils/AgeUtils';
import type {
	BetaCodeResponse,
	RelationshipResponse,
	UserGuildSettingsResponse,
	UserPartialResponse,
	UserPrivateResponse,
	UserProfileResponse,
	UserSettingsResponse,
} from './UserTypes';

export const mapUserToPartialResponse = (user: User): UserPartialResponse => {
	const isBot = user.isBot;
	const isPremium = user.isPremium();

	let avatarHash = user.avatarHash;
	if (avatarHash?.startsWith('a_') && !isPremium && !isBot) {
		avatarHash = avatarHash.substring(2);
	}

	return {
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator.toString().padStart(4, '0'),
		global_name: user.globalName,
		avatar: avatarHash,
		avatar_color: user.avatarColor,
		bot: isBot || undefined,
		system: user.isSystem || undefined,
		flags: Number((user.flags ?? 0n) & PUBLIC_USER_FLAGS),
	};
};

export const hasPartialUserFieldsChanged = (oldUser: User, newUser: User): boolean => {
	const oldPartial = mapUserToPartialResponse(oldUser);
	const newPartial = mapUserToPartialResponse(newUser);

	return (
		oldPartial.username !== newPartial.username ||
		oldPartial.discriminator !== newPartial.discriminator ||
		oldPartial.global_name !== newPartial.global_name ||
		oldPartial.avatar !== newPartial.avatar ||
		oldPartial.avatar_color !== newPartial.avatar_color ||
		oldPartial.bot !== newPartial.bot ||
		oldPartial.system !== newPartial.system ||
		oldPartial.flags !== newPartial.flags
	);
};

export const mapUserToPrivateResponse = (user: User): UserPrivateResponse => {
	const isPremium = user.isPremium();

	let requiredActions: Array<string> | undefined;
	if (user.suspiciousActivityFlags != null && user.suspiciousActivityFlags > 0) {
		const actions: Array<string> = [];
		for (const [key, value] of Object.entries(SuspiciousActivityFlags)) {
			if (user.suspiciousActivityFlags & value) {
				actions.push(key);
			}
		}
		if (actions.length > 0) {
			requiredActions = actions;
		}
	}

	return {
		...mapUserToPartialResponse(user),
		acls: Array.from(user.acls),
		email: user.email ?? null,
		phone: user.phone ?? null,
		bio: user.bio,
		pronouns: user.pronouns,
		accent_color: user.accentColor,
		banner: isPremium ? user.bannerHash : null,
		banner_color: isPremium ? user.bannerColor : null,
		mfa_enabled: (user.authenticatorTypes?.size ?? 0) > 0,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : undefined,
		verified: user.emailVerified,
		premium_type: isPremium ? user.premiumType : 0,
		premium_since: isPremium ? (user.premiumSince?.toISOString() ?? null) : null,
		premium_until: user.premiumUntil?.toISOString() ?? null,
		premium_will_cancel: user.premiumWillCancel ?? false,
		premium_billing_cycle: user.premiumBillingCycle || null,
		premium_lifetime_sequence: user.premiumLifetimeSequence ?? null,
		premium_badge_hidden: !!(user.flags & UserFlags.PREMIUM_BADGE_HIDDEN),
		premium_badge_masked: !!(user.flags & UserFlags.PREMIUM_BADGE_MASKED),
		premium_badge_timestamp_hidden: !!(user.flags & UserFlags.PREMIUM_BADGE_TIMESTAMP_HIDDEN),
		premium_badge_sequence_hidden: !!(user.flags & UserFlags.PREMIUM_BADGE_SEQUENCE_HIDDEN),
		premium_purchase_disabled: !!(user.flags & UserFlags.PREMIUM_PURCHASE_DISABLED),
		premium_enabled_override: !!(user.flags & UserFlags.PREMIUM_ENABLED_OVERRIDE),
		password_last_changed_at: user.passwordLastChangedAt?.toISOString() ?? null,
		required_actions: requiredActions ?? null,
		nsfw_allowed: isUserAdult(user.dateOfBirth),
		pending_manual_verification: !!(user.flags & UserFlags.PENDING_MANUAL_VERIFICATION),
		has_dismissed_premium_onboarding:
			user.premiumSince != null &&
			user.premiumOnboardingDismissedAt != null &&
			user.premiumOnboardingDismissedAt >= user.premiumSince,
		has_ever_purchased: user.hasEverPurchased,
		has_unread_gift_inventory:
			user.giftInventoryServerSeq != null &&
			(user.giftInventoryClientSeq == null || user.giftInventoryClientSeq < user.giftInventoryServerSeq),
		unread_gift_inventory_count:
			user.giftInventoryServerSeq != null ? user.giftInventoryServerSeq - (user.giftInventoryClientSeq ?? 0) : 0,
		used_mobile_client: !!(user.flags & UserFlags.USED_MOBILE_CLIENT),
		pending_bulk_message_deletion:
			user.pendingBulkMessageDeletionAt != null
				? {
						scheduled_at: user.pendingBulkMessageDeletionAt.toISOString(),
						channel_count: user.pendingBulkMessageDeletionChannelCount ?? 0,
						message_count: user.pendingBulkMessageDeletionMessageCount ?? 0,
					}
				: null,
	};
};

export const mapUserToProfileResponse = (user: User): UserProfileResponse => ({
	bio: user.bio,
	pronouns: user.pronouns,
	banner: user.isPremium() ? user.bannerHash : null,
	banner_color: user.isPremium() ? user.bannerColor : null,
	accent_color: user.accentColor,
});

export const mapUserToOAuthResponse = (user: User, opts?: {includeEmail?: boolean}) => {
	const includeEmail = opts?.includeEmail && !!user.email;
	return {
		sub: user.id.toString(),
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator.toString().padStart(4, '0'),
		avatar: user.avatarHash,
		verified: user.emailVerified ?? false,
		email: includeEmail ? user.email : null,
		flags: Number((user.flags ?? 0n) & PUBLIC_USER_FLAGS),
		public_flags: Number((user.flags ?? 0n) & PUBLIC_USER_FLAGS),
		global_name: user.globalName ?? null,
		bot: user.isBot || false,
		system: user.isSystem || false,
		acls: Array.from(user.acls),
	};
};

export const mapGuildMemberToProfileResponse = (
	guildMember: GuildMember | null | undefined,
): UserProfileResponse | null => {
	if (!guildMember) return null;

	return {
		bio: guildMember.bio,
		pronouns: guildMember.pronouns,
		banner: guildMember.bannerHash,
		accent_color: guildMember.accentColor,
	};
};

export const mapUserSettingsToResponse = (params: {settings: UserSettings}): UserSettingsResponse => {
	const {settings} = params;
	return {
		status: settings.status,
		status_resets_at: settings.statusResetsAt?.toISOString() ?? null,
		status_resets_to: settings.statusResetsTo,
		theme: settings.theme,
		guild_positions: settings.guildPositions?.map(String) ?? [],
		locale: settings.locale,
		restricted_guilds: [...settings.restrictedGuilds].map(String),
		default_guilds_restricted: settings.defaultGuildsRestricted,
		inline_attachment_media: settings.inlineAttachmentMedia,
		inline_embed_media: settings.inlineEmbedMedia,
		gif_auto_play: settings.gifAutoPlay,
		render_embeds: settings.renderEmbeds,
		render_reactions: settings.renderReactions,
		animate_emoji: settings.animateEmoji,
		animate_stickers: settings.animateStickers,
		render_spoilers: settings.renderSpoilers,
		message_display_compact: settings.compactMessageDisplay,
		friend_source_flags: settings.friendSourceFlags,
		incoming_call_flags: settings.incomingCallFlags,
		group_dm_add_permission_flags: settings.groupDmAddPermissionFlags,
		guild_folders:
			settings.guildFolders?.map((folder) => ({
				id: folder.folderId,
				name: folder.name,
				color: folder.color,
				guild_ids: folder.guildIds.map(String),
			})) ?? [],
		custom_status: settings.customStatus
			? {
					text: settings.customStatus.text,
					expires_at: settings.customStatus.expiresAt?.toISOString(),
					emoji_id: settings.customStatus.emojiId?.toString(),
					emoji_name: settings.customStatus.emojiName,
					emoji_animated: settings.customStatus.emojiAnimated,
				}
			: null,
		afk_timeout: settings.afkTimeout,
		time_format: settings.timeFormat,
		developer_mode: settings.developerMode,
	};
};

export const mapRelationshipToResponse = async (params: {
	relationship: Relationship;
	userPartialResolver: (userId: UserID) => Promise<UserPartialResponse>;
}): Promise<RelationshipResponse> => {
	const {relationship, userPartialResolver} = params;
	const userPartial = await userPartialResolver(relationship.targetUserId);
	return {
		id: relationship.targetUserId.toString(),
		type: relationship.type,
		user: userPartial,
		since: relationship.since?.toISOString(),
		nickname: relationship.nickname,
	};
};

export const mapBetaCodeToResponse = async (params: {
	betaCode: BetaCode;
	userPartialResolver: (userId: UserID) => Promise<UserPartialResponse>;
}): Promise<BetaCodeResponse> => {
	const {betaCode, userPartialResolver} = params;
	return {
		code: betaCode.code,
		created_at: betaCode.createdAt.toISOString(),
		redeemed_at: betaCode.redeemedAt?.toISOString() || null,
		redeemer: betaCode.redeemerId ? await userPartialResolver(betaCode.redeemerId) : null,
	};
};

const mapMuteConfigToResponse = (
	muteConfig: MuteConfiguration | null,
): {end_time: string | null; selected_time_window: number} | null =>
	muteConfig
		? {
				end_time: muteConfig.endTime?.toISOString() ?? null,
				selected_time_window: muteConfig.selectedTimeWindow ?? 0,
			}
		: null;

const mapChannelOverrideToResponse = (
	override: GuildChannelOverride,
): {
	collapsed: boolean;
	message_notifications: number;
	muted: boolean;
	mute_config: {end_time: string | null; selected_time_window: number} | null;
} => ({
	collapsed: override.collapsed,
	message_notifications: override.messageNotifications ?? 0,
	muted: override.muted,
	mute_config: mapMuteConfigToResponse(override.muteConfig),
});

export const mapUserGuildSettingsToResponse = (settings: UserGuildSettings): UserGuildSettingsResponse => ({
	guild_id: settings.guildId === createGuildID(0n) ? null : settings.guildId.toString(),
	message_notifications: settings.messageNotifications ?? 0,
	muted: settings.muted,
	mute_config: mapMuteConfigToResponse(settings.muteConfig),
	mobile_push: settings.mobilePush,
	suppress_everyone: settings.suppressEveryone,
	suppress_roles: settings.suppressRoles,
	hide_muted_channels: settings.hideMutedChannels,
	channel_overrides: settings.channelOverrides.size
		? Object.fromEntries(
				Array.from(settings.channelOverrides.entries()).map(([channelId, override]) => [
					channelId.toString(),
					mapChannelOverrideToResponse(override),
				]),
			)
		: null,
	version: settings.version,
});
