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

import {createGuildID, type GuildID, type UserID} from '@fluxer/api/src/BrandedTypes';
import {getGlobalLimitConfigSnapshot} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {GuildChannelOverride} from '@fluxer/api/src/models/GuildChannelOverride';
import type {GuildMember} from '@fluxer/api/src/models/GuildMember';
import type {MuteConfiguration} from '@fluxer/api/src/models/MuteConfiguration';
import type {Relationship} from '@fluxer/api/src/models/Relationship';
import type {User} from '@fluxer/api/src/models/User';
import type {UserGuildSettings} from '@fluxer/api/src/models/UserGuildSettings';
import type {UserSettings} from '@fluxer/api/src/models/UserSettings';
import {isUserAdult} from '@fluxer/api/src/utils/AgeUtils';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
	type GuildFolderIcon,
	PUBLIC_USER_FLAGS,
	SuspiciousActivityFlags,
	UNCATEGORIZED_FOLDER_ID,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import type {
	RelationshipResponse,
	UserGuildSettingsResponse,
	UserPartialResponse,
	UserPrivateResponse,
	UserProfileResponse,
	UserSettingsResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const PUBLIC_USER_FLAGS_WITHOUT_STAFF = PUBLIC_USER_FLAGS & ~UserFlags.STAFF;

function getVisiblePublicUserFlags(userFlags: bigint): bigint {
	return (userFlags & UserFlags.STAFF_HIDDEN) !== 0n ? PUBLIC_USER_FLAGS_WITHOUT_STAFF : PUBLIC_USER_FLAGS;
}

function mapUserFlagsToPublicBitfield(user: User): number {
	const flags = user.flags ?? 0n;
	return Number(flags & getVisiblePublicUserFlags(flags));
}

export function mapUserToPartialResponse(user: User): UserPartialResponse {
	const isBot = user.isBot;

	let avatarHash = user.avatarHash;
	const snapshot = getGlobalLimitConfigSnapshot();

	if (avatarHash?.startsWith('a_')) {
		if (snapshot != null) {
			const ctx = createLimitMatchContext({user});
			const hasAnimatedAvatar = resolveLimitSafe(snapshot, ctx, 'feature_animated_avatar', 0);
			if (hasAnimatedAvatar === 0 && !isBot) {
				avatarHash = avatarHash.substring(2);
			}
		}
	}

	const isDeleted = (user.flags & UserFlags.DELETED) !== 0n && !user.isSystem;
	if (isDeleted) {
		return {
			id: user.id.toString(),
			username: DELETED_USER_USERNAME,
			discriminator: DELETED_USER_DISCRIMINATOR.toString().padStart(4, '0'),
			global_name: DELETED_USER_GLOBAL_NAME,
			avatar: null,
			avatar_color: null,
			bot: isBot || undefined,
			system: user.isSystem || undefined,
			flags: 0,
		};
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
		flags: mapUserFlagsToPublicBitfield(user),
	};
}

export function hasPartialUserFieldsChanged(oldUser: User, newUser: User): boolean {
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
}

export function mapUserToPrivateResponse(user: User): UserPrivateResponse {
	const snapshot = getGlobalLimitConfigSnapshot();
	const isStaff = (user.flags & UserFlags.STAFF) !== 0n;
	const partialResponse = mapUserToPartialResponse(user);

	const ctx = createLimitMatchContext({user});
	const hasAnimatedBanner = resolveLimitSafe(snapshot, ctx, 'feature_animated_banner', 0);
	const isActuallyPremium = user.isPremium();
	const traitSet = new Set<string>();
	for (const trait of user.traits ?? []) {
		if (trait && trait !== 'premium') {
			traitSet.add(trait);
		}
	}
	if (isActuallyPremium) {
		traitSet.add('premium');
	}

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

	const traits = Array.from(traitSet).sort();

	return {
		...partialResponse,
		flags: mapUserFlagsToPublicBitfield(user),
		is_staff: isStaff,
		acls: Array.from(user.acls),
		traits,
		email: user.email ?? null,
		email_bounced: user.emailBounced,
		phone: user.phone ?? null,
		bio: user.bio,
		pronouns: user.pronouns,
		accent_color: user.accentColor,
		banner: hasAnimatedBanner > 0 ? user.bannerHash : null,
		banner_color: hasAnimatedBanner > 0 ? user.bannerColor : null,
		mfa_enabled: (user.authenticatorTypes?.size ?? 0) > 0,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : undefined,
		verified: user.emailVerified,
		premium_type: isActuallyPremium ? user.premiumType : 0,
		premium_since: isActuallyPremium ? (user.premiumSince?.toISOString() ?? null) : null,
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
}

export function mapUserToProfileResponse(user: User): UserProfileResponse {
	const snapshot = getGlobalLimitConfigSnapshot();

	const ctx = createLimitMatchContext({user});
	const hasAnimatedBanner = resolveLimitSafe(snapshot, ctx, 'feature_animated_banner', 0);

	return {
		bio: user.bio,
		pronouns: user.pronouns,
		banner: hasAnimatedBanner > 0 ? user.bannerHash : null,
		banner_color: hasAnimatedBanner > 0 ? user.bannerColor : null,
		accent_color: user.accentColor,
	};
}

export function mapUserToOAuthResponse(user: User, opts?: {includeEmail?: boolean}) {
	const includeEmail = opts?.includeEmail && !!user.email;
	return {
		sub: user.id.toString(),
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator.toString().padStart(4, '0'),
		avatar: user.avatarHash,
		verified: includeEmail ? (user.emailVerified ?? false) : undefined,
		email: includeEmail ? (user.email ?? null) : null,
		flags: mapUserFlagsToPublicBitfield(user),
		global_name: user.globalName ?? null,
		bot: user.isBot || false,
		system: user.isSystem || false,
		acls: Array.from(user.acls),
		avatar_color: user.avatarColor,
	};
}

export function mapGuildMemberToProfileResponse(
	guildMember: GuildMember | null | undefined,
): UserProfileResponse | null {
	if (!guildMember) return null;

	return {
		bio: guildMember.bio,
		pronouns: guildMember.pronouns,
		banner: guildMember.bannerHash,
		accent_color: guildMember.accentColor,
	};
}

interface GuildFolderResponse {
	id: number;
	name: string | null;
	color: number | null;
	flags: number;
	icon: GuildFolderIcon;
	guild_ids: Array<string>;
}

function reconcileGuildFolders(
	folders: Array<GuildFolderResponse>,
	memberGuildIds: ReadonlyArray<GuildID>,
): Array<GuildFolderResponse> {
	const memberSet = new Set(memberGuildIds.map(String));
	const seenGuildIds = new Set<string>();

	const reconciledFolders = folders.map((folder) => ({
		...folder,
		guild_ids: folder.guild_ids.filter((guildId) => {
			if (!memberSet.has(guildId)) {
				return false;
			}
			if (seenGuildIds.has(guildId)) {
				return false;
			}
			seenGuildIds.add(guildId);
			return true;
		}),
	}));

	const missingGuildIds = memberGuildIds.filter((id) => !seenGuildIds.has(String(id))).map(String);

	if (missingGuildIds.length > 0) {
		const uncategorizedIndex = reconciledFolders.findIndex((f) => f.id === UNCATEGORIZED_FOLDER_ID);
		if (uncategorizedIndex === -1) {
			reconciledFolders.push({
				id: UNCATEGORIZED_FOLDER_ID,
				name: null,
				color: null,
				flags: 0,
				icon: DEFAULT_GUILD_FOLDER_ICON,
				guild_ids: missingGuildIds,
			});
		} else {
			reconciledFolders[uncategorizedIndex].guild_ids = [
				...reconciledFolders[uncategorizedIndex].guild_ids,
				...missingGuildIds,
			];
		}
	}

	return reconciledFolders.filter((f) => f.guild_ids.length > 0);
}

export function mapUserSettingsToResponse(params: {
	settings: UserSettings;
	memberGuildIds?: ReadonlyArray<GuildID>;
}): UserSettingsResponse {
	const {settings, memberGuildIds} = params;

	let guildFolders: Array<GuildFolderResponse>;

	if (settings.guildFolders != null && settings.guildFolders.length > 0) {
		guildFolders = settings.guildFolders.map((folder) => ({
			id: folder.folderId,
			name: folder.name,
			color: folder.color,
			flags: folder.flags,
			icon: folder.icon,
			guild_ids: folder.guildIds.map(String),
		}));
	} else if (settings.guildPositions != null && settings.guildPositions.length > 0) {
		guildFolders = [
			{
				id: UNCATEGORIZED_FOLDER_ID,
				name: null,
				color: null,
				flags: 0,
				icon: DEFAULT_GUILD_FOLDER_ICON,
				guild_ids: settings.guildPositions.map(String),
			},
		];
	} else {
		guildFolders = [];
	}

	if (memberGuildIds != null) {
		guildFolders = reconcileGuildFolders(guildFolders, memberGuildIds);
	}

	return {
		status: settings.status,
		status_resets_at: settings.statusResetsAt?.toISOString() ?? null,
		status_resets_to: settings.statusResetsTo,
		theme: settings.theme,
		locale: settings.locale,
		restricted_guilds: [...settings.restrictedGuilds].map(String),
		bot_restricted_guilds: [...settings.botRestrictedGuilds].map(String),
		default_guilds_restricted: settings.defaultGuildsRestricted,
		bot_default_guilds_restricted: settings.botDefaultGuildsRestricted,
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
		guild_folders: guildFolders,
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
		trusted_domains: [...settings.trustedDomains],
		default_hide_muted_channels: settings.defaultHideMutedChannels,
	};
}

export async function mapRelationshipToResponse(params: {
	relationship: Relationship;
	userPartialResolver: (userId: UserID) => Promise<UserPartialResponse>;
}): Promise<RelationshipResponse> {
	const {relationship, userPartialResolver} = params;
	const userPartial = await userPartialResolver(relationship.targetUserId);
	return {
		id: relationship.targetUserId.toString(),
		type: relationship.type,
		user: userPartial,
		since: relationship.since?.toISOString(),
		nickname: relationship.nickname,
	};
}

const mapMuteConfigToResponse = (
	muteConfig: MuteConfiguration | null,
): {end_time: string | null; selected_time_window: number} | null =>
	muteConfig
		? {
				end_time: muteConfig.endTime?.toISOString() ?? null,
				selected_time_window: muteConfig.selectedTimeWindow ?? 0,
			}
		: null;

function mapChannelOverrideToResponse(override: GuildChannelOverride): {
	collapsed: boolean;
	message_notifications: number;
	muted: boolean;
	mute_config: {end_time: string | null; selected_time_window: number} | null;
} {
	return {
		collapsed: override.collapsed,
		message_notifications: override.messageNotifications ?? 0,
		muted: override.muted,
		mute_config: mapMuteConfigToResponse(override.muteConfig),
	};
}

export function mapUserGuildSettingsToResponse(settings: UserGuildSettings): UserGuildSettingsResponse {
	return {
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
	};
}
