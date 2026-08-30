// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelMessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
	type GuildFolderIcon,
	PremiumFlags,
	PUBLIC_USER_FLAGS,
	UNCATEGORIZED_FOLDER_ID,
	type UserAuthenticatorType,
	UserAuthenticatorTypes,
	UserFlags,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import type {
	RelationshipResponse,
	UserGuildSettingsResponse,
	UserPartialResponse,
	UserPrivateResponse,
	UserProfileResponse,
	UserSettingsResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createGuildID, type UserID} from '../BrandedTypes';
import {stripAvatarForUser, stripBannerForUser} from '../infrastructure/AssetEntitlementUtils';
import type {GuildChannelOverride} from '../models/GuildChannelOverride';
import type {GuildMember} from '../models/GuildMember';
import type {MuteConfiguration} from '../models/MuteConfiguration';
import type {Relationship} from '../models/Relationship';
import type {User} from '../models/User';
import type {UserGuildSettings} from '../models/UserGuildSettings';
import type {UserSettings} from '../models/UserSettings';
import {canUserAccessNsfwContent} from '../utils/AgeUtils';
import {canUseProfileTimezone, getRequiredActions} from './UserHelpers';

const PUBLIC_USER_FLAGS_WITHOUT_STAFF = PUBLIC_USER_FLAGS & ~UserFlags.STAFF;

function getVisiblePublicUserFlags(userFlags: bigint): bigint {
	return (userFlags & UserFlags.STAFF_HIDDEN) !== 0n ? PUBLIC_USER_FLAGS_WITHOUT_STAFF : PUBLIC_USER_FLAGS;
}

function mapUserFlagsToPublicBitfield(user: User): number {
	const flags = user.flags ?? 0n;
	return Number(flags & getVisiblePublicUserFlags(flags));
}

function getActiveAuthenticatorTypes(user: User): Array<UserAuthenticatorType> {
	return Array.from(user.authenticatorTypes ?? []).filter(
		(type): type is UserAuthenticatorType =>
			type === UserAuthenticatorTypes.TOTP || type === UserAuthenticatorTypes.WEBAUTHN,
	);
}

function sortUserIds(userIds: Iterable<UserID>): Array<string> {
	return Array.from(userIds)
		.sort((left, right) => {
			if (left < right) {
				return -1;
			}
			if (left > right) {
				return 1;
			}
			return 0;
		})
		.map((userId) => userId.toString());
}

export function mapUserToPartialResponse(user: User): UserPartialResponse {
	const isBot = user.isBot;
	const avatarHash = stripAvatarForUser(user);
	const isDeleted = (user.flags & UserFlags.DELETED) !== 0n && user.pendingDeletionAt === null && !user.isSystem;
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
			mention_flags: undefined,
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
		mention_flags: user.mentionFlags ? user.mentionFlags : undefined,
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
		oldPartial.flags !== newPartial.flags ||
		oldPartial.mention_flags !== newPartial.mention_flags
	);
}

export function mapUserToPrivateResponse(user: User): UserPrivateResponse {
	const isStaff = (user.flags & UserFlags.STAFF) !== 0n;
	const partialResponse = mapUserToPartialResponse(user);
	const isActuallyPremium = user.isPremium();
	const includeProfileTimezone = canUseProfileTimezone(user);
	const traitSet = new Set<string>();
	for (const trait of user.traits ?? []) {
		if (trait && trait !== 'premium') {
			traitSet.add(trait);
		}
	}
	if (isActuallyPremium) {
		traitSet.add('premium');
	}
	const requiredActions = [...getRequiredActions(user)];
	const traits = Array.from(traitSet).sort();
	const authenticatorTypes = getActiveAuthenticatorTypes(user);
	return {
		...partialResponse,
		flags: mapUserFlagsToPublicBitfield(user),
		is_staff: isStaff,
		acls: Array.from(user.acls),
		traits,
		email: user.email ?? null,
		email_bounced: user.emailBounced,
		phone: null,
		has_verified_phone: user.hasVerifiedPhone,
		bio: user.bio,
		pronouns: user.pronouns,
		accent_color: user.accentColor,
		...(includeProfileTimezone
			? {
					timezone: user.timezone,
					timezone_privacy_flags: user.timezonePrivacyFlags,
				}
			: {}),
		banner: stripBannerForUser(user),
		banner_color: user.bannerColor,
		mfa_enabled: authenticatorTypes.length > 0,
		authenticator_types: authenticatorTypes.length > 0 ? authenticatorTypes : undefined,
		verified: user.emailVerified,
		premium_type: isActuallyPremium ? (user.premiumType ?? UserPremiumTypes.NONE) : UserPremiumTypes.NONE,
		premium_since: isActuallyPremium ? (user.premiumSince?.toISOString() ?? null) : null,
		premium_until: user.effectivePremiumUntil?.toISOString() ?? null,
		premium_will_cancel: user.premiumWillCancel ?? false,
		premium_billing_cycle: user.premiumBillingCycle || null,
		premium_lifetime_sequence: user.premiumLifetimeSequence ?? null,
		premium_grace_ends_at: user.premiumGraceEndsAt?.toISOString() ?? null,
		premium_discriminator: !!(user.premiumFlags & PremiumFlags.DISCRIMINATOR),
		premium_badge_hidden: !!(user.premiumFlags & PremiumFlags.BADGE_HIDDEN),
		premium_badge_masked: !!(user.premiumFlags & PremiumFlags.BADGE_MASKED),
		premium_badge_timestamp_hidden: !!(user.premiumFlags & PremiumFlags.BADGE_TIMESTAMP_HIDDEN),
		premium_badge_sequence_hidden: !!(user.premiumFlags & PremiumFlags.BADGE_SEQUENCE_HIDDEN),
		premium_purchase_disabled: !!(user.premiumFlags & PremiumFlags.PURCHASE_DISABLED),
		premium_enabled_override: !!(user.premiumFlags & PremiumFlags.ENABLED_OVERRIDE),
		premium_perks_disabled: !!(user.premiumFlags & PremiumFlags.PERKS_DISABLED),
		password_last_changed_at: user.passwordLastChangedAt?.toISOString() ?? null,
		last_voice_activity_sharing_change_at: user.lastVoiceActivitySharingChangeAt?.toISOString() ?? null,
		required_actions: requiredActions,
		nsfw_allowed: canUserAccessNsfwContent(user),
		has_dismissed_premium_onboarding: isActuallyPremium && user.premiumOnboardingDismissedAt != null,
		has_ever_purchased: user.hasEverPurchased,
		has_unread_gift_inventory:
			user.giftInventoryServerSeq != null &&
			(user.giftInventoryClientSeq == null || user.giftInventoryClientSeq < user.giftInventoryServerSeq),
		unread_gift_inventory_count:
			user.giftInventoryServerSeq != null ? user.giftInventoryServerSeq - (user.giftInventoryClientSeq ?? 0) : 0,
		age_verified_adult: !!(user.flags & UserFlags.AGE_VERIFIED_ADULT) || undefined,
		terms_agreed_at: user.termsAgreedAt?.toISOString() ?? null,
		privacy_agreed_at: user.privacyAgreedAt?.toISOString() ?? null,
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

export function mapUserToProfileResponse(user: User, options?: {restrictProfile?: boolean}): UserProfileResponse {
	if (options?.restrictProfile) {
		return {
			bio: null,
			pronouns: null,
			banner: stripBannerForUser(user),
			banner_color: user.bannerColor,
			accent_color: user.accentColor,
		};
	}
	return {
		bio: user.bio,
		pronouns: user.pronouns,
		banner: stripBannerForUser(user),
		banner_color: user.bannerColor,
		accent_color: user.accentColor,
	};
}

export function mapUserToOAuthResponse(
	user: User,
	opts?: {
		includeEmail?: boolean;
	},
) {
	const includeEmail = opts?.includeEmail && !!user.email;
	return {
		sub: user.id.toString(),
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator.toString().padStart(4, '0'),
		avatar: stripAvatarForUser(user),
		verified: includeEmail ? (user.emailVerified ?? false) : undefined,
		email: includeEmail ? (user.email ?? null) : null,
		flags: mapUserFlagsToPublicBitfield(user),
		global_name: user.globalName ?? null,
		bot: user.isBot || false,
		system: user.isSystem || false,
		avatar_color: user.avatarColor,
	};
}

export function mapGuildMemberToProfileResponse(
	guildMember: GuildMember | null | undefined,
	options?: {restrictProfile?: boolean},
): UserProfileResponse | null {
	if (!guildMember) return null;
	if (options?.restrictProfile) {
		return {
			bio: null,
			pronouns: null,
			banner: guildMember.isPremiumSanitized ? null : guildMember.bannerHash,
			accent_color: guildMember.accentColor,
		};
	}
	return {
		bio: guildMember.bio,
		pronouns: guildMember.pronouns,
		banner: guildMember.isPremiumSanitized ? null : guildMember.bannerHash,
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

export function mapUserSettingsToResponse(params: {settings: UserSettings}): UserSettingsResponse {
	const {settings} = params;
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
		sensitive_content_friend_dm_filter: settings.sensitiveContentFriendDmFilter,
		sensitive_content_non_friend_dm_filter: settings.sensitiveContentNonFriendDmFilter,
		sensitive_content_guild_filter: settings.sensitiveContentGuildFilter as 0 | 1,
		suppress_unprivileged_self_mentions: settings.suppressUnprivilegedSelfMentions,
		suppress_unprivileged_self_mentions_bypass_user_ids: sortUserIds(
			settings.suppressUnprivilegedSelfMentionBypassUserIds,
		),
		staff_dm_access_user_ids: sortUserIds(settings.staffDmAccessUserIds),
		synced_preferences: settings.syncedPreferences,
		profile_privacy: settings.profilePrivacy,
		default_share_voice_activity: settings.defaultShareVoiceActivity,
	};
}

export async function mapRelationshipToResponse(params: {
	relationship: Relationship;
	userPartialResolver: (userId: UserID) => Promise<UserPartialResponse>;
	inverseRelationshipResolver?: (relationship: Relationship) => Promise<Relationship | null>;
}): Promise<RelationshipResponse> {
	const {relationship, userPartialResolver, inverseRelationshipResolver} = params;
	const [userPartial, inverse] = await Promise.all([
		userPartialResolver(relationship.targetUserId),
		inverseRelationshipResolver ? inverseRelationshipResolver(relationship) : Promise.resolve(null),
	]);
	return {
		id: relationship.targetUserId.toString(),
		type: relationship.type,
		user: userPartial,
		since: relationship.since?.toISOString(),
		nickname: relationship.nickname,
		share_voice_activity: relationship.shareVoiceActivity,
		friend_shares_voice_activity: inverse?.shareVoiceActivity ?? true,
	};
}

const mapMuteConfigToResponse = (
	muteConfig: MuteConfiguration | null,
): {
	end_time: string | null;
	selected_time_window: number;
} | null =>
	muteConfig
		? {
				end_time: muteConfig.endTime?.toISOString() ?? null,
				selected_time_window: muteConfig.selectedTimeWindow ?? 0,
			}
		: null;

function mapChannelOverrideToResponse(override: GuildChannelOverride): {
	collapsed: boolean;
	message_notifications: ChannelMessageNotifications;
	muted: boolean;
	mute_config: {
		end_time: string | null;
		selected_time_window: number;
	} | null;
	unread_badges: ChannelMessageNotifications | null;
} {
	return {
		collapsed: override.collapsed,
		message_notifications: override.messageNotifications ?? 0,
		muted: override.muted,
		mute_config: mapMuteConfigToResponse(override.muteConfig),
		unread_badges: override.unreadBadges ?? null,
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
		unread_badges: settings.unreadBadges ?? null,
		version: settings.version,
	};
}
