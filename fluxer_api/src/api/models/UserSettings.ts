// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LocaleCode} from '@fluxer/constants/src/Locales';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	FriendSourceFlags,
	GroupDmAddPermissionFlags,
	IncomingCallFlags,
	type ProfilePrivacyLevel,
	ProfilePrivacyLevels,
	RenderSpoilers,
	type RenderSpoilersValue,
	SensitiveMediaFilterLevel,
	type SensitiveMediaFilterLevelValue,
	type StickerAnimationOption,
	StickerAnimationOptions,
	type ThemeType,
	ThemeTypes,
	type TimeFormatType,
	UNCATEGORIZED_FOLDER_ID,
	UserExplicitContentFilterTypes,
} from '@fluxer/constants/src/UserConstants';
import {decodeSyncedPreferences} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import type {GuildID, UserID} from '../BrandedTypes';
import {Config} from '../Config';
import type {UserSettingsRow} from '../database/types/UserTypes';
import {UserCustomStatus} from './UserCustomStatus';
import {UserGuildFolder} from './UserGuildFolder';

export class UserSettings {
	readonly userId: UserID;
	readonly locale: LocaleCode;
	readonly theme: string;
	readonly status: string;
	readonly statusResetsAt: Date | null;
	readonly statusResetsTo: string | null;
	readonly customStatus: UserCustomStatus | null;
	readonly developerMode: boolean;
	readonly compactMessageDisplay: boolean;
	readonly animateEmoji: boolean;
	readonly animateStickers: StickerAnimationOption;
	readonly gifAutoPlay: boolean;
	readonly renderEmbeds: boolean;
	readonly renderReactions: boolean;
	readonly renderSpoilers: RenderSpoilersValue;
	readonly inlineAttachmentMedia: boolean;
	readonly inlineEmbedMedia: boolean;
	readonly explicitContentFilter: number;
	readonly friendSourceFlags: number;
	readonly incomingCallFlags: number;
	readonly groupDmAddPermissionFlags: number;
	readonly defaultGuildsRestricted: boolean;
	readonly botDefaultGuildsRestricted: boolean;
	readonly restrictedGuilds: Set<GuildID>;
	readonly botRestrictedGuilds: Set<GuildID>;
	readonly guildPositions: Array<GuildID>;
	readonly guildFolders: Array<UserGuildFolder>;
	readonly afkTimeout: number;
	readonly timeFormat: TimeFormatType;
	readonly trustedDomains: Set<string>;
	readonly defaultHideMutedChannels: boolean;
	readonly sensitiveContentFriendDmFilter: SensitiveMediaFilterLevelValue;
	readonly sensitiveContentNonFriendDmFilter: SensitiveMediaFilterLevelValue;
	readonly sensitiveContentGuildFilter: SensitiveMediaFilterLevelValue;
	readonly suppressUnprivilegedSelfMentions: boolean;
	readonly suppressUnprivilegedSelfMentionBypassUserIds: Set<UserID>;
	readonly staffDmAccessUserIds: Set<UserID>;
	readonly syncedPreferences: string;
	readonly profilePrivacy: ProfilePrivacyLevel;
	readonly defaultShareVoiceActivity: boolean;
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
		this.animateStickers = (row.animate_stickers ?? 0) as StickerAnimationOption;
		this.gifAutoPlay = row.gif_auto_play ?? false;
		this.renderEmbeds = row.render_embeds ?? false;
		this.renderReactions = row.render_reactions ?? false;
		this.renderSpoilers = (row.render_spoilers ?? 0) as RenderSpoilersValue;
		this.inlineAttachmentMedia = row.inline_attachment_media ?? false;
		this.inlineEmbedMedia = row.inline_embed_media ?? false;
		this.explicitContentFilter = row.explicit_content_filter ?? 0;
		this.friendSourceFlags = row.friend_source_flags ?? 0;
		this.incomingCallFlags = row.incoming_call_flags ?? 0;
		this.groupDmAddPermissionFlags = row.group_dm_add_permission_flags ?? 0;
		this.defaultGuildsRestricted = row.default_guilds_restricted ?? false;
		this.botDefaultGuildsRestricted = row.bot_default_guilds_restricted ?? false;
		this.restrictedGuilds = row.restricted_guilds ?? new Set();
		this.botRestrictedGuilds = row.bot_restricted_guilds ?? new Set();
		this.guildPositions = row.guild_positions ?? [];
		this.guildFolders = (row.guild_folders ?? []).map((folder) => new UserGuildFolder(folder));
		this.afkTimeout = row.afk_timeout ?? 600;
		this.timeFormat = (row.time_format ?? 0) as TimeFormatType;
		this.trustedDomains = row.trusted_domains ?? new Set();
		this.defaultHideMutedChannels = row.default_hide_muted_channels ?? false;
		this.sensitiveContentFriendDmFilter = (row.sensitive_content_friend_dm_filter ??
			SensitiveMediaFilterLevel.SHOW) as SensitiveMediaFilterLevelValue;
		this.sensitiveContentNonFriendDmFilter = (row.sensitive_content_non_friend_dm_filter ??
			SensitiveMediaFilterLevel.SHOW) as SensitiveMediaFilterLevelValue;
		this.sensitiveContentGuildFilter = (row.sensitive_content_guild_filter ??
			SensitiveMediaFilterLevel.SHOW) as SensitiveMediaFilterLevelValue;
		this.suppressUnprivilegedSelfMentions = row.suppress_unprivileged_self_mentions ?? false;
		this.suppressUnprivilegedSelfMentionBypassUserIds =
			row.suppress_unprivileged_self_mentions_bypass_user_ids ?? new Set<UserID>();
		this.staffDmAccessUserIds = row.staff_dm_access_user_ids ?? new Set<UserID>();
		this.syncedPreferences = normalizeStoredSyncedPreferences(row.synced_preferences);
		this.profilePrivacy = (row.profile_privacy ?? ProfilePrivacyLevels.ALL_GUILDS) as ProfilePrivacyLevel;
		this.defaultShareVoiceActivity = row.default_share_voice_activity ?? true;
		this.version = row.version;
	}

	getUncategorizedFolder(): UserGuildFolder | null {
		return this.guildFolders.find((folder) => folder.folderId === UNCATEGORIZED_FOLDER_ID) ?? null;
	}

	getOrderedGuildIds(): Array<GuildID> {
		return this.guildFolders.flatMap((folder) => folder.guildIds);
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
			bot_default_guilds_restricted: this.botDefaultGuildsRestricted,
			restricted_guilds: this.restrictedGuilds.size > 0 ? this.restrictedGuilds : null,
			bot_restricted_guilds: this.botRestrictedGuilds.size > 0 ? this.botRestrictedGuilds : null,
			guild_positions: this.guildPositions.length > 0 ? this.guildPositions : null,
			guild_folders: this.guildFolders.length > 0 ? this.guildFolders.map((folder) => folder.toGuildFolder()) : null,
			afk_timeout: this.afkTimeout,
			time_format: this.timeFormat,
			trusted_domains: this.trustedDomains.size > 0 ? this.trustedDomains : null,
			default_hide_muted_channels: this.defaultHideMutedChannels,
			sensitive_content_friend_dm_filter: this.sensitiveContentFriendDmFilter,
			sensitive_content_non_friend_dm_filter: this.sensitiveContentNonFriendDmFilter,
			sensitive_content_guild_filter: this.sensitiveContentGuildFilter,
			suppress_unprivileged_self_mentions: this.suppressUnprivilegedSelfMentions,
			suppress_unprivileged_self_mentions_bypass_user_ids:
				this.suppressUnprivilegedSelfMentionBypassUserIds.size > 0
					? this.suppressUnprivilegedSelfMentionBypassUserIds
					: null,
			staff_dm_access_user_ids: this.staffDmAccessUserIds.size > 0 ? this.staffDmAccessUserIds : null,
			synced_preferences: this.syncedPreferences === '' ? null : this.syncedPreferences,
			profile_privacy: this.profilePrivacy,
			default_share_voice_activity: this.defaultShareVoiceActivity,
			version: this.version,
		};
	}

	static getDefaultUserSettings({
		userId,
		locale,
		isAdult,
		theme,
	}: {
		userId: UserID;
		locale: LocaleCode;
		isAdult: boolean;
		theme?: ThemeType | null;
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
			theme: theme ?? ThemeTypes.SYSTEM,
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
			bot_default_guilds_restricted: false,
			restricted_guilds: new Set(),
			bot_restricted_guilds: new Set(),
			guild_positions: [],
			guild_folders: [
				{
					folder_id: UNCATEGORIZED_FOLDER_ID,
					name: null,
					color: null,
					flags: 0,
					icon: DEFAULT_GUILD_FOLDER_ICON,
					guild_ids: [],
				},
			],
			afk_timeout: 600,
			time_format: 0,
			trusted_domains: new Set(),
			default_hide_muted_channels: false,
			sensitive_content_friend_dm_filter: isAdult ? SensitiveMediaFilterLevel.SHOW : SensitiveMediaFilterLevel.BLUR,
			sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.BLOCK,
			sensitive_content_guild_filter: SensitiveMediaFilterLevel.BLUR,
			suppress_unprivileged_self_mentions: false,
			suppress_unprivileged_self_mentions_bypass_user_ids: null,
			staff_dm_access_user_ids: null,
			synced_preferences: null,
			profile_privacy: ProfilePrivacyLevels.ALL_GUILDS,
			default_share_voice_activity: true,
			version: 1,
		};
	}
}

function normalizeStoredSyncedPreferences(raw: string | null | undefined): string {
	if (!raw) return '';
	try {
		decodeSyncedPreferences(raw);
	} catch {
		return raw;
	}
	return raw;
}
