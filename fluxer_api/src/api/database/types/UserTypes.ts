// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AttachmentID,
	ChannelID,
	EmojiID,
	EntranceSoundID,
	GuildID,
	MemeID,
	MessageID,
	UserID,
} from '@app/api/BrandedTypes';
import type {LocaleCode} from '@fluxer/constants/src/Locales';
import type {GuildFolderIcon, MentionReplyPreference} from '@fluxer/constants/src/UserConstants';
import type {types} from 'cassandra-driver';

export type PushSubscriptionPlatform =
	| 'web_push'
	| 'android_fcm'
	| 'ios_apns'
	| 'ios_apns_voip'
	| 'android_unified_push';

export interface UserRow {
	user_id: UserID;
	username: string;
	discriminator: number;
	global_name: string | null;
	bot: boolean | null;
	system: boolean | null;
	email: string | null;
	email_verified: boolean | null;
	email_bounced: boolean | null;
	password_hash: string | null;
	password_last_changed_at: Date | null;
	totp_secret: string | null;
	authenticator_types: Set<number> | null;
	avatar_hash: string | null;
	avatar_color: number | null;
	banner_hash: string | null;
	banner_color: number | null;
	bio: string | null;
	pronouns: string | null;
	accent_color: number | null;
	timezone: string | null;
	timezone_privacy_flags: number | null;
	date_of_birth: types.LocalDate | null;
	locale: string | null;
	flags: bigint | null;
	premium_flags?: number | null;
	premium_type: number | null;
	premium_since: Date | null;
	premium_until: Date | null;
	premium_gift_extension_ends_at: Date | null;
	premium_will_cancel: boolean | null;
	premium_billing_cycle: string | null;
	premium_lifetime_sequence: number | null;
	premium_grace_ends_at: Date | null;
	stripe_subscription_id: string | null;
	stripe_customer_id: string | null;
	has_ever_purchased: boolean | null;
	terms_agreed_at: Date | null;
	privacy_agreed_at: Date | null;
	last_active_at: Date | null;
	last_active_ip: string | null;
	temp_banned_until: Date | null;
	pending_bulk_message_deletion_at: Date | null;
	pending_bulk_message_deletion_channel_count: number | null;
	pending_bulk_message_deletion_message_count: number | null;
	pending_deletion_at: Date | null;
	deletion_started_at?: Date | null;
	deletion_reason_code: number | null;
	deletion_public_reason: string | null;
	deletion_audit_log_reason: string | null;
	deletion_scheduled_by?: UserID | null;
	deletion_scheduled_at?: Date | null;
	content_hidden_since?: Date | null;
	acls: Set<string> | null;
	traits: Set<string> | null;
	first_refund_at: Date | null;
	gift_inventory_server_seq: number | null;
	gift_inventory_client_seq: number | null;
	premium_onboarding_dismissed_at: Date | null;
	mention_flags?: MentionReplyPreference | null;
	last_voice_activity_sharing_change_at: Date | null;
	version: number;
}

export const USER_COLUMNS = [
	'user_id',
	'username',
	'discriminator',
	'global_name',
	'bot',
	'system',
	'email',
	'email_verified',
	'email_bounced',
	'password_hash',
	'password_last_changed_at',
	'totp_secret',
	'authenticator_types',
	'avatar_hash',
	'avatar_color',
	'banner_hash',
	'banner_color',
	'bio',
	'pronouns',
	'accent_color',
	'timezone',
	'timezone_privacy_flags',
	'date_of_birth',
	'locale',
	'flags',
	'premium_flags',
	'premium_type',
	'premium_since',
	'premium_until',
	'premium_gift_extension_ends_at',
	'premium_will_cancel',
	'premium_billing_cycle',
	'premium_lifetime_sequence',
	'premium_grace_ends_at',
	'stripe_subscription_id',
	'stripe_customer_id',
	'has_ever_purchased',
	'terms_agreed_at',
	'privacy_agreed_at',
	'last_active_at',
	'last_active_ip',
	'temp_banned_until',
	'pending_bulk_message_deletion_at',
	'pending_bulk_message_deletion_channel_count',
	'pending_bulk_message_deletion_message_count',
	'pending_deletion_at',
	'deletion_started_at',
	'deletion_reason_code',
	'deletion_public_reason',
	'deletion_audit_log_reason',
	'deletion_scheduled_by',
	'deletion_scheduled_at',
	'content_hidden_since',
	'acls',
	'traits',
	'first_refund_at',
	'gift_inventory_server_seq',
	'gift_inventory_client_seq',
	'premium_onboarding_dismissed_at',
	'mention_flags',
	'last_voice_activity_sharing_change_at',
	'version',
] as const satisfies ReadonlyArray<keyof UserRow>;
export const EMPTY_USER_ROW: UserRow = {
	user_id: -1n as UserID,
	username: '',
	discriminator: 0,
	global_name: null,
	bot: null,
	system: null,
	email: null,
	email_verified: null,
	email_bounced: null,
	password_hash: null,
	password_last_changed_at: null,
	totp_secret: null,
	authenticator_types: null,
	avatar_hash: null,
	avatar_color: null,
	banner_hash: null,
	banner_color: null,
	bio: null,
	pronouns: null,
	accent_color: null,
	timezone: null,
	timezone_privacy_flags: null,
	date_of_birth: null,
	locale: null,
	flags: null,
	premium_flags: null,
	premium_type: null,
	premium_since: null,
	premium_until: null,
	premium_gift_extension_ends_at: null,
	premium_will_cancel: null,
	premium_billing_cycle: null,
	premium_lifetime_sequence: null,
	premium_grace_ends_at: null,
	stripe_subscription_id: null,
	stripe_customer_id: null,
	has_ever_purchased: null,
	terms_agreed_at: null,
	privacy_agreed_at: null,
	last_active_at: null,
	last_active_ip: null,
	temp_banned_until: null,
	pending_bulk_message_deletion_at: null,
	pending_bulk_message_deletion_channel_count: null,
	pending_bulk_message_deletion_message_count: null,
	pending_deletion_at: null,
	deletion_started_at: null,
	deletion_reason_code: null,
	deletion_public_reason: null,
	deletion_audit_log_reason: null,
	deletion_scheduled_by: null,
	deletion_scheduled_at: null,
	content_hidden_since: null,
	acls: null,
	traits: null,
	first_refund_at: null,
	gift_inventory_server_seq: null,
	gift_inventory_client_seq: null,
	premium_onboarding_dismissed_at: null,
	mention_flags: null,
	last_voice_activity_sharing_change_at: null,
	version: 1,
};

export interface CustomStatus {
	text: string | null;
	emoji_id: EmojiID | null;
	emoji_name: string | null;
	emoji_animated: boolean;
	expires_at: Date | null;
}

export interface GuildFolder {
	folder_id: number;
	name: string | null;
	color: number | null;
	flags: number | null;
	icon: GuildFolderIcon | null;
	guild_ids: Array<GuildID> | null;
}

export interface UserSettingsRow {
	user_id: UserID;
	locale: LocaleCode;
	theme: string;
	status: string;
	status_resets_at: Date | null;
	status_resets_to: string | null;
	custom_status: CustomStatus | null;
	developer_mode: boolean;
	message_display_compact: boolean;
	animate_emoji: boolean;
	animate_stickers: number;
	gif_auto_play: boolean;
	render_embeds: boolean;
	render_reactions: boolean;
	render_spoilers: number;
	inline_attachment_media: boolean;
	inline_embed_media: boolean;
	explicit_content_filter: number;
	friend_source_flags: number;
	incoming_call_flags: number;
	group_dm_add_permission_flags: number;
	default_guilds_restricted: boolean;
	bot_default_guilds_restricted: boolean;
	restricted_guilds: Set<GuildID> | null;
	bot_restricted_guilds: Set<GuildID> | null;
	guild_positions: Array<GuildID> | null;
	guild_folders: Array<GuildFolder> | null;
	afk_timeout: number | null;
	time_format: number | null;
	trusted_domains: Set<string> | null;
	default_hide_muted_channels: boolean | null;
	sensitive_content_friend_dm_filter: number | null;
	sensitive_content_non_friend_dm_filter: number | null;
	sensitive_content_guild_filter: number | null;
	suppress_unprivileged_self_mentions: boolean | null;
	suppress_unprivileged_self_mentions_bypass_user_ids: Set<UserID> | null;
	staff_dm_access_user_ids: Set<UserID> | null;
	synced_preferences: string | null;
	profile_privacy: number | null;
	default_share_voice_activity: boolean | null;
	privacy_setup_version: number | null;
	privacy_setup_completed_at: Date | null;
	version: number;
}

export interface RelationshipRow {
	source_user_id: UserID;
	target_user_id: UserID;
	type: number;
	nickname: string | null;
	since: Date | null;
	share_voice_activity: boolean | null;
	version: number;
}

export interface NoteRow {
	source_user_id: UserID;
	target_user_id: UserID;
	note: string;
	version: number;
}

export interface MuteConfig {
	end_time: Date | null;
	selected_time_window: number | null;
}

export interface ChannelOverride {
	collapsed: boolean;
	message_notifications: number | null;
	muted: boolean;
	mute_config: MuteConfig | null;
	unread_badges: number | null;
	flags?: number | null;
}

export interface UserGuildSettingsRow {
	user_id: UserID;
	guild_id: GuildID;
	message_notifications: number | null;
	muted: boolean;
	mute_config: MuteConfig | null;
	mobile_push: boolean;
	suppress_everyone: boolean;
	suppress_roles: boolean;
	hide_muted_channels: boolean;
	channel_overrides: Map<ChannelID, ChannelOverride> | null;
	unread_badges: number | null;
	version: number;
}

export interface SavedMessageRow {
	user_id: UserID;
	channel_id: ChannelID;
	message_id: MessageID;
	saved_at: Date;
}

export const SAVED_MESSAGE_COLUMNS = [
	'user_id',
	'channel_id',
	'message_id',
	'saved_at',
] as const satisfies ReadonlyArray<keyof SavedMessageRow>;

export interface FavoriteMemeRow {
	user_id: UserID;
	meme_id: MemeID;
	name: string;
	alt_text: string | null;
	tags: Array<string> | null;
	attachment_id: AttachmentID;
	filename: string;
	content_type: string;
	content_hash: string | null;
	size: bigint;
	width: number | null;
	height: number | null;
	duration: number | null;
	is_gifv: boolean;
	klipy_slug: string | null;
	tenor_id_str: string | null;
	media_formats: string | null;
	placeholder: string | null;
	version: number;
}

export const FAVORITE_MEME_COLUMNS = [
	'user_id',
	'meme_id',
	'name',
	'alt_text',
	'tags',
	'attachment_id',
	'filename',
	'content_type',
	'content_hash',
	'size',
	'width',
	'height',
	'duration',
	'is_gifv',
	'klipy_slug',
	'tenor_id_str',
	'media_formats',
	'placeholder',
	'version',
] as const satisfies ReadonlyArray<keyof FavoriteMemeRow>;

export interface RecentMentionRow {
	user_id: UserID;
	channel_id: ChannelID;
	message_id: MessageID;
	guild_id: GuildID;
	is_everyone: boolean;
	is_role: boolean;
}

export const RECENT_MENTION_COLUMNS = [
	'user_id',
	'channel_id',
	'message_id',
	'guild_id',
	'is_everyone',
	'is_role',
] as const satisfies ReadonlyArray<keyof RecentMentionRow>;

export interface UserHarvestRow {
	user_id: UserID;
	harvest_id: bigint;
	requested_at: Date;
	attempt_id?: string | null;
	started_at: Date | null;
	completed_at: Date | null;
	failed_at: Date | null;
	terminal_failed_at?: Date | null;
	storage_key: string | null;
	file_size: bigint | null;
	progress_percent: number;
	progress_step: string | null;
	error_message: string | null;
	download_url_expires_at: Date | null;
}

export const USER_HARVEST_COLUMNS = [
	'user_id',
	'harvest_id',
	'requested_at',
	'attempt_id',
	'started_at',
	'completed_at',
	'failed_at',
	'terminal_failed_at',
	'storage_key',
	'file_size',
	'progress_percent',
	'progress_step',
	'error_message',
	'download_url_expires_at',
] as const satisfies ReadonlyArray<keyof UserHarvestRow>;

export interface PushSubscriptionRow {
	user_id: UserID;
	subscription_id: string;
	auth_session_id_hash: string | null;
	endpoint: string;
	p256dh_key: string | null;
	auth_key: string | null;
	user_agent: string | null;
	platform?: PushSubscriptionPlatform | null;
	app_id?: string | null;
	provider_environment?: string | null;
	thread_channels?: boolean | null;
}

export const PUSH_SUBSCRIPTION_COLUMNS = [
	'user_id',
	'subscription_id',
	'auth_session_id_hash',
	'endpoint',
	'p256dh_key',
	'auth_key',
	'user_agent',
	'platform',
	'app_id',
	'provider_environment',
	'thread_channels',
] as const satisfies ReadonlyArray<keyof PushSubscriptionRow>;

export interface UserContactChangeLogRow {
	user_id: UserID;
	event_id: types.TimeUuid;
	field: string;
	old_value: string | null;
	new_value: string | null;
	reason: string;
	actor_user_id: UserID | null;
	event_at: Date;
}

export const USER_SETTINGS_COLUMNS = [
	'user_id',
	'locale',
	'theme',
	'status',
	'status_resets_at',
	'status_resets_to',
	'custom_status',
	'developer_mode',
	'message_display_compact',
	'animate_emoji',
	'animate_stickers',
	'gif_auto_play',
	'render_embeds',
	'render_reactions',
	'render_spoilers',
	'inline_attachment_media',
	'inline_embed_media',
	'explicit_content_filter',
	'friend_source_flags',
	'incoming_call_flags',
	'group_dm_add_permission_flags',
	'default_guilds_restricted',
	'bot_default_guilds_restricted',
	'restricted_guilds',
	'bot_restricted_guilds',
	'guild_positions',
	'guild_folders',
	'afk_timeout',
	'time_format',
	'trusted_domains',
	'default_hide_muted_channels',
	'sensitive_content_friend_dm_filter',
	'sensitive_content_non_friend_dm_filter',
	'sensitive_content_guild_filter',
	'suppress_unprivileged_self_mentions',
	'suppress_unprivileged_self_mentions_bypass_user_ids',
	'staff_dm_access_user_ids',
	'synced_preferences',
	'profile_privacy',
	'default_share_voice_activity',
	'privacy_setup_version',
	'privacy_setup_completed_at',
	'version',
] as const satisfies ReadonlyArray<keyof UserSettingsRow>;
export const USER_GUILD_SETTINGS_COLUMNS = [
	'user_id',
	'guild_id',
	'message_notifications',
	'muted',
	'mute_config',
	'mobile_push',
	'suppress_everyone',
	'suppress_roles',
	'hide_muted_channels',
	'channel_overrides',
	'unread_badges',
	'version',
] as const satisfies ReadonlyArray<keyof UserGuildSettingsRow>;
export const RELATIONSHIP_COLUMNS = [
	'source_user_id',
	'target_user_id',
	'type',
	'nickname',
	'since',
	'share_voice_activity',
	'version',
] as const satisfies ReadonlyArray<keyof RelationshipRow>;
export const NOTE_COLUMNS = ['source_user_id', 'target_user_id', 'note', 'version'] as const satisfies ReadonlyArray<
	keyof NoteRow
>;
export const USER_CONTACT_CHANGE_LOG_COLUMNS = [
	'user_id',
	'event_id',
	'field',
	'old_value',
	'new_value',
	'reason',
	'actor_user_id',
	'event_at',
] as const satisfies ReadonlyArray<keyof UserContactChangeLogRow>;

export interface UserByUsernameRow {
	username: string;
	discriminator: number;
	user_id: UserID;
}

export interface UserByEmailRow {
	email_lower: string;
	user_id: UserID;
}

export interface UserEmailOwnerRow {
	email_lower: string;
	user_id: UserID | null;
	claimed_at: Date | null;
	claimed: boolean;
}

export interface UserByStripeCustomerIdRow {
	stripe_customer_id: string;
	user_id: UserID;
}

export interface UserByStripeSubscriptionIdRow {
	stripe_subscription_id: string;
	user_id: UserID;
}

export interface UserByLastActiveIpRow {
	last_active_ip: string;
	user_id: UserID;
	last_active_at: Date | null;
}

export interface UserByLastActiveIpTrustKeyRow {
	last_active_ip_trust_key: string;
	user_id: UserID;
	last_active_at: Date | null;
}

export const USER_BY_USERNAME_COLUMNS = ['username', 'discriminator', 'user_id'] as const satisfies ReadonlyArray<
	keyof UserByUsernameRow
>;
export const USER_BY_EMAIL_COLUMNS = ['email_lower', 'user_id'] as const satisfies ReadonlyArray<keyof UserByEmailRow>;
export const USER_EMAIL_OWNER_COLUMNS = [
	'email_lower',
	'user_id',
	'claimed_at',
	'claimed',
] as const satisfies ReadonlyArray<keyof UserEmailOwnerRow>;
export const USER_BY_STRIPE_CUSTOMER_ID_COLUMNS = ['stripe_customer_id', 'user_id'] as const satisfies ReadonlyArray<
	keyof UserByStripeCustomerIdRow
>;
export const USER_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS = [
	'stripe_subscription_id',
	'user_id',
] as const satisfies ReadonlyArray<keyof UserByStripeSubscriptionIdRow>;
export const USER_BY_LAST_ACTIVE_IP_COLUMNS = [
	'last_active_ip',
	'user_id',
	'last_active_at',
] as const satisfies ReadonlyArray<keyof UserByLastActiveIpRow>;
export const USER_BY_LAST_ACTIVE_IP_TRUST_KEY_COLUMNS = [
	'last_active_ip_trust_key',
	'user_id',
	'last_active_at',
] as const satisfies ReadonlyArray<keyof UserByLastActiveIpTrustKeyRow>;

export interface UserDmHistoryRow {
	user_id: UserID;
	channel_id: ChannelID;
}

export const USER_DM_HISTORY_COLUMNS = ['user_id', 'channel_id'] as const satisfies ReadonlyArray<
	keyof UserDmHistoryRow
>;

export interface UserEntranceSoundRow {
	user_id: UserID;
	sound_id: EntranceSoundID;
	name: string;
	hash: string;
	extension: string;
	content_type: string;
	duration_ms: number;
	size_bytes: number;
	created_at: Date;
	version: number;
}

export const USER_ENTRANCE_SOUND_COLUMNS = [
	'user_id',
	'sound_id',
	'name',
	'hash',
	'extension',
	'content_type',
	'duration_ms',
	'size_bytes',
	'created_at',
	'version',
] as const satisfies ReadonlyArray<keyof UserEntranceSoundRow>;

export interface UserEntranceSoundSelectionRow {
	user_id: UserID;
	scope_id: string;
	sound_id: EntranceSoundID;
}

export const USER_ENTRANCE_SOUND_SELECTION_COLUMNS = [
	'user_id',
	'scope_id',
	'sound_id',
] as const satisfies ReadonlyArray<keyof UserEntranceSoundSelectionRow>;
