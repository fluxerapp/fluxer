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

import type {types} from 'cassandra-driver';
import type {AttachmentID, ChannelID, EmojiID, GuildID, MemeID, MessageID, UserID} from '~/BrandedTypes';

type Nullish<T> = T | null;

export interface UserRow {
	user_id: UserID;
	username: string;
	discriminator: number;
	global_name: Nullish<string>;
	bot: Nullish<boolean>;
	system: Nullish<boolean>;
	email: Nullish<string>;
	email_verified: Nullish<boolean>;
	email_bounced: Nullish<boolean>;
	phone: Nullish<string>;
	password_hash: Nullish<string>;
	password_last_changed_at: Nullish<Date>;
	totp_secret: Nullish<string>;
	authenticator_types: Nullish<Set<number>>;
	avatar_hash: Nullish<string>;
	avatar_color: Nullish<number>;
	banner_hash: Nullish<string>;
	banner_color: Nullish<number>;
	bio: Nullish<string>;
	pronouns: Nullish<string>;
	accent_color: Nullish<number>;
	date_of_birth: Nullish<types.LocalDate>;
	locale: Nullish<string>;
	flags: Nullish<bigint>;
	premium_type: Nullish<number>;
	premium_since: Nullish<Date>;
	premium_until: Nullish<Date>;
	premium_will_cancel: Nullish<boolean>;
	premium_billing_cycle: Nullish<string>;
	premium_lifetime_sequence: Nullish<number>;
	stripe_subscription_id: Nullish<string>;
	stripe_customer_id: Nullish<string>;
	has_ever_purchased: Nullish<boolean>;
	suspicious_activity_flags: Nullish<number>;
	terms_agreed_at: Nullish<Date>;
	privacy_agreed_at: Nullish<Date>;
	last_active_at: Nullish<Date>;
	last_active_ip: Nullish<string>;
	temp_banned_until: Nullish<Date>;
	pending_bulk_message_deletion_at: Nullish<Date>;
	pending_bulk_message_deletion_channel_count: Nullish<number>;
	pending_bulk_message_deletion_message_count: Nullish<number>;
	pending_deletion_at: Nullish<Date>;
	deletion_reason_code: Nullish<number>;
	deletion_public_reason: Nullish<string>;
	deletion_audit_log_reason: Nullish<string>;
	acls: Nullish<Set<string>>;
	first_refund_at: Nullish<Date>;
	beta_code_allowance: Nullish<number>;
	beta_code_last_reset_at: Nullish<Date>;
	gift_inventory_server_seq: Nullish<number>;
	gift_inventory_client_seq: Nullish<number>;
	premium_onboarding_dismissed_at: Nullish<Date>;
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
	'phone',
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
	'date_of_birth',
	'locale',
	'flags',
	'premium_type',
	'premium_since',
	'premium_until',
	'premium_will_cancel',
	'premium_billing_cycle',
	'premium_lifetime_sequence',
	'stripe_subscription_id',
	'stripe_customer_id',
	'has_ever_purchased',
	'suspicious_activity_flags',
	'terms_agreed_at',
	'privacy_agreed_at',
	'last_active_at',
	'last_active_ip',
	'temp_banned_until',
	'pending_bulk_message_deletion_at',
	'pending_bulk_message_deletion_channel_count',
	'pending_bulk_message_deletion_message_count',
	'pending_deletion_at',
	'deletion_reason_code',
	'deletion_public_reason',
	'deletion_audit_log_reason',
	'acls',
	'first_refund_at',
	'beta_code_allowance',
	'beta_code_last_reset_at',
	'gift_inventory_server_seq',
	'gift_inventory_client_seq',
	'premium_onboarding_dismissed_at',
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
	phone: null,
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
	date_of_birth: null,
	locale: null,
	flags: null,
	premium_type: null,
	premium_since: null,
	premium_until: null,
	premium_will_cancel: null,
	premium_billing_cycle: null,
	premium_lifetime_sequence: null,
	stripe_subscription_id: null,
	stripe_customer_id: null,
	has_ever_purchased: null,
	suspicious_activity_flags: null,
	terms_agreed_at: null,
	privacy_agreed_at: null,
	last_active_at: null,
	last_active_ip: null,
	temp_banned_until: null,
	pending_bulk_message_deletion_at: null,
	pending_bulk_message_deletion_channel_count: null,
	pending_bulk_message_deletion_message_count: null,
	pending_deletion_at: null,
	deletion_reason_code: null,
	deletion_public_reason: null,
	deletion_audit_log_reason: null,
	acls: null,
	first_refund_at: null,
	beta_code_allowance: null,
	beta_code_last_reset_at: null,
	gift_inventory_server_seq: null,
	gift_inventory_client_seq: null,
	premium_onboarding_dismissed_at: null,
	version: 1,
};

export interface CustomStatus {
	text: Nullish<string>;
	emoji_id: Nullish<EmojiID>;
	emoji_name: Nullish<string>;
	emoji_animated: boolean;
	expires_at: Nullish<Date>;
}

export interface GuildFolder {
	folder_id: number;
	name: Nullish<string>;
	color: Nullish<number>;
	guild_ids: Nullish<Array<GuildID>>;
}

export interface UserSettingsRow {
	user_id: UserID;
	locale: string;
	theme: string;
	status: string;
	status_resets_at: Nullish<Date>;
	status_resets_to: Nullish<string>;
	custom_status: Nullish<CustomStatus>;
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
	restricted_guilds: Nullish<Set<GuildID>>;
	guild_positions: Nullish<Array<GuildID>>;
	guild_folders: Nullish<Array<GuildFolder>>;
	afk_timeout: Nullish<number>;
	time_format: Nullish<number>;
	version: number;
}

export interface RelationshipRow {
	source_user_id: UserID;
	target_user_id: UserID;
	type: number;
	nickname: Nullish<string>;
	since: Nullish<Date>;
	version: number;
}

export interface NoteRow {
	source_user_id: UserID;
	target_user_id: UserID;
	note: string;
	version: number;
}

export interface MuteConfig {
	end_time: Nullish<Date>;
	selected_time_window: Nullish<number>;
}

export interface ChannelOverride {
	collapsed: boolean;
	message_notifications: Nullish<number>;
	muted: boolean;
	mute_config: Nullish<MuteConfig>;
}

export interface UserGuildSettingsRow {
	user_id: UserID;
	guild_id: GuildID;
	message_notifications: Nullish<number>;
	muted: boolean;
	mute_config: Nullish<MuteConfig>;
	mobile_push: boolean;
	suppress_everyone: boolean;
	suppress_roles: boolean;
	hide_muted_channels: boolean;
	channel_overrides: Nullish<Map<ChannelID, ChannelOverride>>;
	version: number;
}

export interface ExpressionPackRow {
	pack_id: GuildID;
	pack_type: string;
	creator_id: UserID;
	name: string;
	description: Nullish<string>;
	created_at: Date;
	updated_at: Date;
	version: number;
}

export interface PackInstallationRow {
	user_id: UserID;
	pack_id: GuildID;
	pack_type: string;
	installed_at: Date;
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

export interface ScheduledMessageRow {
	user_id: UserID;
	scheduled_message_id: MessageID;
	channel_id: ChannelID;
	payload: string;
	scheduled_at: Date;
	scheduled_local_at: string;
	timezone: string;
	status: string;
	status_reason: string | null;
	created_at: Date;
	invalidated_at: Date | null;
}

export const SCHEDULED_MESSAGE_COLUMNS = [
	'user_id',
	'scheduled_message_id',
	'channel_id',
	'payload',
	'scheduled_at',
	'scheduled_local_at',
	'timezone',
	'status',
	'status_reason',
	'created_at',
	'invalidated_at',
] as const satisfies ReadonlyArray<keyof ScheduledMessageRow>;

export interface FavoriteMemeRow {
	user_id: UserID;
	meme_id: MemeID;
	name: string;
	alt_text: Nullish<string>;
	tags: Nullish<Array<string>>;
	attachment_id: AttachmentID;
	filename: string;
	content_type: string;
	content_hash: Nullish<string>;
	size: bigint;
	width: Nullish<number>;
	height: Nullish<number>;
	duration: Nullish<number>;
	is_gifv: boolean;
	tenor_id_str: Nullish<string>;
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
	'tenor_id_str',
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
	started_at: Nullish<Date>;
	completed_at: Nullish<Date>;
	failed_at: Nullish<Date>;
	storage_key: Nullish<string>;
	file_size: Nullish<bigint>;
	progress_percent: number;
	progress_step: Nullish<string>;
	error_message: Nullish<string>;
	download_url_expires_at: Nullish<Date>;
}

export const USER_HARVEST_COLUMNS = [
	'user_id',
	'harvest_id',
	'requested_at',
	'started_at',
	'completed_at',
	'failed_at',
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
	endpoint: string;
	p256dh_key: string;
	auth_key: string;
	user_agent: Nullish<string>;
}

export const PUSH_SUBSCRIPTION_COLUMNS = [
	'user_id',
	'subscription_id',
	'endpoint',
	'p256dh_key',
	'auth_key',
	'user_agent',
] as const satisfies ReadonlyArray<keyof PushSubscriptionRow>;

export interface UserContactChangeLogRow {
	user_id: UserID;
	event_id: types.TimeUuid;
	field: string;
	old_value: Nullish<string>;
	new_value: Nullish<string>;
	reason: string;
	actor_user_id: Nullish<UserID>;
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
	'restricted_guilds',
	'guild_positions',
	'guild_folders',
	'afk_timeout',
	'time_format',
	'version',
] as const satisfies ReadonlyArray<keyof UserSettingsRow>;

export const EXPRESSION_PACK_COLUMNS = [
	'pack_id',
	'pack_type',
	'creator_id',
	'name',
	'description',
	'created_at',
	'updated_at',
	'version',
] as const satisfies ReadonlyArray<keyof ExpressionPackRow>;

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
	'version',
] as const satisfies ReadonlyArray<keyof UserGuildSettingsRow>;

export const RELATIONSHIP_COLUMNS = [
	'source_user_id',
	'target_user_id',
	'type',
	'nickname',
	'since',
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

export interface UserByPhoneRow {
	phone: string;
	user_id: UserID;
}

export interface UserByStripeCustomerIdRow {
	stripe_customer_id: string;
	user_id: UserID;
}

export interface UserByStripeSubscriptionIdRow {
	stripe_subscription_id: string;
	user_id: UserID;
}

export const USER_BY_USERNAME_COLUMNS = ['username', 'discriminator', 'user_id'] as const satisfies ReadonlyArray<
	keyof UserByUsernameRow
>;

export const USER_BY_EMAIL_COLUMNS = ['email_lower', 'user_id'] as const satisfies ReadonlyArray<keyof UserByEmailRow>;

export const USER_BY_PHONE_COLUMNS = ['phone', 'user_id'] as const satisfies ReadonlyArray<keyof UserByPhoneRow>;

export const USER_BY_STRIPE_CUSTOMER_ID_COLUMNS = ['stripe_customer_id', 'user_id'] as const satisfies ReadonlyArray<
	keyof UserByStripeCustomerIdRow
>;

export const USER_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS = [
	'stripe_subscription_id',
	'user_id',
] as const satisfies ReadonlyArray<keyof UserByStripeSubscriptionIdRow>;

export interface UsersPendingDeletionRow {
	deletion_date: string;
	pending_deletion_at: Date;
	user_id: UserID;
	deletion_reason_code: number;
}

export const USERS_PENDING_DELETION_COLUMNS = [
	'deletion_date',
	'pending_deletion_at',
	'user_id',
	'deletion_reason_code',
] as const satisfies ReadonlyArray<keyof UsersPendingDeletionRow>;

export interface UserDmHistoryRow {
	user_id: UserID;
	channel_id: ChannelID;
}

export const USER_DM_HISTORY_COLUMNS = ['user_id', 'channel_id'] as const satisfies ReadonlyArray<
	keyof UserDmHistoryRow
>;
