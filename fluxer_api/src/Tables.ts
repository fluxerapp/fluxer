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

import type {AttachmentID, ChannelID, GuildID, MemeID, UserID} from '~/BrandedTypes';
import {defineTable} from '~/database/Cassandra';
import {
	ADMIN_ARCHIVE_COLUMNS,
	ADMIN_AUDIT_LOG_COLUMNS,
	type AdminArchiveRow,
	type AdminAuditLogRow,
	APPLICATION_COLUMNS,
	type ApplicationByOwnerRow,
	type ApplicationRow,
	ATTACHMENT_LOOKUP_COLUMNS,
	type AttachmentLookupRow,
	AUTH_SESSION_COLUMNS,
	AUTHORIZED_IP_COLUMNS,
	type AuthorizedIpRow,
	type AuthSessionRow,
	BANNED_EMAIL_COLUMNS,
	BANNED_IP_COLUMNS,
	BANNED_PHONE_COLUMNS,
	type BannedEmailRow,
	type BannedIpRow,
	type BannedPhoneRow,
	BETA_CODE_BY_CODE_COLUMNS,
	BETA_CODE_COLUMNS,
	type BetaCodeByCodeRow,
	type BetaCodeRow,
	CHANNEL_COLUMNS,
	CHANNEL_EMPTY_BUCKET_COLUMNS,
	CHANNEL_MESSAGE_BUCKET_COLUMNS,
	CHANNEL_PIN_COLUMNS,
	CHANNEL_STATE_COLUMNS,
	CHANNELS_BY_GUILD_COLUMNS,
	type ChannelEmptyBucketRow,
	type ChannelMessageBucketRow,
	type ChannelPinRow,
	type ChannelRow,
	type ChannelStateRow,
	type ChannelsByGuildRow,
	DM_STATE_COLUMNS,
	type DmStateRow,
	DSA_REPORT_EMAIL_VERIFICATION_COLUMNS,
	DSA_REPORT_TICKET_COLUMNS,
	type DSAReportEmailVerificationRow,
	type DSAReportTicketRow,
	EMAIL_CHANGE_TICKET_COLUMNS,
	EMAIL_CHANGE_TOKEN_COLUMNS,
	EMAIL_REVERT_TOKEN_COLUMNS,
	EMAIL_VERIFICATION_TOKEN_COLUMNS,
	type EmailChangeTicketRow,
	type EmailChangeTokenRow,
	type EmailRevertTokenRow,
	type EmailVerificationTokenRow,
	EXPRESSION_PACK_COLUMNS,
	type ExpressionPackRow,
	FAVORITE_MEME_COLUMNS,
	type FavoriteMemeRow,
	GIFT_CODE_BY_CREATOR_COLUMNS,
	GIFT_CODE_BY_PAYMENT_INTENT_COLUMNS,
	GIFT_CODE_BY_REDEEMER_COLUMNS,
	GIFT_CODE_COLUMNS,
	type GiftCodeByCreatorRow,
	type GiftCodeByPaymentIntentRow,
	type GiftCodeByRedeemerRow,
	type GiftCodeRow,
	GUILD_AUDIT_LOG_COLUMNS,
	GUILD_BAN_COLUMNS,
	GUILD_BY_OWNER_ID_COLUMNS,
	GUILD_COLUMNS,
	GUILD_EMOJI_BY_EMOJI_ID_COLUMNS,
	GUILD_EMOJI_COLUMNS,
	GUILD_MEMBER_BY_USER_ID_COLUMNS,
	GUILD_MEMBER_COLUMNS,
	GUILD_ROLE_COLUMNS,
	GUILD_STICKER_BY_STICKER_ID_COLUMNS,
	GUILD_STICKER_COLUMNS,
	type GuildAuditLogRow,
	type GuildBanRow,
	type GuildByOwnerIdRow,
	type GuildEmojiRow,
	type GuildMemberByUserIdRow,
	type GuildMemberRow,
	type GuildRoleRow,
	type GuildRow,
	type GuildStickerRow,
	IAR_SUBMISSION_COLUMNS,
	type IARSubmissionRow,
	INSTANCE_CONFIGURATION_COLUMNS,
	INVITE_COLUMNS,
	type InstanceConfigurationRow,
	type InviteRow,
	IP_AUTHORIZATION_TOKEN_COLUMNS,
	type IpAuthorizationTokenRow,
	MESSAGE_BY_AUTHOR_COLUMNS,
	MESSAGE_COLUMNS,
	MESSAGE_REACTION_COLUMNS,
	type MessageByAuthorRow,
	type MessageReactionRow,
	type MessageRow,
	MFA_BACKUP_CODE_COLUMNS,
	type MfaBackupCodeRow,
	NOTE_COLUMNS,
	type NoteRow,
	OAUTH2_ACCESS_TOKEN_COLUMNS,
	OAUTH2_AUTHORIZATION_CODE_COLUMNS,
	OAUTH2_REFRESH_TOKEN_COLUMNS,
	type OAuth2AccessTokenByUserRow,
	type OAuth2AccessTokenRow,
	type OAuth2AuthorizationCodeRow,
	type OAuth2RefreshTokenByUserRow,
	type OAuth2RefreshTokenRow,
	PASSWORD_RESET_TOKEN_COLUMNS,
	PAYMENT_BY_PAYMENT_INTENT_COLUMNS,
	PAYMENT_BY_SUBSCRIPTION_COLUMNS,
	PAYMENT_BY_USER_COLUMNS,
	PAYMENT_COLUMNS,
	type PackInstallationRow,
	type PasswordResetTokenRow,
	type PaymentByPaymentIntentRow,
	type PaymentBySubscriptionRow,
	type PaymentByUserRow,
	type PaymentRow,
	PENDING_VERIFICATION_COLUMNS,
	type PendingVerificationRow,
	PHONE_TOKEN_COLUMNS,
	type PhoneTokenRow,
	PRIVATE_CHANNEL_COLUMNS,
	type PrivateChannelRow,
	PUSH_SUBSCRIPTION_COLUMNS,
	type PushSubscriptionRow,
	RECENT_MENTION_COLUMNS,
	RELATIONSHIP_COLUMNS,
	type RecentMentionRow,
	type RelationshipRow,
	SAVED_MESSAGE_COLUMNS,
	type SavedMessageRow,
	SCHEDULED_MESSAGE_COLUMNS,
	type ScheduledMessageRow,
	USER_BY_EMAIL_COLUMNS,
	USER_BY_PHONE_COLUMNS,
	USER_BY_STRIPE_CUSTOMER_ID_COLUMNS,
	USER_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS,
	USER_BY_USERNAME_COLUMNS,
	USER_COLUMNS,
	USER_CONTACT_CHANGE_LOG_COLUMNS,
	USER_DM_HISTORY_COLUMNS,
	USER_GUILD_SETTINGS_COLUMNS,
	USER_HARVEST_COLUMNS,
	USER_SETTINGS_COLUMNS,
	USERS_PENDING_DELETION_COLUMNS,
	type UserByEmailRow,
	type UserByPhoneRow,
	type UserByStripeCustomerIdRow,
	type UserByStripeSubscriptionIdRow,
	type UserByUsernameRow,
	type UserContactChangeLogRow,
	type UserDmHistoryRow,
	type UserGuildSettingsRow,
	type UserHarvestRow,
	type UserRow,
	type UserSettingsRow,
	type UsersPendingDeletionRow,
	VISIONARY_SLOT_COLUMNS,
	type VisionarySlotRow,
	WEBAUTHN_CREDENTIAL_COLUMNS,
	WEBHOOK_COLUMNS,
	type WebAuthnCredentialRow,
	type WebhookRow,
} from '~/database/CassandraTypes';
import {ATTACHMENT_DECAY_COLUMNS, type AttachmentDecayRow} from '~/types/AttachmentDecayTypes';

export const Users = defineTable<UserRow, 'user_id'>({
	name: 'users',
	columns: USER_COLUMNS,
	primaryKey: ['user_id'],
});

export const UsersPendingDeletion = defineTable<
	UsersPendingDeletionRow,
	'deletion_date' | 'pending_deletion_at' | 'user_id'
>({
	name: 'users_pending_deletion',
	columns: USERS_PENDING_DELETION_COLUMNS,
	primaryKey: ['deletion_date', 'pending_deletion_at', 'user_id'],
});

export const UserDmHistory = defineTable<UserDmHistoryRow, 'user_id' | 'channel_id'>({
	name: 'user_dm_history',
	columns: USER_DM_HISTORY_COLUMNS,
	primaryKey: ['user_id', 'channel_id'],
});

export const UserByUsername = defineTable<UserByUsernameRow, 'username' | 'discriminator' | 'user_id'>({
	name: 'users_by_username',
	columns: USER_BY_USERNAME_COLUMNS,
	primaryKey: ['username', 'discriminator', 'user_id'],
});

export const UserByEmail = defineTable<UserByEmailRow, 'email_lower' | 'user_id'>({
	name: 'users_by_email',
	columns: USER_BY_EMAIL_COLUMNS,
	primaryKey: ['email_lower', 'user_id'],
});

export const UserByPhone = defineTable<UserByPhoneRow, 'phone' | 'user_id'>({
	name: 'users_by_phone',
	columns: USER_BY_PHONE_COLUMNS,
	primaryKey: ['phone', 'user_id'],
});

export const UserByStripeCustomerId = defineTable<UserByStripeCustomerIdRow, 'stripe_customer_id' | 'user_id'>({
	name: 'users_by_stripe_customer_id',
	columns: USER_BY_STRIPE_CUSTOMER_ID_COLUMNS,
	primaryKey: ['stripe_customer_id', 'user_id'],
});

export const UserByStripeSubscriptionId = defineTable<
	UserByStripeSubscriptionIdRow,
	'stripe_subscription_id' | 'user_id'
>({
	name: 'users_by_stripe_subscription_id',
	columns: USER_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS,
	primaryKey: ['stripe_subscription_id', 'user_id'],
});

export const UserSettings = defineTable<UserSettingsRow, 'user_id'>({
	name: 'user_settings',
	columns: USER_SETTINGS_COLUMNS,
	primaryKey: ['user_id'],
});

export const UserGuildSettings = defineTable<UserGuildSettingsRow, 'user_id' | 'guild_id'>({
	name: 'user_guild_settings',
	columns: USER_GUILD_SETTINGS_COLUMNS,
	primaryKey: ['user_id', 'guild_id'],
});

export const UserContactChangeLogs = defineTable<UserContactChangeLogRow, 'user_id' | 'event_id'>({
	name: 'user_contact_change_logs',
	columns: USER_CONTACT_CHANGE_LOG_COLUMNS,
	primaryKey: ['user_id', 'event_id'],
});

export const Notes = defineTable<NoteRow, 'source_user_id' | 'target_user_id'>({
	name: 'notes',
	columns: NOTE_COLUMNS,
	primaryKey: ['source_user_id', 'target_user_id'],
});

export const Relationships = defineTable<RelationshipRow, 'source_user_id' | 'target_user_id' | 'type'>({
	name: 'relationships',
	columns: RELATIONSHIP_COLUMNS,
	primaryKey: ['source_user_id', 'target_user_id', 'type'],
});

export const UserHarvests = defineTable<UserHarvestRow, 'user_id' | 'harvest_id'>({
	name: 'user_harvests',
	columns: USER_HARVEST_COLUMNS,
	primaryKey: ['user_id', 'harvest_id'],
});

export const Guilds = defineTable<GuildRow, 'guild_id'>({
	name: 'guilds',
	columns: GUILD_COLUMNS,
	primaryKey: ['guild_id'],
});

export const GuildBans = defineTable<GuildBanRow, 'guild_id' | 'user_id'>({
	name: 'guild_bans',
	columns: GUILD_BAN_COLUMNS,
	primaryKey: ['guild_id', 'user_id'],
});

export const GuildAuditLogs = defineTable<GuildAuditLogRow, 'guild_id' | 'log_id'>({
	name: 'guild_audit_logs_v2',
	columns: GUILD_AUDIT_LOG_COLUMNS,
	primaryKey: ['guild_id', 'log_id'],
});

export const GuildAuditLogsByUser = defineTable<GuildAuditLogRow, 'guild_id' | 'user_id' | 'log_id'>({
	name: 'guild_audit_logs_v2_by_user',
	columns: GUILD_AUDIT_LOG_COLUMNS,
	primaryKey: ['guild_id', 'user_id', 'log_id'],
});

export const GuildAuditLogsByAction = defineTable<GuildAuditLogRow, 'guild_id' | 'action_type' | 'log_id'>({
	name: 'guild_audit_logs_v2_by_action',
	columns: GUILD_AUDIT_LOG_COLUMNS,
	primaryKey: ['guild_id', 'action_type', 'log_id'],
});

export const GuildAuditLogsByUserAction = defineTable<
	GuildAuditLogRow,
	'guild_id' | 'user_id' | 'action_type' | 'log_id'
>({
	name: 'guild_audit_logs_v2_by_user_action',
	columns: GUILD_AUDIT_LOG_COLUMNS,
	primaryKey: ['guild_id', 'user_id', 'action_type', 'log_id'],
});

export const GuildMembersByUserId = defineTable<GuildMemberByUserIdRow, 'user_id' | 'guild_id'>({
	name: 'guild_members_by_user_id',
	columns: GUILD_MEMBER_BY_USER_ID_COLUMNS,
	primaryKey: ['user_id', 'guild_id'],
});

export const GuildsByOwnerId = defineTable<GuildByOwnerIdRow, 'owner_id' | 'guild_id'>({
	name: 'guilds_by_owner_id',
	columns: GUILD_BY_OWNER_ID_COLUMNS,
	primaryKey: ['owner_id', 'guild_id'],
});

export const GuildEmojis = defineTable<GuildEmojiRow, 'guild_id' | 'emoji_id'>({
	name: 'guild_emojis',
	columns: GUILD_EMOJI_COLUMNS,
	primaryKey: ['guild_id', 'emoji_id'],
});

export const GuildEmojisByEmojiId = defineTable<GuildEmojiRow, 'emoji_id'>({
	name: 'guild_emojis_by_emoji_id',
	columns: GUILD_EMOJI_BY_EMOJI_ID_COLUMNS,
	primaryKey: ['emoji_id'],
});

export const GuildStickers = defineTable<GuildStickerRow, 'guild_id' | 'sticker_id'>({
	name: 'guild_stickers',
	columns: GUILD_STICKER_COLUMNS,
	primaryKey: ['guild_id', 'sticker_id'],
});

export const GuildStickersByStickerId = defineTable<GuildStickerRow, 'sticker_id'>({
	name: 'guild_stickers_by_sticker_id',
	columns: GUILD_STICKER_BY_STICKER_ID_COLUMNS,
	primaryKey: ['sticker_id'],
});

export const GuildRoles = defineTable<GuildRoleRow, 'guild_id' | 'role_id'>({
	name: 'guild_roles',
	columns: GUILD_ROLE_COLUMNS,
	primaryKey: ['guild_id', 'role_id'],
});

export const GuildMembers = defineTable<GuildMemberRow, 'guild_id' | 'user_id'>({
	name: 'guild_members',
	columns: GUILD_MEMBER_COLUMNS,
	primaryKey: ['guild_id', 'user_id'],
});

export const Channels = defineTable<ChannelRow, 'channel_id' | 'soft_deleted'>({
	name: 'channels',
	columns: CHANNEL_COLUMNS,
	primaryKey: ['channel_id', 'soft_deleted'],
});

export const ChannelsByGuild = defineTable<ChannelsByGuildRow, 'guild_id' | 'channel_id'>({
	name: 'channels_by_guild_id',
	columns: CHANNELS_BY_GUILD_COLUMNS,
	primaryKey: ['guild_id', 'channel_id'],
});

export const ChannelState = defineTable<ChannelStateRow, 'channel_id'>({
	name: 'channel_state',
	columns: CHANNEL_STATE_COLUMNS,
	primaryKey: ['channel_id'],
});

export const ChannelPins = defineTable<ChannelPinRow, 'channel_id' | 'pinned_timestamp' | 'message_id'>({
	name: 'channel_pins',
	columns: CHANNEL_PIN_COLUMNS,
	primaryKey: ['channel_id', 'pinned_timestamp', 'message_id'],
});

export const ChannelMessageBuckets = defineTable<ChannelMessageBucketRow, 'channel_id' | 'bucket', 'channel_id'>({
	name: 'channel_message_buckets',
	columns: CHANNEL_MESSAGE_BUCKET_COLUMNS,
	primaryKey: ['channel_id', 'bucket'],
	partitionKey: ['channel_id'],
});

export const ChannelEmptyBuckets = defineTable<ChannelEmptyBucketRow, 'channel_id' | 'bucket', 'channel_id'>({
	name: 'channel_empty_buckets',
	columns: CHANNEL_EMPTY_BUCKET_COLUMNS,
	primaryKey: ['channel_id', 'bucket'],
	partitionKey: ['channel_id'],
});

export const PrivateChannels = defineTable<PrivateChannelRow, 'user_id' | 'channel_id'>({
	name: 'private_channels',
	columns: PRIVATE_CHANNEL_COLUMNS,
	primaryKey: ['user_id', 'channel_id'],
});

export const DmStates = defineTable<DmStateRow, 'hi_user_id' | 'lo_user_id' | 'channel_id'>({
	name: 'dm_states',
	columns: DM_STATE_COLUMNS,
	primaryKey: ['hi_user_id', 'lo_user_id', 'channel_id'],
});

interface PinnedDmRow {
	user_id: bigint;
	channel_id: bigint;
	sort_order: number;
}

const PINNED_DM_COLUMNS = ['user_id', 'channel_id', 'sort_order'] as const satisfies ReadonlyArray<keyof PinnedDmRow>;

export const PinnedDms = defineTable<PinnedDmRow, 'user_id' | 'channel_id'>({
	name: 'pinned_dms',
	columns: PINNED_DM_COLUMNS,
	primaryKey: ['user_id', 'channel_id'],
});

interface ReadStateRow {
	user_id: bigint;
	channel_id: bigint;
}

const READ_STATE_COLUMNS = ['user_id', 'channel_id'] as const satisfies ReadonlyArray<keyof ReadStateRow>;

export const ReadStates = defineTable<ReadStateRow, 'user_id' | 'channel_id'>({
	name: 'read_states',
	columns: READ_STATE_COLUMNS,
	primaryKey: ['user_id', 'channel_id'],
});

export const Messages = defineTable<MessageRow, 'channel_id' | 'bucket' | 'message_id'>({
	name: 'messages',
	columns: MESSAGE_COLUMNS,
	primaryKey: ['channel_id', 'bucket', 'message_id'],
});

export const MessagesByAuthor = defineTable<MessageByAuthorRow, 'author_id' | 'channel_id' | 'message_id'>({
	name: 'messages_by_author_id',
	columns: MESSAGE_BY_AUTHOR_COLUMNS,
	primaryKey: ['author_id', 'channel_id', 'message_id'],
});

export const MessageReactions = defineTable<
	MessageReactionRow,
	'channel_id' | 'bucket' | 'message_id' | 'emoji_id' | 'emoji_name' | 'user_id',
	'channel_id' | 'bucket' | 'message_id'
>({
	name: 'message_reactions',
	columns: MESSAGE_REACTION_COLUMNS,
	primaryKey: ['channel_id', 'bucket', 'message_id', 'emoji_id', 'emoji_name', 'user_id'],
	partitionKey: ['channel_id', 'bucket', 'message_id'],
});

export const AttachmentLookup = defineTable<AttachmentLookupRow, 'channel_id' | 'attachment_id' | 'filename'>({
	name: 'attachment_lookup',
	columns: ATTACHMENT_LOOKUP_COLUMNS,
	primaryKey: ['channel_id', 'attachment_id', 'filename'],
});

export const RecentMentions = defineTable<RecentMentionRow, 'user_id' | 'message_id'>({
	name: 'recent_mentions',
	columns: RECENT_MENTION_COLUMNS,
	primaryKey: ['user_id', 'message_id'],
});

interface RecentMentionsByGuildRow {
	user_id: bigint;
	guild_id: bigint;
	message_id: bigint;
	channel_id: bigint;
	is_everyone: boolean;
	is_role: boolean;
}

const RECENT_MENTIONS_BY_GUILD_COLUMNS = [
	'user_id',
	'guild_id',
	'message_id',
	'channel_id',
	'is_everyone',
	'is_role',
] as const satisfies ReadonlyArray<keyof RecentMentionsByGuildRow>;

export const RecentMentionsByGuild = defineTable<RecentMentionsByGuildRow, 'user_id' | 'guild_id' | 'message_id'>({
	name: 'recent_mentions_by_guild',
	columns: RECENT_MENTIONS_BY_GUILD_COLUMNS,
	primaryKey: ['user_id', 'guild_id', 'message_id'],
});

export const SavedMessages = defineTable<SavedMessageRow, 'user_id' | 'message_id'>({
	name: 'saved_messages',
	columns: SAVED_MESSAGE_COLUMNS,
	primaryKey: ['user_id', 'message_id'],
});

export const ScheduledMessages = defineTable<ScheduledMessageRow, 'user_id' | 'scheduled_message_id'>({
	name: 'scheduled_messages',
	columns: SCHEDULED_MESSAGE_COLUMNS,
	primaryKey: ['user_id', 'scheduled_message_id'],
});

export const PushSubscriptions = defineTable<PushSubscriptionRow, 'user_id' | 'subscription_id'>({
	name: 'push_subscriptions',
	columns: PUSH_SUBSCRIPTION_COLUMNS,
	primaryKey: ['user_id', 'subscription_id'],
});

export const Payments = defineTable<PaymentRow, 'checkout_session_id'>({
	name: 'payments',
	columns: PAYMENT_COLUMNS,
	primaryKey: ['checkout_session_id'],
});

export const PaymentsByPaymentIntent = defineTable<PaymentByPaymentIntentRow, 'payment_intent_id'>({
	name: 'payments_by_payment_intent',
	columns: PAYMENT_BY_PAYMENT_INTENT_COLUMNS,
	primaryKey: ['payment_intent_id'],
});

export const PaymentsBySubscription = defineTable<PaymentBySubscriptionRow, 'subscription_id'>({
	name: 'payments_by_subscription',
	columns: PAYMENT_BY_SUBSCRIPTION_COLUMNS,
	primaryKey: ['subscription_id'],
});

export const PaymentsByUser = defineTable<PaymentByUserRow, 'user_id' | 'created_at'>({
	name: 'payments_by_user',
	columns: PAYMENT_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'created_at'],
});

export const VisionarySlots = defineTable<VisionarySlotRow, 'slot_index'>({
	name: 'visionary_slots',
	columns: VISIONARY_SLOT_COLUMNS,
	primaryKey: ['slot_index'],
});

export const GiftCodes = defineTable<GiftCodeRow, 'code'>({
	name: 'gift_codes',
	columns: GIFT_CODE_COLUMNS,
	primaryKey: ['code'],
});

export const GiftCodesByCreator = defineTable<GiftCodeByCreatorRow, 'created_by_user_id' | 'code'>({
	name: 'gift_codes_by_creator',
	columns: GIFT_CODE_BY_CREATOR_COLUMNS,
	primaryKey: ['created_by_user_id', 'code'],
});

export const GiftCodesByPaymentIntent = defineTable<GiftCodeByPaymentIntentRow, 'stripe_payment_intent_id'>({
	name: 'gift_codes_by_payment_intent',
	columns: GIFT_CODE_BY_PAYMENT_INTENT_COLUMNS,
	primaryKey: ['stripe_payment_intent_id'],
});

export const GiftCodesByRedeemer = defineTable<GiftCodeByRedeemerRow, 'redeemed_by_user_id' | 'code'>({
	name: 'gift_codes_by_redeemer',
	columns: GIFT_CODE_BY_REDEEMER_COLUMNS,
	primaryKey: ['redeemed_by_user_id', 'code'],
});

export const BetaCodes = defineTable<BetaCodeRow, 'creator_id' | 'code'>({
	name: 'beta_codes',
	columns: BETA_CODE_COLUMNS,
	primaryKey: ['creator_id', 'code'],
});

export const BetaCodesByCode = defineTable<BetaCodeByCodeRow, 'code' | 'creator_id'>({
	name: 'beta_codes_by_code',
	columns: BETA_CODE_BY_CODE_COLUMNS,
	primaryKey: ['code', 'creator_id'],
});

export const AdminArchivesBySubject = defineTable<AdminArchiveRow, 'subject_type' | 'subject_id' | 'archive_id'>({
	name: 'admin_archives_by_subject',
	columns: ADMIN_ARCHIVE_COLUMNS,
	primaryKey: ['subject_type', 'subject_id', 'archive_id'],
});

export const AdminArchivesByRequester = defineTable<AdminArchiveRow, 'requested_by' | 'archive_id'>({
	name: 'admin_archives_by_requester',
	columns: ADMIN_ARCHIVE_COLUMNS,
	primaryKey: ['requested_by', 'archive_id'],
});

export const AdminArchivesByType = defineTable<AdminArchiveRow, 'subject_type' | 'archive_id'>({
	name: 'admin_archives_by_type',
	columns: ADMIN_ARCHIVE_COLUMNS,
	primaryKey: ['subject_type', 'archive_id'],
});

export const AdminAuditLogs = defineTable<AdminAuditLogRow, 'log_id'>({
	name: 'admin_audit_logs',
	columns: ADMIN_AUDIT_LOG_COLUMNS,
	primaryKey: ['log_id'],
});

export const BannedIps = defineTable<BannedIpRow, 'ip'>({
	name: 'banned_ips',
	columns: BANNED_IP_COLUMNS,
	primaryKey: ['ip'],
});

export const BannedEmails = defineTable<BannedEmailRow, 'email_lower'>({
	name: 'banned_emails',
	columns: BANNED_EMAIL_COLUMNS,
	primaryKey: ['email_lower'],
});

export const BannedPhones = defineTable<BannedPhoneRow, 'phone'>({
	name: 'banned_phones',
	columns: BANNED_PHONE_COLUMNS,
	primaryKey: ['phone'],
});

export const PendingVerifications = defineTable<PendingVerificationRow, 'user_id'>({
	name: 'pending_verifications',
	columns: PENDING_VERIFICATION_COLUMNS,
	primaryKey: ['user_id'],
});

export const IARSubmissions = defineTable<IARSubmissionRow, 'report_id'>({
	name: 'iar_submissions',
	columns: IAR_SUBMISSION_COLUMNS,
	primaryKey: ['report_id'],
});

export const DSAReportEmailVerifications = defineTable<DSAReportEmailVerificationRow, 'email_lower'>({
	name: 'dsa_report_email_verifications',
	columns: DSA_REPORT_EMAIL_VERIFICATION_COLUMNS,
	primaryKey: ['email_lower'],
});

export const DSAReportTickets = defineTable<DSAReportTicketRow, 'ticket'>({
	name: 'dsa_report_tickets',
	columns: DSA_REPORT_TICKET_COLUMNS,
	primaryKey: ['ticket'],
});

export const EmailVerificationTokens = defineTable<EmailVerificationTokenRow, 'token_' | 'user_id'>({
	name: 'email_verification_tokens',
	columns: EMAIL_VERIFICATION_TOKEN_COLUMNS,
	primaryKey: ['token_', 'user_id'],
});

export const PasswordResetTokens = defineTable<PasswordResetTokenRow, 'token_' | 'user_id'>({
	name: 'password_reset_tokens',
	columns: PASSWORD_RESET_TOKEN_COLUMNS,
	primaryKey: ['token_', 'user_id'],
});

export const EmailRevertTokens = defineTable<EmailRevertTokenRow, 'token_' | 'user_id'>({
	name: 'email_revert_tokens',
	columns: EMAIL_REVERT_TOKEN_COLUMNS,
	primaryKey: ['token_', 'user_id'],
});

export const PhoneTokens = defineTable<PhoneTokenRow, 'token_' | 'user_id'>({
	name: 'phone_tokens',
	columns: PHONE_TOKEN_COLUMNS,
	primaryKey: ['token_', 'user_id'],
});

export const AuthSessions = defineTable<AuthSessionRow, 'session_id_hash'>({
	name: 'auth_sessions',
	columns: AUTH_SESSION_COLUMNS,
	primaryKey: ['session_id_hash'],
});

export const AuthSessionsByUserId = defineTable<
	{user_id: UserID; session_id_hash: Buffer},
	'user_id' | 'session_id_hash'
>({
	name: 'auth_sessions_by_user_id',
	columns: ['user_id', 'session_id_hash'],
	primaryKey: ['user_id', 'session_id_hash'],
});

export const MfaBackupCodes = defineTable<MfaBackupCodeRow, 'user_id' | 'code'>({
	name: 'mfa_backup_codes',
	columns: MFA_BACKUP_CODE_COLUMNS,
	primaryKey: ['user_id', 'code'],
});

export const WebAuthnCredentials = defineTable<WebAuthnCredentialRow, 'user_id' | 'credential_id'>({
	name: 'webauthn_credentials',
	columns: WEBAUTHN_CREDENTIAL_COLUMNS,
	primaryKey: ['user_id', 'credential_id'],
});

export const WebAuthnCredentialLookup = defineTable<{credential_id: string; user_id: UserID}, 'credential_id'>({
	name: 'webauthn_credential_lookup',
	columns: ['credential_id', 'user_id'],
	primaryKey: ['credential_id'],
});

export const IpAuthorizationTokens = defineTable<IpAuthorizationTokenRow, 'token_' | 'user_id'>({
	name: 'ip_authorization_tokens',
	columns: IP_AUTHORIZATION_TOKEN_COLUMNS,
	primaryKey: ['token_', 'user_id'],
});

export const AuthorizedIps = defineTable<AuthorizedIpRow, 'user_id' | 'ip'>({
	name: 'authorized_ips_v2',
	columns: AUTHORIZED_IP_COLUMNS,
	primaryKey: ['user_id', 'ip'],
});

export const EmailChangeTickets = defineTable<EmailChangeTicketRow, 'ticket'>({
	name: 'email_change_tickets',
	columns: EMAIL_CHANGE_TICKET_COLUMNS,
	primaryKey: ['ticket'],
});

export const EmailChangeTokens = defineTable<EmailChangeTokenRow, 'token_'>({
	name: 'email_change_tokens',
	columns: EMAIL_CHANGE_TOKEN_COLUMNS,
	primaryKey: ['token_'],
});

export const PendingVerificationsByTime = defineTable<{created_at: Date; user_id: UserID}, 'created_at' | 'user_id'>({
	name: 'pending_verifications_by_time',
	columns: ['created_at', 'user_id'],
	primaryKey: ['created_at', 'user_id'],
});

interface AttachmentDecayByExpiryRow {
	expiry_bucket: number;
	expires_at: Date;
	attachment_id: AttachmentID;
	channel_id: ChannelID;
	message_id: bigint;
}

const ATTACHMENT_DECAY_BY_EXPIRY_COLUMNS = [
	'expiry_bucket',
	'expires_at',
	'attachment_id',
	'channel_id',
	'message_id',
] as const satisfies ReadonlyArray<keyof AttachmentDecayByExpiryRow>;

export const AttachmentDecayById = defineTable<AttachmentDecayRow, 'attachment_id'>({
	name: 'attachment_decay_by_id',
	columns: ATTACHMENT_DECAY_COLUMNS,
	primaryKey: ['attachment_id'],
});

export const AttachmentDecayByExpiry = defineTable<
	AttachmentDecayByExpiryRow,
	'expiry_bucket' | 'expires_at' | 'attachment_id'
>({
	name: 'attachment_decay_by_expiry',
	columns: ATTACHMENT_DECAY_BY_EXPIRY_COLUMNS,
	primaryKey: ['expiry_bucket', 'expires_at', 'attachment_id'],
});

interface FavoriteMemesByMemeIdRow {
	meme_id: MemeID;
	user_id: UserID;
}

const FAVORITE_MEMES_BY_MEME_ID_COLUMNS = ['meme_id', 'user_id'] as const satisfies ReadonlyArray<
	keyof FavoriteMemesByMemeIdRow
>;

export const FavoriteMemes = defineTable<FavoriteMemeRow, 'user_id' | 'meme_id'>({
	name: 'favorite_memes',
	columns: FAVORITE_MEME_COLUMNS,
	primaryKey: ['user_id', 'meme_id'],
});

export const FavoriteMemesByMemeId = defineTable<FavoriteMemesByMemeIdRow, 'meme_id' | 'user_id'>({
	name: 'favorite_memes_by_meme_id',
	columns: FAVORITE_MEMES_BY_MEME_ID_COLUMNS,
	primaryKey: ['meme_id', 'user_id'],
});

export const ExpressionPacks = defineTable<ExpressionPackRow, 'pack_id'>({
	name: 'expression_packs',
	columns: EXPRESSION_PACK_COLUMNS,
	primaryKey: ['pack_id'],
});

export const ExpressionPacksByCreator = defineTable<ExpressionPackRow, 'creator_id' | 'pack_id'>({
	name: 'expression_packs_by_creator',
	columns: EXPRESSION_PACK_COLUMNS,
	primaryKey: ['creator_id', 'pack_id'],
	partitionKey: ['creator_id'],
});

const PACK_INSTALLATION_COLUMNS = ['user_id', 'pack_id', 'pack_type', 'installed_at'] as const satisfies ReadonlyArray<
	keyof PackInstallationRow
>;

export const PackInstallations = defineTable<PackInstallationRow, 'user_id' | 'pack_id'>({
	name: 'pack_installations',
	columns: PACK_INSTALLATION_COLUMNS,
	primaryKey: ['user_id', 'pack_id'],
});

interface InvitesByChannelRow {
	channel_id: ChannelID;
	code: string;
}

interface InvitesByGuildRow {
	guild_id: GuildID;
	code: string;
}

const INVITES_BY_CHANNEL_COLUMNS = ['channel_id', 'code'] as const satisfies ReadonlyArray<keyof InvitesByChannelRow>;

const INVITES_BY_GUILD_COLUMNS = ['guild_id', 'code'] as const satisfies ReadonlyArray<keyof InvitesByGuildRow>;

export const Invites = defineTable<InviteRow, 'code'>({
	name: 'invites',
	columns: INVITE_COLUMNS,
	primaryKey: ['code'],
});

export const InvitesByChannel = defineTable<InvitesByChannelRow, 'channel_id' | 'code'>({
	name: 'invites_by_channel_id',
	columns: INVITES_BY_CHANNEL_COLUMNS,
	primaryKey: ['channel_id', 'code'],
});

export const InvitesByGuild = defineTable<InvitesByGuildRow, 'guild_id' | 'code'>({
	name: 'invites_by_guild_id',
	columns: INVITES_BY_GUILD_COLUMNS,
	primaryKey: ['guild_id', 'code'],
});

const APPLICATIONS_BY_OWNER_COLUMNS = ['owner_user_id', 'application_id'] as const satisfies ReadonlyArray<
	keyof ApplicationByOwnerRow
>;

export const Applications = defineTable<ApplicationRow, 'application_id'>({
	name: 'applications',
	columns: APPLICATION_COLUMNS,
	primaryKey: ['application_id'],
});

export const ApplicationsByOwner = defineTable<ApplicationByOwnerRow, 'owner_user_id' | 'application_id'>({
	name: 'applications_by_owner',
	columns: APPLICATIONS_BY_OWNER_COLUMNS,
	primaryKey: ['owner_user_id', 'application_id'],
});

const OAUTH2_ACCESS_TOKENS_BY_USER_COLUMNS = ['user_id', 'token_'] as const satisfies ReadonlyArray<
	keyof OAuth2AccessTokenByUserRow
>;

const OAUTH2_REFRESH_TOKENS_BY_USER_COLUMNS = ['user_id', 'token_'] as const satisfies ReadonlyArray<
	keyof OAuth2RefreshTokenByUserRow
>;

export const OAuth2AuthorizationCodes = defineTable<OAuth2AuthorizationCodeRow, 'code'>({
	name: 'oauth2_authorization_codes',
	columns: OAUTH2_AUTHORIZATION_CODE_COLUMNS,
	primaryKey: ['code'],
});

export const OAuth2AccessTokens = defineTable<OAuth2AccessTokenRow, 'token_'>({
	name: 'oauth2_access_tokens',
	columns: OAUTH2_ACCESS_TOKEN_COLUMNS,
	primaryKey: ['token_'],
});

export const OAuth2AccessTokensByUser = defineTable<OAuth2AccessTokenByUserRow, 'user_id' | 'token_'>({
	name: 'oauth2_access_tokens_by_user',
	columns: OAUTH2_ACCESS_TOKENS_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'token_'],
});

export const OAuth2RefreshTokens = defineTable<OAuth2RefreshTokenRow, 'token_'>({
	name: 'oauth2_refresh_tokens',
	columns: OAUTH2_REFRESH_TOKEN_COLUMNS,
	primaryKey: ['token_'],
});

export const OAuth2RefreshTokensByUser = defineTable<OAuth2RefreshTokenByUserRow, 'user_id' | 'token_'>({
	name: 'oauth2_refresh_tokens_by_user',
	columns: OAUTH2_REFRESH_TOKENS_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'token_'],
});

interface WebhooksByChannelRow {
	channel_id: ChannelID;
	webhook_id: bigint;
}

interface WebhooksByGuildRow {
	guild_id: GuildID;
	webhook_id: bigint;
}

const WEBHOOKS_BY_CHANNEL_COLUMNS = ['channel_id', 'webhook_id'] as const satisfies ReadonlyArray<
	keyof WebhooksByChannelRow
>;

const WEBHOOKS_BY_GUILD_COLUMNS = ['guild_id', 'webhook_id'] as const satisfies ReadonlyArray<keyof WebhooksByGuildRow>;

export const Webhooks = defineTable<WebhookRow, 'webhook_id' | 'webhook_token'>({
	name: 'webhooks',
	columns: WEBHOOK_COLUMNS,
	primaryKey: ['webhook_id', 'webhook_token'],
});

export const WebhooksByChannel = defineTable<WebhooksByChannelRow, 'channel_id' | 'webhook_id'>({
	name: 'webhooks_by_channel_id',
	columns: WEBHOOKS_BY_CHANNEL_COLUMNS,
	primaryKey: ['channel_id', 'webhook_id'],
});

export const WebhooksByGuild = defineTable<WebhooksByGuildRow, 'guild_id' | 'webhook_id'>({
	name: 'webhooks_by_guild_id',
	columns: WEBHOOKS_BY_GUILD_COLUMNS,
	primaryKey: ['guild_id', 'webhook_id'],
});

export const InstanceConfiguration = defineTable<InstanceConfigurationRow, 'key'>({
	name: 'instance_configuration',
	columns: INSTANCE_CONFIGURATION_COLUMNS,
	primaryKey: ['key'],
});
