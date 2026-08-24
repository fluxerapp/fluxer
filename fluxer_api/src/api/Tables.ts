// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, GuildID, MemeID, PasswordResetToken, UserID} from './BrandedTypes';
import {defineTable} from './database/CassandraTableDsl';
import {
	ADMIN_ARCHIVE_COLUMNS,
	ADMIN_AUDIT_LOG_COLUMNS,
	type AdminArchiveRow,
	type AdminAuditLogRow,
	BANNED_AVATAR_HASH_COLUMNS,
	BANNED_EMAIL_COLUMNS,
	BANNED_FILE_SHA_COLUMNS,
	BANNED_IP_COLUMNS,
	BANNED_PHONE_PREFIX_COLUMNS,
	BANNED_PHRASE_COLUMNS,
	BANNED_PROFILE_SUBSTRING_COLUMNS,
	BANNED_URL_COLUMNS,
	BANNED_URL_DOMAIN_COLUMNS,
	type BannedAvatarHashRow,
	type BannedEmailRow,
	type BannedFileShaRow,
	type BannedIpRow,
	type BannedPhonePrefixRow,
	type BannedPhraseRow,
	type BannedProfileSubstringRow,
	type BannedUrlDomainRow,
	type BannedUrlRow,
	DISPOSABLE_EMAIL_DOMAIN_COLUMNS,
	type DisposableEmailDomainRow,
	SUSPICIOUS_EMAIL_DOMAIN_COLUMNS,
	type SuspiciousEmailDomainRow,
} from './database/types/AdminArchiveTypes';
import {
	ADMIN_API_KEY_BY_CREATOR_COLUMNS,
	ADMIN_API_KEY_COLUMNS,
	type AdminApiKeyByCreatorRow,
	type AdminApiKeyRow,
} from './database/types/AdminAuthTypes';
import {
	ATTACHMENT_UPLOAD_TRACE_BY_ATTACHMENT_COLUMNS,
	ATTACHMENT_UPLOAD_TRACE_BY_KEY_COLUMNS,
	type AttachmentUploadTraceByAttachmentRow,
	type AttachmentUploadTraceByKeyRow,
} from './database/types/AttachmentUploadTypes';
import {
	AUTH_SESSION_COLUMNS,
	AUTH_SESSION_TOMBSTONE_COLUMNS,
	AUTHORIZED_IP_COLUMNS,
	AUTHORIZED_IP_TRUST_KEY_COLUMNS,
	type AuthorizedIpRow,
	type AuthorizedIpTrustKeyRow,
	type AuthSessionRow,
	type AuthSessionTombstoneRow,
	EMAIL_CHANGE_TICKET_COLUMNS,
	EMAIL_CHANGE_TOKEN_COLUMNS,
	EMAIL_REVERT_TOKEN_COLUMNS,
	EMAIL_VERIFICATION_TOKEN_COLUMNS,
	type EmailChangeTicketRow,
	type EmailChangeTokenRow,
	type EmailRevertTokenRow,
	type EmailVerificationTokenRow,
	IP_AUTHORIZATION_TOKEN_COLUMNS,
	type IpAuthorizationTokenRow,
	MFA_BACKUP_CODE_COLUMNS,
	type MfaBackupCodeRow,
	PASSWORD_CHANGE_TICKET_COLUMNS,
	PASSWORD_RESET_TOKEN_COLUMNS,
	type PasswordChangeTicketRow,
	type PasswordResetTokenRow,
	PHONE_TOKEN_COLUMNS,
	type PhoneTokenRow,
	USER_COUNTRY_HISTORY_COLUMNS,
	USER_SSO_IDENTITY_COLUMNS,
	type UserCountryHistoryRow,
	type UserSsoIdentityRow,
	WEBAUTHN_CREDENTIAL_COLUMNS,
	type WebAuthnCredentialRow,
} from './database/types/AuthTypes';
import {
	BILLING_ACTION_INTENT_COLUMNS,
	BILLING_CHARGE_BY_CUSTOMER_COLUMNS,
	BILLING_CHARGE_COLUMNS,
	BILLING_CHECKOUT_SESSION_BY_CUSTOMER_COLUMNS,
	BILLING_CHECKOUT_SESSION_COLUMNS,
	BILLING_CUSTOMER_BY_USER_ID_COLUMNS,
	BILLING_CUSTOMER_COLUMNS,
	BILLING_DISPUTE_BY_CHARGE_COLUMNS,
	BILLING_DISPUTE_COLUMNS,
	BILLING_INVOICE_BY_CUSTOMER_COLUMNS,
	BILLING_INVOICE_BY_SUBSCRIPTION_COLUMNS,
	BILLING_INVOICE_COLUMNS,
	BILLING_PAYMENT_BY_INVOICE_COLUMNS,
	BILLING_PAYMENT_COLUMNS,
	BILLING_PAYMENT_INTENT_BY_CUSTOMER_COLUMNS,
	BILLING_PAYMENT_INTENT_COLUMNS,
	BILLING_PAYMENT_METHOD_BY_CUSTOMER_COLUMNS,
	BILLING_PAYMENT_METHOD_COLUMNS,
	BILLING_PRICE_COLUMNS,
	BILLING_PRODUCT_COLUMNS,
	BILLING_REFUND_BY_CHARGE_COLUMNS,
	BILLING_REFUND_BY_INVOICE_COLUMNS,
	BILLING_REFUND_BY_PAYMENT_INTENT_COLUMNS,
	BILLING_REFUND_COLUMNS,
	BILLING_SUBSCRIPTION_BY_CUSTOMER_COLUMNS,
	BILLING_SUBSCRIPTION_BY_USER_COLUMNS,
	BILLING_SUBSCRIPTION_COLUMNS,
	type BillingActionIntentRow,
	type BillingChargeByCustomerRow,
	type BillingChargeRow,
	type BillingCheckoutSessionByCustomerRow,
	type BillingCheckoutSessionRow,
	type BillingCustomerByUserIdRow,
	type BillingCustomerRow,
	type BillingDisputeByChargeRow,
	type BillingDisputeRow,
	type BillingInvoiceByCustomerRow,
	type BillingInvoiceBySubscriptionRow,
	type BillingInvoiceRow,
	type BillingPaymentByInvoiceRow,
	type BillingPaymentIntentByCustomerRow,
	type BillingPaymentIntentRow,
	type BillingPaymentMethodByCustomerRow,
	type BillingPaymentMethodRow,
	type BillingPaymentRow,
	type BillingPriceRow,
	type BillingProductRow,
	type BillingRefundByChargeRow,
	type BillingRefundByInvoiceRow,
	type BillingRefundByPaymentIntentRow,
	type BillingRefundRow,
	type BillingSubscriptionByCustomerRow,
	type BillingSubscriptionByUserRow,
	type BillingSubscriptionRow,
} from './database/types/BillingTypes';
import {
	CHANNEL_COLUMNS,
	CHANNELS_BY_GUILD_COLUMNS,
	type ChannelRow,
	type ChannelsByGuildRow,
	DM_STATE_COLUMNS,
	type DmStateRow,
	INVITE_COLUMNS,
	type InviteRow,
	PRIVATE_CHANNEL_COLUMNS,
	type PrivateChannelRow,
	WEBHOOK_COLUMNS,
	type WebhookRow,
} from './database/types/ChannelTypes';
import {USER_CONNECTION_COLUMNS, type UserConnectionRow} from './database/types/ConnectionTypes';
import {
	NCMEC_ATTACHMENT_SUBMISSION_COLUMNS,
	NCMEC_USER_WORKFLOW_COLUMNS,
	type NcmecAttachmentSubmissionRow,
	type NcmecUserWorkflowRow,
} from './database/types/CsamTypes';
import {
	GUILD_DISCOVERY_BY_STATUS_COLUMNS,
	GUILD_DISCOVERY_COLUMNS,
	type GuildDiscoveryByStatusRow,
	type GuildDiscoveryRow,
} from './database/types/GuildDiscoveryTypes';
import {
	GUILD_AUDIT_LOG_COLUMNS,
	GUILD_BAN_BY_EMAIL_COLUMNS,
	GUILD_BAN_BY_USER_ID_COLUMNS,
	GUILD_BAN_COLUMNS,
	GUILD_COLUMNS,
	GUILD_EMOJI_BY_EMOJI_ID_COLUMNS,
	GUILD_EMOJI_COLUMNS,
	GUILD_MEMBER_BY_USER_ID_COLUMNS,
	GUILD_MEMBER_COLUMNS,
	GUILD_MEMBERSHIP_METADATA_COLUMNS,
	GUILD_ROLE_COLUMNS,
	GUILD_STICKER_BY_STICKER_ID_COLUMNS,
	GUILD_STICKER_COLUMNS,
	type GuildAuditLogRow,
	type GuildBanByEmailRow,
	type GuildBanByUserIdRow,
	type GuildBanRow,
	type GuildEmojiRow,
	type GuildMemberByUserIdRow,
	type GuildMemberRow,
	type GuildMembershipMetadataRow,
	type GuildRoleRow,
	type GuildRow,
	type GuildStickerRow,
} from './database/types/GuildTypes';
import {INSTANCE_CONFIGURATION_COLUMNS, type InstanceConfigurationRow} from './database/types/InstanceConfigTypes';
import {
	JOB_ACTIVE_COLUMNS,
	JOB_BY_DAY_BUCKET_COLUMNS,
	JOB_BY_ID_COLUMNS,
	type JobActiveRow,
	type JobByDayBucketRow,
	type JobByIdRow,
} from './database/types/JobLedgerTypes';
import {
	ATTACHMENT_LOOKUP_COLUMNS,
	type AttachmentLookupRow,
	CHANNEL_EMPTY_BUCKET_COLUMNS,
	CHANNEL_MESSAGE_BUCKET_COLUMNS,
	CHANNEL_PIN_COLUMNS,
	CHANNEL_STATE_COLUMNS,
	type ChannelEmptyBucketRow,
	type ChannelMessageBucketRow,
	type ChannelPinRow,
	type ChannelStateRow,
	MESSAGE_BY_AUTHOR_COLUMNS,
	MESSAGE_COLUMNS,
	MESSAGE_REACTION_COLUMNS,
	type MessageByAuthorRow,
	type MessageReactionRow,
	type MessageRow,
} from './database/types/MessageTypes';
import {
	APPLICATION_COLUMNS,
	type ApplicationByOwnerRow,
	type ApplicationRow,
	OAUTH2_ACCESS_TOKEN_COLUMNS,
	OAUTH2_AUTHORIZATION_CODE_COLUMNS,
	OAUTH2_REFRESH_TOKEN_COLUMNS,
	type OAuth2AccessTokenByUserRow,
	type OAuth2AccessTokenRow,
	type OAuth2AuthorizationCodeRow,
	type OAuth2RefreshTokenByUserRow,
	type OAuth2RefreshTokenRow,
} from './database/types/OAuth2Types';
import {
	GIFT_CODE_BY_CREATOR_COLUMNS,
	GIFT_CODE_BY_PAYMENT_INTENT_COLUMNS,
	GIFT_CODE_BY_REDEEMER_COLUMNS,
	GIFT_CODE_COLUMNS,
	type GiftCodeByCreatorRow,
	type GiftCodeByPaymentIntentRow,
	type GiftCodeByRedeemerRow,
	type GiftCodeRow,
	PAYMENT_BY_PAYMENT_INTENT_COLUMNS,
	PAYMENT_BY_SUBSCRIPTION_COLUMNS,
	PAYMENT_BY_USER_COLUMNS,
	PAYMENT_COLUMNS,
	type PaymentByPaymentIntentRow,
	type PaymentBySubscriptionRow,
	type PaymentByUserRow,
	type PaymentRow,
	VISIONARY_SLOT_COLUMNS,
	type VisionarySlotRow,
} from './database/types/PaymentTypes';
import {
	DSA_REPORT_EMAIL_VERIFICATION_COLUMNS,
	DSA_REPORT_TICKET_COLUMNS,
	type DSAReportEmailVerificationRow,
	type DSAReportTicketRow,
	IAR_SUBMISSION_COLUMNS,
	type IARSubmissionRow,
	MESSAGE_REPORT_SUBMISSION_BY_REPORTER_COLUMNS,
	type MessageReportSubmissionByReporterRow,
} from './database/types/ReportTypes';
import {
	INBOUND_SMS_CHALLENGE_BY_USER_COLUMNS,
	INBOUND_SMS_CHALLENGE_COLUMNS,
	type InboundSmsChallengeByUserRow,
	type InboundSmsChallengeRow,
	LATEST_RISK_CONTEXT_BY_USER_COLUMNS,
	type LatestRiskContextByUserRow,
	PHONE_LOOKUP_CACHE_COLUMNS,
	PHONE_VERIFICATION_ATTEMPT_COLUMNS,
	type PhoneLookupCacheRow,
	type PhoneVerificationAttemptRow,
	REGISTRATION_EVENT_BY_EMAIL_DOMAIN_COLUMNS,
	REGISTRATION_EVENT_BY_IP_COLUMNS,
	REGISTRATION_EVENT_BY_PLUS_ADDRESS_BASE_COLUMNS,
	REGISTRATION_EVENT_BY_SUBNET_COLUMNS,
	type RegistrationEventByEmailDomainRow,
	type RegistrationEventByIpRow,
	type RegistrationEventByPlusAddressBaseRow,
	type RegistrationEventBySubnetRow,
	RISK_ASSESSMENT_BY_USER_COLUMNS,
	RISK_ASSESSMENT_COLUMNS,
	RISK_OUTCOME_BY_ASN_COLUMNS,
	RISK_OUTCOME_BY_EMAIL_DOMAIN_COLUMNS,
	RISK_OUTCOME_BY_IP_COLUMNS,
	RISK_OUTCOME_BY_SUBNET_COLUMNS,
	type RiskAssessmentByUserRow,
	type RiskAssessmentRow,
	type RiskOutcomeByAsnRow,
	type RiskOutcomeByEmailDomainRow,
	type RiskOutcomeByIpRow,
	type RiskOutcomeBySubnetRow,
	SUSPICIOUS_IP_COLUMNS,
	type SuspiciousIpRow,
} from './database/types/RiskTypes';
import {
	EXPRESSION_PACK_COLUMNS,
	type ExpressionPackRow,
	FAVORITE_MEME_COLUMNS,
	type FavoriteMemeRow,
	NOTE_COLUMNS,
	type NoteRow,
	type PackInstallationRow,
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
	USER_BY_LAST_ACTIVE_IP_COLUMNS,
	USER_BY_LAST_ACTIVE_IP_TRUST_KEY_COLUMNS,
	USER_BY_STRIPE_CUSTOMER_ID_COLUMNS,
	USER_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS,
	USER_BY_USERNAME_COLUMNS,
	USER_COLUMNS,
	USER_CONTACT_CHANGE_LOG_COLUMNS,
	USER_DM_HISTORY_COLUMNS,
	USER_EMAIL_OWNER_COLUMNS,
	USER_ENTRANCE_SOUND_COLUMNS,
	USER_ENTRANCE_SOUND_SELECTION_COLUMNS,
	USER_GUILD_SETTINGS_COLUMNS,
	USER_HARVEST_COLUMNS,
	USER_SETTINGS_COLUMNS,
	USERS_PENDING_DELETION_COLUMNS,
	type UserByEmailRow,
	type UserByLastActiveIpRow,
	type UserByLastActiveIpTrustKeyRow,
	type UserByStripeCustomerIdRow,
	type UserByStripeSubscriptionIdRow,
	type UserByUsernameRow,
	type UserContactChangeLogRow,
	type UserDmHistoryRow,
	type UserEmailOwnerRow,
	type UserEntranceSoundRow,
	type UserEntranceSoundSelectionRow,
	type UserGuildSettingsRow,
	type UserHarvestRow,
	type UserRow,
	type UserSettingsRow,
	type UsersPendingDeletionRow,
} from './database/types/UserTypes';
import {ATTACHMENT_DECAY_COLUMNS, type AttachmentDecayRow} from './types/AttachmentDecayTypes';

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
export const UserEmailOwners = defineTable<UserEmailOwnerRow, 'email_lower'>({
	name: 'user_email_owners',
	columns: USER_EMAIL_OWNER_COLUMNS,
	primaryKey: ['email_lower'],
});
export const UserSsoIdentities = defineTable<UserSsoIdentityRow, 'provider_id' | 'subject', 'provider_id'>({
	name: 'user_sso_identities',
	columns: USER_SSO_IDENTITY_COLUMNS,
	primaryKey: ['provider_id', 'subject'],
	partitionKey: ['provider_id'],
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
export const UserByLastActiveIp = defineTable<UserByLastActiveIpRow, 'last_active_ip' | 'user_id'>({
	name: 'users_by_last_active_ip',
	columns: USER_BY_LAST_ACTIVE_IP_COLUMNS,
	primaryKey: ['last_active_ip', 'user_id'],
});
export const UserByLastActiveIpTrustKey = defineTable<
	UserByLastActiveIpTrustKeyRow,
	'last_active_ip_trust_key' | 'user_id'
>({
	name: 'users_by_last_active_ip_trust_key_v1',
	columns: USER_BY_LAST_ACTIVE_IP_TRUST_KEY_COLUMNS,
	primaryKey: ['last_active_ip_trust_key', 'user_id'],
});
export const UserSettings = defineTable<UserSettingsRow, 'user_id'>({
	name: 'user_settings',
	columns: USER_SETTINGS_COLUMNS,
	primaryKey: ['user_id'],
});
export const UserEntranceSounds = defineTable<UserEntranceSoundRow, 'user_id' | 'sound_id'>({
	name: 'user_entrance_sound',
	columns: USER_ENTRANCE_SOUND_COLUMNS,
	primaryKey: ['user_id', 'sound_id'],
});
export const UserEntranceSoundSelections = defineTable<UserEntranceSoundSelectionRow, 'user_id' | 'scope_id'>({
	name: 'user_entrance_sound_selection',
	columns: USER_ENTRANCE_SOUND_SELECTION_COLUMNS,
	primaryKey: ['user_id', 'scope_id'],
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
export const UserConnections = defineTable<UserConnectionRow, 'user_id' | 'connection_type' | 'connection_id'>({
	name: 'user_connections',
	columns: USER_CONNECTION_COLUMNS,
	primaryKey: ['user_id', 'connection_type', 'connection_id'],
	partitionKey: ['user_id'],
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
export const RelationshipsByTarget = defineTable<RelationshipRow, 'target_user_id' | 'source_user_id' | 'type'>({
	name: 'relationships_by_target',
	columns: RELATIONSHIP_COLUMNS,
	primaryKey: ['target_user_id', 'source_user_id', 'type'],
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
export const GuildDiscovery = defineTable<GuildDiscoveryRow, 'guild_id'>({
	name: 'guild_discovery',
	columns: GUILD_DISCOVERY_COLUMNS,
	primaryKey: ['guild_id'],
});
export const GuildDiscoveryByStatus = defineTable<
	GuildDiscoveryByStatusRow,
	'status' | 'applied_at' | 'guild_id',
	'status'
>({
	name: 'guild_discovery_by_status',
	columns: GUILD_DISCOVERY_BY_STATUS_COLUMNS,
	primaryKey: ['status', 'applied_at', 'guild_id'],
	partitionKey: ['status'],
});
export const GuildBans = defineTable<GuildBanRow, 'guild_id' | 'user_id'>({
	name: 'guild_bans',
	columns: GUILD_BAN_COLUMNS,
	primaryKey: ['guild_id', 'user_id'],
});
export const GuildBansByEmail = defineTable<GuildBanByEmailRow, 'guild_id' | 'email'>({
	name: 'guild_bans_by_email',
	columns: GUILD_BAN_BY_EMAIL_COLUMNS,
	primaryKey: ['guild_id', 'email'],
	partitionKey: ['guild_id', 'email'],
});
export const GuildBansByUserId = defineTable<GuildBanByUserIdRow, 'user_id' | 'guild_id'>({
	name: 'guild_bans_by_user_id',
	columns: GUILD_BAN_BY_USER_ID_COLUMNS,
	primaryKey: ['user_id', 'guild_id'],
	partitionKey: ['user_id'],
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
export const GuildMembershipMetadata = defineTable<GuildMembershipMetadataRow, 'guild_id' | 'user_id'>({
	name: 'guild_membership_metadata',
	columns: GUILD_MEMBERSHIP_METADATA_COLUMNS,
	primaryKey: ['guild_id', 'user_id'],
});
export const GuildMembersByUserId = defineTable<GuildMemberByUserIdRow, 'user_id' | 'guild_id'>({
	name: 'guild_members_by_user_id',
	columns: GUILD_MEMBER_BY_USER_ID_COLUMNS,
	primaryKey: ['user_id', 'guild_id'],
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
export const Messages = defineTable<MessageRow, 'channel_id' | 'bucket' | 'message_id', 'channel_id' | 'bucket'>({
	name: 'messages',
	columns: MESSAGE_COLUMNS,
	primaryKey: ['channel_id', 'bucket', 'message_id'],
	partitionKey: ['channel_id', 'bucket'],
});
export const MessagesByAuthorV2 = defineTable<MessageByAuthorRow, 'author_id' | 'message_id'>({
	name: 'messages_by_author_id_v2',
	columns: MESSAGE_BY_AUTHOR_COLUMNS,
	primaryKey: ['author_id', 'message_id'],
});
export const MessageReactions = defineTable<
	MessageReactionRow,
	'channel_id' | 'bucket' | 'message_id' | 'emoji_id' | 'emoji_name' | 'user_id',
	'channel_id' | 'bucket'
>({
	name: 'message_reactions',
	columns: MESSAGE_REACTION_COLUMNS,
	primaryKey: ['channel_id', 'bucket', 'message_id', 'emoji_id', 'emoji_name', 'user_id'],
	partitionKey: ['channel_id', 'bucket'],
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
export const GiftCodesByPaymentIntent = defineTable<
	GiftCodeByPaymentIntentRow,
	'stripe_payment_intent_id' | 'code',
	'stripe_payment_intent_id'
>({
	name: 'gift_codes_by_payment_intent',
	columns: GIFT_CODE_BY_PAYMENT_INTENT_COLUMNS,
	primaryKey: ['stripe_payment_intent_id', 'code'],
	partitionKey: ['stripe_payment_intent_id'],
});
export const GiftCodesByRedeemer = defineTable<GiftCodeByRedeemerRow, 'redeemed_by_user_id' | 'code'>({
	name: 'gift_codes_by_redeemer',
	columns: GIFT_CODE_BY_REDEEMER_COLUMNS,
	primaryKey: ['redeemed_by_user_id', 'code'],
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
export const AdminApiKeys = defineTable<AdminApiKeyRow, 'key_id'>({
	name: 'admin_api_keys',
	columns: ADMIN_API_KEY_COLUMNS,
	primaryKey: ['key_id'],
});
export const AdminApiKeysByCreator = defineTable<AdminApiKeyByCreatorRow, 'created_by_user_id' | 'key_id'>({
	name: 'admin_api_keys_by_creator',
	columns: ADMIN_API_KEY_BY_CREATOR_COLUMNS,
	primaryKey: ['created_by_user_id', 'key_id'],
	partitionKey: ['created_by_user_id'],
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
export const BannedPhonePrefixes = defineTable<BannedPhonePrefixRow, 'prefix'>({
	name: 'banned_phone_prefixes',
	columns: BANNED_PHONE_PREFIX_COLUMNS,
	primaryKey: ['prefix'],
});
export const SuspiciousEmailDomains = defineTable<SuspiciousEmailDomainRow, 'domain'>({
	name: 'suspicious_email_domains',
	columns: SUSPICIOUS_EMAIL_DOMAIN_COLUMNS,
	primaryKey: ['domain'],
});
export const DisposableEmailDomains = defineTable<DisposableEmailDomainRow, 'domain'>({
	name: 'disposable_email_domains',
	columns: DISPOSABLE_EMAIL_DOMAIN_COLUMNS,
	primaryKey: ['domain'],
});
export const BannedPhrases = defineTable<BannedPhraseRow, 'phrase'>({
	name: 'banned_phrases',
	columns: BANNED_PHRASE_COLUMNS,
	primaryKey: ['phrase'],
});
export const BannedUrls = defineTable<BannedUrlRow, 'url_canonical'>({
	name: 'banned_urls',
	columns: BANNED_URL_COLUMNS,
	primaryKey: ['url_canonical'],
});
export const BannedUrlDomains = defineTable<BannedUrlDomainRow, 'domain'>({
	name: 'banned_url_domains',
	columns: BANNED_URL_DOMAIN_COLUMNS,
	primaryKey: ['domain'],
});
export const BannedFileShas = defineTable<BannedFileShaRow, 'sha256_hex'>({
	name: 'banned_file_shas',
	columns: BANNED_FILE_SHA_COLUMNS,
	primaryKey: ['sha256_hex'],
});
export const BannedAvatarHashes = defineTable<BannedAvatarHashRow, 'hash_short'>({
	name: 'banned_avatar_hashes',
	columns: BANNED_AVATAR_HASH_COLUMNS,
	primaryKey: ['hash_short'],
});
export const BannedProfileSubstrings = defineTable<BannedProfileSubstringRow, 'scope' | 'substring'>({
	name: 'banned_profile_substrings',
	columns: BANNED_PROFILE_SUBSTRING_COLUMNS,
	primaryKey: ['scope', 'substring'],
});
export const IARSubmissions = defineTable<IARSubmissionRow, 'report_id'>({
	name: 'iar_submissions',
	columns: IAR_SUBMISSION_COLUMNS,
	primaryKey: ['report_id'],
});
export const MessageReportSubmissionsByReporter = defineTable<
	MessageReportSubmissionByReporterRow,
	'reporter_id' | 'channel_id' | 'message_id',
	'reporter_id'
>({
	name: 'message_report_submissions_by_reporter',
	columns: MESSAGE_REPORT_SUBMISSION_BY_REPORTER_COLUMNS,
	primaryKey: ['reporter_id', 'channel_id', 'message_id'],
	partitionKey: ['reporter_id'],
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
export const PasswordResetTokensByUserId = defineTable<
	{
		user_id: UserID;
		token_: PasswordResetToken;
	},
	'user_id' | 'token_'
>({
	name: 'password_reset_tokens_by_user_id',
	columns: ['user_id', 'token_'],
	primaryKey: ['user_id', 'token_'],
});
export const EmailRevertTokens = defineTable<EmailRevertTokenRow, 'token_' | 'user_id'>({
	name: 'email_revert_tokens',
	columns: EMAIL_REVERT_TOKEN_COLUMNS,
	primaryKey: ['token_', 'user_id'],
});
export const PhoneTokens = defineTable<PhoneTokenRow, 'token_'>({
	name: 'phone_tokens',
	columns: PHONE_TOKEN_COLUMNS,
	primaryKey: ['token_'],
});
export const AuthSessions = defineTable<AuthSessionRow, 'session_id_hash'>({
	name: 'auth_sessions',
	columns: AUTH_SESSION_COLUMNS,
	primaryKey: ['session_id_hash'],
});
export const AuthSessionsByUserId = defineTable<
	{
		user_id: UserID;
		session_id_hash: Buffer;
	},
	'user_id' | 'session_id_hash'
>({
	name: 'auth_sessions_by_user_id',
	columns: ['user_id', 'session_id_hash'],
	primaryKey: ['user_id', 'session_id_hash'],
});
export const AuthSessionTombstones = defineTable<AuthSessionTombstoneRow, 'user_id' | 'session_id_hash'>({
	name: 'auth_session_tombstones',
	columns: AUTH_SESSION_TOMBSTONE_COLUMNS,
	primaryKey: ['user_id', 'session_id_hash'],
});
export const UserCountryHistory = defineTable<UserCountryHistoryRow, 'user_id' | 'country'>({
	name: 'user_country_history',
	columns: USER_COUNTRY_HISTORY_COLUMNS,
	primaryKey: ['user_id', 'country'],
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
export const WebAuthnCredentialLookup = defineTable<
	{
		credential_id: string;
		user_id: UserID;
	},
	'credential_id'
>({
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
export const AuthorizedIpTrustKeys = defineTable<AuthorizedIpTrustKeyRow, 'user_id' | 'trust_key'>({
	name: 'authorized_ip_trust_keys_v1',
	columns: AUTHORIZED_IP_TRUST_KEY_COLUMNS,
	primaryKey: ['user_id', 'trust_key'],
});
export const PasswordChangeTickets = defineTable<PasswordChangeTicketRow, 'ticket'>({
	name: 'password_change_tickets',
	columns: PASSWORD_CHANGE_TICKET_COLUMNS,
	primaryKey: ['ticket'],
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
export const JobsById = defineTable<JobByIdRow, 'job_id'>({
	name: 'jobs_by_id',
	columns: JOB_BY_ID_COLUMNS,
	primaryKey: ['job_id'],
});
export const JobsByDayBucket = defineTable<JobByDayBucketRow, 'bucket_day' | 'created_at' | 'job_id'>({
	name: 'jobs_by_day_bucket',
	columns: JOB_BY_DAY_BUCKET_COLUMNS,
	primaryKey: ['bucket_day', 'created_at', 'job_id'],
	partitionKey: ['bucket_day'],
});
export const JobsActive = defineTable<JobActiveRow, 'job_id'>({
	name: 'jobs_active',
	columns: JOB_ACTIVE_COLUMNS,
	primaryKey: ['job_id'],
});
export const AttachmentUploadTracesByKey = defineTable<AttachmentUploadTraceByKeyRow, 'upload_key'>({
	name: 'attachment_upload_traces_by_key',
	columns: ATTACHMENT_UPLOAD_TRACE_BY_KEY_COLUMNS,
	primaryKey: ['upload_key'],
});
export const AttachmentUploadTracesByAttachment = defineTable<AttachmentUploadTraceByAttachmentRow, 'attachment_id'>({
	name: 'attachment_upload_traces_by_attachment',
	columns: ATTACHMENT_UPLOAD_TRACE_BY_ATTACHMENT_COLUMNS,
	primaryKey: ['attachment_id'],
});
export const NcmecAttachmentSubmissions = defineTable<NcmecAttachmentSubmissionRow, 'attachment_id'>({
	name: 'ncmec_attachment_submissions',
	columns: NCMEC_ATTACHMENT_SUBMISSION_COLUMNS,
	primaryKey: ['attachment_id'],
});
export const NcmecUserWorkflows = defineTable<NcmecUserWorkflowRow, 'user_id'>({
	name: 'ncmec_user_workflows',
	columns: NCMEC_USER_WORKFLOW_COLUMNS,
	primaryKey: ['user_id'],
});
export const RegistrationEventsByIp = defineTable<RegistrationEventByIpRow, 'ip' | 'created_at' | 'user_id', 'ip'>({
	name: 'registration_events_by_ip',
	columns: REGISTRATION_EVENT_BY_IP_COLUMNS,
	primaryKey: ['ip', 'created_at', 'user_id'],
	partitionKey: ['ip'],
});
export const RegistrationEventsBySubnet = defineTable<
	RegistrationEventBySubnetRow,
	'subnet' | 'created_at' | 'user_id',
	'subnet'
>({
	name: 'registration_events_by_subnet',
	columns: REGISTRATION_EVENT_BY_SUBNET_COLUMNS,
	primaryKey: ['subnet', 'created_at', 'user_id'],
	partitionKey: ['subnet'],
});
export const RegistrationEventsByEmailDomain = defineTable<
	RegistrationEventByEmailDomainRow,
	'email_domain' | 'created_at' | 'user_id',
	'email_domain'
>({
	name: 'registration_events_by_email_domain',
	columns: REGISTRATION_EVENT_BY_EMAIL_DOMAIN_COLUMNS,
	primaryKey: ['email_domain', 'created_at', 'user_id'],
	partitionKey: ['email_domain'],
});
export const RegistrationEventsByPlusAddressBase = defineTable<
	RegistrationEventByPlusAddressBaseRow,
	'plus_address_base' | 'created_at' | 'user_id',
	'plus_address_base'
>({
	name: 'registration_events_by_plus_address_base',
	columns: REGISTRATION_EVENT_BY_PLUS_ADDRESS_BASE_COLUMNS,
	primaryKey: ['plus_address_base', 'created_at', 'user_id'],
	partitionKey: ['plus_address_base'],
});
export const LatestRiskContextByUser = defineTable<LatestRiskContextByUserRow, 'user_id'>({
	name: 'latest_risk_context_by_user',
	columns: LATEST_RISK_CONTEXT_BY_USER_COLUMNS,
	primaryKey: ['user_id'],
});
export const SuspiciousIps = defineTable<SuspiciousIpRow, 'ip'>({
	name: 'suspicious_ips',
	columns: SUSPICIOUS_IP_COLUMNS,
	primaryKey: ['ip'],
});
export const RiskOutcomesByIp = defineTable<RiskOutcomeByIpRow, 'ip' | 'created_at' | 'user_id' | 'outcome_code', 'ip'>(
	{
		name: 'risk_outcomes_by_ip',
		columns: RISK_OUTCOME_BY_IP_COLUMNS,
		primaryKey: ['ip', 'created_at', 'user_id', 'outcome_code'],
		partitionKey: ['ip'],
	},
);
export const RiskOutcomesBySubnet = defineTable<
	RiskOutcomeBySubnetRow,
	'subnet' | 'created_at' | 'user_id' | 'outcome_code',
	'subnet'
>({
	name: 'risk_outcomes_by_subnet',
	columns: RISK_OUTCOME_BY_SUBNET_COLUMNS,
	primaryKey: ['subnet', 'created_at', 'user_id', 'outcome_code'],
	partitionKey: ['subnet'],
});
export const RiskOutcomesByEmailDomain = defineTable<
	RiskOutcomeByEmailDomainRow,
	'email_domain' | 'created_at' | 'user_id' | 'outcome_code',
	'email_domain'
>({
	name: 'risk_outcomes_by_email_domain',
	columns: RISK_OUTCOME_BY_EMAIL_DOMAIN_COLUMNS,
	primaryKey: ['email_domain', 'created_at', 'user_id', 'outcome_code'],
	partitionKey: ['email_domain'],
});
export const RiskOutcomesByAsn = defineTable<
	RiskOutcomeByAsnRow,
	'asn' | 'created_at' | 'user_id' | 'outcome_code',
	'asn'
>({
	name: 'risk_outcomes_by_asn',
	columns: RISK_OUTCOME_BY_ASN_COLUMNS,
	primaryKey: ['asn', 'created_at', 'user_id', 'outcome_code'],
	partitionKey: ['asn'],
});
export const RiskAssessments = defineTable<RiskAssessmentRow, 'assessment_id'>({
	name: 'risk_assessments',
	columns: RISK_ASSESSMENT_COLUMNS,
	primaryKey: ['assessment_id'],
});
export const RiskAssessmentsByUser = defineTable<RiskAssessmentByUserRow, 'user_id' | 'created_at', 'user_id'>({
	name: 'risk_assessments_by_user',
	columns: RISK_ASSESSMENT_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'created_at'],
	partitionKey: ['user_id'],
});
export const InboundSmsChallenges = defineTable<InboundSmsChallengeRow, 'challenge_code'>({
	name: 'inbound_sms_challenges',
	columns: INBOUND_SMS_CHALLENGE_COLUMNS,
	primaryKey: ['challenge_code'],
});
export const InboundSmsChallengesByUser = defineTable<
	InboundSmsChallengeByUserRow,
	'user_id' | 'created_at',
	'user_id'
>({
	name: 'inbound_sms_challenges_by_user',
	columns: INBOUND_SMS_CHALLENGE_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'created_at'],
	partitionKey: ['user_id'],
});
export const PhoneLookupCache = defineTable<PhoneLookupCacheRow, 'phone'>({
	name: 'phone_lookup_cache',
	columns: PHONE_LOOKUP_CACHE_COLUMNS,
	primaryKey: ['phone'],
});
export const PhoneVerificationAttempts = defineTable<PhoneVerificationAttemptRow, 'attempt_id'>({
	name: 'phone_verification_attempts',
	columns: PHONE_VERIFICATION_ATTEMPT_COLUMNS,
	primaryKey: ['attempt_id'],
});
export const BillingCustomers = defineTable<BillingCustomerRow, 'provider_id'>({
	name: 'billing_customers',
	columns: BILLING_CUSTOMER_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingCustomersByUserId = defineTable<BillingCustomerByUserIdRow, 'user_id' | 'provider_id'>({
	name: 'billing_customers_by_user_id',
	columns: BILLING_CUSTOMER_BY_USER_ID_COLUMNS,
	primaryKey: ['user_id', 'provider_id'],
});
export const BillingProducts = defineTable<BillingProductRow, 'provider_id'>({
	name: 'billing_products',
	columns: BILLING_PRODUCT_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingPrices = defineTable<BillingPriceRow, 'provider_id'>({
	name: 'billing_prices',
	columns: BILLING_PRICE_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingPaymentMethods = defineTable<BillingPaymentMethodRow, 'provider_id'>({
	name: 'billing_payment_methods',
	columns: BILLING_PAYMENT_METHOD_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingPaymentMethodsByCustomer = defineTable<
	BillingPaymentMethodByCustomerRow,
	'customer_id' | 'provider_id'
>({
	name: 'billing_payment_methods_by_customer',
	columns: BILLING_PAYMENT_METHOD_BY_CUSTOMER_COLUMNS,
	primaryKey: ['customer_id', 'provider_id'],
});
export const BillingSubscriptions = defineTable<BillingSubscriptionRow, 'provider_id'>({
	name: 'billing_subscriptions',
	columns: BILLING_SUBSCRIPTION_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingSubscriptionsByCustomer = defineTable<
	BillingSubscriptionByCustomerRow,
	'customer_id' | 'provider_id'
>({
	name: 'billing_subscriptions_by_customer',
	columns: BILLING_SUBSCRIPTION_BY_CUSTOMER_COLUMNS,
	primaryKey: ['customer_id', 'provider_id'],
});
export const BillingSubscriptionsByUser = defineTable<BillingSubscriptionByUserRow, 'user_id' | 'provider_id'>({
	name: 'billing_subscriptions_by_user',
	columns: BILLING_SUBSCRIPTION_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'provider_id'],
});
export const BillingInvoices = defineTable<BillingInvoiceRow, 'provider_id'>({
	name: 'billing_invoices',
	columns: BILLING_INVOICE_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingInvoicesByCustomer = defineTable<
	BillingInvoiceByCustomerRow,
	'customer_id' | 'stripe_created_at' | 'provider_id',
	'customer_id'
>({
	name: 'billing_invoices_by_customer',
	columns: BILLING_INVOICE_BY_CUSTOMER_COLUMNS,
	primaryKey: ['customer_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['customer_id'],
});
export const BillingInvoicesBySubscription = defineTable<
	BillingInvoiceBySubscriptionRow,
	'subscription_id' | 'stripe_created_at' | 'provider_id',
	'subscription_id'
>({
	name: 'billing_invoices_by_subscription',
	columns: BILLING_INVOICE_BY_SUBSCRIPTION_COLUMNS,
	primaryKey: ['subscription_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['subscription_id'],
});
export const BillingPaymentIntents = defineTable<BillingPaymentIntentRow, 'provider_id'>({
	name: 'billing_payment_intents',
	columns: BILLING_PAYMENT_INTENT_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingPaymentIntentsByCustomer = defineTable<
	BillingPaymentIntentByCustomerRow,
	'customer_id' | 'stripe_created_at' | 'provider_id',
	'customer_id'
>({
	name: 'billing_payment_intents_by_customer',
	columns: BILLING_PAYMENT_INTENT_BY_CUSTOMER_COLUMNS,
	primaryKey: ['customer_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['customer_id'],
});
export const BillingCharges = defineTable<BillingChargeRow, 'provider_id'>({
	name: 'billing_charges',
	columns: BILLING_CHARGE_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingChargesByCustomer = defineTable<
	BillingChargeByCustomerRow,
	'customer_id' | 'stripe_created_at' | 'provider_id',
	'customer_id'
>({
	name: 'billing_charges_by_customer',
	columns: BILLING_CHARGE_BY_CUSTOMER_COLUMNS,
	primaryKey: ['customer_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['customer_id'],
});
export const BillingPayments = defineTable<BillingPaymentRow, 'provider_id'>({
	name: 'billing_payments',
	columns: BILLING_PAYMENT_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingPaymentsByInvoice = defineTable<
	BillingPaymentByInvoiceRow,
	'invoice_id' | 'stripe_created_at' | 'provider_id',
	'invoice_id'
>({
	name: 'billing_payments_by_invoice',
	columns: BILLING_PAYMENT_BY_INVOICE_COLUMNS,
	primaryKey: ['invoice_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['invoice_id'],
});
export const BillingRefunds = defineTable<BillingRefundRow, 'provider_id'>({
	name: 'billing_refunds',
	columns: BILLING_REFUND_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingRefundsByCharge = defineTable<
	BillingRefundByChargeRow,
	'charge_id' | 'stripe_created_at' | 'provider_id',
	'charge_id'
>({
	name: 'billing_refunds_by_charge',
	columns: BILLING_REFUND_BY_CHARGE_COLUMNS,
	primaryKey: ['charge_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['charge_id'],
});
export const BillingRefundsByPaymentIntent = defineTable<
	BillingRefundByPaymentIntentRow,
	'payment_intent_id' | 'stripe_created_at' | 'provider_id',
	'payment_intent_id'
>({
	name: 'billing_refunds_by_payment_intent',
	columns: BILLING_REFUND_BY_PAYMENT_INTENT_COLUMNS,
	primaryKey: ['payment_intent_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['payment_intent_id'],
});
export const BillingRefundsByInvoice = defineTable<
	BillingRefundByInvoiceRow,
	'invoice_id' | 'stripe_created_at' | 'provider_id',
	'invoice_id'
>({
	name: 'billing_refunds_by_invoice',
	columns: BILLING_REFUND_BY_INVOICE_COLUMNS,
	primaryKey: ['invoice_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['invoice_id'],
});
export const BillingCheckoutSessions = defineTable<BillingCheckoutSessionRow, 'provider_id'>({
	name: 'billing_checkout_sessions',
	columns: BILLING_CHECKOUT_SESSION_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingCheckoutSessionsByCustomer = defineTable<
	BillingCheckoutSessionByCustomerRow,
	'customer_id' | 'stripe_created_at' | 'provider_id',
	'customer_id'
>({
	name: 'billing_checkout_sessions_by_customer',
	columns: BILLING_CHECKOUT_SESSION_BY_CUSTOMER_COLUMNS,
	primaryKey: ['customer_id', 'stripe_created_at', 'provider_id'],
	partitionKey: ['customer_id'],
});
export const BillingDisputes = defineTable<BillingDisputeRow, 'provider_id'>({
	name: 'billing_disputes',
	columns: BILLING_DISPUTE_COLUMNS,
	primaryKey: ['provider_id'],
});
export const BillingDisputesByCharge = defineTable<BillingDisputeByChargeRow, 'charge_id' | 'provider_id'>({
	name: 'billing_disputes_by_charge',
	columns: BILLING_DISPUTE_BY_CHARGE_COLUMNS,
	primaryKey: ['charge_id', 'provider_id'],
});
export const BillingActionIntents = defineTable<BillingActionIntentRow, 'intent_id'>({
	name: 'billing_action_intents',
	columns: BILLING_ACTION_INTENT_COLUMNS,
	primaryKey: ['intent_id'],
});
