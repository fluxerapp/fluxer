// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureValue} from '@app/api/database/capture/CaptureCodec';
import {CAPTURE_SUBJECT_PREFIX, type CaptureProcess, type CaptureScope} from '@app/api/database/capture/CaptureRecords';

export type CdcOutcome = 'applied' | 'unknown';

interface CdcKey {
	t: string;
	s: CaptureScope;
	c: Record<string, CaptureValue>;
}

export interface KeyRecord {
	kind: 'k';
	producer: string;
	seq: number;
	ts: number;
	out: CdcOutcome;
	lwt?: true;
	keys: Array<CdcKey>;
}

interface CdcOpenRecord {
	kind: 'life';
	producer: string;
	state: 'open';
	pod: string;
}

interface CdcAliveRecord {
	kind: 'life';
	producer: string;
	state: 'alive';
	last_seq: number;
}

interface CdcLossyRecord {
	kind: 'life';
	producer: string;
	state: 'lossy';
	from_seq: number;
	to_seq: number;
}

export interface CdcClosedRecord {
	kind: 'life';
	producer: string;
	state: 'closed';
	last_seq: number;
	dropped: number;
}

interface CdcDisabledRecord {
	kind: 'life';
	producer: string;
	state: 'disabled';
}

export type CdcLifeRecord = CdcOpenRecord | CdcAliveRecord | CdcLossyRecord | CdcClosedRecord | CdcDisabledRecord;
export type CdcRecord = KeyRecord | CdcLifeRecord;
export type CdcSubjectKind = 'k' | 'life';

export const CDC_SUBJECTS: ReadonlyArray<string> = [
	`${CAPTURE_SUBJECT_PREFIX}.*.k`,
	`${CAPTURE_SUBJECT_PREFIX}.*.life`,
];

export function cdcSubject(process: CaptureProcess, kind: CdcSubjectKind): string {
	return `${CAPTURE_SUBJECT_PREFIX}.${process}.${kind}`;
}

export const CDC_TABLES: ReadonlySet<string> = new Set([
	'admin_api_keys',
	'admin_audit_logs',
	'app_attest_challenges',
	'app_attest_key_lookup',
	'app_attest_keys',
	'applications',
	'auth_session_tombstones',
	'auth_sessions',
	'authorized_ip_trust_keys_v1',
	'authorized_ips_v2',
	'banned_avatar_hashes',
	'banned_emails',
	'banned_file_shas',
	'banned_ips',
	'banned_phrases',
	'banned_profile_substrings',
	'banned_url_domains',
	'banned_urls',
	'billing_charges',
	'billing_customers',
	'billing_document_numbers',
	'billing_documents',
	'billing_invoices',
	'billing_payment_intents',
	'billing_payment_methods',
	'billing_payments',
	'billing_prices',
	'billing_refunds',
	'billing_subscriptions',
	'channels',
	'crossposted_messages',
	'custom_badges',
	'dm_states',
	'donors',
	'dsa_report_email_verifications',
	'dsa_report_tickets',
	'enforcement_decision_evidence',
	'enforcement_decision_notes',
	'enforcement_decisions',
	'enforcement_review_queue',
	'favorite_memes',
	'forum_pinned_thread',
	'gift_codes',
	'guild_bans',
	'guild_discovery',
	'guild_emojis',
	'guild_members',
	'guild_membership_metadata',
	'guild_roles',
	'guild_stickers',
	'guild_thread_state',
	'guilds',
	'iar_report_notes',
	'iar_submissions',
	'instance_configuration',
	'invites',
	'message_reactions',
	'messages',
	'notes',
	'operator_pass_certificate_links',
	'operator_pass_certificate_numbers',
	'operator_pass_certificates',
	'operator_pass_labs_greetings',
	'operator_pass_labs_members',
	'operator_pass_offers',
	'operator_pass_purchases',
	'operator_pass_releases',
	'operator_pass_removal_history',
	'operator_pass_removals',
	'operator_pass_unit_events',
	'operator_pass_units',
	'operator_pass_visionary_activations',
	'operator_pass_waitlist',
	'operator_pass_wall_audit_logs',
	'operator_pass_wall_avatar_refs',
	'operator_pass_wall_decisions',
	'operator_pass_wall_entries',
	'operator_pass_wall_events',
	'operator_pass_wall_revisions',
	'operator_pass_wall_state',
	'operator_pass_wall_tombstones',
	'operator_passes',
	'payment_dispute_reviews',
	'payments',
	'pinned_dms',
	'private_channels',
	'recent_mentions',
	'relationships',
	'saved_messages',
	'store_purchases',
	'thread_members',
	'thread_parent_config',
	'thread_state',
	'thread_stats',
	'user_connections',
	'user_contact_change_logs',
	'user_dm_history',
	'user_email_owners',
	'user_entrance_sound',
	'user_entrance_sound_selection',
	'user_guild_settings',
	'user_harvests',
	'user_recovery_kits',
	'user_settings',
	'user_sso_identities',
	'users',
	'visionary_slots',
	'voice_join_tokens',
	'voice_p2p_connection_reports',
	'voice_regions',
	'voice_servers',
]);

export const CDC_SKIPPED: ReadonlySet<string> = new Set([
	'active_threads_by_guild',
	'admin_api_keys_by_creator',
	'admin_archives_by_requester',
	'admin_archives_by_subject',
	'admin_archives_by_type',
	'applications_by_owner',
	'archived_threads_by_parent',
	'attachment_decay_by_expiry',
	'attachment_decay_by_id',
	'attachment_lookup',
	'attachment_upload_traces_by_attachment',
	'attachment_upload_traces_by_key',
	'auth_sessions_by_app_channel',
	'auth_sessions_by_user_id',
	'billing_customers_by_user_id',
	'billing_documents_by_month',
	'billing_documents_by_source',
	'billing_documents_by_user',
	'billing_invoices_by_customer',
	'billing_payment_methods_by_customer',
	'billing_payments_by_invoice',
	'billing_refunds_by_charge',
	'billing_refunds_by_invoice',
	'billing_refunds_by_payment_intent',
	'billing_subscriptions_by_customer',
	'billing_subscriptions_by_user',
	'cassandra_migrations',
	'cassandra_migrations_lock',
	'channel_empty_buckets',
	'channel_message_buckets',
	'channel_pins',
	'channel_state',
	'channels_by_guild_id',
	'crosspost_sources_by_channel',
	'custom_badges_by_position',
	'donor_magic_link_tokens',
	'donor_magic_link_tokens_by_email',
	'donors_by_stripe_customer_id',
	'donors_by_stripe_subscription_id',
	'email_change_tickets',
	'email_change_tokens',
	'email_revert_tokens',
	'email_verification_tokens',
	'enforcement_decisions_by_subject',
	'favorite_memes_by_meme_id',
	'gift_codes_by_creator',
	'gift_codes_by_payment_intent',
	'gift_codes_by_redeemer',
	'guild_audit_logs_v2',
	'guild_audit_logs_v2_by_action',
	'guild_audit_logs_v2_by_user',
	'guild_audit_logs_v2_by_user_action',
	'guild_bans_by_email',
	'guild_bans_by_user_id',
	'guild_discovery_by_status',
	'guild_emojis_by_emoji_id',
	'guild_members_by_user_id',
	'guild_report_submissions_by_reporter',
	'guild_stickers_by_sticker_id',
	'guild_visits_by_guild',
	'guild_visits_by_user',
	'invites_by_channel_id',
	'invites_by_guild_id',
	'ip_authorization_tokens',
	'jobs_active',
	'jobs_by_day_bucket',
	'jobs_by_id',
	'message_report_submissions_by_reporter',
	'messages_by_author_id_v2',
	'mfa_backup_codes',
	'oauth2_access_tokens',
	'oauth2_access_tokens_by_user',
	'oauth2_authorization_codes',
	'oauth2_refresh_tokens',
	'oauth2_refresh_tokens_by_user',
	'operator_pass_certificates_by_purchase',
	'operator_pass_offers_by_user',
	'operator_pass_purchases_by_payment_intent',
	'operator_pass_purchases_by_purchaser',
	'operator_pass_units_by_holder',
	'operator_pass_units_by_purchase',
	'operator_pass_waitlist_by_entry_id',
	'password_change_tickets',
	'password_reset_tokens',
	'password_reset_tokens_by_user_id',
	'payment_dispute_reviews_by_purchaser',
	'payments_by_payment_intent',
	'payments_by_subscription',
	'payments_by_user',
	'push_subscriptions',
	'read_states',
	'recent_mentions_by_guild',
	'relationships_by_target',
	'store_account_tokens',
	'store_account_tokens_by_user',
	'store_purchases_by_user',
	'suspicious_ips',
	'thread_members_by_user',
	'thread_only_channels_by_guild',
	'threads_by_parent',
	'user_report_submissions_by_reporter',
	'user_sso_identities_by_user',
	'users_by_acl',
	'users_by_email',
	'users_by_last_active_ip',
	'users_by_last_active_ip_trust_key_v1',
	'users_by_stripe_customer_id',
	'users_by_stripe_subscription_id',
	'users_by_username',
	'webauthn_credential_lookup',
	'webauthn_credentials',
	'webhooks',
	'webhooks_by_channel_id',
	'webhooks_by_guild_id',
	'webhooks_by_source_channel_id',
]);

export const CDC_DROPPED_COLUMNS: Readonly<Record<string, ReadonlyArray<string>>> = {
	admin_api_keys: ['key_hash'],
	applications: ['bot_token_hash', 'bot_token_preview', 'client_secret_hash'],
	dsa_report_email_verifications: ['code_hash'],
	operator_pass_waitlist: ['verify_token_hash'],
	store_purchases: ['account_token'],
	user_connections: ['credential_payload', 'verification_token'],
	user_recovery_kits: ['secret_hash'],
	users: ['password_hash', 'totp_secret'],
	voice_join_tokens: ['secret_hash'],
	voice_servers: ['api_secret'],
};

export const CDC_EMITTED_COLUMNS: Readonly<Record<string, ReadonlyArray<string>>> = {
	message_reactions: ['created_at', 'emoji_animated'],
	messages: [
		'author_id',
		'edited_timestamp',
		'flags',
		'has_reaction',
		'mention_everyone',
		'pinned_timestamp',
		'type',
		'version',
		'webhook_avatar_hash',
		'webhook_id',
	],
};

const IMAGE_HASH = 'content hash of a public image or file, not a credential';
const MODERATION_EMAIL = 'support and moderation look accounts up by email';
const MODERATION_IP = 'support and moderation look abuse up by IP';
const BILLING_EMAIL = 'billing support looks payments up by email';

export const CDC_KEPT_SENSITIVE: Readonly<Record<string, Readonly<Record<string, string>>>> = {
	app_attest_keys: {public_key: 'an App Attest public key, which proves nothing without the device'},
	applications: {
		bot_token_created_at: 'the time the bot token was made, not the token',
		client_secret_created_at: 'the time the client secret was made, not the secret',
	},
	auth_session_tombstones: {
		client_ip: MODERATION_IP,
		session_id_hash: 'a key column, sha256 of a random session token, never accepted as a credential',
	},
	auth_sessions: {
		client_ip: MODERATION_IP,
		session_id_hash: 'the partition key, sha256 of a random session token, never accepted as a credential',
	},
	authorized_ips_v2: {ip: MODERATION_IP},
	banned_avatar_hashes: {hash_short: IMAGE_HASH},
	banned_emails: {email_lower: MODERATION_EMAIL},
	banned_ips: {ip: MODERATION_IP},
	billing_charges: {receipt_email: BILLING_EMAIL},
	billing_customers: {email: BILLING_EMAIL},
	billing_documents: {donor_email_hash: 'a lookup hash of a donor email, which is kept in donors anyway'},
	billing_payment_intents: {receipt_email: BILLING_EMAIL},
	billing_payment_methods: {billing_email: BILLING_EMAIL},
	channels: {icon_hash: IMAGE_HASH},
	donors: {email: BILLING_EMAIL},
	dsa_report_email_verifications: {email_lower: MODERATION_EMAIL},
	dsa_report_tickets: {email_lower: MODERATION_EMAIL},
	favorite_memes: {content_hash: IMAGE_HASH},
	guild_bans: {email: MODERATION_EMAIL, ip: MODERATION_IP},
	guild_members: {avatar_hash: IMAGE_HASH, banner_hash: IMAGE_HASH},
	guild_roles: {icon_hash: IMAGE_HASH},
	guilds: {
		banner_hash: IMAGE_HASH,
		embed_splash_hash: IMAGE_HASH,
		icon_hash: IMAGE_HASH,
		splash_hash: IMAGE_HASH,
	},
	iar_submissions: {
		reported_guild_icon_hash: IMAGE_HASH,
		reported_user_avatar_hash: IMAGE_HASH,
		reported_webhook_avatar_hash: IMAGE_HASH,
		reported_webhook_creator_avatar_hash: IMAGE_HASH,
		reported_webhook_default_avatar_hash: IMAGE_HASH,
		reporter_email: MODERATION_EMAIL,
	},
	messages: {webhook_avatar_hash: IMAGE_HASH},
	operator_pass_certificate_links: {
		token_: 'the partition key, the link of a certificate page that is public by design',
	},
	operator_pass_certificates: {link_token: 'the link of a certificate page that is public by design'},
	operator_pass_purchases: {waiver_consent_ip: 'evidence of the withdrawal waiver consent'},
	operator_pass_visionary_activations: {consent_ip: 'evidence of the activation consent'},
	operator_pass_waitlist: {consent_ip: 'evidence of the waitlist consent', email: MODERATION_EMAIL},
	operator_pass_wall_tombstones: {token_: 'the partition key, a wall entry id, not a credential'},
	operator_passes: {waiver_consent_ip: 'evidence of the withdrawal waiver consent'},
	private_channels: {channel_icon_hash: IMAGE_HASH},
	user_email_owners: {email_lower: MODERATION_EMAIL},
	user_entrance_sound: {hash: IMAGE_HASH},
	users: {
		avatar_hash: IMAGE_HASH,
		banner_hash: IMAGE_HASH,
		email: MODERATION_EMAIL,
		last_active_ip: MODERATION_IP,
	},
	voice_join_tokens: {
		token_id: 'the partition key, an id, while the secret is in the dropped secret_hash',
		used_by_ip: MODERATION_IP,
	},
	voice_p2p_connection_reports: {ip: MODERATION_IP},
};
