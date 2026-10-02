import {createHash} from 'node:crypto';

export type Id = string;
export type Flags64 = string;
export type Channel = "stable" | "canary" | "worker" | "internal" | "import" | "other";
export type Meta = { ip: string | null, country: string | null, ua: string | null, locale: string | null, channel: Channel, request_id: string | null, };
export type Kind = "registration" | "email_changed" | "profile_updated" | "account_changed" | "admin_action" | "account_deleted" | "report_filed" | "email_bounced" | "action_outcome" | "login" | "session_started" | "guild_joined" | "dm_opened" | "message_created" | "message_updated" | "friend_request" | "http_errors" | "user_blocked";
export type Event = { v: number, id: string, at_ms: number, key: string, meta: Meta, } & ({ "kind": "registration", "data": Registration } | { "kind": "email_changed", "data": EmailChanged } | { "kind": "profile_updated", "data": ProfileUpdated } | { "kind": "account_changed", "data": AccountChanged } | { "kind": "admin_action", "data": AdminAction } | { "kind": "account_deleted", "data": AccountDeleted } | { "kind": "report_filed", "data": ReportFiled } | { "kind": "email_bounced", "data": EmailBounced } | { "kind": "action_outcome", "data": ActionOutcome } | { "kind": "login", "data": Login } | { "kind": "session_started", "data": SessionStarted } | { "kind": "guild_joined", "data": GuildJoined } | { "kind": "dm_opened", "data": DmOpened } | { "kind": "message_created", "data": MessageCreated } | { "kind": "message_updated", "data": MessageUpdated } | { "kind": "friend_request", "data": FriendRequest } | { "kind": "http_errors", "data": HttpErrors } | { "kind": "user_blocked", "data": UserBlocked });
export type Body = { "kind": "registration", "data": Registration } | { "kind": "email_changed", "data": EmailChanged } | { "kind": "profile_updated", "data": ProfileUpdated } | { "kind": "account_changed", "data": AccountChanged } | { "kind": "admin_action", "data": AdminAction } | { "kind": "account_deleted", "data": AccountDeleted } | { "kind": "report_filed", "data": ReportFiled } | { "kind": "email_bounced", "data": EmailBounced } | { "kind": "action_outcome", "data": ActionOutcome } | { "kind": "login", "data": Login } | { "kind": "session_started", "data": SessionStarted } | { "kind": "guild_joined", "data": GuildJoined } | { "kind": "dm_opened", "data": DmOpened } | { "kind": "message_created", "data": MessageCreated } | { "kind": "message_updated", "data": MessageUpdated } | { "kind": "friend_request", "data": FriendRequest } | { "kind": "http_errors", "data": HttpErrors } | { "kind": "user_blocked", "data": UserBlocked };
export type Registration = { user_id: Id, method: RegMethod, email: string | null, username: string, username_user_chosen: boolean, global_name: string | null, locale: string | null, timezone: string | null, invite_code: string | null, suspicious_flags: number, flags: Flags64, };
export type RegMethod = "password" | "unclaimed" | "oauth" | "other";
export type EmailChanged = { user_id: Id, new_email: string, was_unclaimed: boolean, has_ever_purchased: boolean, suspicious_flags: number, suspicious_flags_before: number | null, };
export type ProfileUpdated = { user_id: Id, username: string | null, global_name: string | null, bio: string | null, avatar_hash: string | null, pronouns: string | null, };
export type AccountChanged = { user_id: Id, source: ChangeSource, flags: Flags64, flags_before: Flags64, suspicious_flags: number, suspicious_flags_before: number, has_verified_phone: boolean, temp_banned_until_ms: number | null, pending_deletion_at_ms: number | null, deletion_reason_code: number | null, };
export type ChangeSource = "registration" | "email_verify" | "phone_verify" | "admin" | "bulk" | "session_start" | "action" | "deletion" | "login" | "other";
export type AdminAction = { admin_id: Id, target_id: Id, action: AdminActionKind, reason_code: number | null, duration_h: number | null, ips: Array<string>, };
export type AdminActionKind = "update_flags" | "set_suspicious_flags" | "disable_suspicious" | "temp_ban" | "unban" | "schedule_deletion" | "cancel_deletion" | "set_phone_verified";
export type AccountDeleted = { user_id: Id, };
export type ReportFiled = { report_id: Id, reporter_id: Id, category: string, target_type: ReportTarget, reported_user_id: Id | null, guild_id: Id | null, message_id: Id | null, channel_id: Id | null, };
export type ReportTarget = "message" | "user" | "guild" | "dsa";
export type EmailBounced = { user_id: Id, kind: BounceKind, provider_event_id: string, email_domain: string, };
export type BounceKind = "hard" | "complaint" | "soft";
export type PhoneMethod = "outbound" | "inbound";
export type Login = { user_id: Id, ok: boolean, failure: string | null, mfa: boolean, new_ip: boolean, };
export type SessionStarted = { user_id: Id, is_bot: boolean, has_verified_phone: boolean, flags: Flags64, suspicious_flags: number, has_ever_paid: boolean, };
export type GuildJoined = { user_id: Id, guild_id: Id, member_count: number, discoverable: boolean, invite_code: string | null, inviter_id: Id | null, join_source: JoinSource, };
export type JoinSource = "creator" | "invite" | "vanity" | "bot_invite" | "admin_force_add" | "discovery" | "other";
export type DmOpened = { user_id: Id, recipient_id: Id, channel_id: Id, recipient_is_friend: boolean, delivered: boolean, };
export type MessageCreated = { user_id: Id, message_id: Id, channel_id: Id, channel_type: ChannelType, guild_id: Id | null, dm_recipient_id: Id | null, recipient_is_friend: boolean, channel_prior_messages: boolean, is_bot: boolean, content: string, attachment_count: number, attachment_names: Array<string>, attachments: Array<AttachmentMeta>, link_domains: Array<string>, invite_codes: Array<string>, invite_guild_ids: Array<Id | null>, invite_guild_owner_ids: Array<Id | null>, mention_user_ids: Array<Id>, mentions_recipient: boolean, mention_everyone: boolean, author_owns_guild: boolean, guild_member_count: number | null, delivered: boolean, };
export type AttachmentMeta = { size: number, content_type: string | null, hash: string | null, };
export type MessageUpdated = MessageCreated;
export type ChannelType = "dm" | "group_dm" | "guild";
export type FriendRequest = { user_id: Id, target_id: Id, delivered: boolean, };
export type UserBlocked = { blocker_id: Id, blocked_id: Id, };
export type HttpErrors = { ip: string, window_ms: number, s401: number, s403: number, s404: number, s429: number, other_4xx: number, auth_failures: number, token_hashes: Array<string>, };
export type ActionEnvelope = { v: number, id: string, key: string, issued_at_ms: number, expires_at_ms: number, } & ({ "type": "set_suspicious_flags", user_id: Id, set: number, clear: number, if_current: number | null, } | { "type": "phone_verified", user_id: Id, method: PhoneMethod, clear_suspicious: number, } | { "type": "temp_ban_ip", ip: string, until_ms: number, } | { "type": "limit_new_conversations", user_id: Id, on: boolean, until_ms: number, });
export type Action = { "type": "set_suspicious_flags", user_id: Id, set: number, clear: number, if_current: number | null, } | { "type": "phone_verified", user_id: Id, method: PhoneMethod, clear_suspicious: number, } | { "type": "temp_ban_ip", ip: string, until_ms: number, } | { "type": "limit_new_conversations", user_id: Id, on: boolean, until_ms: number, };
export type ActionOutcome = { action_id: string, action_type: string, status: OutcomeStatus, detail: string | null, observed: Observed | null, user_id?: Id, };
export type OutcomeStatus = "applied" | "noop" | "conflict" | "expired" | "ineligible" | "exempt" | "unsupported" | "failed";
export type Observed = { flags: Flags64, suspicious_flags: number, has_verified_phone: boolean, deleted: boolean, };
export type StartReq = { v: number, user_id: Id, user_flags: Flags64, has_verified_phone: boolean, phone: string, requested_channel: PhoneChannel | null, client_ip: string, captcha_passed: boolean, };
export type PhoneChannel = "sms" | "inbound_challenge";
export type StartReply = { "result": "sms_sent" } | { "result": "inbound_challenge" } & Challenge | { "result": "error" } & PhoneError;
export type Challenge = { challenge_code: string, our_number: string, expires_at_ms: number, };
export type CheckReq = { v: number, user_id: Id, user_flags: Flags64, phone: string, code: string, client_ip: string, };
export type CheckReply = { "result": "verified", action: ActionEnvelope, } | { "result": "error" } & PhoneError;
export type ChallengeReq = { v: number, user_id: Id, };
export type ChallengeReply = { "result": "inbound_challenge" } & Challenge | { "result": "error" } & PhoneError;
export type PhoneError = { code: PhoneErrorCode, retry_after_s: number | null, rate_limit_scope: RateLimitScope | null, limit: number | null, message: string | null, };
export type PhoneErrorCode = "invalid_phone" | "country_not_supported" | "lookup_unavailable" | "not_in_service" | "not_mobile" | "needs_review" | "inbound_required" | "already_used" | "captcha_required" | "invalid_code" | "rate_limited" | "unavailable" | "deadline_exceeded" | "unsupported_contract";
export type RateLimitScope = "user" | "shared";
export type InboundSms = { v: number, received_at_ms: number, signature: string, body: string, };

export const CONTRACT_VERSION = 1;
export const EVENTS_STREAM = 'EVENTS';
export const ACTIONS_STREAM = 'ACTIONS';
export const ACTION_PARTITIONS = 64;
export const MAX_ACTION_ATTEMPTS = 10;
export const LISTS_BUCKET = 'lists';
export const SHARED_LISTS = ['ip_blocked', 'email_tld_blocked', 'email_domain_exempt'] as const;
export const PHONE_INBOUND_SUBJECT = 'phone.inbound';
export const RPC_PHONE_START = 'rpc.phone.v1.start';
export const RPC_PHONE_CHECK = 'rpc.phone.v1.check';
export const RPC_PHONE_CHALLENGE = 'rpc.phone.v1.challenge';
export const PHONE_RPC_VERSION = 1;
export const PHONE_DEADLINE_MS = {start: 12000, check: 10000, challenge: 3000} as const;
export const PHONE_RETRY_MIN_REMAINING_MS = 4000;
export const PHONE_CLIENT_TIMEOUT_SLACK_MS = 250;
export const HEADER = {
	msgId: 'Nats-Msg-Id',
	ttl: 'Nats-TTL',
	spooled: 'Fluxer-Spooled',
	deadline: 'Fluxer-Deadline',
	idempotencyKey: 'Fluxer-Idempotency-Key',
	requestId: 'Fluxer-Request-Id',
	twilioSignature: 'X-Twilio-Signature',
} as const;
export type EventKind = Kind;
export const EVENT_KINDS: ReadonlyArray<EventKind> = [
	'registration',
	'email_changed',
	'profile_updated',
	'account_changed',
	'admin_action',
	'account_deleted',
	'report_filed',
	'email_bounced',
	'action_outcome',
	'login',
	'session_started',
	'guild_joined',
	'dm_opened',
	'message_created',
	'message_updated',
	'friend_request',
	'http_errors',
	'user_blocked',
];
export const EVENT_MAJOR: Record<EventKind, number> = {
	registration: 1,
	email_changed: 1,
	profile_updated: 1,
	account_changed: 1,
	admin_action: 1,
	account_deleted: 1,
	report_filed: 1,
	email_bounced: 1,
	action_outcome: 1,
	login: 1,
	session_started: 1,
	guild_joined: 1,
	dm_opened: 1,
	message_created: 1,
	message_updated: 1,
	friend_request: 1,
	http_errors: 1,
	user_blocked: 1,
};
export const EVENT_TTL: Record<EventKind, string> = {
	registration: 'never',
	email_changed: 'never',
	profile_updated: '3024000',
	account_changed: 'never',
	admin_action: 'never',
	account_deleted: 'never',
	report_filed: 'never',
	email_bounced: 'never',
	action_outcome: 'never',
	login: '3024000',
	session_started: '3024000',
	guild_joined: '3024000',
	dm_opened: '259200',
	message_created: '259200',
	message_updated: '259200',
	friend_request: '259200',
	http_errors: '3600',
	user_blocked: '259200',
};
export const EVENT_CLASS: Record<EventKind, 'fact' | 'signal'> = {
	registration: 'fact',
	email_changed: 'fact',
	profile_updated: 'fact',
	account_changed: 'fact',
	admin_action: 'fact',
	account_deleted: 'fact',
	report_filed: 'fact',
	email_bounced: 'fact',
	action_outcome: 'fact',
	login: 'signal',
	session_started: 'signal',
	guild_joined: 'signal',
	dm_opened: 'signal',
	message_created: 'signal',
	message_updated: 'signal',
	friend_request: 'signal',
	http_errors: 'signal',
	user_blocked: 'signal',
};
export const effectsConsumer = (p: number): string => `effects-${String(p).padStart(2, '0')}`;
export function keyToken(key: string): string {
	if (/^\d+$/.test(key)) return key;
	return createHash('sha256').update(key).digest('hex').slice(0, 16);
}
export const eventSubject = (kind: EventKind, key: string): string => `evt.in.${kind}.${keyToken(key)}`;
