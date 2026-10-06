import {createHash} from 'node:crypto';

export type Id = string;
export type Flags64 = string;
export type Channel = "stable" | "canary" | "worker" | "internal" | "import" | "other";
export type Meta = { ip: string | null, country: string | null, ua: string | null, locale: string | null, channel: Channel, request_id: string | null, };
export type Kind = "registration" | "email_changed" | "profile_updated" | "account_changed" | "admin_action" | "account_deleted" | "report_filed" | "email_bounced" | "action_outcome" | "login" | "session_started" | "guild_joined" | "dm_opened" | "message_created" | "message_updated" | "friend_request" | "http_errors" | "user_blocked" | "guild_member_banned" | "guild_member_unbanned" | "report_resolved";
export type Event = { v: number, id: string, at_ms: number, key: string, meta: Meta, } & ({ "kind": "registration", "data": Registration } | { "kind": "email_changed", "data": EmailChanged } | { "kind": "profile_updated", "data": ProfileUpdated } | { "kind": "account_changed", "data": AccountChanged } | { "kind": "admin_action", "data": AdminAction } | { "kind": "account_deleted", "data": AccountDeleted } | { "kind": "report_filed", "data": ReportFiled } | { "kind": "email_bounced", "data": EmailBounced } | { "kind": "action_outcome", "data": ActionOutcome } | { "kind": "login", "data": Login } | { "kind": "session_started", "data": SessionStarted } | { "kind": "guild_joined", "data": GuildJoined } | { "kind": "dm_opened", "data": DmOpened } | { "kind": "message_created", "data": MessageCreated } | { "kind": "message_updated", "data": MessageUpdated } | { "kind": "friend_request", "data": FriendRequest } | { "kind": "http_errors", "data": HttpErrors } | { "kind": "user_blocked", "data": UserBlocked } | { "kind": "guild_member_banned", "data": GuildMemberBanned } | { "kind": "guild_member_unbanned", "data": GuildMemberUnbanned } | { "kind": "report_resolved", "data": ReportResolved });
export type Body = { "kind": "registration", "data": Registration } | { "kind": "email_changed", "data": EmailChanged } | { "kind": "profile_updated", "data": ProfileUpdated } | { "kind": "account_changed", "data": AccountChanged } | { "kind": "admin_action", "data": AdminAction } | { "kind": "account_deleted", "data": AccountDeleted } | { "kind": "report_filed", "data": ReportFiled } | { "kind": "email_bounced", "data": EmailBounced } | { "kind": "action_outcome", "data": ActionOutcome } | { "kind": "login", "data": Login } | { "kind": "session_started", "data": SessionStarted } | { "kind": "guild_joined", "data": GuildJoined } | { "kind": "dm_opened", "data": DmOpened } | { "kind": "message_created", "data": MessageCreated } | { "kind": "message_updated", "data": MessageUpdated } | { "kind": "friend_request", "data": FriendRequest } | { "kind": "http_errors", "data": HttpErrors } | { "kind": "user_blocked", "data": UserBlocked } | { "kind": "guild_member_banned", "data": GuildMemberBanned } | { "kind": "guild_member_unbanned", "data": GuildMemberUnbanned } | { "kind": "report_resolved", "data": ReportResolved };
export type Registration = { user_id: Id, method: RegMethod, email: string | null, username: string, username_user_chosen: boolean, global_name: string | null, locale: string | null, timezone: string | null, invite_code: string | null, flags: Flags64, };
export type RegMethod = "password" | "unclaimed" | "oauth" | "other";
export type EmailChanged = { user_id: Id, new_email: string, was_unclaimed: boolean, has_ever_purchased: boolean, };
export type ProfileUpdated = { user_id: Id, username: string | null, global_name: string | null, bio: string | null, avatar_hash: string | null, pronouns: string | null, };
export type AccountChanged = { user_id: Id, source: ChangeSource, flags: Flags64, flags_before: Flags64, temp_banned_until_ms: number | null, pending_deletion_at_ms: number | null, deletion_reason_code: number | null, };
export type ChangeSource = "registration" | "email_verify" | "admin" | "bulk" | "session_start" | "action" | "deletion" | "login" | "other";
export type AdminAction = { admin_id: Id, target_id: Id, action: AdminActionKind, reason_code: number | null, duration_h: number | null, ips: Array<string>, };
export type AdminActionKind = "update_flags" | "temp_ban" | "unban" | "schedule_deletion" | "cancel_deletion";
export type AccountDeleted = { user_id: Id, };
export type ReportFiled = { report_id: Id, reporter_id: Id, category: string, target_type: ReportTarget, reported_user_id: Id | null, guild_id: Id | null, message_id: Id | null, channel_id: Id | null, };
export type ReportTarget = "message" | "user" | "guild" | "dsa";
export type EmailBounced = { user_id: Id, kind: BounceKind, provider_event_id: string, email_domain: string, };
export type BounceKind = "hard" | "complaint" | "soft";
export type Login = { user_id: Id, ok: boolean, failure: string | null, mfa: boolean, new_ip: boolean, };
export type SessionStarted = { user_id: Id, is_bot: boolean, flags: Flags64, has_ever_paid: boolean, };
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
export type GuildMemberBanned = { guild_id: Id, user_id: Id, moderator_id: Id, by: BanBy, guild_member_count: number, target_moderator: boolean, expires_at_ms: number | null, };
export type GuildMemberUnbanned = { guild_id: Id, user_id: Id, moderator_id: Id, by: BanBy, };
export type BanBy = "moderator" | "staff";
export type ReportResolved = { report_id: Id, reporter_id: Id, category: string, target_type: ReportTarget, reported_user_id: Id | null, outcome: ReportOutcome, resolved_by: ResolvedBy, };
export type ReportOutcome = "actioned" | "no_violation" | "duplicate" | "auto_resolved" | "unspecified";
export type ResolvedBy = "staff" | "system";
export type ActionEnvelope = { v: number, id: string, key: string, issued_at_ms: number, expires_at_ms: number, } & ({ "type": "set_account_limit", user_id: Id, on: boolean, } | { "type": "temp_ban_ip", ip: string, until_ms: number, } | { "type": "limit_new_conversations", user_id: Id, on: boolean, until_ms: number, } | { "type": "hide_recent_messages", user_id: Id, since_ms: number, on: boolean, } | { "type": "hide_profile", user_id: Id, on: boolean, } | { "type": "delete_user_messages", user_id: Id, on: boolean, });
export type Action = { "type": "set_account_limit", user_id: Id, on: boolean, } | { "type": "temp_ban_ip", ip: string, until_ms: number, } | { "type": "limit_new_conversations", user_id: Id, on: boolean, until_ms: number, } | { "type": "hide_recent_messages", user_id: Id, since_ms: number, on: boolean, } | { "type": "hide_profile", user_id: Id, on: boolean, } | { "type": "delete_user_messages", user_id: Id, on: boolean, };
export type ActionOutcome = { action_id: string, action_type: string, status: OutcomeStatus, detail: string | null, observed: Observed | null, user_id?: Id, };
export type OutcomeStatus = "applied" | "noop" | "conflict" | "expired" | "ineligible" | "exempt" | "unsupported" | "failed";
export type Observed = { flags: Flags64, deleted: boolean, };

export const CONTRACT_VERSION = 1;
export const EVENTS_STREAM = 'EVENTS';
export const ACTIONS_STREAM = 'ACTIONS';
export const ACTION_PARTITIONS = 64;
export const MAX_ACTION_ATTEMPTS = 10;
export const LISTS_BUCKET = 'lists';
export const SHARED_LISTS = ['ip_blocked', 'email_tld_blocked', 'email_domain_exempt'] as const;
export const HEADER = {
	msgId: 'Nats-Msg-Id',
	ttl: 'Nats-TTL',
	spooled: 'Fluxer-Spooled',
	requestId: 'Fluxer-Request-Id',
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
	'guild_member_banned',
	'guild_member_unbanned',
	'report_resolved',
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
	guild_member_banned: 1,
	guild_member_unbanned: 1,
	report_resolved: 1,
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
	guild_member_banned: '3024000',
	guild_member_unbanned: '3024000',
	report_resolved: 'never',
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
	guild_member_banned: 'fact',
	guild_member_unbanned: 'fact',
	report_resolved: 'fact',
};
export const effectsConsumer = (p: number): string => `effects-${String(p).padStart(2, '0')}`;
export function keyToken(key: string): string {
	if (/^\d+$/.test(key)) return key;
	return createHash('sha256').update(key).digest('hex').slice(0, 16);
}
export const eventSubject = (kind: EventKind, key: string): string => `evt.in.${kind}.${keyToken(key)}`;
