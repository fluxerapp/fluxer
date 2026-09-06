---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway events
description: Every main Gateway Dispatch event, its payload, its delivery scope, and its replay behaviour.
---

A Dispatch is a message from the [Gateway](/gateway/overview/). It tells a client that something happened, such as a new message arriving or a member joining a guild. Every Dispatch has [Opcode](/gateway/opcodes-and-close-codes/#opcodes) 0, the name of the event, and the event's data. [Event filtering](/gateway/event-filtering/) defines the gates each one passes on its way to a socket.

## Dispatch envelope

| Field | Type | Description |
| --- | --- | --- |
| op | integer | Value 0 |
| t | string | Uppercase event name |
| s | integer | Non-negative session sequence |
| d | any | Payload defined for the named event |

```json
{
  "op": 0,
  "t": "MESSAGE_CREATE",
  "s": 17,
  "d": { }
}
```

[Ready](#ready) establishes sequence 1. Live Dispatches advance it by one. Replayed Dispatches keep their original sequence and can have gaps. [Resumed](#resumed) has the session's current sequence without advancing it and establishes the new live baseline. The sequence is local to one Gateway session and orders nothing across shards or HTTP operations.

## Dispatch delivery

A guild-scoped Dispatch is filtered by guild availability, then by channel visibility and permissions, then by the session's active or passive state, and finally by the session-level shard filter and `ignored_events` list. An account-scoped Dispatch is subject only to the session-level filters. [Event filtering](/gateway/event-filtering/) defines each gate.

Most guild-scoped Dispatches have a `guild_id` string. Three identify the guild as `id`: [Guild Create](#guild-create), [Guild Sync](#guild-sync), and every [Guild Delete](#guild-delete) other than the one the guild itself dispatches when the guild is deleted. [Guild Counts Update](#guild-counts-update) and [Channel Member Counts Update](#channel-member-counts-update) have no top-level `guild_id`, and each entry in their `counts` array has its own.

The originating session is excluded from a Dispatch only for [Message Reaction Add](#message-reaction-add) and [Message Reaction Remove](#message-reaction-remove) in a guild channel, and only when the request supplied a `session_id`. That field is removed from the payload. The same field on a direct message or group direct message reaction is forwarded to every recipient unchanged and excludes nobody. The actor that issues any other mutation receives the resulting Dispatch like every other eligible session.

A Dispatch is buffered for [Resume](/gateway/commands/#resume) replay unless it is [Guild Sync](#guild-sync), [Guild Member List Update](#guild-member-list-update), or [Guild Members Chunk](#guild-members-chunk). Those three are delivered live and never retained. A single oversized Dispatch is delivered but not retained, as [Limits and rate limits](/gateway/limits-and-rate-limits/#replay-and-backpressure) describes. [Ready](#ready) and the guild burst that follows it for a bot session are also sent outside the replay buffer. The initial [Call Create](#call-create) events are retained like any other Dispatch and are replayed on Resume.

## Dispatch events

| Event | Description | Scope |
| --- | --- | --- |
| [Ready](#ready) | The initial session state after a successful Identify | Session lifecycle |
| [Resumed](#resumed) | Retained replay completes after a successful Resume | Session lifecycle |
| [Sessions Replace](#sessions-replace) | The account's live session presence set is replaced | Current user |
| [Auth Session Change](#auth-session-change) | The account's authentication session is rotated | Current user |
| [Rate Limited](#rate-limited) | A member request is refused by its budget | Command response |
| [User Update](#user-update) | The current user's account record changes | Current user |
| [User Settings Update](#user-settings-update) | The current user's account-wide settings change | Current user |
| [User Guild Settings Update](#user-guild-settings-update) | One guild notification record changes | Current user |
| [User Note Update](#user-note-update) | The current user writes or clears a private note | Current user |
| [User Pinned DMs Update](#user-pinned-dms-update) | The current user's pinned private channel set is replaced | Current user |
| [User Connections Update](#user-connections-update) | The current user's external connection set is replaced | Current user |
| [WebAuthn Credentials Update](#webauthn-credentials-update) | The current user's WebAuthn credential set is replaced | Current user |
| [Relationship Add](#relationship-add) | The current user gains a relationship | Current user |
| [Relationship Update](#relationship-update) | One of the current user's relationships changes | Current user |
| [Relationship Remove](#relationship-remove) | The current user loses a relationship | Current user |
| [Saved Message Create](#saved-message-create) | The current user saves a message | Current user |
| [Saved Message Delete](#saved-message-delete) | The current user unsaves a message | Current user |
| [Recent Mention Delete](#recent-mention-delete) | The current user removes a recent mention | Current user |
| [Favorite Meme Create](#favorite-meme-create) | The current user saves a meme | Current user |
| [Favorite Meme Update](#favorite-meme-update) | One of the current user's memes changes | Current user |
| [Favorite Meme Delete](#favorite-meme-delete) | The current user deletes a meme | Current user |
| [Guild Create](#guild-create) | A guild becomes available to the session | Guild connection |
| [Guild Sync](#guild-sync) | A subscribed session receives a replacement guild snapshot | Guild connection |
| [Guild Update](#guild-update) | A guild's configuration changes | Guild connection |
| [Guild Delete](#guild-delete) | A guild leaves the session's visibility or becomes unavailable | Guild connection |
| [Guild Role Create](#guild-role-create) | A role is created in a guild | Guild connection |
| [Guild Role Update](#guild-role-update) | Exactly one role record changes | Guild connection |
| [Guild Role Update Bulk](#guild-role-update-bulk) | One operation changes several role records together | Guild connection |
| [Guild Role Delete](#guild-role-delete) | A role is deleted from a guild | Guild connection |
| [Guild Emojis Update](#guild-emojis-update) | A guild's emoji collection is replaced | Guild connection |
| [Guild Stickers Update](#guild-stickers-update) | A guild's sticker collection is replaced | Guild connection |
| [Channel Create](#channel-create) | A channel becomes visible to the session | Channel visibility |
| [Channel Update](#channel-update) | A visible channel changes | Channel visibility |
| [Channel Update Bulk](#channel-update-bulk) | One operation changes several channels together | Channel visibility |
| [Channel Delete](#channel-delete) | A channel leaves the session's visibility | Channel visibility |
| [Channel Recipient Add](#channel-recipient-add) | A user joins a group direct message the session belongs to | Private channel |
| [Channel Recipient Remove](#channel-recipient-remove) | A user leaves a group direct message the session belongs to | Private channel |
| [Webhooks Update](#webhooks-update) | The webhook set of a viewable guild channel changes | Channel visibility |
| [Invite Create](#invite-create) | An invite is created | Invite audience |
| [Invite Delete](#invite-delete) | An invite is deleted | Invite audience |
| [Guild Member Add](#guild-member-add) | A user becomes a member of a connected guild | Guild connection |
| [Guild Member Update](#guild-member-update) | A member's guild state or public user representation changes | Guild connection |
| [Guild Member Remove](#guild-member-remove) | A user stops being a member of a connected guild | Guild connection |
| [Guild Members Chunk](#guild-members-chunk) | A bounded member result answers Request Guild Members | Command response |
| [Guild Member List Update](#guild-member-list-update) | A subscribed member list resynchronises the subscriber's ranges | Member list subscription |
| [Guild Audit Log Entry Create](#guild-audit-log-entry-create) | An audit log entry is written in a guild | Holders of `VIEW_AUDIT_LOG` |
| [Guild Ban Add](#guild-ban-add) | A guild ban is created | Guild connection |
| [Guild Ban Remove](#guild-ban-remove) | A guild ban is removed | Guild connection |
| [Presence Update](#presence-update) | One visible presence changes | Presence subscription |
| [Presence Update Bulk](#presence-update-bulk) | A recovering guild delivers its visible presences together | Guild connection |
| [Passive Updates](#passive-updates) | Passive channel watermarks and voice states advance for one session | Passive session |
| [Message Create](#message-create) | A visible message is created | Channel visibility |
| [Message Update](#message-update) | A visible message changes and is republished in full | Message access |
| [Message Delete](#message-delete) | One visible message is deleted | Message access |
| [Message Delete Bulk](#message-delete-bulk) | Several messages in one channel are deleted together | Channel visibility |
| [Message ACK](#message-ack) | The current user's read state advances for a channel | Current user |
| [Message Reaction Add](#message-reaction-add) | A user adds a reaction to a message | Message access |
| [Message Reaction Add Many](#message-reaction-add-many) | A debouncing session receives several reaction additions as one event | Message access |
| [Message Reaction Remove](#message-reaction-remove) | One user's reaction is removed from a message | Message access |
| [Message Reaction Remove All](#message-reaction-remove-all) | Every reaction is removed from a message at once | Message access |
| [Message Reaction Remove Emoji](#message-reaction-remove-emoji) | Every reaction using one emoji is removed from a message | Message access |
| [Typing Start](#typing-start) | A visible user begins typing in a channel | Channel visibility |
| [Channel Pins Update](#channel-pins-update) | A channel's most recent pin time changes | Channel visibility |
| [Channel Pins ACK](#channel-pins-ack) | The current user acknowledges a channel's pins | Current user |
| [Voice State Update](#voice-state-update) | A guild or call participant's voice state changes | Channel visibility |
| [Voice State Ack](#voice-state-ack) | The session's own voice mutation is applied or rejected | Current session |
| [Voice Server Update](#voice-server-update) | The session receives or replaces its own voice grant | Current session |
| [Entrance Sound Play](#entrance-sound-play) | A participant's entrance sound plays in a voice channel | Voice channel |
| [Call Create](#call-create) | A DM or group DM call begins or becomes visible | Call recipient |
| [Call Update](#call-update) | The ringing set, participant roster, or region of a call changes | Call recipient |
| [Call Delete](#call-delete) | A call ends or becomes unavailable | Call recipient |
| [Guild Counts Update](#guild-counts-update) | Member and online counts are returned for connected guilds | Command response |
| [Channel Member Counts Update](#channel-member-counts-update) | Per-channel member and online counts are returned for one guild | Command response |

## Session and current user

### <span id="ready-object"></span>READY

The initial session state. Sent once after a successful [Identify](/gateway/commands/#identify), always with sequence 1.

| Field | Type | Description |
| --- | --- | --- |
| session_id | string | Identifier for [Resume](/gateway/commands/#resume) |
| version | integer | Gateway API version the connection negotiated, always `1` |
| user | [user](/http-api/users/#user-object) object | The authenticated account in its private representation |
| guilds<sup>1</sup> | array[[guild ready object](#guild-ready-object)] | The session's guilds |
| private_channels | array[[channel](/http-api/channels/#channel-object) object] | Direct message and group direct message channels |
| relationships<sup>2</sup> | array[[relationship](/http-api/users/relationships/#relationship-object) object] | The account's relationships |
| presences<sup>3</sup> | array[[presence object](#presence-object)] | Visible presences at connection time |
| users<sup>4</sup> | array[[partial user](/http-api/users/#partial-user-object) object] | Users referenced by the payload |
| sessions | array[[session presence object](#session-presence-object)] | The account's other live sessions |
| read_states | array[[read state](/http-api/read-states/#read-state-object) object] | Per-channel read state |
| user_settings | ?[user settings](/http-api/users/#user-settings-object) object | Account-wide settings |
| user_guild_settings | array[[user guild settings](/http-api/users/settings/#user-guild-settings-object) object] | Per-guild notification settings |
| notes | map[snowflake, string] | Private notes keyed by user ID |
| pinned_dms | array[snowflake] | Pinned private channel IDs |
| favorite_memes | array[[meme](/http-api/memes/#meme-object) object] | Saved memes |
| webauthn_credentials | array[[WebAuthn credential object](#webauthn-credential-object)] | Registered WebAuthn credentials |
| rtc_regions | array[[RTC region object](#rtc-region-object)] | Voice regions, ordered nearest first |
| country_code | string | Country resolved from the connecting address, `US` when the address resolves to none |
| latitude? | string | Latitude resolved from the connecting address, rendered as a decimal string |
| longitude? | string | Longitude resolved from the connecting address, rendered as a decimal string |
| auth_session_id_hash? | string | Base64url hash identifying the authentication session |
| shard? | array[integer] | The accepted `[shard_id, shard_count]` pair, present only when Identify supplied one |
| _timings? | object | HTTP-side timing breakdown, present only for a staff account |
| _timings_gw? | object | Gateway-side timing breakdown, present only for a staff account |

<sup>1</sup> A bot session always receives an empty array here and the guilds arrive as the [Guild Create](#guild-create) burst described below

<sup>2</sup> Each entry has its `user` field removed and the removed accounts appear in `users` instead, so a client resolves a relationship through the entry's `id`

<sup>3</sup> A bot session always receives an empty array here

<sup>4</sup> Collected from the account's relationships, the recipients of its private channels, and the members in `guilds`, deduplicated by account ID. A bot session always receives an empty array here

Ready is sent outside the replay buffer, so a [Resume](/gateway/commands/#resume) never replays it.

A bot session receives one [Guild Create](#guild-create) per available guild immediately after Ready, and one [Guild Delete](#guild-delete) per unavailable guild. Those Dispatches are also sent outside the replay buffer.

Shortly after Ready, every session receives one [Call Create](#call-create) for each of its private channels that has an active call.

#### Guild ready object

The same structure appears in Ready, [Guild Create](#guild-create), and [Guild Sync](#guild-sync).

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | Guild ID |
| properties | [guild](/http-api/guilds/#guild-object) object | Guild record without its roles, channels, emojis, stickers, or members |
| roles | array[[guild role](/http-api/permissions/#guild-role-object) object] | Every role in the guild |
| channels | array[[channel](/http-api/channels/#channel-object) object] | Channels the session can view |
| emojis | array[[guild emoji](/http-api/guild-emojis/#guild-emoji-object) object] | Guild emojis |
| stickers | array[[guild sticker](/http-api/guild-stickers/#guild-sticker-object) object] | Guild stickers |
| members<sup>1</sup> | array[[guild member](/http-api/guild-members/#guild-member-object) object] | The members the session needs immediately |
| member_count | integer | Total member count |
| online_count<sup>2</sup> | integer | Online member count |
| presences<sup>3</sup> | array[[presence object](#presence-object)] | Always an empty array |
| voice_states | array[[voice state object](#voice-state-object)] | Voice states in channels the session can view |
| joined_at | ?ISO8601 timestamp | When the account joined the guild, null when the account is not a member |
| unavailable? | boolean | Whether the guild is unavailable |
| unavailable_hidden? | boolean | Whether an unavailable guild is hidden from the client |

<sup>1</sup> The session's own member object plus the member object of every participant named by `voice_states`, and nothing else. A client that needs the rest of the roster asks for it with [Request Guild Members](/gateway/commands/#request-guild-members) or subscribes to a member list through [Lazy Request](/gateway/commands/#lazy-request)

<sup>2</sup> Counts the members that hold a live Gateway session and publish a status other than `offline` or `invisible`. Every recipient receives the same guild-wide figure. [Guild Counts Update](#guild-counts-update) reports a per-viewer count

<sup>3</sup> Guild presences arrive as separate [Presence Update](#presence-update) and [Presence Update Bulk](#presence-update-bulk) Dispatches

An unavailable guild is reduced to `id` and `unavailable: true`, plus `unavailable_hidden: true` when the guild is hidden. It has none of the other fields.

Inside [Ready](#ready), and inside the [Guild Create](#guild-create) burst a bot receives immediately after Ready, each member of this object has its `user` replaced by `{"id": "..."}`. On a user session the removed accounts appear in the Ready payload's `users` array. A bot's `users` array is empty, so a bot pulls those accounts with [Request Guild Members](/gateway/commands/#request-guild-members). A [Guild Create](#guild-create) sent later in the session, and every [Guild Sync](#guild-sync), have the members with `user` intact.

#### Session presence object

| Field | Type | Description |
| --- | --- | --- |
| session_id | string | Session identifier, or the literal `all` for the aggregate entry |
| status | string | `online`, `idle`, `dnd`, `invisible`, or `offline` |
| afk | boolean | Whether the session is away |
| mobile | boolean | Whether the session is mobile |

The first entry always has `session_id: "all"` and the account's flattened status.

#### WebAuthn credential object

| Field | Type | Description |
| --- | --- | --- |
| id | string | Credential ID |
| name | string | Credential name |
| created_at | ISO8601 timestamp | When the credential was registered |
| last_used_at | ?ISO8601 timestamp | When the credential was last used |

#### RTC region object

| Field | Type | Description |
| --- | --- | --- |
| id | string | Region ID |
| name | string | Human-readable region name |
| emoji | string | Region emoji |

The array always begins with a synthetic entry whose `id` is `automatic`, `name` is `Automatic`, and `emoji` is the globe. Choosing that entry leaves the region to the server. Every other entry is a region the account can select, ordered by distance from the `latitude` and `longitude` in the [Identify properties](/gateway/commands/#identify-properties-object). Fluxer orders by distance only when both values parse as finite numbers, and otherwise orders by region ID.

### RESUMED

Sent after a successful [Resume](/gateway/commands/#resume) has replayed every retained Dispatch above the supplied sequence.

| Field | Type | Description |
| --- | --- | --- |
| _timings_gw? | object | Gateway-side timing breakdown, present only for a staff account |

The payload is otherwise empty. Resumed has the session's current sequence in `s` without advancing it, and that sequence becomes the new live baseline.

### <span id="sessions-replace"></span>SESSIONS_REPLACE

The account's set of live sessions changed. The payload is a bare JSON array of [session presence objects](#session-presence-object) and replaces the client's copy in full. [Ready](#ready) sends the initial set as `sessions`.

### <span id="auth-session-change"></span>AUTH_SESSION_CHANGE

The account's authentication session was rotated, for example by a password change on another device.

| Field | Type | Description |
| --- | --- | --- |
| old_auth_session_id_hash | string | Base64url hash of the authentication session that was replaced |
| new_auth_session_id_hash | string | Base64url hash of the replacement authentication session |
| new_token | string | The token that replaces the one the client currently holds |

Every session of the account receives the event, including the one that caused the rotation. A client MUST use `new_token` for every later HTTP request and for any later [Resume](/gateway/commands/#resume) or [Identify](/gateway/commands/#identify). A client whose own `auth_session_id_hash` from [Ready](#ready) equals `old_auth_session_id_hash` MUST replace it with `new_auth_session_id_hash`.

### <span id="rate-limited"></span>RATE_LIMITED

A [Request Guild Members](/gateway/commands/#request-guild-members) command was refused by the bot full-member-list budget. Delivered to the requesting session alone.

| Field | Type | Description |
| --- | --- | --- |
| opcode | integer | The refused opcode, which is always `8` |
| retry_after | number | Seconds until the request can be retried |
| meta | object | Context for the refused request |

`meta` has `guild_id` and, when the request named exactly one guild and supplied a valid nonce, `nonce`.

That budget admits one unfiltered member request per bot account and guild every 30,000 ms, and `retry_after` is the remainder of that window expressed in seconds. Every other command refusal is silent.

### <span id="user-update"></span>USER_UPDATE

The current user's account record changed. The payload is the complete [user object](/http-api/users/#user-object) in its private representation.

Fluxer also pushes the change to every account that holds a presence subscription for this user. Those accounts receive it as a [Presence Update](#presence-update) whose `user` is the new representation, so a username or avatar change reaches them without a second lookup. Fluxer sends none of those when the user's published presence is `offline`.

### <span id="user-settings-update"></span>USER_SETTINGS_UPDATE

The current user's account-wide settings changed. The payload is the complete [user settings object](/http-api/users/#user-settings-object).

Fluxer republishes the account's presence on every settings update, whether or not `status` or `custom_status` changed. A `status` of `offline` in the payload is treated as `invisible`, and it forces every live session of the account to that status.

### <span id="user-guild-settings-update"></span>USER_GUILD_SETTINGS_UPDATE

One guild's notification settings changed. The payload is the complete user guild settings object for that guild.

### <span id="user-note-update"></span>USER_NOTE_UPDATE

The current user wrote or cleared a private note.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | User the note is about |
| note | string | Note text, empty when cleared |

### <span id="user-pinned-dms-update"></span>USER_PINNED_DMS_UPDATE

The current user's pinned private channel set changed. The payload is a bare JSON array of channel ID strings in pinned order and replaces the client's copy in full. [Ready](#ready) sends the initial set as `pinned_dms`.

### <span id="user-connections-update"></span>USER_CONNECTIONS_UPDATE

The current user's external connection set changed.

| Field | Type | Description |
| --- | --- | --- |
| connections | array[connection object] | Every connection the account holds |

The array replaces the client's copy in full.

### <span id="webauthn-credentials-update"></span>WEBAUTHN_CREDENTIALS_UPDATE

The current user's WebAuthn credential set changed. The payload is a bare JSON array of [WebAuthn credential objects](#webauthn-credential-object) and replaces the client's copy in full. [Ready](#ready) sends the initial set as `webauthn_credentials`.

### <span id="relationship-add"></span>RELATIONSHIP_ADD

The current user gained a relationship. The payload is the complete relationship object.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The other user's ID |
| type | integer | [Relationship type](/http-api/users/relationships/#relationship-types) |
| user | [partial user](/http-api/users/#partial-user-object) object | The other user |
| since? | ISO8601 timestamp | When the relationship was created, absent when the record has no timestamp |
| nickname | ?string | Private nickname for the other user |
| share_voice_activity | boolean | Whether the current user shares voice activity with this friend on the Active Now panel |
| friend_shares_voice_activity<sup>1</sup> | boolean | Whether the other user shares voice activity with the current user |

<sup>1</sup> Always `true` on this Dispatch, even for a friendship whose counterpart has sharing off

A client that needs the real value MUST read it from [List relationships](/http-api/users/relationships/#list-relationships).

### <span id="relationship-update"></span>RELATIONSHIP_UPDATE

An existing relationship changed. The payload has the same structure as [Relationship Add](#relationship-add).

`friend_shares_voice_activity` is the counterpart's real setting only on the pair of Dispatches that [Modify voice activity sharing](/http-api/users/settings/#modify-voice-activity-sharing) sends to both parties of each friendship. Every other Relationship Update sends `true`.

### <span id="relationship-remove"></span>RELATIONSHIP_REMOVE

A relationship ended.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The other user's ID |

### <span id="saved-message-create"></span>SAVED_MESSAGE_CREATE

The current user saved a message. The payload is the complete [message object](/http-api/messages/#message-object) as that user sees it.

### <span id="saved-message-delete"></span>SAVED_MESSAGE_DELETE

The current user unsaved a message.

| Field | Type | Description |
| --- | --- | --- |
| message_id | snowflake | The message that is no longer saved |

### <span id="recent-mention-delete"></span>RECENT_MENTION_DELETE

The current user removed a message from the recent mention feed.

| Field | Type | Description |
| --- | --- | --- |
| message_id | snowflake | The message removed from the feed |

### <span id="favorite-meme-create"></span>FAVORITE_MEME_CREATE

The current user saved a meme. The payload is the complete [meme object](/http-api/memes/#meme-object).

### <span id="favorite-meme-update"></span>FAVORITE_MEME_UPDATE

One of the current user's memes changed. The payload is the complete [meme object](/http-api/memes/#meme-object).

### <span id="favorite-meme-delete"></span>FAVORITE_MEME_DELETE

The current user deleted a meme.

| Field | Type | Description |
| --- | --- | --- |
| meme_id | snowflake | The deleted meme |

## Guilds and channels

### <span id="guild-create"></span>GUILD_CREATE

A guild became available to the session. The payload is a [guild ready object](#guild-ready-object).

Every collection in the event replaces the client's copy for that guild.

A user session receives Guild Create when a guild becomes available after Ready, for example after joining one or after an unavailable guild recovers. A bot session receives one for every guild in the burst that follows Ready.

### <span id="guild-sync"></span>GUILD_SYNC

A session that asked for a sync through [Lazy Request](/gateway/commands/#lazy-request) receives a replacement snapshot of the guild. The payload is a [guild ready object](#guild-ready-object) and has the same replacement semantics as [Guild Create](#guild-create).

Fluxer sends a sync when the subscription flips the guild between active and passive, and when `sync: true` names a guild the session has not already synced. A second `sync: true` for an already-synced guild sends nothing.

### <span id="guild-update"></span>GUILD_UPDATE

A guild's configuration changed. The payload is the complete [guild object](/http-api/guilds/#guild-object) with `guild_id` added, which repeats the object's own `id`.

An unavailable guild still dispatches Guild Update, and nothing else. A client learns from it that a guild entered or left the unavailable state.

### <span id="guild-delete"></span>GUILD_DELETE

A guild left the session's visibility, or became unavailable.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | Guild ID |
| guild_id?<sup>1</sup> | snowflake | Repeats `id` |
| unavailable? | boolean | True when the guild is temporarily unavailable |
| unavailable_hidden? | boolean | True when an unavailable guild is hidden from the client |

<sup>1</sup> Present only when the guild itself is deleted. The payload has `id` alone when the account leaves a guild or is removed from one, and `id` with `unavailable` in the unavailable form

Without `unavailable`, the account is no longer a member and the client discards the guild. With `unavailable: true`, the guild is retained in a placeholder state and a later [Guild Create](#guild-create) restores it.

### <span id="guild-role-create"></span>GUILD_ROLE_CREATE

A role was created in a guild.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the role belongs to |
| role | [guild role](/http-api/permissions/#guild-role-object) object | The created role |

### <span id="guild-role-update"></span>GUILD_ROLE_UPDATE

Exactly one role record changed.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the role belongs to |
| role | [guild role](/http-api/permissions/#guild-role-object) object | The role's complete updated representation |

### <span id="guild-role-update-bulk"></span>GUILD_ROLE_UPDATE_BULK

One operation changed several roles together, most often a reorder.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the roles belong to |
| roles | array[[guild role](/http-api/permissions/#guild-role-object) object] | Every changed role in its complete updated representation |

### <span id="guild-role-delete"></span>GUILD_ROLE_DELETE

A role was deleted from a guild.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the role belonged to |
| role_id | snowflake | The deleted role |

### <span id="guild-emojis-update"></span>GUILD_EMOJIS_UPDATE

A guild's emoji collection changed.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the emojis belong to |
| emojis | array[[guild emoji](/http-api/guild-emojis/#guild-emoji-object) object] | The complete emoji collection |

The array replaces the client's copy for that guild. Fluxer does not send per-emoji create, update, or delete events.

### <span id="guild-stickers-update"></span>GUILD_STICKERS_UPDATE

A guild's sticker collection changed.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the stickers belong to |
| stickers | array[[guild sticker](/http-api/guild-stickers/#guild-sticker-object) object] | The complete sticker collection |

The array replaces the client's copy for that guild. Fluxer does not send per-sticker create, update, or delete events.

### <span id="channel-create"></span>CHANNEL_CREATE

A channel became visible to the session, whether newly created or newly permitted. The payload is the complete [channel object](/http-api/channels/#channel-object), with `guild_id` present for a guild channel.

### <span id="channel-update"></span>CHANNEL_UPDATE

A visible channel changed. The payload is the complete [channel object](/http-api/channels/#channel-object).

### <span id="channel-update-bulk"></span>CHANNEL_UPDATE_BULK

One operation changed several channels together, most often a reorder.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the channels belong to |
| channels | array[[channel](/http-api/channels/#channel-object) object] | Every changed channel in its complete updated representation |

Each recipient's copy of `channels` is trimmed to the channels that recipient can view, so two sessions in the same guild can receive different arrays from one operation. A recipient whose trimmed array would be empty receives no Dispatch at all and consumes no sequence number.

### <span id="channel-delete"></span>CHANNEL_DELETE

A channel left the session's visibility, whether deleted or newly hidden. The payload is the complete [channel object](/http-api/channels/#channel-object) as it was before the change.

Recipients are the sessions that could see the channel before it was deleted.

### <span id="channel-recipient-add"></span>CHANNEL_RECIPIENT_ADD

A user joined a group direct message the session belongs to.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Group direct message channel |
| user | [partial user](/http-api/users/#partial-user-object) object | The user that joined |

### <span id="channel-recipient-remove"></span>CHANNEL_RECIPIENT_REMOVE

A user left a group direct message the session belongs to.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Group direct message channel |
| user | [partial user](/http-api/users/#partial-user-object) object | The user that left |

### <span id="webhooks-update"></span>WEBHOOKS_UPDATE

The webhook set of a guild channel changed. The event has no webhook data, so a client that needs the new set reads it over the HTTP API.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the channel belongs to |
| channel_id | snowflake | Channel whose webhooks changed |

### <span id="invite-create"></span>INVITE_CREATE

An invite was created. The payload is the complete [invite object](/http-api/invites/#invite-object) extended with its [invite metadata](/http-api/invites/#invite-metadata-object).

A guild invite reaches the sessions holding `MANAGE_CHANNELS` on the invite's channel. A group direct message invite reaches every recipient of that group, with no permission check.

### <span id="invite-delete"></span>INVITE_DELETE

An invite was deleted.

| Field | Type | Description |
| --- | --- | --- |
| code | string | The deleted invite code |
| channel_id? | snowflake | Channel the invite pointed at, absent when the invite stored none |
| guild_id?<sup>1</sup> | snowflake | Guild the invite belonged to |

<sup>1</sup> Present on every guild invite. A group direct message invite has no `guild_id`

Recipients are chosen the same way as for [Invite Create](#invite-create).

## Members and moderation

### <span id="guild-member-add"></span>GUILD_MEMBER_ADD

A user became a member of a guild the session is connected to. The payload is the complete [guild member object](/http-api/guild-members/#guild-member-object) with `guild_id` added.

### <span id="guild-member-update"></span>GUILD_MEMBER_UPDATE

A member's guild state or public user representation changed. The payload is the complete [guild member object](/http-api/guild-members/#guild-member-object) with `guild_id` added.

In a large guild, a passive session receives this event only when the subject is its own user.

### <span id="guild-member-remove"></span>GUILD_MEMBER_REMOVE

A user stopped being a member of a guild the session is connected to.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the user left |
| user<sup>1</sup> | object | The user that is no longer a member |

<sup>1</sup> The object has `id` alone. No other account field is sent, so a client MUST resolve the account from state it already holds

In a large guild, a passive session receives this event only when the subject is its own user.

### <span id="guild-members-chunk"></span>GUILD_MEMBERS_CHUNK

Answers [Request Guild Members](/gateway/commands/#request-guild-members) for the requesting session.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the members belong to |
| members | array[[guild member](/http-api/guild-members/#guild-member-object) object] | Up to 1,000 members |
| chunk_index | integer | Zero-based index of this chunk |
| chunk_count | integer | Total chunks in this response |
| presences? | array[[presence object](#presence-object)] | Present only when the request set `presences` and at least one member has a visible presence |
| nonce? | string | Echoed only when the request named exactly one guild and supplied a valid nonce |

A request that matches no member still produces one chunk with an empty `members` array, `chunk_index` 0, and `chunk_count` 1.

This event is delivered live and is never retained for [Resume](/gateway/commands/#resume) replay.

### <span id="guild-member-list-update"></span>GUILD_MEMBER_LIST_UPDATE

A member list the session subscribed to through [Lazy Request](/gateway/commands/#lazy-request) resynchronises the subscriber's ranges.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the list belongs to |
| id | string | List identifier, which is always the channel ID as a string |
| channel_id? | snowflake | Channel the list is scoped to |
| member_count | integer | Total members in the list |
| online_count | integer | Online members in the list |
| groups | array[[member list group object](#member-list-group-object)] | Group headers in list order |
| ops | array[[member list operation object](#member-list-operation-object)] | Operations to apply |

#### Member list group object

| Field | Type | Description |
| --- | --- | --- |
| id<sup>1</sup> | string | Group identifier, which is a hoisted role ID, `online`, or `offline` |
| count | integer | Members in the group |

<sup>1</sup> Hoisted role groups come first in role order, then `online`, then `offline`

A group whose count is `0` is omitted. The `offline` group is also omitted once it holds more than 1,000 members, and in that case the offline members are omitted from `items` as well. `member_count` can then exceed the number of items a client can ever read back.

#### Member list operation object

| Field | Type | Description |
| --- | --- | --- |
| op | string | Operation kind, which is always `SYNC` |
| range | array[integer] | Inclusive `[start, end]` range this operation replaces |
| items | array[[member list item object](#member-list-item-object)] | Replacement items for the range |

`SYNC` is the only operation Fluxer sends. A client MUST ignore an operation whose `op` it does not recognise and whose range fails the bounds in [Lazy Request](/gateway/commands/#lazy-request).

#### Member list item object

Each item has exactly one of the two fields.

| Field | Type | Description |
| --- | --- | --- |
| group? | [member list group object](#member-list-group-object) | A group header occupying one list position |
| member?<sup>1</sup> | [guild member](/http-api/guild-members/#guild-member-object) object | A member of the list |

<sup>1</sup> Extended with a `presence` field that always exists. It is the guild's [presence object](#presence-object) for that member when the member is visibly online to the guild, and otherwise the placeholder `{"status": "offline", "mobile": false, "afk": false}`

### <span id="guild-audit-log-entry-create"></span>GUILD_AUDIT_LOG_ENTRY_CREATE

An audit log entry was written. The payload has the shape of a [guild audit log entry object](/http-api/guild-audit-logs/#guild-audit-log-entry-object) with `guild_id` added. It always has `id`, `action_type`, `user_id`, and `target_id`. It has `reason` when the request supplied one, `options` when the entry recorded any metadata, and `changes` when at least one change survives scrubbing.

The `ip` change key is stripped from `changes`, so an entry whose only change was `ip` has no `changes` at all. A client MUST treat an absent `options` or `changes` as an empty set.

Recipients are every session in the guild that holds `VIEW_AUDIT_LOG`, including the session that performed the action.

### <span id="guild-ban-add"></span>GUILD_BAN_ADD

A guild ban was created.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the ban was created in |
| user<sup>1</sup> | object | The banned user |

<sup>1</sup> The object has `id` alone. No other account field is sent

### <span id="guild-ban-remove"></span>GUILD_BAN_REMOVE

A guild ban was removed.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the ban was removed from |
| user<sup>1</sup> | object | The unbanned user |

<sup>1</sup> The object has `id` alone. No other account field is sent

### <span id="presence-update"></span>PRESENCE_UPDATE

One visible presence changed. The payload is a [presence object](#presence-object).

A session receives a presence for a friend, for a recipient of a group direct message it belongs to, and for a guild member it subscribed to through the `members` array of a [Lazy Request](/gateway/commands/#lazy-request). A bot session holds no friend or group direct message subscription, so the guild path is the only one that reaches it.

The session holds every Presence Update from Identify until it dispatches [Ready](#ready), so a client never receives a presence before its initial state arrives. Right after Ready the session drops every held presence for a subject the Ready `presences` array already covers and releases the rest in one burst. When Ready has not been dispatched 10,000 ms after Identify, the session releases the queue anyway.

After the burst a Dispatch is held again when the session cannot place it. The session holds a guild-scoped Presence Update while it is not connected to the guild in `guild_id`. It holds an account-scoped one while the subject is neither a friend nor a recipient of a group direct message the session belongs to. A later [Relationship Add](#relationship-add), [Relationship Update](#relationship-update), [Channel Create](#channel-create), [Channel Update](#channel-update), or [Channel Recipient Add](#channel-recipient-add) naming the same account releases the held Dispatch. The hold queue keeps at most 2,048 entries and drops the oldest, so a held presence that is never placed is eventually discarded.

#### Presence object

| Field | Type | Description |
| --- | --- | --- |
| user | [partial user](/http-api/users/#partial-user-object) object | The user the presence belongs to |
| status | string | `online`, `idle`, `dnd`, or `offline` |
| mobile | boolean | Whether the user's active status comes from a mobile session |
| afk | boolean | Whether every one of the user's sessions is away |
| custom_status | ?[custom status object](#custom-status-object) | The user's custom status |
| guild_id? | snowflake | Guild context, present when the presence arrived through a guild |

An account's published `status` is the highest-precedence status across its live sessions, resolved in the order `dnd`, `online`, `idle`, `invisible`, and finally `offline` when no session is live. A session that selected `invisible`, and an account with no live session, both publish `status: "offline"`. A session that lost its transport is published as `offline` 5,000 ms later, even though it stays resumable for the rest of its 60,000 ms retention window, and a successful [Resume](/gateway/commands/#resume) republishes the status it last selected.

`mobile` is true only when the account's resolved status is `online` and at least one online session declared itself mobile. `afk` is false whenever `mobile` is true, and otherwise true only when every live session is away.

`custom_status` is suppressed to `null` whenever the published status is `offline`, so an invisible account never leaks one.

#### Custom status object

| Field | Type | Description |
| --- | --- | --- |
| text | ?string | Custom status text |
| expires_at | ?ISO8601 timestamp | When the custom status expires |
| emoji_id | ?snowflake | Custom emoji ID |
| emoji_name | ?string | Unicode emoji, or the custom emoji's name |
| emoji_animated | boolean | Whether the custom emoji is animated |

### <span id="presence-update-bulk"></span>PRESENCE_UPDATE_BULK

Several visible presences are delivered in one Dispatch. A guild sends this only when it leaves the unavailable state. Every session that stayed connected receives it immediately after the [Guild Create](#guild-create) that restores the guild.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild context applied to every entry |
| presences<sup>1</sup> | array[[presence object](#presence-object)] | The presences, at most 500 per Dispatch |

<sup>1</sup> Only visibly online presences are included, and the recipient's own presence is removed. A batch that would be empty produces no Dispatch, and a set larger than 500 is split across consecutive Dispatches

Every entry has the batch's guild context. A client MUST treat each entry as if it named `guild_id` itself.

### <span id="passive-updates"></span>PASSIVE_UPDATES

A passive session in a guild with more than 250 members receives the changes it would otherwise have missed. The guild runs the cycle every 30,000 ms and sends nothing when the cycle finds no change.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the update covers |
| channels | map[snowflake, snowflake] | Changed `last_message_id` per channel |
| voice_states?<sup>1</sup> | array[[voice state object](#voice-state-object)] | Changed voice states in channels the session can view |

<sup>1</sup> Omitted when the set would be empty. A participant that left a viewable channel since the previous cycle appears here with `channel_id` set to null

`channels` contains only the channels whose `last_message_id` changed since the previous update for that session, and only channels that session can view. `voice_states` contains only the voice states whose `version` advanced.

## Messages and reactions

### <span id="message-create"></span>MESSAGE_CREATE

A visible message was created. The payload is the complete [message object](/http-api/messages/#message-object) with the fields below added.

| Field | Type | Description |
| --- | --- | --- |
| channel_type | integer | [Channel type](/http-api/channels/#channel-types) of the channel the message was created in |
| nicks? | map[snowflake, string] | Group direct message nicknames, present only for a group direct message that stores at least one |
| mention_here? | boolean | Always `true` when present, and present only when the message has a here mention |
| guild_id? | snowflake | Guild the channel belongs to |
| member?<sup>1</sup> | [guild member](/http-api/guild-members/#guild-member-object) object | The author's guild member object, present in a guild channel |

<sup>1</sup> The `user` field is removed from it, and the account is in the message's `author`

Message Create alone overrides both the passive filter and the `ignored_events` list, and the two use different tests. The passive filter is defeated by a direct mention, a mention of one of the user's roles, an everyone mention, or a here mention. The `ignored_events` list is defeated by a direct, everyone, or here mention alone.

### <span id="message-update"></span>MESSAGE_UPDATE

A visible message changed. The payload is the complete current [message object](/http-api/messages/#message-object). In a guild channel it is extended with `guild_id` and with `member`, the author's guild member object with its `user` field removed. It has no `channel_type`, `nicks`, or `mention_here`.

A recipient must hold `READ_MESSAGE_HISTORY` on the channel, or the message must be newer than the guild's message history cutoff.

### <span id="message-delete"></span>MESSAGE_DELETE

One visible message was deleted.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The deleted message |
| channel_id | snowflake | Channel the message was in |
| content?<sup>1</sup> | ?string | Content the message held before it was deleted |
| author_id?<sup>1</sup> | snowflake | Account that wrote the message |
| guild_id? | snowflake | Guild the channel belongs to |
| member?<sup>2</sup> | [guild member](/http-api/guild-members/#guild-member-object) object | The author's guild member object, present in a guild channel |

<sup>1</sup> Both fields are omitted when the deletion came from moderation tooling, and `author_id` is also omitted for a message with no author

<sup>2</sup> The `user` field is removed from it, and the whole field is absent when `author_id` is absent or the author is no longer a member

### <span id="message-delete-bulk"></span>MESSAGE_DELETE_BULK

Several messages in one channel were deleted together.

| Field | Type | Description |
| --- | --- | --- |
| ids | array[snowflake] | The deleted messages |
| channel_id | snowflake | Channel the messages were in |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="message-ack"></span>MESSAGE_ACK

The current user's read state advanced for a channel, usually because another of the account's sessions read it.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel whose read state advanced |
| message_id | snowflake | Message the read state now points at |
| mention_count | integer | Remaining mention count for the channel |
| manual? | boolean | Whether the acknowledgement was explicit |
| version? | string | Read state version as a decimal string |

### <span id="message-reaction-add"></span>MESSAGE_REACTION_ADD

A user added a reaction to a message.

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | User that reacted |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message that was reacted to |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The reacting user's guild member object, present in a guild channel |

In a guild channel the session named by the request's `session_id` is excluded and that field is removed from the payload. In a direct message or group direct message the field is delivered as `session_id` and excludes nobody, so a client MUST tolerate receiving its own reaction back.

#### Reaction emoji object

| Field | Type | Description |
| --- | --- | --- |
| name | string | Unicode emoji, or the custom emoji's name |
| id? | snowflake | Custom emoji ID, absent for a Unicode emoji |
| animated?<sup>1</sup> | boolean | Whether the custom emoji is animated |

<sup>1</sup> Present only on the [Message Reaction Add](#message-reaction-add) that creates the first reaction with that emoji on the message. An addition to an emoji that already has a reactor, a [Message Reaction Remove](#message-reaction-remove), and a [Message Reaction Remove Emoji](#message-reaction-remove-emoji) omit the field

Neither `id` nor `animated` is ever null. A Unicode reaction omits both, so a client distinguishes the two forms by the presence of `id`. A client MUST NOT read an absent `animated` as `false`.

### <span id="message-reaction-add-many"></span>MESSAGE_REACTION_ADD_MANY

A session that set the `DEBOUNCE_MESSAGE_REACTIONS` [session flag](/gateway/commands/#session-flags) coalesces a run of reaction additions in a direct message or group direct message into one Dispatch. A reaction in a guild channel is never coalesced and arrives as its own [Message Reaction Add](#message-reaction-add). The session opens a 650 ms window on the first addition and sends the coalesced Dispatch when the window closes. The window holds at most 512 additions and drops the oldest beyond that.

| Field | Type | Description |
| --- | --- | --- |
| channel_id<sup>1</sup> | snowflake | Channel the message is in |
| message_id<sup>1</sup> | snowflake | Message that was reacted to |
| guild_id?<sup>1</sup> | snowflake | Guild the channel belongs to |
| reactions | array[[reaction addition object](#reaction-addition-object)] | The coalesced additions, in arrival order |

<sup>1</sup> Taken from the first addition in the window. The window is per session, so a run that spans several messages reports every addition under the first message's channel, message, and guild, and a client MUST read each addition's own identity from that entry

A window that closes holding exactly one addition sends [Message Reaction Add](#message-reaction-add) instead, and a session without the flag receives one Message Reaction Add per addition.

#### Reaction addition object

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | User that reacted |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The reacting user's guild member object, present in a guild channel |

### <span id="message-reaction-remove"></span>MESSAGE_REACTION_REMOVE

One user's reaction was removed from a message.

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | User whose reaction was removed |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message the reaction was on |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The user's guild member object, present in a guild channel |

Exclusion works exactly as it does for [Message Reaction Add](#message-reaction-add), so the acting session is dropped in a guild channel and kept in a private one.

For a debouncing session, a removal that matches a still-buffered addition by message, user, and emoji removes both, so neither reaches the socket.

### <span id="message-reaction-remove-all"></span>MESSAGE_REACTION_REMOVE_ALL

Every reaction was removed from a message at once.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message whose reactions were cleared |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="message-reaction-remove-emoji"></span>MESSAGE_REACTION_REMOVE_EMOJI

Every reaction using one emoji was removed from a message.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message the reactions were on |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji whose reactions were removed |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="typing-start"></span>TYPING_START

A visible user began typing in a channel.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the user is typing in |
| user_id | snowflake | User that started typing |
| timestamp | integer | Unix seconds when typing started |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The typing user's guild member object, present in a guild channel |

The `typing` override set through [Lazy Request](/gateway/commands/#lazy-request) decides delivery in a guild. With no override, a session receives the event when it is active in the guild or when the guild has 250 members or fewer, so a passive session in a small guild still receives it. A guild that sets the `TYPING_EVENTS` bit in its [disabled operations](/http-api/guilds/#disabled-guild-operations) produces the event for nobody.

### <span id="channel-pins-update"></span>CHANNEL_PINS_UPDATE

A channel's most recent pin time changed.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel whose pins changed |
| last_pin_timestamp | ?ISO8601 timestamp | Time of the most recent pin, null when nothing is pinned |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="channel-pins-ack"></span>CHANNEL_PINS_ACK

The current user acknowledged a channel's pins. Every session of the account receives it, including the one that issued the acknowledgement.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel whose pins were acknowledged |
| timestamp | ISO8601 timestamp | Time the acknowledgement recorded for the channel |

## Voice and calls

### <span id="voice-state-update"></span>VOICE_STATE_UPDATE

A participant's voice state changed. The payload is a [voice state object](#voice-state-object).

Recipients are the sessions that can view the voice channel, passive sessions included. A passive session in a large guild also receives the changed voice states through [Passive Updates](#passive-updates).

A `channel_id` of null means the participant left.

#### Voice state object

| Field | Type | Description |
| --- | --- | --- |
| guild_id | ?snowflake | Guild the voice channel belongs to, null in a call |
| channel_id | ?snowflake | Voice channel, null when the participant left |
| user_id | ?snowflake | Participant |
| connection_id | ?string | Voice connection identity |
| session_id | ?string | Gateway session that owns the connection |
| member | ?[guild member](/http-api/guild-members/#guild-member-object) object | The participant's guild member object, null in a call |
| mute | boolean | Server mute |
| deaf | boolean | Server deafen |
| self_mute | boolean | Local microphone mute |
| self_deaf | boolean | Local output deafen |
| self_video | boolean | Whether the participant publishes camera video |
| self_stream<sup>1</sup> | boolean | Whether the connection advertises a screenshare track |
| is_mobile | boolean | Whether the participant is on a mobile client |
| suppress | boolean | Whether the participant is suppressed |
| viewer_stream_keys | array[string] | Streams this connection is watching |
| e2ee_capable | boolean | Whether the participant's client supports end-to-end encrypted voice |
| version | integer | Monotonic version of this participant's voice state |

<sup>1</sup> Publisher-asserted. In a guild voice channel Fluxer sets it to false when the participant lacks `STREAM`

The broadcast form has no `region_id`, `server_id`, `latitude`, or `longitude`.

### <span id="voice-state-ack"></span>VOICE_STATE_ACK

Reports the outcome of the session's own [Voice State Update](/gateway/commands/#voice-state-update). Sent only when that command supplied `mutation_id`, and delivered to the requesting session alone.

| Field | Type | Description |
| --- | --- | --- |
| mutation_id | string | The `mutation_id` the command supplied |
| runtime_epoch<sup>1</sup> | string | The `runtime_epoch` the command supplied |
| connection_id | ?string | Voice connection the mutation applied to |
| guild_id | ?snowflake | Guild the mutation applied to |
| channel_id | ?snowflake | Channel the mutation applied to |
| status | string | `applied` or `rejected` |
| server_version | integer | The voice state version after the mutation |
| canonical_state<sup>2</sup> | [voice state object](#voice-state-object) | The authoritative voice state, an empty object when none exists |
| error_code? | string | Stable rejection code, present only when `status` is `rejected` |
| error_message? | string | Human-readable rejection message |

<sup>1</sup> Echoed back unchanged. A command that omitted the field produces the literal string `undefined` here, so a client MUST compare the value against the epoch it sent

<sup>2</sup> Also has the `region_id` and `server_id` fields a [Voice State Update](#voice-state-update) omits, and never has coordinates. Both extra fields are internal routing identity, and a client MUST NOT depend on them

A mutation whose `base_version` is more than one behind the server's current version is rejected after the connection lookup and before the permission checks, with `error_code` and `error_message` both set to `stale_base_version`.

Every other rejection has one of these codes, with `error_message` set to the registry text for the same code.

#### `VOICE_CONNECTION_NOT_FOUND`

The named voice connection does not exist and no matching pending connection could be restored.

#### `VOICE_PENDING_EXPIRED`

The pending voice connection expired before the mutation arrived.

#### `VOICE_INVALID_STATE`

A `viewer_stream_keys` entry is malformed, names another channel, or names a connection that is not in the channel.

#### `VOICE_MEMBER_TIMED_OUT`<sup>1</sup>

The member is timed out.

#### `VOICE_PERMISSION_DENIED`<sup>1</sup>

The user lacks `VIEW_CHANNEL` or `CONNECT` on the target channel.

#### `VOICE_CHANNEL_FULL`<sup>1</sup>

The channel is at its `user_limit`.

#### `VOICE_CONNECTION_LIMIT_REACHED`<sup>1</sup>

The user holds too many voice connections.

#### `VOICE_CAMERA_USER_LIMIT`

The channel already has 25 users with cameras enabled.

#### `VOICE_E2EE_REQUIRED`<sup>1</sup>

The channel is end-to-end encrypted and the client does not support it.

<sup>1</sup> Checked only when the mutation moves the connection to a different channel. A mutation that keeps the connection in its current channel skips these checks and is applied

Only a command that supplies `connection_id` can produce an ack. A command that omits it opens a new connection, and a `connection_id` that belongs to another user is rejected before any other check. A refusal of either kind produces no Dispatch. Without `mutation_id`, a refused voice state update also produces no Dispatch at all.

### <span id="voice-server-update"></span>VOICE_SERVER_UPDATE

The session received or replaced its own voice grant. Delivered to the requesting session alone.

| Field | Type | Description |
| --- | --- | --- |
| token | string | The LiveKit access token this connection presents |
| endpoint | string | The LiveKit signalling URL to connect to, a `ws://` or `wss://` address |
| connection_id | string | The voice connection the grant covers |
| channel_id | snowflake | The channel the grant covers |
| guild_id?<sup>1</sup> | snowflake | The guild the channel belongs to |
| e2ee_key?<sup>2</sup> | string | The key material the channel's end-to-end encryption uses |

<sup>1</sup> Present for a guild voice channel and absent for a call, so a client reads the scope from this field

<sup>2</sup> Present only when the channel is end-to-end encrypted

Fluxer uses LiveKit for voice media. There is no second voice websocket, no voice opcode set, and no UDP discovery step. A client opens a LiveKit connection to `endpoint`, presents `token` there, and speaks the LiveKit protocol from that point on. [Voice](/voice/) states the room naming, the participant identity, and the track sources a grant admits.

A grant is issued when a connection opens, when it moves to another channel, and when its region changes. Toggling `self_mute`, `self_video`, or `self_stream` produces no new grant. A call region change reissues one grant to each participant of the call.

### <span id="entrance-sound-play"></span>ENTRANCE_SOUND_PLAY

A participant asked for their entrance sound to play in a voice channel they are already connected to.

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | Participant whose sound plays |
| channel_id | snowflake | Voice channel |
| guild_id | ?snowflake | Guild the channel belongs to, null for a call |
| sound_id | snowflake | Entrance sound |
| hash | string | Content hash of the sound file |
| url | string | URL to fetch the sound from |
| duration_ms | integer | Sound duration in milliseconds |
| content_type | string | MIME type of the sound file |

Recipients are every other account with a voice state in that channel, one Dispatch each and at most one per account. The requesting account never receives its own sound.

### <span id="call-create"></span>CALL_CREATE

A direct message or group direct message call began, or became visible in the session's initial state.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the call is in |
| message_id | snowflake | Call message that opened the call |
| region | ?string | Voice region serving the call, null until one is chosen |
| ringing | array[snowflake] | Recipients currently being rung |
| voice_states<sup>1</sup> | array[[voice state object](#voice-state-object)] | Participants, ordered by participant ID |
| recipients?<sup>2</sup> | array[snowflake] | Every recipient of the channel |
| created_at?<sup>2</sup> | integer | Unix milliseconds when the call was opened |

<sup>1</sup> Each entry also has the `region_id` and `server_id` fields a [Voice State Update](#voice-state-update) omits. Both are internal routing identity, and a client MUST NOT depend on them

<sup>2</sup> Present only when the session pulled the call's state for itself

A session pulls the call's state for itself in two cases. The first is the Call Create it receives shortly after [Ready](#ready) for a private channel that already has a call. The second is the Call Create that reattaches the session to a call it lost, whether or not that loss produced a [Call Delete](#call-delete).

Recipients are every recipient of the channel, whether or not they joined the call. The same set receives [Call Update](#call-update) and [Call Delete](#call-delete).

### <span id="call-update"></span>CALL_UPDATE

The ringing set, participant roster, or region of an active call changed. The payload has the same structure as [Call Create](#call-create) without `recipients` and `created_at`.

A Call Update is sent only when the computed payload differs from the last one the call published, so an operation with no visible effect produces nothing.

### <span id="call-delete"></span>CALL_DELETE

A call ended, or became unavailable.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the call was in |
| unavailable?<sup>1</sup> | boolean | True when the call became unavailable |

<sup>1</sup> Absent when the call ended

With `unavailable: true` the call became unreachable. The session schedules a reattach 1,000 ms later and retries with backoff up to 15 times. A successful reattach delivers a fresh [Call Create](#call-create) with `recipients` and `created_at`, and an exhausted one delivers nothing further. A client MUST hold the call in a placeholder state.

## Count response events

### <span id="guild-counts-update"></span>GUILD_COUNTS_UPDATE

Answers [Request Guild Counts](/gateway/commands/#request-guild-counts) for the requesting session.

| Field | Type | Description |
| --- | --- | --- |
| counts | array[[guild count entry object](#guild-count-entry-object)] | One entry per guild that answered in time |
| nonce? | string | Echoed when the request supplied a valid nonce |

A guild that is not connected, or that missed its deadline, has no entry in `counts`.

#### Guild count entry object

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the counts describe |
| member_count | integer | Total members |
| online_count<sup>1</sup> | integer | Online members visible to the requesting account |

<sup>1</sup> Counts only the online members that share at least one channel the requesting account can view. An account holding `ADMINISTRATOR` receives the guild's whole online count instead, and an account that can view no channel at all receives `1` when it is itself online and `0` when it is not

### <span id="channel-member-counts-update"></span>CHANNEL_MEMBER_COUNTS_UPDATE

Answers [Request Channel Member Counts](/gateway/commands/#request-channel-member-counts) for the requesting session.

| Field | Type | Description |
| --- | --- | --- |
| counts | array[[channel count entry object](#channel-count-entry-object)] | One entry per channel that answered |
| nonce? | string | Echoed when the request supplied a valid nonce |

A channel the session cannot view, and a channel on which it lacks `VIEW_CHANNEL_MEMBERS`, has no entry in `counts`.

#### Channel count entry object

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the channel belongs to |
| channel_id | snowflake | Channel the counts describe |
| member_count | integer | Members that can view the channel |
| online_count | integer | Online members that can view the channel |

## Resource representation

Every resource object named on this page has the representation defined by the [HTTP API](/http-api/). A Dispatch payload with a resource object has the same fields, with the guild-scoped events adding `guild_id` and the message and reaction events adding `member` as documented above.

Two reductions are specific to the Gateway and appear nowhere in the HTTP API. [Ready](#ready) strips `user` from each relationship and from each guild member and hoists those accounts into its `users` array. The `member` added to a message event has its own `user` removed, and the account is in the message's `author`. A client MUST resolve those accounts from the surrounding payload.

Every Dispatch payload also drops six fields the Gateway keeps for its own indexing: `recipient_ids`, `role_index`, `channel_index`, `member_role_index`, `role_perms_cache`, and `overwrite_perms_cache`.
