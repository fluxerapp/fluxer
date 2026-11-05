---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway events
description: Every Gateway Dispatch event with its payload and who receives it.
---

A Dispatch tells a [Gateway](/gateway/overview/) session that something changed or replies to one of its commands. It has [opcode](/gateway/opcodes-and-close-codes/#opcodes) 0, an event name in `t` and the payload in `d`.

## Dispatch envelope

| Field | Type | Description |
| --- | --- | --- |
| op | integer | Value 0 |
| t | string | Uppercase event name |
| s | integer | Non-negative session sequence |
| d | any | Payload defined for the event in `t` |

```json
{
  "op": 0,
  "t": "MESSAGE_CREATE",
  "s": 17,
  "d": { }
}
```

[Ready](#ready) has sequence 1 and each live Dispatch after it adds one. Replayed Dispatches keep their original sequence and can have gaps. The sequence counts Dispatches for one session and says nothing about order across shards or HTTP requests.

## Dispatch delivery

[Event filtering](/gateway/event-filtering/) decides which sessions receive each Dispatch. A user session without the [`CHANNEL_THREADS`](/gateway/threads/#session-flag) flag also gets reduced payloads and no thread events.

Most guild events have a `guild_id`. [Guild Create](#guild-create), [Guild Sync](#guild-sync) and [Guild Delete](#guild-delete) use `id`, and [Guild Counts Update](#guild-counts-update) and [Channel Member Counts Update](#channel-member-counts-update) put `guild_id` on each entry.

The session that made a change receives the Dispatch like any other session. The one exception is a guild-channel reaction, where the request's `session_id` excludes that session from [Message Reaction Add](#message-reaction-add) and [Message Reaction Remove](#message-reaction-remove).

[Resume](/gateway/commands/#resume) replays every Dispatch except [Ready](#ready) and the [Guild Create](#guild-create) and [Guild Delete](#guild-delete) burst a bot gets after Ready. These events are live only and are never replayed.

- [Guild Sync](#guild-sync)
- [Guild Member List Update](#guild-member-list-update)
- [Guild Members Chunk](#guild-members-chunk)
- [Thread List Sync](/gateway/threads/#thread-list-sync)
- [Thread Member List Update](/gateway/threads/#thread-member-list-update)

[Limits and rate limits](/gateway/limits-and-rate-limits/#replay-and-backpressure) explains when an oversized Dispatch is not kept.

## Dispatch events

| Event | Description |
| --- | --- |
| [Ready](#ready) | The initial session state after a successful Identify |
| [Resumed](#resumed) | Replay after a Resume is finished |
| [Sessions Replace](#sessions-replace) | The account's live sessions changed |
| [Auth Session Change](#auth-session-change) | A password change replaced the authentication session |
| [Rate Limited](#rate-limited) | A bot asked for a full member list too often |
| [User Update](#user-update) | The current user's account record changes |
| [User Settings Update](#user-settings-update) | The current user's account-wide settings change |
| [User Guild Settings Update](#user-guild-settings-update) | One guild notification record changes |
| [User Note Update](#user-note-update) | The current user writes or clears a private note |
| [User Pinned DMs Update](#user-pinned-dms-update) | The current user's pinned private channel set is replaced |
| [User Connections Update](#user-connections-update) | The current user's external connection set is replaced |
| [WebAuthn Credentials Update](#webauthn-credentials-update) | The current user's WebAuthn credential set is replaced |
| [Relationship Add](#relationship-add) | The current user gains a relationship |
| [Relationship Update](#relationship-update) | One of the current user's relationships changes |
| [Relationship Remove](#relationship-remove) | The current user loses a relationship |
| [Saved Message Create](#saved-message-create) | The current user saves a message |
| [Saved Message Delete](#saved-message-delete) | The current user unsaves a message |
| [Recent Mention Delete](#recent-mention-delete) | The current user removes a recent mention |
| [Favorite Meme Create](#favorite-meme-create) | The current user saves a meme |
| [Favorite Meme Update](#favorite-meme-update) | One of the current user's memes changes |
| [Favorite Meme Delete](#favorite-meme-delete) | The current user deletes a meme |
| [Guild Create](#guild-create) | A guild becomes available to the session |
| [Guild Sync](#guild-sync) | A subscribed session receives the guild's full current state again |
| [Guild Update](#guild-update) | A guild's configuration changes |
| [Guild Delete](#guild-delete) | A guild leaves the session's visibility or becomes unavailable |
| [Guild Role Create](#guild-role-create) | A role is created in a guild |
| [Guild Role Update](#guild-role-update) | One role changed |
| [Guild Role Update Bulk](#guild-role-update-bulk) | Several roles changed at once |
| [Guild Role Delete](#guild-role-delete) | A role is deleted from a guild |
| [Guild Emojis Update](#guild-emojis-update) | A guild's emojis change |
| [Guild Stickers Update](#guild-stickers-update) | A guild's stickers change |
| [Channel Create](#channel-create) | A channel becomes visible to the session |
| [Channel Update](#channel-update) | A visible channel changes |
| [Channel Update Bulk](#channel-update-bulk) | One operation changes several channels together |
| [Channel Delete](#channel-delete) | A channel leaves the session's visibility |
| [Channel Recipient Add](#channel-recipient-add) | A user joins a group direct message the session belongs to |
| [Channel Recipient Remove](#channel-recipient-remove) | A user leaves a group direct message the session belongs to |
| [Webhooks Update](#webhooks-update) | The webhook set of a viewable guild channel changes |
| [Invite Create](#invite-create) | An invite is created |
| [Invite Delete](#invite-delete) | An invite is deleted |
| [Thread Create](/gateway/threads/#thread-create) | A thread is created or the session joins one |
| [Thread Update](/gateway/threads/#thread-update) | A visible thread changes |
| [Thread Delete](/gateway/threads/#thread-delete) | A thread is deleted |
| [Thread List Sync](/gateway/threads/#thread-list-sync) | The session's active thread set is replaced |
| [Thread Member Update](/gateway/threads/#thread-member-update) | The session's own thread membership changes |
| [Thread Members Update](/gateway/threads/#thread-members-update) | Users join or leave a thread |
| [Thread Member List Update](/gateway/threads/#thread-member-list-update) | A subscribed thread member list changed |
| [Forum Unreads](/gateway/threads/#forum-unreads) | Unread counts for forum posts are returned |
| [Guild Member Add](#guild-member-add) | A user becomes a member of a connected guild |
| [Guild Member Update](#guild-member-update) | A member's guild state or public user representation changes |
| [Guild Member Remove](#guild-member-remove) | A user stops being a member of a connected guild |
| [Guild Members Chunk](#guild-members-chunk) | The answer to Request Guild Members |
| [Guild Member List Update](#guild-member-list-update) | A subscribed member list changed |
| [Guild Audit Log Entry Create](#guild-audit-log-entry-create) | An audit log entry is written in a guild |
| [Guild Ban Add](#guild-ban-add) | A guild ban is created |
| [Guild Ban Remove](#guild-ban-remove) | A guild ban is removed |
| [Presence Update](#presence-update) | One visible presence changes |
| [Presence Update Bulk](#presence-update-bulk) | Online presences of a guild that was unavailable |
| [Passive Updates](#passive-updates) | Channel `last_message_id` values or voice states changed for one passive session |
| [Message Create](#message-create) | A visible message is created |
| [Message Update](#message-update) | A message changed |
| [Message Delete](#message-delete) | One visible message is deleted |
| [Message Delete Bulk](#message-delete-bulk) | Several messages in one channel are deleted together |
| [Message ACK](#message-ack) | The current user's read state advances for a channel |
| [Message Reaction Add](#message-reaction-add) | A user adds a reaction to a message |
| [Message Reaction Add Many](#message-reaction-add-many) | Several reactions grouped into one event |
| [Message Reaction Remove](#message-reaction-remove) | One user's reaction is removed from a message |
| [Message Reaction Remove All](#message-reaction-remove-all) | Every reaction is removed from a message at once |
| [Message Reaction Remove Emoji](#message-reaction-remove-emoji) | Every reaction using one emoji is removed from a message |
| [Typing Start](#typing-start) | A visible user begins typing in a channel |
| [Channel Pins Update](#channel-pins-update) | A channel's most recent pin time changes |
| [Channel Pins ACK](#channel-pins-ack) | The current user acknowledges a channel's pins |
| [Voice State Update](#voice-state-update) | A guild or call participant's voice state changes |
| [Voice Server Update](#voice-server-update) | The session receives or replaces its own voice grant |
| [Voice Signal](#voice-signal) | A signaling message from another peer-to-peer connection |
| [Entrance Sound Play](#entrance-sound-play) | A participant's entrance sound plays in a voice channel |
| [Call Create](#call-create) | A private channel call begins or becomes visible |
| [Call Update](#call-update) | The ringing set, participant roster, or region of a call changes |
| [Call Delete](#call-delete) | A call ends or becomes unavailable |
| [Guild Counts Update](#guild-counts-update) | Member and online counts are returned for connected guilds |
| [Channel Member Counts Update](#channel-member-counts-update) | Per-channel member and online counts are returned for one guild |

## Session and current user

### <span id="ready-object"></span>READY

The session's initial state, sent once after a successful [Identify](/gateway/commands/#identify).

| Field | Type | Description |
| --- | --- | --- |
| session_id | string | Identifier for [Resume](/gateway/commands/#resume) |
| version | integer | Gateway API version the connection negotiated, always `1` |
| user | [user](/http-api/users/#user-object) object | The authenticated account in its private representation |
| guilds<sup>1</sup> | array[[guild ready object](#guild-ready-object)] | The session's guilds |
| private_channels | array[[channel](/http-api/channels/#channel-object) object] | Direct message and group direct message channels |
| relationships<sup>2</sup> | array[[relationship](/http-api/users/relationships/#relationship-object) object] | The account's relationships |
| presences<sup>3</sup> | array[[presence object](#presence-object)] | Visible presences at connection time |
| users<sup>2</sup> | array[[partial user](/http-api/users/#partial-user-object) object] | Users referenced by the payload |
| sessions | array[[session presence object](#session-presence-object)] | Every live session of the account, after a combined `all` entry |
| read_states | array[[read state](/http-api/read-states/#read-state-object) object] | Per-channel read state |
| user_settings | ?[user settings](/http-api/users/#user-settings-object) object | Account-wide settings |
| user_guild_settings | array[[user guild settings](/http-api/users/settings/#user-guild-settings-object) object] | Per-guild notification settings |
| notes | map[snowflake, string] | Private notes keyed by user ID |
| pinned_dms | array[snowflake] | Pinned private channel IDs |
| favorite_memes | array[[meme](/http-api/memes/#meme-object) object] | Saved memes |
| webauthn_credentials | array[[WebAuthn credential object](#webauthn-credential-object)] | Registered WebAuthn credentials |
| rtc_regions | array[[RTC region object](#rtc-region-object)] | Voice regions, nearest first |
| country_code | string | Country of the connecting address, `US` when unknown |
| latitude? | string | Latitude of the connecting address as a decimal string |
| longitude? | string | Longitude of the connecting address as a decimal string |
| auth_session_id_hash? | string | Base64url hash identifying the authentication session |
| shard? | array[integer] | The accepted `[shard_id, shard_count]` pair, when Identify sent one |
| _timings? | object | HTTP-side timing breakdown, only for a staff account |
| _timings_gw? | object | Gateway-side timing breakdown, only for a staff account |

<sup>1</sup> On a bot session every entry is `{"id": "...", "unavailable": true}`, and a [Guild Create](#guild-create) or [Guild Delete](#guild-delete) for each one follows Ready

<sup>2</sup> Relationships in Ready have no `user`, and each guild member's `user` has only `id`. The full accounts are in `users`. A bot gets an empty `users`, its Guild Create burst after Ready has members with the same `{"id": "..."}` user, and it fetches the accounts with [Request Guild Members](/gateway/commands/#request-guild-members). A later Guild Create and every Guild Sync have full members.

<sup>3</sup> Always empty for a bot session

Shortly after Ready, the session gets a [Call Create](#call-create) for each private channel with an active call.

#### Guild ready object

The guild state in Ready, [Guild Create](#guild-create) and [Guild Sync](#guild-sync).

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | Guild ID |
| properties | [guild](/http-api/guilds/#guild-object) object | Guild record without its roles, channels, emojis, stickers or members |
| roles | array[[guild role](/http-api/permissions/#guild-role-object) object] | Every role in the guild |
| channels | array[[channel](/http-api/channels/#channel-object) object] | Channels the session can view |
| emojis | array[[guild emoji](/http-api/guild-emojis/#guild-emoji-object) object] | Every emoji in the guild |
| stickers | array[[guild sticker](/http-api/guild-stickers/#guild-sticker-object) object] | Every sticker in the guild |
| members<sup>1</sup> | array[[guild member](/http-api/guild-members/#guild-member-object) object] | The members the session needs immediately |
| member_count | integer | Total member count |
| online_count | integer | Members with a live session and a status other than `offline` or `invisible` |
| presences<sup>2</sup> | array[[presence object](#presence-object)] | Always an empty array |
| voice_states | array[[voice state object](#voice-state-object)] | Voice states in channels the session can view |
| joined_at | ?ISO8601 timestamp | When the account joined the guild, null when it is not a member |
| unavailable? | boolean | Whether the guild is unavailable |
| visiting? | boolean | Whether the account is [visiting](/http-api/guilds/#visiting-guilds) the guild without being a member |
| unavailable_hidden? | boolean | Whether an unavailable guild is hidden from the client |
| threads?<sup>3</sup> | array[[thread](/http-api/threads/#thread-object) object] | Active threads, as [Guild threads](/gateway/threads/#guild-threads) describes |

<sup>1</sup> Only the session's own member and the members in `voice_states`. Use [Request Guild Members](/gateway/commands/#request-guild-members) or a member list from [Lazy Request](/gateway/commands/#lazy-request) for the rest

<sup>2</sup> Presences arrive as [Presence Update](#presence-update) and [Presence Update Bulk](#presence-update-bulk)

<sup>3</sup> Only for a session with [thread access](/gateway/threads/#session-flag)

An unavailable guild has only `id`, `unavailable: true` and, when hidden, `unavailable_hidden: true`.

#### Session presence object

| Field | Type | Description |
| --- | --- | --- |
| session_id | string | Session identifier, or `all` for the combined entry |
| status | string | `online`, `idle`, `dnd`, `invisible` or `offline` |
| afk | boolean | Whether the session is away |
| mobile | boolean | Whether the session is mobile |

The first entry is always `all`, with the account's combined status.

#### WebAuthn credential object

| Field | Type | Description |
| --- | --- | --- |
| id | string | Credential ID |
| name | string | Credential name |
| created_at | ISO8601 timestamp | When the credential was registered |
| last_used_at | ?ISO8601 timestamp | When the credential was last used |
| rp_id | string | The domain the passkey was created for, as in the [WebAuthn credential object](/http-api/users/mfa/#webauthn-credential-object) |

#### RTC region object

| Field | Type | Description |
| --- | --- | --- |
| id | string | Region ID |
| name | string | Human-readable region name |
| emoji | string | Region emoji |

The first entry is always `automatic`, which lets Fluxer pick the region. The rest are sorted by distance from the `latitude` and `longitude` in the [Identify properties](/gateway/commands/#identify-properties-object), or by ID when those are missing.

### RESUMED

Sent after a successful [Resume](/gateway/commands/#resume) has replayed the missed Dispatches. Its `s` is the current sequence and the next live Dispatch adds one.

| Field | Type | Description |
| --- | --- | --- |
| _timings_gw? | object | Gateway-side timing breakdown, only for a staff account |

### <span id="sessions-replace"></span>SESSIONS_REPLACE

The account's live sessions changed. The payload is an array of every [session presence object](#session-presence-object), and it replaces the `sessions` list from [Ready](#ready).

### <span id="auth-session-change"></span>AUTH_SESSION_CHANGE

A password change replaced the account's authentication session.

| Field | Type | Description |
| --- | --- | --- |
| old_auth_session_id_hash | string | Base64url hash of the replaced authentication session |
| new_auth_session_id_hash | string | Base64url hash of the new authentication session |

Fluxer closes the Gateway sessions of the old authentication session before it sends this event, and they never receive it.

### <span id="rate-limited"></span>RATE_LIMITED

A bot sent [Request Guild Members](/gateway/commands/#request-guild-members) for the full member list of a guild more than once in 30 seconds. Only the requesting session receives it.

| Field | Type | Description |
| --- | --- | --- |
| opcode | integer | The refused opcode, always `8` |
| retry_after | number | Seconds until you can retry |
| meta | object | `guild_id`, plus `nonce` when the request had one |

### <span id="user-update"></span>USER_UPDATE

The current user's account changed. The payload is the [user object](/http-api/users/#user-object) in its private representation.

### <span id="user-settings-update"></span>USER_SETTINGS_UPDATE

The current user's settings changed. The payload is the [user settings object](/http-api/users/#user-settings-object).

### <span id="user-guild-settings-update"></span>USER_GUILD_SETTINGS_UPDATE

The notification settings for one guild changed. The payload is that guild's [user guild settings object](/http-api/users/settings/#user-guild-settings-object).

### <span id="user-note-update"></span>USER_NOTE_UPDATE

The current user wrote or cleared a private note.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | User the note is about |
| note | string | Note text, empty when cleared |

### <span id="user-pinned-dms-update"></span>USER_PINNED_DMS_UPDATE

The pinned private channels changed. The payload is an array of every pinned channel ID in order, and it replaces `pinned_dms` from [Ready](#ready).

### <span id="user-connections-update"></span>USER_CONNECTIONS_UPDATE

The current user's external connections changed.

| Field | Type | Description |
| --- | --- | --- |
| connections | array[connection object] | Every connection on the account |

### <span id="webauthn-credentials-update"></span>WEBAUTHN_CREDENTIALS_UPDATE

The current user's passkeys changed. The payload is an array of every [WebAuthn credential object](#webauthn-credential-object), and it replaces `webauthn_credentials` from [Ready](#ready).

### <span id="relationship-add"></span>RELATIONSHIP_ADD

The current user has a new relationship.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The other user's ID |
| type | integer | [Relationship type](/http-api/users/relationships/#relationship-types) |
| user | [partial user](/http-api/users/#partial-user-object) object | The other user |
| since? | ISO8601 timestamp | When the relationship was created |
| nickname | ?string | Private nickname for the other user |
| share_voice_activity | boolean | Whether the current user shares voice activity with this friend |
| friend_shares_voice_activity<sup>1</sup> | boolean | Whether the other user shares voice activity with the current user |

<sup>1</sup> Only accurate in a Relationship Update sent by [Modify voice activity sharing](/http-api/users/settings/#modify-voice-activity-sharing). [List relationships](/http-api/users/relationships/#list-relationships) always has the real value.

### <span id="relationship-update"></span>RELATIONSHIP_UPDATE

A relationship changed. The payload has the same fields as [Relationship Add](#relationship-add).

### <span id="relationship-remove"></span>RELATIONSHIP_REMOVE

A relationship ended.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The other user's ID |

### <span id="saved-message-create"></span>SAVED_MESSAGE_CREATE

The current user saved a message. The payload is the [message object](/http-api/messages/#message-object).

### <span id="saved-message-delete"></span>SAVED_MESSAGE_DELETE

The current user unsaved a message.

| Field | Type | Description |
| --- | --- | --- |
| message_id | snowflake | The message that is no longer saved |

### <span id="recent-mention-delete"></span>RECENT_MENTION_DELETE

The current user removed a message from their recent mentions.

| Field | Type | Description |
| --- | --- | --- |
| message_id | snowflake | The message removed from recent mentions |

### <span id="favorite-meme-create"></span>FAVORITE_MEME_CREATE

The current user saved a meme. The payload is the [meme object](/http-api/memes/#meme-object).

### <span id="favorite-meme-update"></span>FAVORITE_MEME_UPDATE

One of the current user's memes changed. The payload is the [meme object](/http-api/memes/#meme-object).

### <span id="favorite-meme-delete"></span>FAVORITE_MEME_DELETE

The current user deleted a meme.

| Field | Type | Description |
| --- | --- | --- |
| meme_id | snowflake | The deleted meme |

## Guilds and channels

### <span id="guild-create"></span>GUILD_CREATE

A guild became available to the session, for example after joining it or when an unavailable guild recovers. The payload is a [guild ready object](#guild-ready-object).

Replace your stored `roles`, `channels`, `emojis`, `stickers`, `voice_states` and `threads` for the guild with the new arrays. `members` is partial, and you add or update those members and keep the others.

A later Guild Create without `visiting` means the account joined.

### <span id="guild-sync"></span>GUILD_SYNC

The guild's full state again, sent after a [Lazy Request](/gateway/commands/#lazy-request) asks for a sync or switches the guild between [active and passive](/gateway/event-filtering/#active-and-passive-guilds). The payload is a [guild ready object](#guild-ready-object), handled like [Guild Create](#guild-create).

### <span id="guild-update"></span>GUILD_UPDATE

A guild's settings changed. The payload is the [guild object](/http-api/guilds/#guild-object) with `guild_id` added. An unavailable guild sends no other event.

### <span id="guild-delete"></span>GUILD_DELETE

The session lost a guild, or the guild became unavailable.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | Guild ID |
| guild_id?<sup>1</sup> | snowflake | Repeats `id` |
| unavailable? | boolean | True when the guild is temporarily unavailable |
| unavailable_hidden? | boolean | True when an unavailable guild is hidden from the client |

<sup>1</sup> Only when the guild itself was deleted

Without `unavailable`, delete everything you store for the guild. With `unavailable: true`, keep it as unavailable until a [Guild Create](#guild-create) arrives.

### <span id="guild-role-create"></span>GUILD_ROLE_CREATE

A role was created.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the role belongs to |
| role | [guild role](/http-api/permissions/#guild-role-object) object | The created role |

### <span id="guild-role-update"></span>GUILD_ROLE_UPDATE

One role changed.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the role belongs to |
| role | [guild role](/http-api/permissions/#guild-role-object) object | The updated role |

### <span id="guild-role-update-bulk"></span>GUILD_ROLE_UPDATE_BULK

Several roles changed at once, usually in a reorder.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the roles belong to |
| roles | array[[guild role](/http-api/permissions/#guild-role-object) object] | Every changed role |

### <span id="guild-role-delete"></span>GUILD_ROLE_DELETE

A role was deleted.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the role belonged to |
| role_id | snowflake | The deleted role |

### <span id="guild-emojis-update"></span>GUILD_EMOJIS_UPDATE

A guild's emojis changed. Replace your stored emojis with the array.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the emojis belong to |
| emojis | array[[guild emoji](/http-api/guild-emojis/#guild-emoji-object) object] | Every emoji in the guild |

### <span id="guild-stickers-update"></span>GUILD_STICKERS_UPDATE

A guild's stickers changed. Replace your stored stickers with the array.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the stickers belong to |
| stickers | array[[guild sticker](/http-api/guild-stickers/#guild-sticker-object) object] | Every sticker in the guild |

### <span id="channel-create"></span>CHANNEL_CREATE

A channel was created or the session can see it for the first time. The payload is the [channel object](/http-api/channels/#channel-object).

### <span id="channel-update"></span>CHANNEL_UPDATE

A visible channel changed. The payload is the [channel object](/http-api/channels/#channel-object).

### <span id="channel-update-bulk"></span>CHANNEL_UPDATE_BULK

Several channels changed at once, usually in a reorder. Each session gets only the channels it can view.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the channels belong to |
| channels | array[[channel](/http-api/channels/#channel-object) object] | Every changed channel |

### <span id="channel-delete"></span>CHANNEL_DELETE

A channel was deleted or the session can no longer see it. The payload is the [channel object](/http-api/channels/#channel-object) as it was before. Remove every thread whose `parent_id` is the channel yourself, because Fluxer sends no [Thread Delete](/gateway/threads/#thread-delete) for them.

### <span id="channel-recipient-add"></span>CHANNEL_RECIPIENT_ADD

A user joined a group direct message the session is in.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Group direct message channel |
| user | [partial user](/http-api/users/#partial-user-object) object | The user that joined |

### <span id="channel-recipient-remove"></span>CHANNEL_RECIPIENT_REMOVE

A user left a group direct message the session is in.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Group direct message channel |
| user | [partial user](/http-api/users/#partial-user-object) object | The user that left |

### <span id="webhooks-update"></span>WEBHOOKS_UPDATE

A guild channel's webhooks changed, including when a channel follow starts or ends. The event has no webhook data. Fetch the list with [List channel webhooks](/http-api/webhooks/#list-channel-webhooks).

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the channel belongs to |
| channel_id | snowflake | Channel whose webhooks changed |

### <span id="invite-create"></span>INVITE_CREATE

An invite was created. The payload is the [invite object](/http-api/invites/#invite-object) with its [invite metadata](/http-api/invites/#invite-metadata-object). A guild invite goes to sessions with `MANAGE_CHANNELS` on its channel, and a group direct message invite goes to every recipient.

### <span id="invite-delete"></span>INVITE_DELETE

An invite was deleted. It goes to the same sessions as [Invite Create](#invite-create).

| Field | Type | Description |
| --- | --- | --- |
| code | string | The deleted invite code |
| channel_id? | snowflake | Channel the invite pointed at |
| guild_id? | snowflake | Guild the invite belonged to, absent for a group direct message invite |

## Members and moderation

### <span id="guild-member-add"></span>GUILD_MEMBER_ADD

A user joined a guild. The payload is the [guild member object](/http-api/guild-members/#guild-member-object) with `guild_id` added.

### <span id="guild-member-update"></span>GUILD_MEMBER_UPDATE

A member or their public profile changed. The payload is the [guild member object](/http-api/guild-members/#guild-member-object) with `guild_id` added.

### <span id="guild-member-remove"></span>GUILD_MEMBER_REMOVE

A user is no longer a member of a guild. Fluxer also sends a [Thread Members Update](/gateway/threads/#thread-members-update) for each thread they were in.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the user left |
| user | object | The user, with `id` only |

### <span id="guild-members-chunk"></span>GUILD_MEMBERS_CHUNK

The answer to [Request Guild Members](/gateway/commands/#request-guild-members). A request that matches nobody still gets one chunk with an empty `members`.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the members belong to |
| members | array[[guild member](/http-api/guild-members/#guild-member-object) object] | Up to 1,000 members |
| chunk_index | integer | Zero-based index of this chunk |
| chunk_count | integer | Total chunks in this response |
| presences? | array[[presence object](#presence-object)] | Visible presences, when the request set `presences` |
| nonce? | string | The request's nonce, when it asked for one guild |

### <span id="guild-member-list-update"></span>GUILD_MEMBER_LIST_UPDATE

A member list the session subscribed to with [Lazy Request](/gateway/commands/#lazy-request) changed, and the event resends the subscribed ranges.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the list belongs to |
| id | string | List identifier, which is the channel ID |
| channel_id? | snowflake | Channel the list is scoped to |
| member_count | integer | Total members in the list |
| online_count | integer | Online members in the list |
| groups | array[[member list group object](#member-list-group-object)] | Group headers in list order |
| ops | array[[member list operation object](#member-list-operation-object)] | Operations to apply |

#### Member list group object

| Field | Type | Description |
| --- | --- | --- |
| id | string | A hoisted role ID, `online` or `offline` |
| count | integer | Members in the group |

Hoisted roles come first, then `online`, then `offline`. Empty groups are left out, and so is the `offline` group and its members when it has more than 1,000 members.

#### Member list operation object

| Field | Type | Description |
| --- | --- | --- |
| op | string | Operation kind, always `SYNC` |
| range | array[integer] | Inclusive `[start, end]` range this operation replaces |
| items | array[[member list item object](#member-list-item-object)] | Replacement items for the range |

#### Member list item object

Each item has exactly one of these fields.

| Field | Type | Description |
| --- | --- | --- |
| group? | [member list group object](#member-list-group-object) | A group header in one list position |
| member?<sup>1</sup> | [guild member](/http-api/guild-members/#guild-member-object) object | A member of the list |

<sup>1</sup> With a `presence` field, which is `{"status": "offline", "mobile": false, "afk": false}` when the member is not visibly online

### <span id="guild-audit-log-entry-create"></span>GUILD_AUDIT_LOG_ENTRY_CREATE

An audit log entry was written. Every session with `VIEW_AUDIT_LOG` gets it. The payload is the [audit log entry](/http-api/guild-audit-logs/#guild-audit-log-entry-object) with `guild_id` added and any `ip` entry left out of `changes`. Treat a missing `options` or `changes` as empty.

### <span id="guild-ban-add"></span>GUILD_BAN_ADD

A user was banned.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the ban was created in |
| user | object | The banned user, with `id` only |

### <span id="guild-ban-remove"></span>GUILD_BAN_REMOVE

A ban was removed.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the ban was removed from |
| user | object | The unbanned user, with `id` only |

### <span id="presence-update"></span>PRESENCE_UPDATE

A visible presence changed. The payload is a [presence object](#presence-object). You get presences for friends, group direct message recipients and the guild members you name in a [Lazy Request](/gateway/commands/#lazy-request), as [Presence subscriptions](/gateway/event-filtering/#presence-subscriptions) describes.

#### Presence object

| Field | Type | Description |
| --- | --- | --- |
| user | [partial user](/http-api/users/#partial-user-object) object | The user the presence belongs to |
| status | string | `online`, `idle`, `dnd` or `offline` |
| mobile | boolean | Whether the user's active status comes from a mobile session |
| afk | boolean | Whether every one of the user's sessions is away, always false when `mobile` is true |
| custom_status | ?[custom status object](#custom-status-object) | The user's custom status, null when `offline` |
| guild_id? | snowflake | Guild the presence came through |

`status` is the first of `dnd`, `online` and `idle` that any live session has. An invisible user, or one with no live session, shows as `offline`. A session that loses its connection stops counting after 5 seconds.

#### Custom status object

| Field | Type | Description |
| --- | --- | --- |
| text | ?string | Custom status text |
| expires_at | ?ISO8601 timestamp | When the custom status expires |
| emoji_id | ?snowflake | Custom emoji ID |
| emoji_name | ?string | Unicode emoji, or the custom emoji's name |
| emoji_animated | boolean | Whether the custom emoji is animated |

### <span id="presence-update-bulk"></span>PRESENCE_UPDATE_BULK

The online presences of a guild that was unavailable, sent right after the [Guild Create](#guild-create) that restores it.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the presences belong to |
| presences | array[[presence object](#presence-object)] | Up to 500 presences, split across several events when there are more |

### <span id="passive-updates"></span>PASSIVE_UPDATES

Fluxer sends this every 30 seconds, when something changed, for a guild with more than 250 members where the session is [passive](/gateway/event-filtering/#active-and-passive-guilds).

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the update is about |
| channels | map[snowflake, snowflake] | New `last_message_id` for each changed channel the session can view |
| voice_states? | array[[voice state object](#voice-state-object)] | Changed voice states, with a null `channel_id` for someone who left |

## Messages and reactions

### <span id="message-create"></span>MESSAGE_CREATE

A message was created. The payload is the [message object](/http-api/messages/#message-object) with these fields added.

| Field | Type | Description |
| --- | --- | --- |
| channel_type | integer | [Channel type](/http-api/channels/#channel-types) of the message's channel |
| nicks? | map[snowflake, string] | Group direct message nicknames, when the group has any |
| mention_here? | boolean | `true` when the message has a here mention |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The author's member object without `user`, in a guild channel |

A [published message](/http-api/messages/#crosspost-message) also arrives as Message Create in each following channel, with the `IS_CROSSPOST` flag.

### <span id="message-update"></span>MESSAGE_UPDATE

A message changed. The payload is the full [message object](/http-api/messages/#message-object) and, in a guild channel, `guild_id` and `member` as in [Message Create](#message-create). Sessions need `READ_MESSAGE_HISTORY` unless the message is newer than the guild's message history cutoff.

### <span id="message-delete"></span>MESSAGE_DELETE

A message was deleted.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The deleted message |
| channel_id | snowflake | Channel the message was in |
| content?<sup>1</sup> | ?string | The message's content before it was deleted |
| author_id?<sup>1</sup> | snowflake | Account that wrote the message |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The author's member object without `user`, when the author is still a member |

<sup>1</sup> Absent when an instance admin or Fluxer itself deleted the message

### <span id="message-delete-bulk"></span>MESSAGE_DELETE_BULK

Several messages in one channel were deleted at once.

| Field | Type | Description |
| --- | --- | --- |
| ids | array[snowflake] | The deleted messages |
| channel_id | snowflake | Channel the messages were in |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="message-ack"></span>MESSAGE_ACK

The current user's read state moved forward in a channel, usually from another of their sessions.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel whose read state moved |
| message_id | snowflake | Message the read state now points at |
| mention_count | integer | Remaining mention count for the channel |
| manual? | boolean | Whether the user marked it read by hand |
| version? | string | Read state version as a decimal string |
| flags? | integer | [Read state flags](/http-api/read-states/#read-state-flags), for a thread, forum or media channel |

### <span id="message-reaction-add"></span>MESSAGE_REACTION_ADD

A user reacted to a message.

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | User that reacted |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message that was reacted to |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The reacting user's member object, in a guild channel |
| session_id? | string | The session that made the request, in a private channel |

#### Reaction emoji object

| Field | Type | Description |
| --- | --- | --- |
| name | string | Unicode emoji, or the custom emoji's name |
| id? | snowflake | Custom emoji ID, absent for a Unicode emoji |
| animated?<sup>1</sup> | boolean | Whether the custom emoji is animated |

<sup>1</sup> Only on the first reaction with that emoji on the message. An absent `animated` does not mean `false`.

### <span id="message-reaction-add-many"></span>MESSAGE_REACTION_ADD_MANY

Reactions in private channels, grouped over 650 ms, for a session with the `DEBOUNCE_MESSAGE_REACTIONS` [session flag](/gateway/commands/#session-flags). Each window keeps only the latest 512 reactions and drops older ones. Each event covers one message. A window with a single reaction sends [Message Reaction Add](#message-reaction-add).

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message that was reacted to |
| guild_id? | snowflake | Guild the channel belongs to |
| reactions | array[[reaction addition object](#reaction-addition-object)] | The reactions, oldest first |

#### Reaction addition object

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | User that reacted |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The reacting user's member object, in a guild channel |

### <span id="message-reaction-remove"></span>MESSAGE_REACTION_REMOVE

A user's reaction was removed.

| Field | Type | Description |
| --- | --- | --- |
| user_id | snowflake | User whose reaction was removed |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message the reaction was on |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The user's member object, in a guild channel |
| session_id? | string | The session that made the request, in a private channel |

### <span id="message-reaction-remove-all"></span>MESSAGE_REACTION_REMOVE_ALL

Every reaction on a message was removed.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message whose reactions were cleared |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="message-reaction-remove-emoji"></span>MESSAGE_REACTION_REMOVE_EMOJI

Every reaction with one emoji was removed from a message.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the message is in |
| message_id | snowflake | Message the reactions were on |
| emoji | [reaction emoji object](#reaction-emoji-object) | The emoji whose reactions were removed |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="typing-start"></span>TYPING_START

A user started typing. [Typing is decided separately](/gateway/event-filtering/#typing-is-decided-separately) explains which sessions get it.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the user is typing in |
| user_id | snowflake | User that started typing |
| timestamp | integer | Unix seconds when typing started |
| guild_id? | snowflake | Guild the channel belongs to |
| member? | [guild member](/http-api/guild-members/#guild-member-object) object | The typing user's member object, in a guild channel |

### <span id="channel-pins-update"></span>CHANNEL_PINS_UPDATE

A channel's most recent pin time changed.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel whose pins changed |
| last_pin_timestamp | ?ISO8601 timestamp | Time of the most recent pin, null when nothing is pinned |
| guild_id? | snowflake | Guild the channel belongs to |

### <span id="channel-pins-ack"></span>CHANNEL_PINS_ACK

The current user marked a channel's pins as seen.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel whose pins were acknowledged |
| timestamp | ISO8601 timestamp | Time the acknowledgement recorded |

## Voice and calls

### <span id="voice-state-update"></span>VOICE_STATE_UPDATE

A participant's voice state changed. The payload is a [voice state object](#voice-state-object), and a null `channel_id` means they left.

#### Voice state object

| Field | Type | Description |
| --- | --- | --- |
| guild_id | ?snowflake | Guild the voice channel belongs to, null in a call |
| channel_id | ?snowflake | Voice channel, null when the participant left |
| user_id | ?snowflake | Participant |
| connection_id | ?string | Voice connection ID |
| session_id | ?string | Gateway session that owns the connection |
| member | ?[guild member](/http-api/guild-members/#guild-member-object) object | The participant's member object, null in a call |
| mute | boolean | Server mute |
| deaf | boolean | Server deafen |
| self_mute | boolean | Local microphone mute |
| self_deaf | boolean | Local output deafen |
| self_video | boolean | Whether the participant sends camera video |
| self_stream<sup>1</sup> | boolean | Whether the connection is sharing its screen |
| is_mobile | boolean | Whether the participant is on a mobile app |
| suppress | boolean | Whether the participant is suppressed |
| viewer_stream_keys | array[string] | Streams this connection is watching |
| e2ee_capable | boolean | Whether the participant's app supports encrypted call media |
| p2p | boolean | Whether this connection sends media [peer-to-peer](#peer-to-peer-voice) |
| version | integer | Version of this participant's voice state, which only goes up |

<sup>1</sup> Always false in a guild voice channel when the participant lacks `STREAM`

### <span id="voice-server-update"></span>VOICE_SERVER_UPDATE

The session got a voice grant, sent only to the session that asked. A LiveKit grant has `token` and `endpoint`. A peer-to-peer grant has `p2p: true` and `ice_servers`, and you follow [Peer-to-peer voice](#peer-to-peer-voice). [Voice](/voice/) has the rest.

| Field | Type | Description |
| --- | --- | --- |
| token?<sup>1</sup> | string | LiveKit access token |
| endpoint?<sup>1</sup> | string | LiveKit `ws://` or `wss://` URL |
| p2p?<sup>2</sup> | boolean | Always `true` when present |
| ice_servers?<sup>2</sup> | array[[ICE server object](#ice-server-object)] | STUN servers for the peer connections, empty when the instance has none |
| connection_id | string | The voice connection the grant is for |
| channel_id | snowflake | The channel the grant is for |
| guild_id? | snowflake | The guild the channel belongs to, absent for a call |
| e2ee_key?<sup>3</sup> | string | Room key for the channel's call media, issued by the server |

<sup>1</sup> On a LiveKit grant

<sup>2</sup> On a peer-to-peer grant

<sup>3</sup> On a LiveKit grant for a channel with encrypted call media. The server creates the key, so this isn't end-to-end encryption

A new grant comes when the connection opens, moves to another channel or changes region. A LiveKit grant for a connection that had a peer-to-peer grant means the call switched to LiveKit. Close your peer connections and connect with the new grant.

#### ICE server object

| Field | Type | Description |
| --- | --- | --- |
| urls | array[string] | The `stun:` URLs of one server |

Fluxer never gives out a TURN server, and peers that can't connect to each other directly can't call.

#### Peer-to-peer voice

A voice channel or call is peer-to-peer when every voice state in it has `p2p: true`. Each pair of connections then has its own WebRTC peer connection and no media goes through LiveKit. Peers exchange session descriptions and ICE candidates with the [Voice Signal](/gateway/commands/#voice-signal) command.

Fluxer decides each join from the channel's state and the `p2p` value in the [Voice State Update](/gateway/commands/#voice-state-update) command. A bot always counts as not sending `p2p: true`.

| Channel or call | The command sent `p2p: true` | Any other `p2p` value |
| --- | --- | --- |
| Peer-to-peer below the participant cap | Peer-to-peer join<sup>1</sup> | Refused with `VOICE_P2P_CONSENT_REQUIRED` |
| Peer-to-peer at the participant cap | Refused with `VOICE_CHANNEL_FULL` | Refused with `VOICE_P2P_CONSENT_REQUIRED` |
| Empty | Peer-to-peer join, or a LiveKit join when Fluxer declines | LiveKit join |
| Every other state | LiveKit join | LiveKit join |

<sup>1</sup> Refused with `VOICE_P2P_UNAVAILABLE` when Fluxer declines peer-to-peer for this join

A refusal arrives as a [Gateway Error](/gateway/opcodes-and-close-codes/#gateway-error) and changes nothing. On `VOICE_P2P_CONSENT_REQUIRED`, ask the user and send the join again with `p2p: true` if they agree. The participant cap is `max_participants` from the [peer-to-peer voice configuration](/admin-api/instance/#peer-to-peer-voice-configuration-object), between 2 and 4.

Fluxer never switches a call to LiveKit on its own. Only a participant sending `p2p: false` for its own connection, or a region change through [Modify call region](/http-api/calls/#modify-call-region) or [Modify channel](/http-api/channels/#modify-channel), switches it. The call stays on LiveKit until it empties. A guild voice channel with `rtc_p2p` set never switches.

### <span id="voice-signal"></span>VOICE_SIGNAL

Another connection in a [peer-to-peer](#peer-to-peer-voice) call sent this session a signaling message with the [Voice Signal](/gateway/commands/#voice-signal) command. Only the session that owns the target connection receives it. Ignore a signal whose `from` is no longer in your roster.

| Field | Type | Description |
| --- | --- | --- |
| guild_id? | snowflake | The guild the channel belongs to, absent for a call |
| channel_id | snowflake | The voice channel or call channel |
| from | string | The `connection_id` of the sender |
| user_id | snowflake | The user who owns the sending connection |
| data | object | The signaling message exactly as the sender sent it |

```json
{
  "guild_id": "1189375284394692608",
  "channel_id": "1189375284394692610",
  "from": "9a41d07c5be2f318",
  "user_id": "1189375284394692700",
  "data": {
    "type": "answer",
    "sdp": "v=0..."
  }
}
```

### <span id="entrance-sound-play"></span>ENTRANCE_SOUND_PLAY

A participant played their entrance sound in the voice channel they are in. Everyone else with a voice state in the channel receives it.

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

### <span id="call-create"></span>CALL_CREATE

A call started in a private channel, or the session learned of an active one after [Ready](#ready) or a lost connection. Every recipient of the channel receives it, and the same goes for [Call Update](#call-update) and [Call Delete](#call-delete).

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the call is in |
| message_id | snowflake | Call message that started the call |
| region | ?string | Voice region of the call, null until one is chosen |
| ringing | array[snowflake] | Recipients being rung |
| voice_states | array[[voice state object](#voice-state-object)] | Participants |
| recipients?<sup>1</sup> | array[snowflake] | Every recipient of the channel |
| created_at?<sup>1</sup> | integer | Unix milliseconds when the call started |

<sup>1</sup> Only after Ready or a lost connection

### <span id="call-update"></span>CALL_UPDATE

Who is ringing, who is in the call or the call's region changed. The payload has the same fields as [Call Create](#call-create) without `recipients` and `created_at`.

### <span id="call-delete"></span>CALL_DELETE

A call ended or became unavailable. Keep an unavailable call until a new [Call Create](#call-create) arrives.

| Field | Type | Description |
| --- | --- | --- |
| channel_id | snowflake | Channel the call was in |
| unavailable? | boolean | True when the call became unavailable, absent when it ended |

## Count response events

### <span id="guild-counts-update"></span>GUILD_COUNTS_UPDATE

The answer to [Request Guild Counts](/gateway/commands/#request-guild-counts). A guild that isn't connected or didn't respond in time has no entry.

| Field | Type | Description |
| --- | --- | --- |
| counts | array[[guild count entry object](#guild-count-entry-object)] | One entry per guild |
| nonce? | string | The request's nonce |

#### Guild count entry object

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the counts are for |
| member_count | integer | Total members |
| online_count | integer | Online members who share a channel you can view, or all of them for `ADMINISTRATOR` |

### <span id="channel-member-counts-update"></span>CHANNEL_MEMBER_COUNTS_UPDATE

The answer to [Request Channel Member Counts](/gateway/commands/#request-channel-member-counts). You only get an entry for a channel where you have `VIEW_CHANNEL_MEMBERS`.

| Field | Type | Description |
| --- | --- | --- |
| counts | array[[channel count entry object](#channel-count-entry-object)] | One entry per channel |
| nonce? | string | The request's nonce |

#### Channel count entry object

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | Guild the channel belongs to |
| channel_id | snowflake | Channel the counts are for |
| member_count | integer | Members who can view the channel |
| online_count | integer | Online members who can view the channel |
