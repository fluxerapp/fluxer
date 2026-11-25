---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Client commands
description: The Gateway commands a client sends, with their payloads, results and close behaviour.
---

Clients send commands over the [Gateway](/gateway/overview/) WebSocket as JSON, with an [opcode](/gateway/opcodes-and-close-codes/#opcodes) in `op` and the data in `d`.

## Command list

| Opcode | Command | Result |
| --- | --- | --- |
| 1 | [Heartbeat](#heartbeat) | Opcode `11` Heartbeat ACK |
| 2 | [Identify](#identify) | [Ready](/gateway/events/#ready), opcode `7` from a draining node, a close frame, or no response while the Gateway keeps the Identify waiting |
| 3 | [Presence Update](#presence-update) | None |
| 4 | [Voice State Update](#voice-state-update) | [Voice State Update](/gateway/events/#voice-state-update) and [Voice Server Update](/gateway/events/#voice-server-update) |
| 6 | [Resume](#resume) | Replayed Dispatches and [Resumed](/gateway/events/#resumed), Invalid Session, or a close frame |
| 8 | [Request Guild Members](#request-guild-members) | [Guild Members Chunk](/gateway/events/#guild-members-chunk) |
| 13 | [Voice Signal](#voice-signal) | [Voice Signal](/gateway/events/#voice-signal) for the target connection |
| 14 | [Lazy Request](#lazy-request) | [Guild Sync](/gateway/events/#guild-sync) and member list and thread list updates |
| 15 | [Request Guild Counts](#request-guild-counts) | [Guild Counts Update](/gateway/events/#guild-counts-update) |
| 16 | [Request Channel Member Counts](#request-channel-member-counts) | [Channel Member Counts Update](/gateway/events/#channel-member-counts-update) |
| 28 | [Request Forum Unreads](/gateway/threads/#request-forum-unreads) | [Forum Unreads](/gateway/threads/#forum-unreads) |

Every command counts against the [payload budgets](/gateway/limits-and-rate-limits/#connection-and-command-rate-limits).

## Malformed commands

| Payload | Close code | Reason |
| --- | --- | --- |
| A command other than Heartbeat, Identify and Resume before Identify or Resume | `4003` | `Not authenticated` |
| No Identify or Resume within one heartbeat interval of Hello | `4003` | `Not authenticated` |
| More than 64 commands between Identify and Ready, or between Resume and Resumed | `4008` | `Rate limited` |
| A frame that does not decode to an object | `4002` | `Decode failed` |
| An object with no `op` | `4002` | `Invalid payload` |
| An unknown opcode, once the session is authenticated | `4001` | `Unknown opcode` |
| A payload with `op` but no `d`, other than Identify and Resume | `4001` | `Unknown opcode` |
| Identify or Resume with no `d`, or on a session that is already authenticated | `4005` | `Already authenticated` |

Request Guild Members, Lazy Request, Request Guild Counts and Request Channel Member Counts never close the connection when they have a `d`, and they skip input they cannot parse. [Request Forum Unreads](/gateway/threads/#request-forum-unreads) closes with `4001` when its `d` is not an object.

## Heartbeat

Opcode `1` sends the last Dispatch sequence the client processed, or `null` before the first Dispatch. The connection closes with `4009` when the [heartbeat timeout](/gateway/limits-and-rate-limits/#session-lifecycle) passes with no acknowledgement.

| Field | Type | Description |
| --- | --- | --- |
| d | ?integer | The last Dispatch sequence the client processed |

```json
{
  "op": 1,
  "d": 42
}
```

Once authenticated, a `d` that is neither `null` nor an integer closes with `4007` and reason `Invalid sequence`.

## Identify

Opcode `2` authenticates and creates a new session.

| Field | Type | Description |
| --- | --- | --- |
| token | string | The account or bot token, with no HTTP authentication prefix |
| properties | [identify properties](#identify-properties-object) object | The software and capabilities of the connecting client |
| presence?<sup>1</sup> | ?[initial presence](#initial-presence-object) object | The presence to publish when the session starts |
| ignored_events?<sup>2</sup> | ?array[string] | The Dispatch event names this session does not want (at most 256) |
| flags? | integer | A non-negative [session flags](#session-flags) bitfield (default `0`) |
| initial_guild_id?<sup>3</sup> | ?snowflake | The [snowflake](/snowflakes/) of one guild the session joins as active |
| shard?<sup>4</sup> | ?array[integer] | The `[shard_id, shard_count]` pair |

<sup>1</sup> Null and an omitted key both use the account's saved status and custom status

<sup>2</sup> An ignored Message Create still arrives for a direct, `@here` or `@everyone` mention, and a dropped event uses no sequence number

<sup>3</sup> The session is [active](/gateway/event-filtering/#active-and-passive-guilds) in that guild without a [Lazy Request](#lazy-request) and gets no initial [Guild Sync](/gateway/events/#guild-sync). An invalid value is ignored

<sup>4</sup> `shard_id` is at least 0 and lower than `shard_count`, which is from 1 to 16,384

```json
{
  "op": 2,
  "d": {
    "token": "...",
    "properties": {
      "os": "Linux",
      "browser": "Fluxer Client",
      "device": "desktop"
    },
    "presence": {
      "status": "online",
      "afk": false,
      "mobile": false
    },
    "ignored_events": ["TYPING_START"],
    "flags": 2,
    "initial_guild_id": "1189375284394692608"
  }
}
```

Identify closes the connection on these faults.

| Fault | Close code | Reason |
| --- | --- | --- |
| A missing `token` or `properties`, or an invalid `properties`, `ignored_events` or `flags` | `4002` | `Invalid identify payload` |
| A `token` that isn't a string of 1 to 256 bytes with at least one visible character | `4004` | `Invalid token` |
| Malformed `shard` | `4010` | `Invalid shard` |
| A bot session with more than 2,500 guilds after the `shard` filter | `4011` | `Sharding required` |
| The backend rejects the token | `4004` | `Invalid token` |
| A non-bot account with 100 live sessions | `4008` | `Too many sessions` |
| The socket already has a session | `4005` | `Already authenticated` |

### Session flags

| Value | Name | Description |
| --- | --- | --- |
| 1 &lt;&lt; 1 | DEBOUNCE_MESSAGE_REACTIONS | Merges runs of reactions into [Message Reaction Add Many](/gateway/events/#message-reaction-add-many) |
| 1 &lt;&lt; 2 | [CHANNEL_THREADS](/gateway/threads/#session-flag) | The client can parse thread, forum and media channels |
| 1 &lt;&lt; 3 | GUILD_VISITS | The session receives the guilds the account is [visiting](/http-api/guilds/#visiting-guilds) |

Undefined bits are ignored.

### Identify properties object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| os | string | The operating system the client runs on |
| browser | string | The client library identifier |
| device | string | The device or application identifier |
| e2ee_capable?<sup>1</sup> | boolean | Whether the client can take part in voice with encrypted call media |
| mobile?<sup>2</sup> | boolean | Whether the session is mobile (default false) |
| latitude?<sup>3</sup> | number or string | The client latitude, at most 32 characters as a string |
| longitude?<sup>3</sup> | number or string | The client longitude, at most 32 characters as a string |

<sup>1</sup> A session without it gets `VOICE_E2EE_REQUIRED` when it joins a voice channel with encrypted call media

<sup>2</sup> Used only when `presence` is absent or null. Otherwise the [initial presence object](#initial-presence-object) decides

<sup>3</sup> Send both to order the Ready `rtc_regions` array by distance

### Initial presence object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| status?<sup>1</sup> | string | The initial status, one of `online`, `idle`, `dnd`, `invisible` or `offline` |
| afk? | boolean | Whether the session is away (default false) |
| mobile? | boolean | Whether the presence is mobile (default false) |
| custom_status?<sup>2</sup> | ?[custom status](#custom-status-object) object | The custom status this session publishes |

<sup>1</sup> Fluxer uses the account's saved status when `status` is missing, empty or not one of these values, and when it is `online` but the saved status is something else

<sup>2</sup> Used only when the account has no saved custom status, and stored as sent

A session whose status is `offline` or `invisible` shows as offline with no custom status to other users.

### Custom status object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| text? | ?string | The text of the custom status (1-128 characters) |
| expires_at?<sup>1</sup> | ?ISO8601 timestamp | The time the custom status expires |
| emoji_id?<sup>2</sup> | ?snowflake | The [snowflake](/snowflakes/) of the custom emoji |
| emoji_name?<sup>3</sup> | ?string | The Unicode emoji (1-32 characters) |

<sup>1</sup> Must be in the future

<sup>2</sup> Must be an existing emoji. Fluxer drops the field when the account has no global expressions entitlement

<sup>3</sup> A single Unicode emoji, which is ignored when `emoji_id` is sent

[Presence Update](/gateway/events/#presence-update) events also include `emoji_animated`.

## Resume

Opcode `6` restores a retained session. All fields are required, and a missing or mistyped field closes with `4002` and reason `Invalid resume payload`.

| Field | Type | Description |
| --- | --- | --- |
| token | string | The token that owns the retained session |
| session_id | string | The session ID from [Ready](/gateway/events/#ready) |
| seq | integer | The last Dispatch sequence the client processed |

Send Resume on a new socket. Fluxer replays every retained Dispatch above `seq` in order and then sends [Resumed](/gateway/events/#resumed). If another socket still has the session, that socket gets opcode `7` and is closed.

| Condition | Result |
| --- | --- |
| Unknown or expired session | Opcode `9` with `d: false` |
| `seq` below the [replay floor](/gateway/limits-and-rate-limits/#replay-and-backpressure) | Opcode `9` with `d: false` |
| Token does not own the session | Close code `4004`, reason `Invalid token` |
| `seq` is negative, above the session sequence or below the acknowledged one | Close code `4007`, reason `Invalid sequence` |
| The session can't be restored right now | Close code `4000`, reason `Session unavailable` |
| The socket already has a session | Close code `4005`, reason `Already authenticated` |

## Presence Update

Opcode `3` replaces the current session presence. Updates over the [Presence Update limit](/gateway/limits-and-rate-limits/#connection-and-command-rate-limits) are dropped without a close.

| Field | Type | Description |
| --- | --- | --- |
| status | string | The status to publish, one of `online`, `idle`, `dnd`, `invisible` or `offline` |
| afk? | boolean | Whether the session is away (default false) |
| mobile? | boolean | Whether the session is mobile (default false) |
| custom_status?<sup>1</sup> | ?[custom status](#custom-status-object) object | The custom status that replaces the current one |

<sup>1</sup> Omitting the key keeps the current custom status and `null` clears it. A rejected object, such as one from a [limited account](/http-api/users/#account-limitation), leaves it unchanged

```json
{
  "op": 3,
  "d": {
    "status": "idle",
    "afk": true,
    "mobile": false,
    "custom_status": {
      "text": "away from keyboard"
    }
  }
}
```

A payload that is not an object, has no `status`, or has a non-empty string `status` outside the set closes with `4002` and reason `Invalid presence payload`. An empty string or a `status` that isn't a string counts as `online`, and an `offline` status is stored as `invisible`.

## Voice State Update

Opcode `4` joins, moves, updates or leaves the voice membership of the current session. Every field is optional. A `d` that is not an object closes with `4002` and reason `Invalid voice payload`, and other invalid updates are dropped.

| Field | Type | Description |
| --- | --- | --- |
| guild_id? | ?snowflake | The guild with the voice channel, where null selects the direct message and group direct message call context |
| channel_id? | ?snowflake | The channel to join or move to, where null leaves the membership |
| connection_id?<sup>1</sup> | ?string | The existing voice connection this update applies to |
| self_mute?<sup>2</sup> | boolean | Whether the client has muted its microphone (default false) |
| self_deaf?<sup>2</sup> | boolean | Whether the client has deafened its output (default false) |
| self_video?<sup>2</sup> | boolean | Whether the client publishes camera video (default false) |
| self_stream?<sup>3</sup> | boolean | Whether this connection advertises a screenshare track (default false) |
| is_mobile?<sup>2</sup> | boolean | Whether this is a mobile voice client (default false) |
| viewer_stream_keys?<sup>4</sup> | ?array[string] | The stream keys this connection watches, where an omitted key keeps the list and null clears it |
| latitude? | number or string | The client latitude, used to pick a voice region |
| longitude? | number or string | The client longitude, used to pick a voice region |
| p2p?<sup>5</sup> | boolean | Whether the client agrees to peer-to-peer media for a new connection, or leaves the mesh on an existing one |

<sup>1</sup> A `channel_id` with no `connection_id` opens a new connection and one with it updates or moves that connection. Leaving a guild with `channel_id: null` and no `connection_id` disconnects every connection the session has in that guild

<sup>2</sup> Only `true` and the string `"true"` set the flag

<sup>3</sup> Set like the other flags, and `false` is published when the member lacks `STREAM` in the channel. It sends no [Voice Server Update](/gateway/events/#voice-server-update)

<sup>4</sup> Each entry is the stream key of a live connection that matches this update's scope, guild and channel. Fluxer drops an update with any other entry

<sup>5</sup> Only the JSON Boolean `true` agrees. On an existing connection, `false` [moves the whole channel or call to LiveKit](/gateway/events/#peer-to-peer-voice), except in a guild voice channel with `rtc_p2p` set. A bot session never agrees

```json
{
  "op": 4,
  "d": {
    "guild_id": "1189375284394692608",
    "channel_id": "1189375284394692610",
    "self_mute": false,
    "self_deaf": false,
    "self_video": false,
    "self_stream": false
  }
}
```

`guild_id: null` with `channel_id: null` and a `connection_id` leaves that direct message or group direct message call connection. Without a `connection_id` it disconnects the session from every voice membership.

Joining or moving sends [Voice Server Update](/gateway/events/#voice-server-update) to you and [Voice State Update](/gateway/events/#voice-state-update) to every session that can see the channel. A refused join comes back as a [Gateway Error](/gateway/opcodes-and-close-codes/#gateway-error) with the refusal code.

Updates over the [Voice State Update limit](/gateway/limits-and-rate-limits/#connection-and-command-rate-limits) are queued, and a newer update for the same `guild_id` and `connection_id` replaces one that is still waiting.

## Voice Signal

Opcode `13` sends one signalling message to another connection in a [peer-to-peer](/gateway/events/#peer-to-peer-voice) voice channel or call. Send it after a [Voice Server Update](/gateway/events/#voice-server-update) with `p2p: true`.

| Field | Type | Description |
| --- | --- | --- |
| guild_id? | ?snowflake | The guild with the voice channel, where null or omitted selects the direct message and group direct message call context |
| channel_id | snowflake | The voice channel or call channel both connections are in |
| to | string | The `connection_id` of the voice state the signal is for |
| data | object | The signalling message, which Fluxer relays unread |

```json
{
  "op": 13,
  "d": {
    "guild_id": "1189375284394692608",
    "channel_id": "1189375284394692610",
    "to": "b7f3c2a94e1d4c0f",
    "data": {
      "type": "offer",
      "sdp": "v=0..."
    }
  }
}
```

Fluxer relays the signal when both the sender and the `to` connection have a voice state with `p2p: true` in that channel. Only the target connection's session gets the [Voice Signal](/gateway/events/#voice-signal). Any other signal is dropped with no event and no close.

## Request Guild Members

Opcode `8` requests member chunks for one or more guilds.

| Field | Type | Description |
| --- | --- | --- |
| guild_id?<sup>1</sup> | snowflake | The ID of the single guild to query |
| guild_ids?<sup>1</sup> | array[snowflake] | A non-empty array of the guild IDs to query |
| query?<sup>2</sup> | string | The display name prefix to match (default empty string) |
| limit?<sup>3</sup> | integer | The result limit (0-100, default 0) |
| user_ids?<sup>4</sup> | array[snowflake] | The explicit user IDs to select (at most 100, default empty) |
| presences?<sup>5</sup> | boolean | Whether results include presences (default false) |
| nonce? | string | A value of at most 32 bytes echoed in each chunk |

<sup>1</sup> `guild_id` is used only when `guild_ids` is absent or empty. The nonce is echoed only when the request has one guild

<sup>2</sup> Matched case-insensitively against the guild nickname, then the global name, then the username

<sup>3</sup> `0` with a non-empty `query` gives 25 results. `0` with an empty `query` requests the full member list, up to 100,000 entries

<sup>4</sup> A non-empty array selects those members and ignores `query` and `limit`. More than 100 entries abandons the request

<sup>5</sup> Offline and invisible presences are omitted

```json
{
  "op": 8,
  "d": {
    "guild_id": "1189375284394692608",
    "query": "ann",
    "limit": 50,
    "presences": true,
    "nonce": "a1b2c3"
  }
}
```

Guilds the session is not connected to are skipped, and a request with no connected guild gets no chunk. A user session needs `MANAGE_ROLES`, `KICK_MEMBERS` and `BAN_MEMBERS` together for a full member list, and the owner and `ADMINISTRATOR` satisfy that. Without them the request is dropped silently. A bot needs no permission for it.

Fluxer drops a bot request that lists more than one guild in `guild_ids`, with no chunk. A bot asking for the full list gets one accepted request per guild every 30 seconds, and another request inside that window gets [Rate Limited](/gateway/events/#rate-limited) and no chunk.

Results arrive as [Guild Members Chunk](/gateway/events/#guild-members-chunk) pages of at most 1,000 members with `chunk_index` and `chunk_count`. Chunks are not replayed on Resume.

## Lazy Request

Opcode `14` sets the per-guild subscriptions that decide member list, typing, sync and [thread](/gateway/threads/#lazy-request-thread-options) traffic.

| Field | Type | Description |
| --- | --- | --- |
| subscriptions<sup>1</sup> | map[snowflake, [guild subscription](#guild-subscription-object) object] | The subscription options for each guild, keyed by guild ID |

<sup>1</sup> Fluxer applies each guild's options again when the session connects to that guild later, and keeps options for at most 10 guilds the session isn't connected to yet

```json
{
  "op": 14,
  "d": {
    "subscriptions": {
      "1189375284394692608": {
        "active": true,
        "typing": true,
        "member_list_channels": {
          "1189375284394692610": [[0, 99]]
        }
      }
    }
  }
}
```

### Guild subscription object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| active?<sup>1</sup> | boolean | Whether the guild is active for this session |
| sync?<sup>1</sup> | boolean | Whether the session requests a [Guild Sync](/gateway/events/#guild-sync) for the guild |
| typing? | boolean | Whether the guild delivers [Typing Start](/gateway/events/#typing-start), overriding the active flag |
| member_list_channels?<sup>2</sup> | map[snowflake, array[array[integer]]] | The member list windows to subscribe to, keyed by channel ID |
| members? | array[snowflake] | The explicit member IDs to subscribe to (at most 1,000) |
| focused_channel_id?<sup>3</sup> | ?snowflake | The channel or public thread a visiting session has open |

<sup>1</sup> A non-Boolean value is ignored

<sup>2</sup> Each range is `[start, end]` with `start` at least 0, `end` at least `start` and at most 100,000, and `end - start` at most 99. Invalid ranges are dropped and each channel keeps its first 10

<sup>3</sup> Used only for a guild the session is visiting, where it is the only option applied. Null or an unreadable channel clears it. A visiting session gets messages and reactions only for this channel, and none for the guild's other channels

A user session is passive in a guild of more than 250 members until `active` is `true` for it, unless it is the Identify `initial_guild_id`, and a [passive session gets only some events](/gateway/event-filtering/#active-and-passive-guilds).

Switching a guild between passive and active sends a [Guild Sync](/gateway/events/#guild-sync) unless the session already has one for that guild. Going passive also clears the [`threads`](/gateway/threads/#lazy-request-thread-options) subscription, and the next switch to active syncs again.

A channel gets [Guild Member List Update](/gateway/events/#guild-member-list-update) only when you have `VIEW_CHANNEL` and `VIEW_CHANNEL_MEMBERS` in it. Thread IDs here subscribe to nothing. Use [`thread_member_lists`](/gateway/threads/#lazy-request-thread-options). Subscribing a channel to a range drops the session's member list in any other channel or guild.

## Request Guild Counts

Opcode `15` requests the member and online counts for guilds.

| Field | Type | Description |
| --- | --- | --- |
| guild_ids | array[snowflake] | The guild IDs to query, deduplicated and truncated to 100 |
| nonce? | string | A value of 1-64 bytes echoed in the result |

```json
{
  "op": 15,
  "d": {
    "guild_ids": ["1189375284394692608"],
    "nonce": "counts-1"
  }
}
```

Guilds the session is not connected to are skipped. Results arrive in one [Guild Counts Update](/gateway/events/#guild-counts-update), and a guild that does not respond within 2,000 ms is left out.

## Request Channel Member Counts

Opcode `16` requests the member and online counts for channels in one guild.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild whose channels are queried |
| channel_ids? | array[snowflake] | The channel IDs to query, deduplicated and truncated to 25 |
| channel_id?<sup>1</sup> | snowflake | The ID of the single channel to query |
| nonce? | string | A value of 1-64 bytes echoed in the result |

<sup>1</sup> Used only when `channel_ids` is absent or not an array

```json
{
  "op": 16,
  "d": {
    "guild_id": "1189375284394692608",
    "channel_ids": ["1189375284394692610"],
    "nonce": "channel-counts-1"
  }
}
```

An invalid or unconnected guild gives an empty result. Results arrive in one [Channel Member Counts Update](/gateway/events/#channel-member-counts-update), and a channel is counted only when it isn't a thread and you have `VIEW_CHANNEL` and `VIEW_CHANNEL_MEMBERS` in it.
