---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Client commands
description: Every main Gateway client command, its payload, bounds, result, and close behaviour.
---

A client command is a payload a client sends to Fluxer over the [Gateway](/gateway/overview/) WebSocket. Each one has an integer [opcode](/gateway/opcodes-and-close-codes/#opcodes) in `op` and the command data in `d`. Fluxer answers with a [Dispatch](/gateway/events/) event, a close frame, or nothing at all.

Except for [Heartbeat](#heartbeat), [Identify](#identify), and [Resume](#resume), every command needs an authenticated session. Sending one too early closes with `4003` and reason `Not authenticated`. Heartbeat and Resume are accepted in any open state, and Identify is accepted only while the connection is unauthenticated.

## Command index

| Opcode | Command | Result |
| --- | --- | --- |
| 1 | Heartbeat | Opcode `11` Heartbeat ACK |
| 2 | Identify | [Ready](/gateway/events/#ready), a close frame, or silence when the payload is held or discarded |
| 3 | Presence Update | No direct response |
| 4 | Voice State Update | [Voice State Ack](/gateway/events/#voice-state-ack), [Voice State Update](/gateway/events/#voice-state-update), and [Voice Server Update](/gateway/events/#voice-server-update) when state changes |
| 6 | Resume | Replayed Dispatches followed by [Resumed](/gateway/events/#resumed), Invalid Session, or a close frame |
| 8 | Request Guild Members | One or more [Guild Members Chunk](/gateway/events/#guild-members-chunk) events |
| 14 | Lazy Request | [Guild Sync](/gateway/events/#guild-sync) and [Guild Member List Update](/gateway/events/#guild-member-list-update) |
| 15 | Request Guild Counts | [Guild Counts Update](/gateway/events/#guild-counts-update) |
| 16 | Request Channel Member Counts | [Channel Member Counts Update](/gateway/events/#channel-member-counts-update) |

## Payload handling

A frame that fails the size, decompression, or decoding checks closes the connection without consuming any budget. A frame that decodes to something other than an object closes with `4002` and reason `Decode failed`, and an object with no `op` closes with `4002` and reason `Invalid payload`.

Fluxer charges every command that gets past those checks against the [source IP, session, and connection payload budgets](/gateway/limits-and-rate-limits/#connection-and-command-rate-limits) before it handles the opcode. Each session gets its own session budget, so two sessions of one account never share one, and Fluxer skips that budget while the connection is unauthenticated.

An opcode outside the registry closes with `4001` and reason `Unknown opcode` once a session is attached, and with `4003` while the connection is unauthenticated. A payload that has `op` but no `d` also closes with `4001`, except for Identify, which closes with `4005`.

:::note[Most command payloads are permissive]
Identify, Resume, and Presence Update reject a malformed payload with a close. The [bounded query commands](#bounded-requests) coerce or discard whatever they cannot parse, so a wrong field type usually produces an empty result.
:::

## Heartbeat

Opcode `1` has the last Dispatch sequence the client processed. Use `null` before the first Dispatch.

| Field | Type | Description |
| --- | --- | --- |
| d | ?integer | The last Dispatch sequence the client processed, or null before the first Dispatch |

```json
{
  "op": 1,
  "d": 42
}
```

Before authentication the Gateway accepts any `d` value and sends Opcode `11`. A payload with no `d` key closes with `4001` and reason `Unknown opcode`.

Once a session exists, a `d` that is neither `null` nor an integer closes with `4007` and reason `Invalid sequence`. Every integer is accepted. A sequence below the one the session has already acknowledged leaves the acknowledged sequence unchanged. Any other integer becomes the acknowledged sequence and trims every retained Dispatch at or below it from the replay buffer.

When the session ends, Fluxer sends Opcode `9` with `d: false`, after which heartbeats are acknowledged again. A heartbeat that arrives in the short window between the session ending and that frame closes with `4007`.

See [Gateway overview](/gateway/overview/#heartbeats) for the timing contract.

## Identify

Opcode `2` authenticates and creates a new session.

| Field | Type | Description |
| --- | --- | --- |
| token | string | The account or bot token, with no HTTP authentication prefix |
| properties | [identify properties](#identify-properties-object) object | The software and capabilities of the connecting client |
| presence?<sup>1</sup> | ?[initial presence](#initial-presence-object) object | The presence to publish when the session starts |
| ignored_events?<sup>2</sup> | ?array[string] | The Dispatch event names this session does not want, at most 256 entries |
| flags? | integer | A non-negative [session flags](#session-flags) bitfield (default `0`) |
| initial_guild_id?<sup>3</sup> | ?snowflake | The [snowflake](/snowflakes/) of one guild the session joins as active |
| shard?<sup>4</sup> | ?array[integer] | The `[shard_id, shard_count]` pair |

<sup>1</sup> Null and an omitted key both select the account's saved status and saved custom status

<sup>2</sup> Names are upper-cased and deduplicated. See [Event filtering](/gateway/event-filtering/) for the exact suppression rule

<sup>3</sup> That guild is marked active and already synced when the session connects to it, so it delivers active traffic without a [Lazy Request](#lazy-request) and sends no [Guild Sync](/gateway/events/#guild-sync). Fluxer discards a value that is not a canonical decimal Snowflake string, and the Identify still succeeds

<sup>4</sup> `shard_count` is an integer from 1 through 16,384, and `shard_id` is an integer that is at least 0 and below `shard_count`

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

`token` and `properties` are required. Fluxer ignores unknown fields. There is no `intents` field and no intent system.

The following faults close with `4002` and reason `Invalid identify payload`:

- A missing `token` or `properties`.
- A `properties` value that is not an object.
- A `properties` object whose `os`, `browser`, or `device` is missing or is not a string.
- An `ignored_events` value that is not an array of strings, or one that holds more than 256 entries.
- A `flags` value that is not a non-negative integer.

A malformed `shard` closes with `4010` and reason `Invalid shard`.

Fluxer refuses a bot session that resolves to more than 2,500 guilds after any `shard` filter is applied. The connection closes with `4011` and reason `Sharding required`. A user session is never refused for its guild count.

A token the backend rejects closes with `4004` and reason `Invalid token`. A non-bot account that already holds 100 live sessions closes with `4008` and reason `Too many sessions`, and a bot credential is not bounded by that count. An Identify sent on a socket that already has a session attached closes with `4005` and reason `Already authenticated`, whether or not it has `d`.

### Session flags

| Value | Name | Description |
| --- | --- | --- |
| 1 &lt;&lt; 1 | DEBOUNCE_MESSAGE_REACTIONS | Coalesce runs of reaction additions into [Message Reaction Add Many](/gateway/events/#message-reaction-add-many) |

Bit `0` and every bit above `1` are undefined. An undefined bit is accepted and ignored without closing the connection.

`DEBOUNCE_MESSAGE_REACTIONS` applies to a reaction in a direct message or group direct message. A reaction in a guild channel is never coalesced and arrives as its own [Message Reaction Add](/gateway/events/#message-reaction-add).

### Identify properties object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| os | string | The operating system the client runs on |
| browser | string | The client library identifier |
| device | string | The device or application identifier |
| e2ee_capable?<sup>1</sup> | boolean | Whether the client can take part in end-to-end encrypted voice |
| mobile?<sup>2</sup> | boolean | Whether the session is mobile (default false) |
| latitude?<sup>3</sup> | string | The client latitude as a decimal string of 1 through 32 characters |
| longitude?<sup>3</sup> | string | The client longitude as a decimal string of 1 through 32 characters |

<sup>1</sup> Only the exact value `true` sets it. A session without it is refused from an end-to-end encrypted voice channel with `VOICE_E2EE_REQUIRED`

<sup>2</sup> Read only when `presence` is absent or null, in which case it decides the session's mobile flag. Otherwise the [initial presence object](#initial-presence-object) decides it

<sup>3</sup> Used to order the Ready `rtc_regions` array by distance

`os`, `browser`, and `device` are required strings. The remaining fields are optional hints. Fluxer accepts and ignores unrecognised properties.

`latitude` and `longitude` are accepted here only as strings. A number fails validation and the whole session start fails, so send `"52.52"`. Both must be sent together to have any effect, and a string that does not parse as a number counts as absent. Omit them when the client has no location, and Ready orders `rtc_regions` by region ID instead.

### Initial presence object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| status?<sup>1</sup> | string | The initial status, accepting `online`, `idle`, `dnd`, `invisible`, or `offline` |
| afk?<sup>2</sup> | boolean | Whether the session is away (default false) |
| mobile?<sup>2</sup> | boolean | Whether the presence is mobile (default false) |
| custom_status?<sup>3</sup> | ?[custom status](#custom-status-object) object | The custom status this session publishes |

<sup>1</sup> The account's saved status wins when `status` is absent, when it is null or any other non-string, when it is the string `unknown`, and when it is `online` while the saved status is not `online`. The empty string resolves to `online`, and the remaining accepted values are used as sent

<sup>2</sup> Only the exact value `true` sets the flag. Every other value resolves to false

<sup>3</sup> Read only when the account has no saved custom status, and stored as sent with no validation

Identify accepts `offline` as a distinct initial status, and [Presence Update](#presence-update) normalises `offline` to `invisible`. A session whose resolved status is `offline` or `invisible` publishes `status: "offline"` and a null `custom_status` to other users.

### Custom status object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| text? | ?string | The text of the custom status, from 1 through 128 characters |
| expires_at?<sup>1</sup> | ?ISO8601 timestamp | The time the custom status expires |
| emoji_id?<sup>2</sup> | ?snowflake | The [snowflake](/snowflakes/) of the custom emoji |
| emoji_name?<sup>3</sup> | ?string | The Unicode emoji, from 1 through 32 characters |

<sup>1</sup> The timestamp is in the future

<sup>2</sup> The Snowflake names an emoji that exists. Fluxer drops the field from the published object, without failing the rest of it, when the account has no global expressions entitlement

<sup>3</sup> A single Unicode emoji. Fluxer strips the field before validation when `emoji_id` is supplied

Only these fields are read. [Presence Update](#presence-update) validates the object against the account before publishing it. [Identify](#identify) does not validate it, and reads it only when the account has no saved custom status.

The published presence adds `emoji_animated` to this object.

## Resume

Opcode `6` restores a retained session.

| Field | Type | Description |
| --- | --- | --- |
| token | string | The token that owns the retained session |
| session_id | string | The session ID from [Ready](/gateway/events/#ready) |
| seq | integer | The last Dispatch sequence the client processed |

All three fields are required. A missing field, a non-string `token` or `session_id`, or a `seq` that is not an integer closes with `4002` and reason `Invalid resume payload`.

An unknown or expired session produces Opcode `9` with `d: false` and leaves the socket unauthenticated. A token that does not own the session closes with `4004` and reason `Invalid token`. A `seq` above the session's current sequence, or below the sequence it has already acknowledged, closes with `4007` and reason `Invalid sequence`. A `seq` below the [replay floor](/gateway/limits-and-rate-limits/#replay-and-backpressure), the highest sequence already dropped from the buffer, also produces Opcode `9` with `d: false`. A negative `seq` closes with `4000` and reason `Session unavailable`, and so does a session that cannot be reached. None of those closes destroys a separately retained session.

A successful Resume replays every retained Dispatch strictly above `seq` in order and finishes with [Resumed](/gateway/events/#resumed). It also replaces the session's socket, and the displaced socket receives Opcode `7` followed by a close.

Fluxer processes Resume in any authentication state. A socket that already has a session attached still processes one, and the named session takes the attached session's place. Send Resume only on a fresh socket.

## Presence Update

Opcode `3` replaces the current session presence.

| Field | Type | Description |
| --- | --- | --- |
| status | string | The status to publish, accepting `online`, `idle`, `dnd`, `invisible`, or `offline` |
| afk?<sup>1</sup> | boolean | Whether the session is away (default false) |
| mobile?<sup>1</sup> | boolean | Whether the session is mobile (default false) |
| custom_status?<sup>2</sup> | ?[custom status](#custom-status-object) object | The custom status that replaces the current one |

<sup>1</sup> Only the exact value `true` sets the flag. Every other value resolves to false

<sup>2</sup> An object identical to the current one in `text`, `expires_at`, `emoji_id`, and `emoji_name` is reused without revalidation

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

`status` is required. A payload that is not an object, an object with no `status` key, and a `status` string outside the accepted set all close with `4002` and reason `Invalid presence payload`. The empty string resolves to `online`.

Fluxer accepts a `status` that is not a string. Null and a Boolean publish the session as offline, and every other non-string value resolves to `online`.

`offline` is normalised to `invisible`, so a Presence Update cannot publish a session as offline while it is connected.

`custom_status` is replaced only when the key is present. Omitting the key keeps the current custom status, `null` clears it, and a value that is neither an object nor null is ignored. An object the backend rejects, such as an `emoji_id` that names no emoji or an `expires_at` in the past, leaves the current custom status in place. The connection stays open.

The published presence has a [custom status](#custom-status-object) object and no activities.

Presence Update has a dedicated limit of five accepted commands per 20 seconds on one WebSocket. A further update inside that window is discarded without closing the connection, after it has consumed the shared payload budgets.

## Voice State Update

Opcode `4` joins, moves, updates, or leaves the voice membership associated with the current Gateway session.

| Field | Type | Description |
| --- | --- | --- |
| guild_id? | ?snowflake | The guild containing the voice channel, where null selects the DM and group DM call context |
| channel_id? | ?snowflake | The channel to join or move to, where null leaves the current membership in that context |
| connection_id?<sup>1</sup> | ?string | The existing voice connection this update applies to |
| self_mute?<sup>2</sup> | boolean | Whether the client has muted its own microphone (default false) |
| self_deaf?<sup>2</sup> | boolean | Whether the client has deafened its own output (default false) |
| self_video?<sup>2</sup> | boolean | Whether the client publishes camera video (default false) |
| self_stream?<sup>5</sup> | boolean | Whether this connection advertises a screenshare track (default false) |
| is_mobile?<sup>2</sup> | boolean | Whether this is a mobile voice client (default false) |
| viewer_stream_keys?<sup>3</sup> | ?array[string] | The stream keys this connection is watching, where an omitted key keeps the current list and null clears it |
| latitude? | number or string | The client latitude, used to pick a voice region |
| longitude? | number or string | The client longitude, used to pick a voice region |
| mutation_id? | string | A client-generated identity echoed in [Voice State Ack](/gateway/events/#voice-state-ack) |
| runtime_epoch? | string | The client runtime generation echoed in Voice State Ack |
| base_version?<sup>4</sup> | integer | The voice state version this update was computed against |

<sup>1</sup> A `channel_id` with no `connection_id` opens a new connection, and a `channel_id` with one updates or moves that connection. An update that leaves one guild, meaning a non-null `guild_id` with `channel_id: null`, requires a `connection_id`, and one that omits it is refused with `VOICE_MISSING_CONNECTION_ID`

<sup>2</sup> Only `true` and the string `"true"` set the flag. Every other value resolves to false

<sup>3</sup> Every entry is a stream key whose scope, guild, and channel match this update. An entry that fails that check, or a value that is not an array, refuses the update with `VOICE_INVALID_STATE`, and an entry naming a connection that does not exist refuses it with `VOICE_CONNECTION_NOT_FOUND`

<sup>4</sup> A non-negative integer. Fluxer treats every other value as absent, which disables the staleness check

<sup>5</sup> Only `true` and the string `"true"` set it, and Fluxer publishes `false` when the member lacks `STREAM` in the channel. The screenshare track rides this same connection, so setting the flag mints no grant and sends no [Voice Server Update](/gateway/events/#voice-server-update)

```json
{
  "op": 4,
  "d": {
    "guild_id": "1189375284394692608",
    "channel_id": "1189375284394692610",
    "self_mute": false,
    "self_deaf": false,
    "self_video": false,
    "self_stream": false,
    "mutation_id": "e0a1c2",
    "base_version": 7
  }
}
```

Every field is optional. A non-null `guild_id` or `channel_id` is a canonical decimal [snowflake](/snowflakes/) string, and a positive integer is also accepted. Every other value fails validation, and Fluxer drops the update with no close code and no event.

`guild_id: null` with `channel_id: null` and a `connection_id` leaves the DM or group DM call connection with that identifier. The same shape without a `connection_id` disconnects the session from every voice membership it holds. `guild_id: null` with a `channel_id` selects the DM or group DM call context.

`latitude` and `longitude` accept a number or a string here, and Fluxer coerces both to a string.

The command has no `session_id` field. The current Gateway session is the membership identity.

Joining or replacing a grant produces [Voice Server Update](/gateway/events/#voice-server-update) with the token and endpoint for the media connection, and [Voice State Update](/gateway/events/#voice-state-update) for every session that can see the channel.

When a guild update has `mutation_id`, Fluxer also reports the outcome to the requesting session as [Voice State Ack](/gateway/events/#voice-state-ack), whose `status` is `applied` or `rejected` and whose `error_code` names the exact refusal. Without `mutation_id` a refusal produces no event at all, and the DM and group DM call context never acks.

`base_version` is a staleness check for an update that names an existing guild connection. An update whose `base_version` is more than one behind that connection's current voice state version is rejected with `stale_base_version`. The check runs after the member, channel, and connection lookups and before the permission checks. It does not apply to opening a new connection, to leaving a channel, or to the DM and group DM call context.

The first two updates in a rolling one-second window are processed immediately. Later updates enter a per-session queue that holds at most 64 commands and drains one command every 500 ms. A newer update replaces an older queued update for the same `guild_id` and `connection_id` pair, and a full queue discards its oldest entry before accepting the new one.

## Request Guild Members

Opcode `8` requests bounded member chunks.

| Field | Type | Description |
| --- | --- | --- |
| guild_id?<sup>1</sup> | snowflake | The ID of the single guild to query |
| guild_ids?<sup>1</sup> | array[snowflake] | A non-empty array of the guild IDs to query |
| query?<sup>2</sup> | string | The display name prefix to match (default empty string) |
| limit?<sup>3</sup> | integer | The result limit, from 0 through 100 and clamped (default 0) |
| user_ids?<sup>4</sup> | array[snowflake] | The explicit user IDs to select, at most 100 (default empty) |
| presences?<sup>5</sup> | boolean | Whether results include presences (default false) |
| nonce? | string | A value of at most 32 bytes echoed in each chunk |

<sup>1</sup> A non-empty `guild_ids` array wins. `guild_id` is read only when `guild_ids` is absent or empty

<sup>2</sup> Matched case-insensitively as a prefix of the member's display name, which is the guild nickname, then the global name, then the username

<sup>3</sup> A `limit` of `0` with a non-empty `query` resolves to 25 results, and a `limit` of `0` with an empty `query` requests the full member list up to 100,000 entries

<sup>4</sup> A non-empty `user_ids` array selects those members directly and ignores `query` and `limit`

<sup>5</sup> Only the exact value `true` sets it. Presences whose status is `offline` or `invisible` are omitted from the result

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

The command never closes the connection. Invalid input is coerced or discarded:

- A guild ID that is not a positive Snowflake abandons the whole request.
- Duplicate guild IDs are collapsed.
- A `user_ids` array longer than 100 entries abandons the whole request. Individual entries that are not positive Snowflakes are dropped.
- A `limit` that is not a non-negative integer becomes `0`, and a larger value is clamped to `100`.
- A `query` that is not a string becomes the empty string.
- A `nonce` that is not a string of at most 32 bytes becomes null.

The nonce is echoed only when the request named exactly one guild.

Fluxer skips a guild the session is not currently connected to. A request that resolves to no connected guild produces no chunk.

A bot requests one guild at a time, and a bot request naming two or more guilds is abandoned.

An empty `query`, a `limit` of `0`, and an empty `user_ids` together request the complete member list. A human account requesting the complete list needs `MANAGE_ROLES`, `KICK_MEMBERS`, and `BAN_MEMBERS` together in that guild, and a request holding only some of them is dropped silently. The guild owner and any member with `ADMINISTRATOR` satisfy that check. A bot requesting the complete list is limited to one accepted request per guild every 30 seconds. A request inside that window produces [Rate Limited](/gateway/events/#rate-limited) and no member chunk.

Results arrive as [Guild Members Chunk](/gateway/events/#guild-members-chunk) in pages of at most 1,000 members, each with `chunk_index` and `chunk_count`. Those chunks are delivered live and are never retained for Resume replay.

Only one member request runs at a time on one WebSocket. While one runs, a newer request replaces any earlier pending request and starts when the active one finishes. [Bounded requests](#bounded-requests) covers the four-slot limit this command shares with the other three.

## Lazy Request

Opcode `14` sets the per-guild subscriptions that decide member list, typing, and synchronisation traffic for the session.

| Field | Type | Description |
| --- | --- | --- |
| subscriptions<sup>1</sup> | map[snowflake, [guild subscription](#guild-subscription-object) object] | The subscription options for each guild, keyed by guild ID |

<sup>1</sup> Fluxer keeps each guild's options for the session and applies them again when the session connects to that guild later

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

`subscriptions` is an object whose keys are canonical decimal Snowflake strings. A key that does not parse, a value that is not an object, and a guild the session is not connected to are all skipped without affecting the rest of the command. The command never closes the connection.

### Guild subscription object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| active?<sup>1</sup> | boolean | Whether the guild is active for this session |
| sync?<sup>1</sup> | boolean | Whether the session requests a [Guild Sync](/gateway/events/#guild-sync) for the guild |
| typing? | boolean | Whether the guild delivers [Typing Start](/gateway/events/#typing-start), overriding the active flag |
| member_list_channels?<sup>2</sup> | map[snowflake, array[array[integer]]] | The member list windows to subscribe to, keyed by channel ID |
| members? | array[snowflake] | The explicit member IDs to subscribe to, at most 1,000 |

<sup>1</sup> Both are Booleans when present. Any other value abandons the rest of the command silently, without a close and without a result

<sup>2</sup> A coalesced subscription waits 100 ms before Fluxer applies it, and ranges arriving inside that window merge into the ranges already buffered for the same channel. A request that has no ranges for a channel discards the ranges already buffered for it

Fluxer applies each option only when its key is present, in the order `active`, `sync`, `member_list_channels`, `members`, `typing`.

Marking a guild active changes how much traffic it produces, and [Event filtering](/gateway/event-filtering/) specifies the difference. A transition from passive to active, and a transition from active to passive, both imply a sync even when `sync` is absent. Every sync request, implied or explicit, is dropped when the guild is already marked synced for that session. Going passive clears that mark, so the next sync request produces a fresh Guild Sync.

`member_list_channels` maps a channel ID to a list of `[start, end]` ranges. A range needs `start` at least 0, `end` at least `start`, `end` at most 100,000, and `end - start` at most 99. Ranges that fail those bounds are dropped, and each channel keeps at most the first 10 that pass. A channel key that is not a Snowflake is skipped.

Fluxer applies a subscription at once when the guild has no coalescing window open, its buffer is empty, and the channel's member list is already built. That request opens the window. Fluxer buffers it as applied and does not apply it a second time when the window closes. Every other subscription waits out the window, including one for a channel whose member list is not built yet and one arriving while the window is open.

`VIEW_CHANNEL` and `VIEW_CHANNEL_MEMBERS` together govern the member list subscription, and both are evaluated for each channel separately. A channel the session cannot view, or can view without holding `VIEW_CHANNEL_MEMBERS` there, receives no [Guild Member List Update](/gateway/events/#guild-member-list-update) while its siblings subscribe normally.

Subscribing a channel to at least one range drops the session's other member list subscriptions in that guild, so one session holds at most one member list per guild.

`members` entries that are not Snowflakes are dropped, and the first 1,000 that pass are kept. A member the session shares no viewable channel with is dropped as well.

`typing` is a Boolean, and Fluxer ignores any other value. It decides [Typing Start](/gateway/events/#typing-start) delivery for the guild independently of the active flag.

## Request Guild Counts

Opcode `15` requests current count records.

| Field | Type | Description |
| --- | --- | --- |
| guild_ids | array[snowflake] | The guild IDs to query, deduplicated, sorted, and truncated to 100 |
| nonce? | string | A value from 1 through 64 bytes echoed in the result |

```json
{
  "op": 15,
  "d": {
    "guild_ids": ["1189375284394692608"],
    "nonce": "counts-1"
  }
}
```

Entries that are not positive Snowflakes are dropped. A guild the session is not connected to is skipped. A `nonce` outside the length bound is omitted from the result. The command never closes the connection.

Results arrive in one [Guild Counts Update](/gateway/events/#guild-counts-update). Each guild is queried with a 2,000 ms deadline under an overall 3,000 ms batch deadline, so a slow guild is omitted from the result.

## Request Channel Member Counts

Opcode `16` requests count records for channels in one guild.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild whose channels are queried |
| channel_ids | array[snowflake] | The channel IDs to query, deduplicated, sorted, and truncated to 25 |
| channel_id?<sup>1</sup> | snowflake | The ID of the single channel to query, read only when `channel_ids` is absent |
| nonce? | string | A value from 1 through 64 bytes echoed in the result |

<sup>1</sup> A `channel_ids` value that is present but is not an array also falls back to `channel_id`

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

A `guild_id` that is not a positive Snowflake, or a guild the session is not connected to, produces an empty result. Channel entries that are not positive Snowflakes are dropped. The command never closes the connection.

Results arrive in one [Channel Member Counts Update](/gateway/events/#channel-member-counts-update) with a 2,000 ms guild deadline. A channel is counted only when the requesting session holds both `VIEW_CHANNEL` and `VIEW_CHANNEL_MEMBERS` there, and every other named channel is omitted from the result.

## Bounded requests

One WebSocket processes at most four bounded requests at once across Request Guild Members, Lazy Request, Request Guild Counts, and Request Channel Member Counts. A command that arrives when all four slots are taken is dropped, without a close and without a result. Each request has a 10,000 ms deadline, after which it produces no further events.

Request Guild Members also keeps one replaceable pending request while another member request is active, whether or not a slot is free.
