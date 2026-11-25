---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Limits and rate limits
description: The heartbeat, session, rate, replay and payload limits on a Gateway connection.
---

Every Gateway connection has limits on what it sends, how long its session lasts and how much Fluxer replays to it. Going over a limit closes the connection, drops the payload or clamps the value.

These limits don't apply to the media connection a client opens to LiveKit or to a peer, and voice joins have their own [admission limits](#voice-admission). A connection that closes for a limit gets a [close code](/gateway/opcodes-and-close-codes/#close-codes). A client message can be at most [16,384 bytes](/gateway/overview/#framing).

## Session lifecycle

| Limit | Value | What happens |
| --- | --- | --- |
| Heartbeat interval | 41,250 ms | Sent in Hello |
| Heartbeat check | Every 13,750 ms | The Gateway checks the heartbeat |
| Heartbeat request | 37,125 ms since the last acknowledgement | The Gateway sends opcode `1`, and you send a heartbeat right away |
| Heartbeat timeout | 45,000 ms with no acknowledgement | The connection closes with `4009` at the next heartbeat check, within 13,750 ms of that point |
| Identify deadline | One heartbeat interval after Hello | A socket with no Identify or Resume closes with `4003` at the next heartbeat check |
| Resume window | 60,000 ms after a disconnect | A Resume restores the status the session had before |
| Offline presence | 5,000 ms after a disconnect | The presence shows as `offline` |
| Live sessions per user | 100 | A further Identify closes with `4008` |
| Shard count | 1 to 16,384 | A `shard` that is not a valid `[shard_id, shard_count]` pair closes with `4010` |
| Guilds per bot shard | 2,500 | A bot shard with more guilds closes with `4011` |

User accounts have no guild limit per session.

When the Gateway is busy, it keeps an Identify waiting and starts the session once it can. Keep heartbeating until Ready arrives. More than 300 Identify and Resume commands from one IP address in a 60-second window close the connection with `4008`. Wait for the next window before you reconnect. The [session start limit](/http-api/gateway/#session-start-limit-object) that `GET /v1/gateway/bot` returns has fixed values for client compatibility, and the Gateway does not enforce it.

## Connection and command rate limits

Every client payload counts towards the first three budgets.

| Scope | Budget | Over the budget |
| --- | --- | --- |
| WebSocket | 600 payloads per rolling 60 seconds | The connection closes with `4008` |
| Session | 600 payloads per fixed 60-second bucket, after authentication | The connection closes with `4008` |
| IP address | 6,000 payloads per fixed 60-second bucket | The connection closes with `4008` |
| Open connections per IP | 256 WebSockets | The new connection closes with `4008` before Hello |
| Identify and Resume per IP | 300 attempts per fixed 60-second window | The connection closes with `4008`, reason `Identify rate limited` or `Resume rate limited` |
| Commands between Identify and Ready, or between Resume and Resumed | 64 queued commands | The connection closes with `4008` |
| Presence Update | 5 per WebSocket per rolling 20 seconds | The command is dropped |
| Request Forum Unreads | 5 per WebSocket per rolling 5 seconds | The command is dropped |
| Voice Signal | 240 per WebSocket per rolling 10 seconds | The command is dropped |
| Request Guild Members | 12 per account and 40 per guild per rolling 10 seconds | The command is dropped |

Fluxer applies the first two Voice State Updates per WebSocket in a rolling second. Later ones wait in a queue of up to 64 and are applied one every 500 ms. A newer update for the same `guild_id` and `connection_id` replaces one that is still waiting, and a full queue drops its oldest update.

A bot can request a guild's complete member list (Request Guild Members with an empty `query`, `limit` 0 and no `user_ids`) once every 30 seconds per guild. An earlier request gets [Rate Limited](/gateway/events/#rate-limited), and reconnecting does not reset the timer.

## Count timeouts

Request Guild Counts and Request Channel Member Counts leave out a guild that does not respond within 2,000 ms.

## Replay and backpressure

The replay buffer keeps at most 4,096 Dispatch events and about 16 MiB of event data, and drops the oldest events past either limit. A single Dispatch over about 2 MiB is sent but never kept, and a Resume with a `seq` below that event gets opcode `9` with `d: false`. Some events are [never replayed](/gateway/events/#dispatch-delivery), and a replay skips their sequence numbers.

The Gateway drops kept events up to the sequence in a heartbeat. Send a sequence only after you finish processing that event. When `seq` is below an event that was dropped from the buffer or never kept, you get opcode `9` with `d: false` and need to Identify again.

## Command payload limits

| Command | Limit |
| --- | --- |
| Identify | At most 256 `ignored_events` |
| Request Guild Members | At most 100 `user_ids`, a `limit` from 0 through 100 and a nonce up to 32 bytes |
| Lazy Request | At most 10 ranges per channel of up to 100 members each, and 1,000 member IDs per guild |
| Request Guild Counts | At most 100 guild IDs and a nonce from 1 through 64 bytes |
| Request Channel Member Counts | At most 25 channel IDs and a nonce from 1 through 64 bytes |
| Request Forum Unreads | At most 40 posts |

Going over an Identify limit closes the connection with `4002`. Other out-of-range values in [client commands](/gateway/commands/) are clamped or dropped, depending on the field.

## Voice admission

A refused voice join does not close the connection. It sends a [Gateway Error](/gateway/opcodes-and-close-codes/#gateway-error) with the refusal code.

A [guild voice channel](/voice/#capacity) admits at most `user_limit` users, where `0` means no limit. Once anyone has a camera on, it admits at most 25 users, and a 26th camera is refused with `VOICE_CAMERA_USER_LIMIT`. A peer-to-peer call has at most [`max_participants`](/admin-api/instance/#peer-to-peer-voice-configuration-object) participants, 2 by default and never more than 4, and refuses more with `VOICE_CHANNEL_FULL`. One user has at most `voice_connection_limit` connections in one guild voice channel (default 5, range 1-100).
