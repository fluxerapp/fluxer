---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Limits and rate limits
description: The heartbeat, session, rate, replay and payload limits on a Gateway connection.
---

Every Gateway connection (not the voice connection) has limits on what it sends, how long its session lasts and how much Fluxer replays to it. Going over a limit closes the connection with a [close code](/gateway/opcodes-and-close-codes/#close-codes), drops the payload or clamps the value. Message size limits are in [Framing](/gateway/overview/#framing).

## Session lifecycle

Hello sends a heartbeat interval of 41,250 ms, and the Gateway checks the heartbeat every 13,750 ms. When 37,125 ms have elapsed since the last acknowledgement, the Gateway sends opcode `1` and you send a heartbeat right away. If no acknowledgement arrives within 45,000 ms, the connection closes with `4009`, and a socket that never sends Identify closes the same way.

A disconnected session stays resumable for 60,000 ms, and its presence shows as `offline` after 5,000 ms. A Resume inside that window restores the status the session had.

One user has at most 100 live sessions, and a further Identify closes with `4008`.

Shard counts run from 1 through 16,384, and one bot shard has at most 2,500 guilds. A bot shard with more guilds closes with `4011`, and a `shard` that is not a valid `[shard_id, shard_count]` pair closes with `4010`. User accounts have no guild limit per session.

When the Gateway is busy, it holds an Identify and starts the session once it can. Keep heartbeating until Ready arrives. An Identify or Resume over the per-IP budget below gets no reply, and you can send a new one after the 60-second window. The [session start limit](/http-api/gateway/#session-start-limit-object) that `GET /v1/gateway/bot` returns has fixed values for client compatibility, and the Gateway does not enforce it.

## Connection and command rate limits

Every client payload counts toward the first three budgets.

| Scope | Budget | Over the budget |
| --- | --- | --- |
| WebSocket | 600 payloads per rolling 60 seconds | The connection closes with `4008` |
| Session | 600 payloads per fixed 60-second bucket, after authentication | The connection closes with `4008` |
| IP address | 6,000 payloads per fixed 60-second bucket | The connection closes with `4008` |
| Open connections per IP | 256 WebSockets | The new connection closes with `4008` before Hello |
| Identify and Resume per IP | 300 attempts per fixed 60-second window | The command is dropped with no reply |
| Presence Update | 5 per WebSocket per rolling 20 seconds | The command is dropped |
| Request Forum Unreads | 5 per WebSocket per rolling 5 seconds | The command is dropped |
| Voice Signal | 240 per WebSocket per rolling 10 seconds | The command is dropped |
| Request Guild Members | 12 per account and 40 per guild per rolling 10 seconds | The command is dropped |

Fluxer applies the first two Voice State Updates per session in a rolling second. Later ones are applied one every 500 ms, and a newer update for the same `guild_id` and `connection_id` replaces one that is still waiting.

A bot can request a guild's complete member list (Request Guild Members with an empty `query`, `limit` 0 and no `user_ids`) once every 30 seconds per guild. An earlier request gets [Rate Limited](/gateway/events/#rate-limited), and reconnecting does not reset the timer.

## Bounded commands

One WebSocket runs at most four of the following commands at once, and each one stops after 10,000 ms.

- [Request Guild Members](/gateway/commands/#request-guild-members)
- [Lazy Request](/gateway/commands/#lazy-request)
- [Request Guild Counts](/gateway/commands/#request-guild-counts)
- [Request Channel Member Counts](/gateway/commands/#request-channel-member-counts)
- [Request Forum Unreads](/gateway/threads/#request-forum-unreads)

A command sent while all four slots are busy is dropped with no result and no close. If you send more Request Guild Members commands while one is running, only the most recent waits and the rest are dropped. A command that times out after sending some of its events does not send the rest.

Request Guild Counts and Request Channel Member Counts leave out a guild that does not respond within 2,000 ms.

## Replay and backpressure

The replay buffer keeps at most 4,096 Dispatch events and about 16 MiB of event data, and drops the oldest events past either limit. A single Dispatch over about 2 MiB is sent but never kept, and a Resume with a `seq` below that event gets opcode `9` with `d: false`. A replay can skip sequence numbers, because some events are [never replayed](/gateway/events/#dispatch-delivery).

Send a sequence in a heartbeat only after you finish processing that event, because the Gateway then drops kept events up to it. When `seq` is below an event that was dropped from the buffer or never kept, you get opcode `9` with `d: false` and need to Identify again.

## Command payload limits

| Command | Limit |
| --- | --- |
| Identify | At most 256 `ignored_events` |
| Request Guild Members | At most 100 `user_ids`, a `limit` from 0 through 100 and a nonce up to 32 bytes |
| Lazy Request | At most 10 ranges per channel of up to 100 members each, and 1,000 member IDs per guild |
| Request Guild Counts | At most 100 guild IDs and a nonce from 1 through 64 bytes |
| Request Channel Member Counts | At most 25 channel IDs and a nonce from 1 through 64 bytes |
| Request Forum Unreads | At most 40 posts |

Going over an Identify limit closes the connection with `4002`. Other out-of-range values are clamped or dropped, as each field in [Client commands](/gateway/commands/) describes.

## Voice admission

A refused voice join does not close the connection. Only `VOICE_CHANNEL_FULL`, `VOICE_P2P_CONSENT_REQUIRED` and `VOICE_P2P_UNAVAILABLE` send a [Gateway Error](/gateway/opcodes-and-close-codes/#gateway-error).

A guild voice channel admits at most `user_limit` users, where `0` means no limit. Once anyone has a camera on, it admits at most 25 users, and a 26th camera is refused with `VOICE_CAMERA_USER_LIMIT`. A peer-to-peer call has at most 4 participants and refuses more with `VOICE_CHANNEL_FULL`. One user has at most `voice_connection_limit` connections in one guild voice channel (default 5, range 1-100). See [Capacity](/voice/#capacity).
