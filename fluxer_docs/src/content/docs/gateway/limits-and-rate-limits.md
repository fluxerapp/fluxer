---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Limits and rate limits
description: Main Gateway session, command, replay, backpressure, and admission limits.
---

Every main Gateway connection has limits on what it sends, how long its session lives, how many commands it runs at once, and how much Fluxer replays to it. Exceeding one closes the connection with a [close code](/gateway/opcodes-and-close-codes/#close-codes), discards the payload, or coerces the value. The [close reason registry](/gateway/opcodes-and-close-codes/#close-reasons) defines each exact reason.

## Transport and encoding

[Framing](/gateway/overview/#framing) owns the protocol version, the payload bound, and the compression contract. One inbound WebSocket message is limited to 4,096 bytes on the wire and to a further 4,096 bytes after decompression, and either bound closes with `4002` and reason `Payload too large`.

A compressed message that decompresses past 10 MiB closes with `4002` and reason `Decompression failed`, before the 4,096-byte bound is reached.

## Session lifecycle

Hello advertises a heartbeat interval of 41,250 ms. The Gateway checks the heartbeat state every 13,750 ms. When 37,125 ms have elapsed since the last acknowledgement it sends Opcode `1` and waits. If no new acknowledgement is accepted before the elapsed time passes 45,000 ms, the connection closes with `4009` and reason `Heartbeat timeout`.

There is no separate authentication deadline. The heartbeat rule alone closes a socket that never authenticates.

A disconnected session remains resumable for 60,000 ms. Its presence is published as `offline` 5,000 ms after the socket drops, well before the retention window ends. A successful Resume inside the window restores the status the session last selected.

One user credential holds at most 100 live sessions. A further Identify closes with `4008` and reason `Too many sessions`. A bot credential is bounded by the guild ceiling below.

Shard counts run from 1 through 16,384. One bot shard covers at most 2,500 guilds. A malformed shard pair closes with `4010` for every credential. A bot assignment above the guild ceiling closes with `4011` and reason `Sharding required`. A user session is never refused for its guild count.

One Gateway node admits 512 concurrent session starts by default, which an operator configures as `max_concurrent_session_starts` in the Gateway rollout config. Setting `session_rollout_percentage` to zero pauses session starts entirely, and a value below 100 admits only that share.

`session_rollout_mode` decides which share. The default `modulo` hashes the account ID, so one account is admitted or refused consistently at a given percentage. The alternative `random` draws once per admission attempt, and the percentage is a share of session starts. One account can be admitted on one attempt and refused on the next.

:::note[A refused session start is held and retried]
The Gateway refuses a session start for draining, capacity, paused starts, the rollout percentage, or a failed backend RPC. It keeps the Identify payload and tries again after a jittered 1,000 ms to 1,999 ms delay until it succeeds. A rollout config change retries a held payload immediately.
:::

## Session start limit

[`GET /v1/gateway/bot`](/http-api/gateway/#get-gateway-information) returns a [session start limit](/http-api/gateway/#session-start-limit-object) object for client compatibility. Its four values are constants. Admission is bounded by the source IP Identify budget, the per-user session count, the node's concurrent session-start bucket, and the rollout percentage.

## Connection and command rate limits

A Gateway node running with `FLUXER_DISABLE_RATE_LIMITS` set to `1`, `true`, or `TRUE` disables nine budgets together. Six are the connection payload budget, the session payload budget, the source IP payload budget, the source IP connection ceiling, the Presence Update budget, and the Voice State Update queue. The other three are the source IP Identify budget, the per-user session count, and the 30-second complete member list budget. The figures below are the enforced defaults.

One WebSocket accepts 600 client payloads in a rolling 60-second window. One authenticated session accepts 600 client payloads in each fixed 60-second bucket. One source IP address accepts 6,000 client payloads in each fixed 60-second bucket. Exceeding any of these budgets closes the current connection with `4008` and reason `Rate limited`.

Fluxer evaluates the three payload budgets before any command-specific budget. The session budget is skipped while the connection is unauthenticated.

One source IP address holds 256 concurrent Gateway WebSockets. A further connection closes with `4008` and reason `Too many connections` before Hello is sent.

One source IP address makes 300 Identify attempts in each fixed 60-second window. A further attempt is discarded without a reply and without a close. The payload is not retained and is never retried, so the client sends a new Identify itself once the budget recovers, before the heartbeat deadline closes the socket.

Presence Update accepts five commands per WebSocket in a rolling 20-second window. A further update is discarded without closing the connection.

Voice State Update processes the first two commands per session in a rolling one-second window immediately. Later updates enter a per-session queue that holds at most 64 commands and drains one command every 500 ms. A newer update replaces an older queued update for the same `guild_id` and `connection_id` pair, and a full queue discards its oldest entry before accepting the new one.

Request Guild Members has one command-specific budget. A bot requesting a complete member list is limited to one accepted request per guild every 30 seconds, and a request inside that window produces [Rate Limited](/gateway/events/#rate-limited). The budget is keyed by the account and the guild together, so reconnecting does not reset it.

## Bounded commands

One WebSocket processes at most four bounded requests at once across [Request Guild Members](/gateway/commands/#request-guild-members), [Lazy Request](/gateway/commands/#lazy-request), [Request Guild Counts](/gateway/commands/#request-guild-counts), and [Request Channel Member Counts](/gateway/commands/#request-channel-member-counts). Each request has a 10,000 ms deadline, after which it stops. Events already emitted are not retracted, so a command that emits its result in several events can deliver a partial result and then stop.

Request Guild Members keeps one replaceable pending request while another member request is active. The most recent further request replaces any earlier pending one and starts when the active request finishes.

A bounded command that finds all four slots occupied is dropped. There is no queue, no result event, and no close.

Request Guild Counts queries each guild with a 2,000 ms deadline under an overall 3,000 ms batch deadline. Request Channel Member Counts queries its guild with a 2,000 ms deadline. A guild that misses its deadline is omitted from the result.

## Replay and backpressure

Resume history retains at most 4,096 Dispatch events and 16,777,216 bytes of retained event data, the 16 MiB total ceiling. When either bound is exceeded, the oldest retained events are dropped from the front of the buffer.

One Dispatch larger than 2,097,152 bytes, the 2 MiB single-event ceiling, is delivered to the socket and never retained. The connection is not closed for it, so a client that resumes across such an event does not receive it again.

Both byte bounds measure the in-memory size of the event inside the session process. That size differs from the JSON payload on the wire, so both figures are approximate.

[Guild Members Chunk](/gateway/events/#guild-members-chunk) is delivered live and is never retained for Resume, whatever its size.

[Guild Sync](/gateway/events/#guild-sync) and [Guild Member List Update](/gateway/events/#guild-member-list-update) are delivered with a sequence and never retained. Those two and Guild Members Chunk are the only events excluded by name. Every other Dispatch is retained, including the pre-encoded guild fan-out that broadcasts one event to every eligible session. A replay is therefore a subset of the sequence range it covers, which is why replay frames have sequence gaps.

A heartbeat with a sequence discards every retained Dispatch at or below that sequence and records it as the acknowledged sequence, so a client MUST acknowledge only a sequence whose events it has finished processing. A heartbeat with `null`, and a heartbeat with a sequence below the acknowledged sequence, change nothing. Resume neither acknowledges nor evicts. A client that never heartbeats with a sequence keeps its full window until the count or byte bound evicts from the front.

Resume closes with `4007` on a `seq` below the acknowledged sequence, and on a `seq` above the session's current sequence.

An eviction from the count or byte bound raises a replay floor to the highest sequence it dropped. A Resume with a `seq` below that floor produces Opcode `9` with `d: false` and no close, so a client that reconnects long after it fell behind Identifies again.

The session holds every Presence Update Dispatch in a pending queue until it dispatches [Ready](/gateway/events/#ready). After Ready, a Dispatch is held only when it names a guild the session is not connected to, or a user who is neither a friend nor a group DM recipient. The queue holds at most 2,048 entries and discards its oldest entry when full. The session releases the queue right after Ready and discards every held entry for a user the `presences` array already covers. A 10,000 ms timer releases it when Ready has not been dispatched by then. A held entry is also released for one user when a relationship or a channel brings that user into scope.

Fluxer drops a presence cast to a session process whose mailbox already holds more than 5,000 messages. A session that cannot keep up sheds presence casts and stays connected.

## Command payload limits

Request Guild Members accepts at most 100 user IDs, a nonce of at most 32 bytes, and a result limit from 0 through 100. Duplicate guild IDs are collapsed, an out-of-range limit is clamped, and an oversized nonce becomes null. An oversized `user_ids` array abandons the request. The nonce is echoed only when the request named exactly one guild.

Lazy Request accepts at most 10 member list ranges per channel, each with `end` at most 100,000 and `end - start` at most 99, and at most 1,000 explicit member IDs per guild. Ranges and IDs that fail validation are dropped, and valid entries past either ceiling are truncated.

Request Guild Counts accepts at most 100 guild IDs after deduplication. Request Channel Member Counts accepts at most 25 channel IDs after deduplication. Both nonces run from 1 through 64 bytes, and a nonce outside that bound is omitted from the result.

Identify accepts at most 256 `ignored_events` entries. A longer array closes with `4002` and reason `Invalid identify payload`. Every other bound on this page coerces or drops. [Client commands](/gateway/commands/) states the exact coercion or drop rule for each field.

## Voice admission

Voice admission follows the enclosing guild, DM, group DM, channel, and permission rules. A refusal is reported as [Voice State Ack](/gateway/events/#voice-state-ack) with an `error_code` and closes nothing. A Voice State Update that has no `mutation_id` produces no event when it is refused.

A guild voice channel admits at most `user_limit` users, where `0` means unlimited. A channel in which any participant has a camera enabled additionally admits at most 25 users in total, whatever its `user_limit`. A channel that already holds 25 users with cameras enabled refuses a further camera with `VOICE_CAMERA_USER_LIMIT`.

One user holds at most `voice_connection_limit` simultaneous voice connections in one guild voice channel. The field is part of the [channel object](/http-api/channels/#channel-object), defaults to 5, and is accepted from 1 through 100. Pending connections that have not yet expired count against it.
