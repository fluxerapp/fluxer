---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Opcodes and close codes
description: Numeric registry for main Gateway opcodes, close codes, and exact close reasons.
---

An opcode is the number that names a [Gateway payload](/gateway/overview/#gateway-payload). A close code is the number the [Gateway](/gateway/overview/) sends when it ends a connection. Every value on this page belongs to the main Gateway alone. [Gateway overview](/gateway/overview/) describes the connection and its state machine, and [Client commands](/gateway/commands/) describes the payloads for these opcodes.

## Opcodes

| Op | Name | Direction |
| --- | --- | --- |
| 0 | Dispatch | Server to client |
| 1 | Heartbeat | Bidirectional |
| 2 | Identify | Client to server |
| 3 | Presence Update | Client to server |
| 4 | Voice State Update | Client to server |
| 5 | Voice Server Ping | Reserved |
| 6 | Resume | Client to server |
| 7 | Reconnect | Server to client |
| 8 | Request Guild Members | Client to server |
| 9 | Invalid Session<sup>1</sup> | Server to client |
| 10 | Hello | Server to client |
| 11 | Heartbeat ACK | Server to client |
| 12 | Gateway Error | Reserved |
| 14 | Lazy Request | Client to server |
| 15 | Request Guild Counts | Client to server |
| 16 | Request Channel Member Counts | Client to server |

<sup>1</sup> Fluxer never sends Opcode 9 with `d: true`, so an Invalid Session is never an instruction to resume

### Payload and when it is sent

| Op | Payload | When sent |
| --- | --- | --- |
| 0 | Event-specific data | Fluxer sends a named event with `t` and a session sequence with `s`<sup>1</sup> |
| 1 | The last processed sequence or `null` from the client, and `null` from the server | The client sends on the advertised schedule. The server requests an immediate heartbeat once 90 per cent of the advertised interval has elapsed since the last acknowledgement<sup>2</sup> |
| 2 | [Identify object](/gateway/commands/#identify) | The client starts one authenticated session |
| 3 | [Presence Update object](/gateway/commands/#presence-update) | The client replaces its session presence |
| 4 | [Voice State Update object](/gateway/commands/#voice-state-update) | The client joins, moves, updates, or leaves voice |
| 5 | Not implemented | Never sent. A client that sends it is closed<sup>3</sup> |
| 6 | [Resume object](/gateway/commands/#resume) | The client restores a retained session<sup>4</sup> |
| 7 | No `d` field | The Gateway asks the client to replace the connection and then closes with `4000`<sup>5</sup> |
| 8 | [Request Guild Members object](/gateway/commands/#request-guild-members) | The client requests bounded member chunks |
| 9 | Literal `false` | The session named by Resume could not be reached, the supplied `seq` is below the replay floor, or an established session ended<sup>6</sup> |
| 10 | Object with `heartbeat_interval` | Sent as the WebSocket is accepted, before any client payload is read |
| 11 | No `d` field | Acknowledges an accepted client heartbeat |
| 12 | Not implemented | Never sent. Errors reach the client as a Dispatch or a close frame |
| 14 | [Lazy Request object](/gateway/commands/#lazy-request) | The client replaces bounded guild subscriptions |
| 15 | [Request Guild Counts object](/gateway/commands/#request-guild-counts) | The client requests current guild count records |
| 16 | [Request Channel Member Counts object](/gateway/commands/#request-channel-member-counts) | The client requests channel count records |

<sup>1</sup> [Resumed](/gateway/events/#resumed) has the session's current sequence without advancing it, and a replayed Dispatch keeps the sequence it was first sent with

<sup>2</sup> The advertised interval is 41,250 ms, so the threshold is 37,125 ms and the acknowledgement deadline is 45,000 ms. Both are tested on a 13,750 ms timer and acted on at the first tick at or after them

<sup>3</sup> A client that sends Opcode 5 or 12 gets the same close as a client that sent an undefined opcode

<sup>4</sup> Resume is accepted whether or not a session is already attached to the connection

<sup>5</sup> Opcode 7 precedes the close when the Gateway node is draining, when the session is fenced for a cluster handoff, and when a Resume from a new socket displaces this one

<sup>6</sup> After the frame, a socket whose session ended is unauthenticated. After a failed Resume, a socket that already held a session still holds it

The registry is complete. Opcode 13 and every value above 16 are undefined.

Fluxer resolves an inbound payload in this order.

1. A payload that has no `d` reaches no command handler. Identify closes with `4005`. Every other opcode, defined or not, closes with `4001`.
2. Heartbeat and Resume are handled whether or not a session is attached.
3. Identify closes with `4005` when a session is already attached.
4. Every remaining opcode closes with `4003` while no session is attached.
5. With a session attached, Presence Update, Voice State Update, Request Guild Members, Lazy Request, Request Guild Counts, and Request Channel Member Counts are handled. Every other opcode, including a server opcode and an undefined value, closes with `4001`.

:::note[Unknown server opcodes are forward compatible]
A client that receives an unknown opcode SHOULD log it and ignore the frame. It MUST NOT close or reconnect solely because the server used an opcode newer than this registry.
:::

See [Client commands](/gateway/commands/) for field tables, validation bounds, examples, and results.

## Close codes

| Code | Name | Meaning |
| --- | --- | --- |
| 4000 | Unknown error | The Gateway drained the connection, or a session operation could not be completed |
| 4001 | Unknown opcode | The opcode is undefined, is a server opcode, or the payload has no `d` |
| 4002 | Decode error | The payload size, compression stream, encoding, or command fields are invalid |
| 4003 | Not authenticated | An authenticated command arrived before Identify or Resume attached a session |
| 4004 | Authentication failed | The token is invalid, or it does not own the retained session named by Resume |
| 4005 | Already authenticated | Identify arrived while a session was attached, or with no `d` |
| 4007 | Invalid sequence | Heartbeat or Resume supplied a sequence outside the bounds that command accepts, as stated in [Invalid sequence](#invalid-sequence) |
| 4008 | Rate limited | A concurrent connection, connection payload, source IP payload, user payload, or session count budget was exceeded |
| 4009 | Session timeout | More than 45,000 ms elapsed since the last accepted heartbeat acknowledgement while the Gateway was awaiting one |
| 4010 | Invalid shard | The Identify `shard` value is not a valid `[shard_id, shard_count]` pair<sup>1</sup> |
| 4011 | Sharding required | More than 2,500 guilds resolve to one bot Gateway session<sup>2</sup> |
| 4012 | Invalid API version | The `v` connection parameter is absent or is not `1` |

<sup>1</sup> `shard_count` is an integer from 1 to 16384, and `shard_id` is a non-negative integer below `shard_count`

<sup>2</sup> The count is taken after the shard filter, so a bot clears it by identifying with a `shard_count` large enough to divide its guilds

Code 4006 is unassigned, and no code above 4012 is defined. [Event filtering](/gateway/event-filtering/) describes how a client bounds the events its session receives.

### Session outcome

| Code | Resumable | Resulting state |
| --- | --- | --- |
| 4000 | Conditional | Closed. A session attached to the connection remains recoverable<sup>1</sup> |
| 4001 | Conditional | Closed. An already established session stays retained |
| 4002 | Conditional | Closed. An already established session stays retained |
| 4003 | No | Closed. No session exists for this connection |
| 4004 | Conditional | Closed. Resume does not invalidate the session it named<sup>2</sup> |
| 4005 | Conditional | Closed. A session established by an earlier Identify stays retained |
| 4007 | No for that attempt | Closed. The named session stays retained until its window expires |
| 4008 | Conditional | Closed. An already established session stays retained |
| 4009 | Conditional | Closed. An already established session stays retained |
| 4010 | No | Closed. No session was created |
| 4011 | No | Closed. No session was created |
| 4012 | No | Closed before Hello. No session exists |

<sup>1</sup> A session the close leaves without a socket is retained for 60,000 ms measured from the moment the socket ends, so a close begins a fresh window. A session displaced by a Resume from a new socket is already attached to that socket and enters no window

<sup>2</sup> A Resume that fails token verification leaves the named session in place for the rest of its retention window, so a later Resume with the owning token still recovers it. An Identify that fails token verification leaves nothing to recover

A session fenced for a cluster handoff stops on this node once its state has been copied to the node taking it over. Fluxer holds that copy for 120,000 ms and sweeps it on a 10,000 ms timer. The next Resume consumes it.

`Resumable` describes only whether an already established session can still be recovered with [Resume](/gateway/commands/#resume). The 60,000 ms retention window and the bounded replay buffer described in [Limits and rate limits](/gateway/limits-and-rate-limits/#replay-and-backpressure) apply unchanged.

:::caution[Reconnecting unchanged reproduces `4004`, `4010`, and `4012`]
A client changes the token, the shard pair, or the version before it reconnects.
:::

### Invalid sequence

Close `4007` follows one rule for [Heartbeat](/gateway/commands/#heartbeat) and another for [Resume](/gateway/commands/#resume).

Heartbeat tests the value's type alone, and only once a session is attached. Before Identify or Resume attaches one, the Gateway accepts every `d` and answers with Opcode 11. With a session attached, the Gateway accepts a `d` that is `null` or any integer, and every other value closes with `4007`. A sequence below the acknowledged sequence leaves that bound unchanged, and any other integer sets it and trims the replay buffer. A heartbeat that arrives in the short window between the session process ending and the socket noticing also closes with `4007`.

Resume tests two bounds and its `seq` must clear both.

- The current sequence. A `seq` above the last sequence the session dispatched closes with `4007`.
- The acknowledged sequence. A `seq` below the last acknowledged sequence closes with `4007`. A heartbeat with a higher sequence moves this bound.

A third bound, the replay floor, produces no close. It is the highest sequence the replay buffer has evicted. When a `seq` is inside both bounds but below the floor, the Gateway sends Opcode 9 with `d: false` and the named session stays retained until its window expires.

## Close reasons

The Gateway sends an exact reason string with every application close.

| Reason | Code | Cause |
| --- | --- | --- |
| Invalid API version | 4012 | The `v` connection parameter is absent or is not `1` |
| Too many connections | 4008 | The source IP already holds 256 concurrent Gateway connections |
| Encode failed | 4002 | Hello could not be encoded<sup>1</sup> |
| Compression failed: zstd-stream | 4002 | Hello could not be compressed on a connection that negotiated zstd<sup>1</sup> |
| Payload too large | 4002 | An inbound message exceeded 4,096 bytes on the wire, or exceeded 4,096 bytes after decompression |
| Decompression failed | 4002 | An inbound compressed message could not be decompressed |
| Decode failed | 4002 | The payload is not valid JSON, or decodes to something other than an object |
| Invalid payload | 4002 | The decoded object has no `op` |
| Rate limited | 4008 | The connection, source IP, or user client payload budget was exceeded |
| Unknown opcode | 4001 | The opcode is undefined, is a server opcode, or the payload has no `d` |
| Not authenticated | 4003 | An authenticated command arrived before a session was attached |
| Already authenticated | 4005 | Identify arrived while a session was attached, or with no `d` |
| Invalid identify payload | 4002 | Identify is missing `token` or `properties`, or a field has the wrong type |
| Invalid shard | 4010 | The Identify `shard` value is not a valid `[shard_id, shard_count]` pair |
| Sharding required | 4011 | More than 2,500 guilds resolve to one bot session after the shard filter |
| Too many sessions | 4008 | The user already holds 100 live Gateway sessions<sup>2</sup> |
| Invalid token | 4004 | The Identify token is invalid, or the Resume token does not own the named session |
| Failed to start session | 4000 | Session creation returned a failure the Gateway does not classify<sup>3</sup> |
| Invalid resume payload | 4002 | Resume is not an object, or `token` or `session_id` is missing or is not a string, or `seq` is missing or is not an integer |
| Invalid sequence | 4007 | Heartbeat or Resume supplied a sequence outside the bounds stated in [Invalid sequence](#invalid-sequence) |
| Invalid presence payload | 4002 | Presence Update is not an object, has no `status`, or has a `status` string that is not a known value |
| Session unavailable | 4000 | The retained session could not be reached while Resume was in progress |
| Session drain requested; reconnect to continue | 4000 | Opcode `7` was sent immediately before the close<sup>4</sup> |
| Heartbeat timeout | 4009 | No new heartbeat acknowledgement was accepted within 45,000 ms of the preceding acknowledgement |

<sup>1</sup> Both reasons are produced only while the Hello frame is being written. A later outbound frame that cannot be encoded or compressed is dropped and the connection stays open

<sup>2</sup> A bot credential is bounded by the guild count that forces sharding, and this budget does not apply to it

<sup>3</sup> The Gateway holds the Identify and retries it silently after a classified transient failure, so the connection stays open. That covers a paused rollout, a draining node, an ineligible account, an Identify rate limit, a saturated start budget, and a failed session RPC

<sup>4</sup> The Gateway node is draining, the session is being fenced for a cluster handoff, or a Resume from a new socket displaced this one

Reason strings are stable wire values. A client branches on the code and MAY record the reason for diagnosis.

:::note[Some refusals send nothing]
A held or discarded Identify, an over-budget Presence Update, a dropped bounded request, and a rejected voice state update produce no close and no reason. [Client commands](/gateway/commands/) states which command behaves that way.
:::

## Ordinary WebSocket closes

A transport can end with no Fluxer application close code, as happens on a network failure, an intermediary reset, and an ordinary `1000` or `1001` close. An established session remains available for 60,000 ms after the transport ends. Ending the transport does not extend that window or its bounded replay history.
