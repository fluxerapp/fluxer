---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Opcodes and close codes
description: The opcodes the main Gateway sends and accepts, and the close codes and reasons it uses.
---

Every [Gateway payload](/gateway/overview/#gateway-payload) has an opcode, and the [Gateway](/gateway/overview/) ends a connection with a close code.

## Opcodes

| Op | Name | Description |
| --- | --- | --- |
| 0 | Dispatch | Server to client. An event with its name in `t` and a sequence in `s` |
| 1 | Heartbeat | Both ways. The client sends its last sequence or `null` in `d`. Respond to a server heartbeat at once |
| 2 | Identify | Client to server. Starts a session with an [Identify object](/gateway/commands/#identify) |
| 3 | Presence Update | Client to server. A [Presence Update object](/gateway/commands/#presence-update) |
| 4 | Voice State Update | Client to server. A [Voice State Update object](/gateway/commands/#voice-state-update) |
| 5 | Voice Server Ping | Reserved. The Gateway closes a client that sends it |
| 6 | Resume | Client to server. Restores a session with a [Resume object](/gateway/commands/#resume) |
| 7 | Reconnect | Server to client, with no `d`, before a close with `4000`. Reconnect and [resume](/gateway/commands/#resume) |
| 8 | Request Guild Members | Client to server. A [Request Guild Members object](/gateway/commands/#request-guild-members) |
| 9 | Invalid Session | Server to client, always with `d: false`. Start a new session with Identify |
| 10 | Hello | Server to client, with `heartbeat_interval`, as soon as the WebSocket opens |
| 11 | Heartbeat ACK | Server to client, with no `d`, for each accepted heartbeat |
| 12 | Gateway Error | Server to client. A [refused voice join](#gateway-error) |
| 13 | Voice Signal | Client to server. A [Voice Signal object](/gateway/commands/#voice-signal) for one other voice connection |
| 14 | Lazy Request | Client to server. A [Lazy Request object](/gateway/commands/#lazy-request) |
| 15 | Request Guild Counts | Client to server. A [Request Guild Counts object](/gateway/commands/#request-guild-counts) |
| 16 | Request Channel Member Counts | Client to server. A [Request Channel Member Counts object](/gateway/commands/#request-channel-member-counts) |
| 28 | Request Forum Unreads | Client to server. A [Request Forum Unreads object](/gateway/threads/#request-forum-unreads) |

Ignore a server opcode that isn't in this table.

## Gateway Error

Opcode `12` tells the session that sent a [Voice State Update](/gateway/commands/#voice-state-update) that the join was refused. It has no `s` or `t`, and the connection stays open.

| Field | Type | Description |
| --- | --- | --- |
| code | string | The refusal code |
| message | string | A readable description for logs |

```json
{
  "op": 12,
  "d": {
    "code": "VOICE_P2P_CONSENT_REQUIRED",
    "message": "This call is peer-to-peer and requires agreement to join"
  }
}
```

The Gateway sends three codes.

- `VOICE_P2P_CONSENT_REQUIRED` means the call is peer-to-peer and the join didn't send `p2p: true`. Ask the user, then join again with `p2p: true`. [Peer-to-peer voice](/gateway/events/#peer-to-peer-voice) has the flow.
- `VOICE_P2P_UNAVAILABLE` means the Gateway declined peer-to-peer for this join. Abort the join and tell the user.
- `VOICE_CHANNEL_FULL` means the channel is at its `user_limit` or the peer-to-peer call is full. Abort the join and tell the user.

Other refused voice joins get no opcode 12.

## Close codes

| Code | Name | Meaning |
| --- | --- | --- |
| 4000 | Unknown error | The node is draining, session creation failed, or the session in Resume did not respond |
| 4001 | Unknown opcode | An undefined or Gateway-only opcode, a payload with no `d`, or opcode 28 without [thread access](/gateway/threads/#session-flag) or an object `d` |
| 4002 | Decode error | The payload size, compression, encoding or command fields are invalid |
| 4003 | Not authenticated | A command arrived before Identify or Resume attached a session |
| 4004 | Authentication failed | The token is invalid, or it doesn't own the session in Resume |
| 4005 | Already authenticated | Identify or Resume arrived while a session was attached, or with no `d` |
| 4007 | Invalid sequence | Heartbeat `d` isn't an integer or `null`, or Resume `seq` is [out of range](#invalid-sequence) |
| 4008 | Rate limited | A connection, payload, source IP or session budget was exceeded |
| 4009 | Session timeout | No heartbeat was accepted for 45,000 ms |
| 4010 | Invalid shard | The Identify `shard` isn't a valid `[shard_id, shard_count]` pair<sup>1</sup> |
| 4011 | Sharding required | More than 2,500 guilds belong to one bot session<sup>2</sup> |
| 4012 | Invalid API version | The `v` connection parameter is missing or isn't `1` |

<sup>1</sup> `shard_count` is 1 to 16384 and `shard_id` is a non-negative integer below it

<sup>2</sup> The Gateway counts guilds after the shard filter. Raise `shard_count` until each shard has at most 2,500.

If the connection had a session, you can [Resume](/gateway/commands/#resume) it for 60,000 ms after a network failure or any close except `1000` and `1001`. A `1000` or `1001` close from the client ends the session right away, so close with another code, such as `4000`, when you plan to resume. When a Resume closes with `4004` or `4007`, the session stays and you can try again. Resume only replays events still in the [replay buffer](/gateway/limits-and-rate-limits/#replay-and-backpressure).

Change the token, shard pair or version before you reconnect after `4004`, `4010` or `4012`.

### Invalid sequence

Heartbeat closes with `4007` only after a session is attached.

Resume closes with `4007` when `seq` is above the last sequence the session dispatched or below the last acknowledged sequence. A `seq` inside those bounds but older than the oldest event in the replay buffer gets opcode 9 with `d: false`, and the session stays available.

## Close reasons

Every close with a 4000-range code has a reason. Branch on the code and log the reason. The reasons below say more than their code. Other codes use the close code name as the reason.

| Reason | Code | Cause |
| --- | --- | --- |
| `Failed to start session` | 4000 | Session creation failed for an unclassified reason |
| `Session unavailable` | 4000 | The session did not respond during Resume |
| `Session drain requested; reconnect to continue` | 4000 | Opcode 7 came just before the close |
| `Encode failed` | 4002 | Hello could not be encoded |
| `Compression failed: zstd-stream` | 4002 | Hello could not be compressed on a zstd connection |
| `Payload too large` | 4002 | A message is over 16,384 bytes, on the wire or after decompression |
| `Decompression failed` | 4002 | A compressed message could not be decompressed |
| `Decode failed` | 4002 | The payload isn't valid JSON or isn't an object |
| `Invalid payload` | 4002 | The decoded object has no `op` |
| `Invalid identify payload` | 4002 | Identify is missing `token` or `properties`, or a field has the wrong type |
| `Invalid resume payload` | 4002 | Resume is missing `token`, `session_id` or an integer `seq`, or a field has the wrong type |
| `Invalid presence payload` | 4002 | Presence Update is not an object or has no valid `status` |
| `Invalid voice payload` | 4002 | Voice State Update `d` is not an object |
| `Invalid token` | 4004 | The Identify token is invalid or the Resume token doesn't own the session |
| `Too many connections` | 4008 | The source IP has 256 concurrent connections |
| `Rate limited` | 4008 | The connection, source IP or user payload budget was exceeded |
| `Too many sessions` | 4008 | The user already has 100 live sessions, which doesn't apply to bots |
| `Identify rate limited` | 4008 | The instance is starting too many sessions at once. Try Identify again after a backoff |
| `Resume rate limited` | 4008 | The instance is resuming too many sessions at once. Try Resume again after a backoff |
| `Heartbeat timeout` | 4009 | No heartbeat was accepted for 45,000 ms |

The Gateway drops [some over-budget commands](/gateway/commands/), such as Presence Update and Voice Signal, without closing the connection.
