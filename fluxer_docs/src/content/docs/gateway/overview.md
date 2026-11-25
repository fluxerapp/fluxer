---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway overview
description: How to connect to the Gateway, keep the connection alive, resume a session and shard a bot.
---

The Gateway is a WebSocket that sends you account and [guild](/http-api/guilds/) events and accepts a small set of [commands](/gateway/commands/). Everything else is an [HTTP API](/http-api/) call.

## Connecting

1. Get `endpoints.gateway` from the [instance discovery document](/http-api/instance/#get-instance-discovery) and open a WebSocket to it with the [connection parameters](#connection-parameters).
2. Wait for opcode 10 [Hello](#hello) and start sending a [heartbeat](#heartbeats) every `heartbeat_interval` milliseconds.
3. Send opcode 2 [Identify](/gateway/commands/#identify) with your token and `properties`.
4. Keep the `session_id` from [Ready](/gateway/events/#ready) for a later [Resume](#resuming-a-session).

```json
{
  "op": 2,
  "d": {
    "token": "YOUR_TOKEN",
    "properties": {
      "os": "Linux",
      "browser": "Fluxer Client",
      "device": "desktop"
    }
  }
}
```

Only `token` and `properties` are required. Send the raw token with no prefix. Bots can call [`GET /v1/gateway/bot`](/http-api/gateway/#get-gateway-information) to get the URL and a recommended [shard](#sharding) count.

## Connection parameters

Add these to the query of the discovered URL and keep any query it already has.

| Field | Type | Description |
| --- | --- | --- |
| v | integer | The Gateway protocol version, which is `1` |
| encoding? | string | The payload encoding, which is always `json` |
| compress? | string | `zstd-stream` for compression, and any other value turns it off |
| stream? | string | `1` or `true`, which `compress=zstd-stream` needs to take effect |

A missing or different `v` closes the connection with `4012` and reason `Invalid API version` before Hello. The reference client connects with this query string.

```text
?v=1&encoding=json&compress=zstd-stream&stream=1
```

## Gateway payload

Every WebSocket message is one UTF-8 JSON payload.

| Field | Type | Description |
| --- | --- | --- |
| op | integer | [Gateway opcode](/gateway/opcodes-and-close-codes/#opcodes) |
| d? | any | The opcode data, which Heartbeat ACK and Reconnect leave out |
| s? | integer | The session sequence, only on Dispatch |
| t? | string | The event name, only on Dispatch |

A payload that is not a JSON object with an `op` closes the connection with a [close code](/gateway/opcodes-and-close-codes/#close-codes).

Ready has `s` 1, and each later live Dispatch raises `s` by one. A replayed Dispatch keeps its original sequence, and [Resumed](/gateway/events/#resumed) has the current sequence without raising it. Some events are [never replayed](/gateway/events/#dispatch-delivery).

## Framing

A client message can be at most 16,384 bytes on the wire and 16,384 bytes after decompression. A larger one closes with `4002`.

[Snowflakes](/snowflakes/) are decimal strings, and every other number is a JSON number.

With `zstd-stream`, every server frame is binary, starting with Hello. Pass every frame to one decompressor in order for the whole connection, and send every client message through one compressor that also lasts the whole connection.

## Hello

The server sends Hello as soon as it accepts the socket. The interval is in milliseconds.

```json
{
  "op": 10,
  "d": {
    "heartbeat_interval": 41250
  }
}
```

A connection that goes over the [open connections per IP](/gateway/limits-and-rate-limits/#connection-and-command-rate-limits) limit closes with `4008` before Hello.

## Identify

Identify creates a session and the Gateway responds with Ready. These failures close the connection.

| Close | Reason | Cause |
| --- | --- | --- |
| 4004 | `Invalid token` | The token is not valid |
| 4008 | `Too many sessions` | The user already has 100 live sessions (bots have no limit) |
| 4008 | `Identify rate limited` | More than 300 Identify and Resume commands came from your IP address in 60 seconds |
| 4010 | `Invalid shard` | The `shard` pair is malformed |
| 4011 | `Sharding required` | A bot shard owns more than 2,500 guilds |
| 4000 | `Failed to start session` | The session could not be created for another reason |

An Identify can stay pending with no reply while the Gateway is busy. Keep heartbeating until Ready arrives. An Identify or Resume over the [per-IP budget](/gateway/limits-and-rate-limits/#connection-and-command-rate-limits) closes the connection with `4008`. Any command other than Heartbeat, Identify or Resume sent before Identify or Resume closes with `4003`. Commands sent after Identify and before Ready wait until the session starts, up to 64 of them, and more close with `4008`. A socket that sends no Identify or Resume within one heartbeat interval of Hello closes with `4003`, and an Identify or Resume on a socket that already has a session closes with `4005`.

## Heartbeats

Send opcode 1 with the sequence of the last Dispatch you processed, or `null` before the first Dispatch.

```json
{
  "op": 1,
  "d": 42
}
```

The server responds with opcode 11 Heartbeat ACK. Once a session exists, a `d` that is neither `null` nor an integer closes with `4007`. If 90 per cent of the interval passes without an acknowledgement, the server sends its own opcode 1 with `d: null`, and you respond with a heartbeat right away. The connection closes with `4009` when the [heartbeat timeout](/gateway/limits-and-rate-limits/#session-lifecycle) passes with no acknowledgement.

The Gateway drops replayable Dispatches up to the sequence in your heartbeat. Only send a sequence you have finished processing.

## Resuming a session

Opcode 6 resumes a session on a new socket within 60,000 ms after the old one closes, unless the client closed it with `1000` or `1001`. Send the original token, the Ready `session_id` and the last sequence you processed. A payload that is missing any of them closes with `4002`.

```json
{
  "op": 6,
  "d": {
    "token": "YOUR_TOKEN",
    "session_id": "6f1d0b7c9a2e4f83b5c1d9e7a4f20b13",
    "seq": 42
  }
}
```

The Gateway replays every Dispatch above `seq` in order and then sends [Resumed](/gateway/events/#resumed). Your presence shows as `offline` 5,000 ms after the socket drops, and a Resume brings back the status you had. The [replay buffer](/gateway/limits-and-rate-limits/#replay-and-backpressure) keeps at most 4,096 Dispatch events and about 16 MiB of event data.

| Condition | Result |
| --- | --- |
| The session is unknown or expired, or `seq` is below the replay floor | [Invalid Session](#invalid-session), and the socket can Identify |
| `seq` is negative, above the session sequence or below the acknowledged one | Close code `4007`, reason `Invalid sequence` |
| The token does not own the session | Close code `4004`, reason `Invalid token` |
| The session does not respond | Close code `4000`, reason `Session unavailable` |

## Reconnect

Opcode 7 asks you to open a new WebSocket and [resume](#resuming-a-session). The Gateway sends it when it moves your session or another socket resumes the same session, and then closes the old socket with close code `4000`. An Identify sent to a draining node also gets opcode 7 and a `4000` close, and you Identify again on a new socket.

## Invalid session

Opcode 9 with `d: false` means the session cannot be resumed, after a failed [Resume](#resuming-a-session) or when the session ends while you are connected. The socket stays open, and you can send Identify on it. Fluxer never sends `d: true`.

## Sharding

Identify takes an optional `shard` of `[shard_id, shard_count]`. `shard_id` is below `shard_count`, and `shard_count` runs from 1 to 16,384. A guild belongs to shard `(guild_id >> 22) % shard_count`.

A session only receives the guilds its shard owns. Ready lists them in `guilds` and echoes the pair as `shard`. A bot gets each guild as unavailable, followed by one [Guild Create](/gateway/events/#guild-create) or [Guild Delete](/gateway/events/#guild-delete). One bot shard can have at most 2,500 guilds, and a bot that sends no `shard` counts all its guilds.

Dispatches for no guild, such as account-level events, only go to shard 0 and to sessions that sent no `shard`, apart from Rate Limited, Guild Counts Update and Channel Member Counts Update.

## Ordering

Events are in order within one session only. When you get [Guild Create](/gateway/events/#guild-create) or [Guild Sync](/gateway/events/#guild-sync), replace your cached roles, channels, emojis, stickers, voice states and threads for that guild, and apply every other Dispatch as a change to that cache.
