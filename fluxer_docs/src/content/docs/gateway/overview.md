---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway overview
description: The main Gateway connection, framing, state machine, heartbeats, resumption, and sharding.
---

The main Gateway is a persistent WebSocket. It sends account and [guild](/http-api/guilds/) events and accepts a small set of bounded client commands. Every other read or mutation is an [HTTP API](/http-api/) operation.

:::note[Each surface has its own framing and authentication]
The main Gateway, the HTTP API, and the [Media Proxy API](/media-proxy/overview/) each define their own. Nothing negotiated on one applies to another.
:::

## Connecting

A client opens the socket, waits for Hello, sends Identify, and then heartbeats for the life of the connection.

1. Read `endpoints.gateway` from the [instance discovery document](/http-api/instance/#get-instance-discovery) and open a WebSocket to it with `?v=1&encoding=json`.
2. Read Opcode 10 [Hello](#hello-and-session-creation) and take `heartbeat_interval` from it.
3. Send Opcode 2 [Identify](/gateway/commands/#identify) with the token and `properties`.
4. Read [Ready](/gateway/events/#ready) and keep its `session_id` for a later [Resume](#resuming-a-session).
5. Send Opcode 1 [Heartbeat](#heartbeats) with the last Dispatch sequence every `heartbeat_interval` milliseconds.

```json
{
  "op": 2,
  "d": {
    "token": "flx_ZDb1GURItsMuYl1zvrgxv2qLBxyNmgNSEaWT",
    "properties": {
      "os": "Linux",
      "browser": "Fluxer Client",
      "device": "desktop"
    }
  }
}
```

`token` and `properties` are the only required fields. The token is the raw account or bot token. It has no HTTP authentication prefix, so a bot sends the token without the `Bot ` prefix the HTTP API requires. [Client commands](/gateway/commands/#identify) defines the rest. Everything the server sends after Ready is a [Dispatch](/gateway/events/#dispatch-delivery), which is one event payload with its name in `t` and its data in `d`.

## Protocol version

Gateway version `1` is the only version. The `v=1` connection parameter selects it. The Gateway closes with `4012` and reason `Invalid API version` for any other value and for an omitted one, before it sends Hello.

## Discovering the endpoint

Read `endpoints.gateway` from the [instance discovery document](/http-api/instance/#get-instance-discovery). A bot can instead call [`GET /v1/gateway/bot`](/http-api/gateway/#get-gateway-information), which returns the same WebSocket URL together with a recommended [shard](#sharding) count and a [session start limit](/http-api/gateway/#session-start-limit-object) object. That route rejects any credential outside bot token form with 401 `INVALID_AUTH_TOKEN`.

## Connection parameters

Append the connection parameters to the discovered URL.

| Field | Type | Description |
| --- | --- | --- |
| v | integer | The Gateway protocol version, which is `1` |
| encoding?<sup>1</sup> | string | The payload encoding |
| compress?<sup>2</sup> | string | The compression stream, which accepts `zstd-stream` or `none` and defaults to no compression |
| stream?<sup>3</sup> | string | The flag that enables the compression stream, which accepts `1` or `true` |

<sup>1</sup> Only `json` is implemented. Every value, including an absent one, selects JSON

<sup>2</sup> Any other value selects no compression

<sup>3</sup> `compress=zstd-stream` selects compression only when `stream` is also `1` or `true`. Without the flag the connection is uncompressed

Unknown parameters are ignored. An unrecognised `compress` or `stream` value selects no compression and the connection stays open. A client MUST read the negotiated representation from the frame type it receives.

The reference client connects with `?v=1&encoding=json&compress=zstd-stream&stream=1`.

:::caution[A connection can close before Hello]
The Gateway validates the version and applies the concurrent connection limit before sending Hello, so a rejected connection can close with no payload.
:::

## Gateway payload

Every logical message uses the Gateway payload.

| Field | Type | Description |
| --- | --- | --- |
| op | integer | [Gateway opcode](/gateway/opcodes-and-close-codes/#opcodes) |
| d?<sup>1</sup> | any | The opcode payload, required in every client-to-server payload and opcode-dependent in server-to-client payloads |
| s? | integer | The non-negative session sequence, present only on Dispatch |
| t? | string | The event name, present only on Dispatch |

<sup>1</sup> Heartbeat ACK and Reconnect have no `d` at all, and Invalid Session has the Boolean `false`

A decoded payload that is not a JSON object closes with `4002` and reason `Decode failed`. An object with no `op` closes with `4002` and reason `Invalid payload`. An object with `op` but no `d` closes with `4001` and reason `Unknown opcode`, except for Identify, which closes with `4005`.

A Dispatch is a server-to-client event payload. Every live Dispatch advances the session sequence by one. A session starts at sequence 0, so the [Ready](/gateway/events/#ready) sequence is 1.

A replayed Dispatch keeps its original sequence, and a replayed run can have gaps, because several event families are delivered live and never retained. [Resumed](/gateway/events/#resumed) has the current sequence and does not advance it, which sets the new live baseline. The sequence is local to one Gateway session and has no meaning across sessions or shards.

## Framing

One inbound WebSocket message is limited to 4,096 bytes on the wire, and a compressed inbound message is limited to a further 4,096 bytes after decompression. A message past either bound closes with `4002` and reason `Payload too large`. An inbound message that cannot be decompressed closes with `4002` and reason `Decompression failed`.

JSON payloads are UTF-8 objects. An uncompressed client payload is sent in a text frame, and a payload the client compressed with the negotiated zstd stream is sent in a binary frame.

The Gateway does not inspect the inbound frame type. It reads the bytes from the negotiated compression alone. A client MUST send every payload in the negotiated representation. On a connection with no negotiated compression the payload is uncompressed, and on a `zstd-stream` connection every client payload goes through the same compression stream in order.

### JSON integer representation

Snowflakes are decimal strings. See [Snowflakes](/snowflakes/) for the identifier contract. Sequences, counts, versions, and bitfields are JSON numbers.

## Compression

`zstd-stream` is a continuous stream in both directions. A client MUST feed every server frame to the same decompressor in arrival order and produce every client frame from the same compressor.

The server compresses at level 3, and one WebSocket message has exactly one Gateway payload.

Hello is already compressed on a connection that negotiated `zstd-stream`, so the first frame such a connection receives is a binary frame.

When the server cannot load the zstd streaming implementation, the connection closes with `4002` and reason `Compression failed: zstd-stream` as it tries to send that first frame.

:::note[Compression is negotiated once]
A connection cannot change its compression stream after the upgrade. Changing it requires a new WebSocket, which means a new Hello and either a new Identify or a Resume.
:::

## Signalling state machine

A connection moves through five states: Opening, Unauthenticated, Starting, Replaying, and Ready. The tables below give every event a state accepts, the action it triggers, and the state it lands in. Heartbeat is accepted in every open state, and Closed is terminal for that WebSocket.

### Opening

| Event and condition | Action | Next state |
| --- | --- | --- |
| WebSocket accepted. `v=1` and connection capacity available | Send Hello | Unauthenticated |
| Invalid version. `v` is absent or is not `1` | Close with `4012` and reason `Invalid API version` | Closed |
| Concurrent connection limit reached. The source IP already holds 256 connections | Close with `4008` and reason `Too many connections` | Closed |

### Unauthenticated

| Event and condition | Action | Next state |
| --- | --- | --- |
| Identify. Valid Identify payload and Identify capacity available | Begin session creation | Starting |
| Identify. Gateway draining, node at capacity, session starts paused, or the account outside the session rollout | Hold the payload and retry it in the background | Unauthenticated |
| Identify. The source IP Identify budget is exhausted | Discard the payload without a reply | Unauthenticated |
| Resume. Valid Resume payload | Resolve the retained session | Starting |
| Authenticated command. Any command other than Heartbeat, Identify, or Resume | Close with `4003` and reason `Not authenticated` | Closed |

### Starting

| Event and condition | Action | Next state |
| --- | --- | --- |
| Session creation succeeds. Identify was accepted | Send Ready | Ready |
| Session creation fails permanently. Invalid token, invalid shard, sharding required, or too many sessions | Close with the mapped code and reason | Closed |
| Session creation fails without a mapped code | Close with `4000` and reason `Failed to start session` | Closed |
| Session creation fails temporarily. Draining, at capacity, RPC failure, timeout, or the account outside the session rollout | Hold the Identify and retry it in the background | Unauthenticated |
| Resume succeeds. The retained session accepted the sequence | Replay retained Dispatches | Replaying |
| Session cannot be resumed. Resume named an unknown or expired session, or a `seq` below the replay floor | Send Invalid Session with `d: false` | Unauthenticated |

### Replaying

| Event and condition | Action | Next state |
| --- | --- | --- |
| Retained replay completes. All retained Dispatches were sent | Send Resumed | Ready |
| Authenticated command. The session is attached | Process the command | Replaying |
| Identify. A session is attached | Close with `4005` and reason `Already authenticated` | Closed |
| Resume. Valid Resume payload | Attach the named session to this socket in place of the current one | Starting |

### Ready

| Event and condition | Action | Next state |
| --- | --- | --- |
| Authenticated command. The command is valid in the session | Process the command | Ready |
| Identify. A session is attached | Close with `4005` and reason `Already authenticated` | Closed |
| Resume. Valid Resume payload | Attach the named session to this socket in place of the current one | Starting |

### Replaying or Ready

| Event and condition | Action | Next state |
| --- | --- | --- |
| The session process ends. The session was terminated while this socket held it | Send Invalid Session with `d: false` | Unauthenticated |
| Gateway drain or session transfer. The node stops serving this session | Send Reconnect and close with `4000` | Closed |
| The session is resumed elsewhere. Another socket attached this session with Resume | Send Reconnect and close with `4000` | Closed |

### Any open state

| Event and condition | Action | Next state |
| --- | --- | --- |
| Heartbeat. No session is attached, or the payload is `null`, or the attached session accepts the sequence | Send Heartbeat ACK | Same state |
| Heartbeat deadline. The connection is awaiting an acknowledgement and more than 45,000 ms have passed since the last one | Close with `4009` and reason `Heartbeat timeout` | Closed |
| Invalid frame or payload. Size, decompression, or decoding validation fails | Close with the applicable close code | Closed |
| Transport terminates. A session exists | Retain the session for 60,000 ms | Closed |

An opcode outside the registry, and a server opcode sent by a client, close with `4001` once a session is attached and with `4003` while the connection is unauthenticated.

## Hello and session creation

The server sends Opcode 10 Hello while accepting the WebSocket.

```json
{
  "op": 10,
  "d": {
    "heartbeat_interval": 41250
  }
}
```

The advertised interval is in milliseconds and is authoritative for the connection.

Opcode 2 Identify creates a session. A successful Identify sends [Ready](/gateway/events/#ready), whose `session_id` identifies the retained session. Fluxer publishes no separate resume URL, so a Resume reconnects to the same Gateway endpoint the client discovered.

An invalid token closes with `4004` and reason `Invalid token`. A user account that already holds 100 live sessions closes with `4008` and reason `Too many sessions`, and a bot credential is not bounded by that maximum. A malformed shard pair closes with `4010` and reason `Invalid shard`. A bot shard assignment that resolves more than 2,500 guilds closes with `4011` and reason `Sharding required`.

:::note[A held Identify sends nothing back]
A draining node, a node at capacity, paused session starts, and an account outside the rollout percentage all keep the payload and retry it every 1 to 2 seconds.
:::

:::caution[An over-budget Identify is dropped and the client resends]
Once the source IP Identify budget is exhausted, Fluxer keeps no copy of the payload. The client sends its next Identify itself, at a rate well below that budget.
:::

## Heartbeats

Opcode 1 is accepted before and after authentication. Before a session exists its payload is ignored and the server still acknowledges. Once a session exists, send the most recently processed Dispatch sequence, or `null` before any Dispatch.

```json
{
  "op": 1,
  "d": 42
}
```

The server answers with Opcode 11 Heartbeat ACK, which has no `d`. Once a session is attached, a `d` value that is neither `null` nor an integer closes with `4007` and reason `Invalid sequence`. A session that does not answer within 5,000 ms closes with the same code and reason.

The Gateway also runs its own timer, which ticks every 13,750 ms. On the first tick at or after 37,125 ms since the last acknowledgement, it sends Opcode 1 with `d: null` and marks the connection as awaiting an acknowledgement. On the first tick more than 45,000 ms after that acknowledgement, it closes with `4009` and reason `Heartbeat timeout`. A connection that never answers is therefore asked at 41,250 ms and closed at 55,000 ms.

A client MUST answer the server's Opcode 1 with its own Opcode 1.

The Gateway resets the elapsed time and clears the awaiting state when it accepts a client Heartbeat. Its own Opcode 1 does neither, so the deadline keeps running from the last client Heartbeat.

A heartbeat with a sequence permanently trims every retained Dispatch at or below that sequence from the replay buffer and records it as the acknowledged sequence. A client MUST send the sequence it has actually processed, because a later Resume from a lower sequence closes with `4007`.

## Resuming a session

Opcode 6 supplies the original token, the Ready `session_id`, and the last processed Dispatch sequence.

```json
{
  "op": 6,
  "d": {
    "token": "...",
    "session_id": "6f1d0b7c9a2e4f83b5c1d9e7a4f20b13",
    "seq": 42
  }
}
```

A successful Resume replays every retained Dispatch above `seq` in order and ends with [Resumed](/gateway/events/#resumed).

All three fields are required. A missing field, a `token` or `session_id` that is not a string, or a `seq` that is not an integer closes with `4002` and reason `Invalid resume payload`.

The Gateway retains a disconnected session for 60,000 ms. An accepted `seq` is no greater than the session's current sequence and no less than the sequence the session has already acknowledged. A `seq` outside either bound closes with `4007` and reason `Invalid sequence`.

An unknown or expired session sends Opcode 9 with `d: false` and leaves the socket unauthenticated. A `seq` inside both bounds but below the replay floor sends the same Opcode 9. The replay floor is the highest sequence the replay buffer has evicted. A token that does not own the named session closes with `4004` and reason `Invalid token`. A session that cannot be reached closes with `4000` and reason `Session unavailable`. None of those failures destroys a separately retained session.

A successful Resume replaces the session's socket and restores the presence status the session last selected. A session whose socket dropped is published as `offline` after 5,000 ms, so a Resume later than that republishes the restored status.

Unlike Identify, Resume is accepted in every open state. A socket that already has a session attached still processes a Resume and attaches the named session in its place. Send Resume only on a fresh socket.

When the resumed session was attached to a different socket, that socket receives Opcode 7 Reconnect and then closes with `4000`.

:::caution[Retention covers reconnection recovery only]
[Limits and rate limits](/gateway/limits-and-rate-limits/#replay-and-backpressure) states the exact bounds, and several high-volume Dispatch events are never retained.
:::

## Reconnect

Opcode 7 Reconnect asks the client to open a new WebSocket. The Gateway sends it when the node drains the session, when the node transfers the session to another node, and to the socket a successful Resume displaces.

```json
{
  "op": 7
}
```

The current socket then closes with `4000` and reason `Session drain requested; reconnect to continue`. The session can be resumed while it remains inside its retention bounds.

## Invalid session

Opcode 9 with `d: false` means the named session cannot be resumed. The Gateway sends it when Resume names an unknown or expired session, when Resume names a `seq` below the replay floor, and when the session process attached to a live connection ends.

```json
{
  "op": 9,
  "d": false
}
```

The socket returns to the unauthenticated state, so it stays open and can Identify again. Fluxer never sends `d: true`.

## Sharding

In Identify, a client MAY supply `shard` as a `[shard_id, shard_count]` pair. `shard_id` is a non-negative integer below `shard_count`, and `shard_count` is from 1 through 16,384. An omitted or null value applies no sharding. A malformed pair closes with `4010` and reason `Invalid shard`.

A guild belongs to `((guild_id >> 22) % shard_count)`, computed on the integer value of the guild's decimal snowflake string.

The pair selects the session's guild membership. At Identify, Fluxer filters the account's guild list to the guilds the shard owns, and the session connects only to those.

For a user session, the filtered set is also the [Ready](/gateway/events/#ready) `guilds` array. For a bot session, it is the guild burst of [Guild Create](/gateway/events/#guild-create) and [Guild Delete](/gateway/events/#guild-delete) Dispatches that follows Ready. Ready echoes the accepted pair back as `shard`.

Fluxer checks only a bot session against the guild ceiling. A bot whose shard owns more than 2,500 guilds closes with `4011` and reason `Sharding required`. A bot that supplies no pair is checked against its whole guild list. A user session is bounded by the 100-session-per-user limit alone, whatever its guild count.

:::note[Shard 0 also receives account-level traffic]
The per-Dispatch shard filter in [Dispatch delivery](/gateway/events/#dispatch-delivery) runs only when `shard_id` is not 0. No other shard receives a copy, so handle those events on shard 0.
:::

Fluxer has no large bot tier, no shard-count alignment requirement, and no Identify concurrency buckets. `GET /v1/gateway/bot` returns a fixed recommendation.

## Ordering

Dispatch ordering applies within one Gateway session. It creates no total order across shards, HTTP responses, or Media Proxy operations.

[Guild Create](/gateway/events/#guild-create) and [Guild Sync](/gateway/events/#guild-sync) are replacement boundaries for the guild they name. Everything else is a delta against the state those boundaries established.
