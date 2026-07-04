# Opcodes and Close Codes

Quick reference for all gateway protocol integers. Source of truth is `constants.erl`; see [shared-utilities.md](shared-utilities.md) for the full constants API and [websocket-handler.md](websocket-handler.md) for how each opcode is dispatched.

---

## Opcodes

Every gateway payload carries an `"op"` integer field. The gateway uses `constants:gateway_opcode/1` to map integers to atoms at the handler layer; `constants:opcode_to_num/1` is the reverse.

| `op` | Name | Direction | When sent |
|------|------|-----------|-----------|
| 0 | `dispatch` | S→C | Server pushes an event to the client. The `"t"` field carries the event name; `"d"` carries the payload; `"s"` is the sequence number used for Resume. |
| 1 | `heartbeat` | C→S | Client sends its current sequence number to keep the connection alive. Server replies immediately with op 11. |
| 2 | `identify` | C→S | Client authenticates with a token and declares its capabilities (shard, encoding, compression, `e2ee_capable`). Must be sent before any other C→S opcode. |
| 3 | `presence_update` | C→S | Client updates its online status, activities, or custom status. |
| 4 | `voice_state_update` | C→S | Client joins, moves between, or leaves a voice channel. |
| 5 | `voice_server_ping` | C→S | Keepalive for an active voice connection. |
| 6 | `resume` | C→S | Client reconnects and requests replay of missed events using a session token and sequence number. |
| 7 | `reconnect` | S→C | Server instructs the client to disconnect and reconnect immediately (e.g. during a rolling deploy drain). The session remains alive for resume. |
| 8 | `request_guild_members` | C→S | Client requests a batch of offline member data for a guild. |
| 9 | `invalid_session` | S→C | Server rejects an Identify or Resume. The `"d"` boolean indicates whether the session can be resumed (`true`) or must start fresh (`false`). |
| 10 | `hello` | S→C | First message after WebSocket upgrade. Carries `heartbeat_interval` (41 250 ms). |
| 11 | `heartbeat_ack` | S→C | Acknowledges a client heartbeat. Also used by the session to advance `ack_seq` and trim the replay buffer. |
| 12 | `gateway_error` | S→C | Server sends a structured error payload outside of a close frame, typically before sending a close. |
| 14 | `lazy_request` | C→S | Client subscribes to member list and typing updates for a set of channels within a guild. |
| 15 | `request_guild_counts` | C→S | Client requests online member counts for one or more guilds. |
| 16 | `request_channel_member_counts` | C→S | Client requests member counts for specific channels within a guild. |

Opcode 13 is not defined. `constants:gateway_opcode/1` returns the atom `unknown` for any integer not in the table.

### Typical session sequence

```
S→C  op 10  HELLO                 heartbeat_interval: 41250 ms
C→S  op 2   IDENTIFY              token, shard, encoding, compress
S→C  op 0   READY                 initial state, session ID
     — heartbeat loop —
C→S  op 1   HEARTBEAT             seq
S→C  op 11  HEARTBEAT_ACK
     — server-pushed events —
S→C  op 0   DISPATCH              t: "MESSAGE_CREATE", d: {...}, s: N
     — disconnect / reconnect —
S→C  op 7   RECONNECT             (drain or rolling deploy)
C→S  op 6   RESUME                token, session_id, seq
S→C  op 0   RESUMED               _timings_gw, missed events replayed
```

For the full lifecycle with rate limiting and encoding details see [websocket-handler.md](websocket-handler.md). For the Resume flow and event buffer see [session-lifecycle.md](session-lifecycle.md).

---

## Close Codes

WebSocket close frames carry a 4-digit code. The gateway maps these via `constants:close_code_to_num/1`. Whether a client should attempt to Resume after receiving a close depends on the code.

| Code | Name | Resumable | When sent |
|------|------|-----------|-----------|
| 4000 | `unknown_error` | yes | Catch-all for unexpected internal errors. |
| 4001 | `unknown_opcode` | yes | Client sent an `op` integer not in the opcode table. |
| 4002 | `decode_error` | yes | Payload exceeded 4 096 bytes, decompression failed, or JSON decoding failed. |
| 4003 | `not_authenticated` | yes | Client sent an opcode that requires an authenticated session before completing Identify. |
| 4004 | `authentication_failed` | no | Token is invalid or the auth service rejected the Identify. Do not retry with the same token. |
| 4005 | `already_authenticated` | yes | Client sent a second Identify on an already-authenticated connection. |
| 4007 | `invalid_seq` | no | Resume was attempted with a sequence number outside the buffered window (`seq > current` or `seq < ack_seq`). Start a fresh session. |
| 4008 | `rate_limited` | yes | Per-IP connection limit (256 connections) or per-connection message budget exceeded. Back off before reconnecting. |
| 4009 | `session_timeout` | no | The 60-second resume window expired before the client reconnected. Start a fresh session. |
| 4010 | `invalid_shard` | no | The `shard` tuple in Identify was malformed (`shard_id >= num_shards`, `num_shards > 16 384`, etc.). Fix the shard parameters before reconnecting. |
| 4011 | `sharding_required` | no | The session would hold more than 2 500 guilds. Use sharding. |
| 4012 | `invalid_api_version` | no | The `?v=` query parameter was not `1`. |
| 4013 | `ack_backpressure` | yes | Heartbeat acknowledgement backlog: the session's replay buffer has grown beyond acceptable bounds. |

Code 4006 is not defined.

**Resumable** means the existing session token and session ID are still valid and a reconnect with op 6 RESUME will replay missed events. Non-resumable codes mean the session is gone; the client must re-identify from scratch.

For the handler code that issues each close code see [websocket-handler.md](websocket-handler.md). For how the session enforces the resume window and buffer limits see [session-lifecycle.md](session-lifecycle.md). For how sharding limits are derived see [architecture-overview.md](architecture-overview.md).
