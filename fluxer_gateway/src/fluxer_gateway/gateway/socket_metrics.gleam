// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type ResumeError, type StartError}
import fluxer_gateway/gateway/compress.{type Compression}
import fluxer_gateway/gateway/heartbeat.{type CheckOutcome}
import fluxer_gateway/gateway/identify.{type IdentifyError}
import fluxer_gateway/gateway/opcode.{type Opcode}
import fluxer_gateway/telemetry/metrics.{type Sample, Sample}
import fluxer_gateway/telemetry/process_info
import fluxer_gateway/util/table
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Family {
  Family(name: String, help: String, label: String, values: List(String))
}

pub type Connect {
  Accepted
  BadApiVersion
  TooManyConnections
}

pub type Disconnect {
  ClientNormal
  ClientCode
  ProtocolError
  TransportLost
  ServerClosed
}

pub type RateLimit {
  OpcodeLimit
  ConnectionLimit
  IdentifyIpLimit
  IdentifyTokenLimit
  IpConnectionsLimit
}

pub type Buffer {
  DeferredBuffer
  EarlyBuffer
}

pub type IdentifyOutcome {
  IdentifyAccepted
  IdentifyThrottled
  IdentifyRateLimited
  IdentifyRejected(IdentifyError)
}

pub type Hold {
  HoldPaused
  HoldThrottled
  HoldStartTimeout
  HoldRateLimited
  HoldUnavailable
}

pub type ResumeOutcome {
  Resumed
  ResumeRejected(ResumeError)
  ResumeTimedOut
  ResumeBadSessionId
  ResumeInvalidPayload
  ResumeRateLimited
}

pub type ConnectionState {
  AwaitingIdentify
  Identifying
  Held
  Resuming
  Attached
  Closing
}

type Counters

type CountersOption {
  WriteConcurrency
}

type Key {
  FluxerGatewaySocketCounters
}

type Stored

type Scan {
  Scan(states: Dict(Int, Int), memory: Int, queued: Int, queued_max: Int)
}

pub const ops_received = "fluxer_gateway_websocket_ops_received_total"

pub const dispatches_by_family =
  "fluxer_gateway_websocket_dispatches_by_family_total"

pub const received_wire_bytes =
  "fluxer_gateway_websocket_received_wire_bytes_total"

pub const received_payload_bytes =
  "fluxer_gateway_websocket_received_payload_bytes_total"

pub const sent_wire_bytes = "fluxer_gateway_websocket_sent_wire_bytes_total"

pub const sent_payload_bytes =
  "fluxer_gateway_websocket_sent_payload_bytes_total"

pub const rate_limited_total = "fluxer_gateway_websocket_rate_limited_total"

pub const buffer_overflows = "fluxer_gateway_websocket_buffer_overflows_total"

pub const send_failures = "fluxer_gateway_websocket_send_failures_total"

pub const heartbeat_checks = "fluxer_gateway_websocket_heartbeat_checks_total"

pub const connects = "fluxer_gateway_websocket_connects_total"

pub const disconnects = "fluxer_gateway_websocket_disconnects_total"

pub const identifies = "fluxer_gateway_websocket_identifies_total"

pub const identify_holds = "fluxer_gateway_websocket_identify_holds_total"

pub const session_starts = "fluxer_gateway_websocket_session_starts_total"

pub const resume_attempts = "fluxer_gateway_websocket_resume_attempts_total"

pub const connections = "fluxer_gateway_websocket_connections"

pub const process_memory = "fluxer_gateway_websocket_process_memory_bytes"

pub const mailbox_messages = "fluxer_gateway_websocket_mailbox_messages"

pub const mailbox_max_messages = "fluxer_gateway_websocket_mailbox_max_messages"

pub const identify_ready_ms = "fluxer_gateway_websocket_identify_ready_ms"

pub const resume_ms = "fluxer_gateway_websocket_resume_ms"

const collector = "fluxer_gateway_websocket"

const compressions = ["none", "zstd_stream"]

const ops_offset = 0

const families_offset = 12

const received_wire_offset = 26

const received_payload_offset = 28

const sent_wire_offset = 30

const sent_payload_offset = 32

const rate_limited_offset = 34

const overflows_offset = 39

const send_failures_slot = 42

const heartbeat_offset = 42

const connects_offset = 45

const disconnects_offset = 48

const identifies_offset = 53

const holds_offset = 59

const starts_offset = 64

const resumes_offset = 72

const slots = 81

pub fn counter_families() -> List(#(Family, Int)) {
  [
    #(
      Family(ops_received, "Client opcodes received", "op", [
        "heartbeat", "identify", "presence_update", "voice_state_update",
        "resume", "request_guild_members", "voice_signal", "lazy_request",
        "request_guild_counts", "request_channel_member_counts",
        "request_forum_unreads", "other",
      ]),
      ops_offset,
    ),
    #(
      Family(
        dispatches_by_family,
        "Dispatch frames sent by event family",
        "family",
        [
          "ready", "resumed", "message", "reaction", "typing", "presence",
          "member", "guild", "channel", "thread", "voice", "call", "user",
          "other",
        ],
      ),
      families_offset,
    ),
    #(
      Family(
        received_wire_bytes,
        "Bytes received from clients as they arrived on the wire",
        "compression",
        compressions,
      ),
      received_wire_offset,
    ),
    #(
      Family(
        received_payload_bytes,
        "Bytes received from clients after decompression",
        "compression",
        compressions,
      ),
      received_payload_offset,
    ),
    #(
      Family(
        sent_wire_bytes,
        "Bytes sent to clients after compression",
        "compression",
        compressions,
      ),
      sent_wire_offset,
    ),
    #(
      Family(
        sent_payload_bytes,
        "JSON bytes sent to clients before compression",
        "compression",
        compressions,
      ),
      sent_payload_offset,
    ),
    #(
      Family(rate_limited_total, "Client actions refused by a limit", "kind", [
        "opcode", "connection", "identify_ip", "identify_token",
        "ip_connections",
      ]),
      rate_limited_offset,
    ),
    #(
      Family(
        buffer_overflows,
        "Sockets closed because a holding buffer filled",
        "buffer",
        ["deferred", "early"],
      ),
      overflows_offset,
    ),
    #(
      Family(heartbeat_checks, "Heartbeat checks by outcome", "outcome", [
        "alive", "requested", "timed_out",
      ]),
      heartbeat_offset,
    ),
    #(
      Family(connects, "WebSocket upgrades by outcome", "outcome", [
        "accepted", "bad_api_version", "too_many_connections",
      ]),
      connects_offset,
    ),
    #(
      Family(disconnects, "WebSocket disconnects by cause", "cause", [
        "client_normal", "client_code", "protocol_error", "transport", "server",
      ]),
      disconnects_offset,
    ),
    #(
      Family(identifies, "IDENTIFY payloads by outcome", "outcome", [
        "accepted", "throttled", "rate_limited", "invalid_payload",
        "invalid_token", "invalid_shard",
      ]),
      identifies_offset,
    ),
    #(
      Family(identify_holds, "Identifies held for a retry by reason", "reason", [
        "paused", "throttled", "start_timeout", "rate_limited", "unavailable",
      ]),
      holds_offset,
    ),
    #(
      Family(session_starts, "Session start replies by outcome", "outcome", [
        "ok", "auth_failed", "sharding_required", "too_many_sessions",
        "rate_limited", "unavailable", "unknown_error", "timeout",
      ]),
      starts_offset,
    ),
    #(
      Family(resume_attempts, "RESUME attempts by outcome", "outcome", [
        "resumed", "invalid_seq", "session_gone", "auth_failed", "unavailable",
        "timeout", "bad_session_id", "invalid_payload", "rate_limited",
      ]),
      resumes_offset,
    ),
  ]
}

pub fn connection_states() -> Family {
  Family(connections, "Open WebSocket connections by state", "state", [
    "awaiting_identify", "identifying", "held", "resuming", "attached",
    "closing",
  ])
}

@external(erlang, "counters", "new")
fn counters_new(size: Int, options: List(CountersOption)) -> Counters

@external(erlang, "counters", "add")
fn counters_add(counters: Counters, index: Int, by: Int) -> Stored

@external(erlang, "counters", "get")
fn counters_get(counters: Counters, index: Int) -> Int

@external(erlang, "persistent_term", "get")
fn get_counters(key: Key, default: Option(Counters)) -> Option(Counters)

@external(erlang, "persistent_term", "put")
fn put_counters(key: Key, value: Option(Counters)) -> Stored

fn counters() -> Counters {
  case get_counters(FluxerGatewaySocketCounters, None) {
    Some(found) -> found
    None -> {
      let created = counters_new(slots, [WriteConcurrency])
      put_counters(FluxerGatewaySocketCounters, Some(created))
      metrics.register_collector(collector, collect)
      created
    }
  }
}

fn bump(index: Int, by: Int) -> Nil {
  counters_add(counters(), index, by)
  Nil
}

pub fn op_received(op: Opcode) -> Nil {
  bump(ops_offset + op_position(op), 1)
}

fn op_position(op: Opcode) -> Int {
  case op {
    opcode.Heartbeat -> 1
    opcode.Identify -> 2
    opcode.PresenceUpdate -> 3
    opcode.VoiceStateUpdate -> 4
    opcode.Resume -> 5
    opcode.RequestGuildMembers -> 6
    opcode.VoiceSignal -> 7
    opcode.LazyRequest -> 8
    opcode.RequestGuildCounts -> 9
    opcode.RequestChannelMemberCounts -> 10
    opcode.RequestForumUnreads -> 11
    _ -> 12
  }
}

pub fn dispatch_sent(event: String) -> Nil {
  bump(families_offset + family_position(event), 1)
}

fn family_position(event: String) -> Int {
  case event {
    "READY" -> 1
    "RESUMED" -> 2
    "MESSAGE_REACTION_" <> _ -> 4
    "MESSAGE_" <> _ -> 3
    "TYPING_" <> _ -> 5
    "PRESENCE_" <> _ -> 6
    "GUILD_MEMBER" <> _ -> 7
    "GUILD_" <> _ -> 8
    "CHANNEL_" <> _ -> 9
    "THREAD_" <> _ -> 10
    "VOICE_" <> _ -> 11
    "CALL_" <> _ -> 12
    "USER_" <> _ | "RELATIONSHIP_" <> _ -> 13
    _ -> 14
  }
}

fn compression_position(compression: Compression) -> Int {
  case compression {
    compress.NoCompression -> 1
    compress.ZstdStream -> 2
  }
}

pub fn received(compression: Compression, wire: Int, payload: Int) -> Nil {
  let position = compression_position(compression)
  let found = counters()
  counters_add(found, received_wire_offset + position, wire)
  counters_add(found, received_payload_offset + position, payload)
  Nil
}

pub fn sent(compression: Compression, wire: Int, payload: Int) -> Nil {
  let position = compression_position(compression)
  let found = counters()
  counters_add(found, sent_wire_offset + position, wire)
  counters_add(found, sent_payload_offset + position, payload)
  Nil
}

pub fn rate_limited(kind: RateLimit) -> Nil {
  let position = case kind {
    OpcodeLimit -> 1
    ConnectionLimit -> 2
    IdentifyIpLimit -> 3
    IdentifyTokenLimit -> 4
    IpConnectionsLimit -> 5
  }
  bump(rate_limited_offset + position, 1)
}

pub fn overflowed(buffer: Buffer) -> Nil {
  let position = case buffer {
    DeferredBuffer -> 1
    EarlyBuffer -> 2
  }
  bump(overflows_offset + position, 1)
}

pub fn send_failed() -> Nil {
  bump(send_failures_slot, 1)
}

pub fn heartbeat_checked(outcome: CheckOutcome) -> Nil {
  let position = case outcome {
    heartbeat.StillAlive -> 1
    heartbeat.RequestHeartbeat(_) -> 2
    heartbeat.TimedOut -> 3
  }
  bump(heartbeat_offset + position, 1)
}

pub fn connected(outcome: Connect) -> Nil {
  let position = case outcome {
    Accepted -> 1
    BadApiVersion -> 2
    TooManyConnections -> 3
  }
  bump(connects_offset + position, 1)
}

pub fn disconnected(cause: Disconnect) -> Nil {
  let position = case cause {
    ClientNormal -> 1
    ClientCode -> 2
    ProtocolError -> 3
    TransportLost -> 4
    ServerClosed -> 5
  }
  bump(disconnects_offset + position, 1)
}

pub fn identified(outcome: IdentifyOutcome) -> Nil {
  let position = case outcome {
    IdentifyAccepted -> 1
    IdentifyThrottled -> 2
    IdentifyRateLimited -> 3
    IdentifyRejected(identify.InvalidPayload) -> 4
    IdentifyRejected(identify.InvalidToken) -> 5
    IdentifyRejected(identify.InvalidShard) -> 6
  }
  bump(identifies_offset + position, 1)
}

pub fn held(reason: Hold) -> Nil {
  let position = case reason {
    HoldPaused -> 1
    HoldThrottled -> 2
    HoldStartTimeout -> 3
    HoldRateLimited -> 4
    HoldUnavailable -> 5
  }
  bump(holds_offset + position, 1)
}

pub fn start_replied(reply: Result(Nil, StartError)) -> Nil {
  let position = case reply {
    Ok(Nil) -> 1
    Error(session.StartAuthFailed) -> 2
    Error(session.StartShardingRequired) -> 3
    Error(session.StartTooManySessions) -> 4
    Error(session.StartRateLimited(..)) -> 5
    Error(session.StartUnavailable(..)) -> 6
    Error(session.StartUnknownError) -> 7
  }
  bump(starts_offset + position, 1)
}

pub fn start_timed_out() -> Nil {
  bump(starts_offset + 8, 1)
}

pub fn resume_finished(outcome: ResumeOutcome) -> Nil {
  let position = case outcome {
    Resumed -> 1
    ResumeRejected(session.InvalidResumeSeq) -> 2
    ResumeRejected(session.SessionGone) -> 3
    ResumeRejected(session.ResumeAuthFailed) -> 4
    ResumeRejected(session.ResumeUnavailable(..)) -> 5
    ResumeTimedOut -> 6
    ResumeBadSessionId -> 7
    ResumeInvalidPayload -> 8
    ResumeRateLimited -> 9
  }
  bump(resumes_offset + position, 1)
}

pub fn identify_to_ready(elapsed_ms: Int) -> Nil {
  metrics.observe_ms(identify_ready_ms, [], elapsed_ms)
}

pub fn resume_to_resumed(elapsed_ms: Int) -> Nil {
  metrics.observe_ms(resume_ms, [], elapsed_ms)
}

fn states() -> table.Table {
  table.named("fluxer_gateway_ws_socket_states")
}

pub fn state_code(state: ConnectionState) -> Int {
  case state {
    AwaitingIdentify -> 1
    Identifying -> 2
    Held -> 3
    Resuming -> 4
    Attached -> 5
    Closing -> 6
  }
}

pub fn track(pid: Pid, state: ConnectionState) -> Nil {
  case table.ensure_public(states()) {
    True -> table.insert(states(), #(pid, state_code(state)))
    False -> Nil
  }
}

pub fn untrack(pid: Pid) -> Nil {
  table.delete(states(), pid)
}

fn collect() -> List(Sample) {
  let found = counters()
  let counted =
    list.flat_map(counter_families(), fn(entry) {
      let #(family, offset) = entry
      list.index_map(family.values, fn(value, index) {
        Sample(
          family.name,
          [#(family.label, value)],
          counters_get(found, offset + index + 1),
        )
      })
    })
  let scan = scan()
  let states = connection_states()
  let by_state =
    list.index_map(states.values, fn(value, index) {
      Sample(
        states.name,
        [#(states.label, value)],
        dict.get(scan.states, index + 1) |> result.unwrap(0),
      )
    })
  list.flatten([
    counted,
    [Sample(send_failures, [], counters_get(found, send_failures_slot))],
    by_state,
    [
      Sample(process_memory, [], scan.memory),
      Sample(mailbox_messages, [], scan.queued),
      Sample(mailbox_max_messages, [], scan.queued_max),
    ],
  ])
}

fn scan() -> Scan {
  let rows: List(#(Pid, Int)) = table.to_list(states())
  list.fold(rows, Scan(dict.new(), 0, 0, 0), fn(scan, row) {
    let #(pid, code) = row
    case process_info.memory_and_queue(pid) {
      Error(Nil) -> {
        table.delete_object(states(), row)
        scan
      }
      Ok(#(memory, queued)) ->
        Scan(
          states: dict.upsert(scan.states, code, fn(count) {
            option.unwrap(count, 0) + 1
          }),
          memory: scan.memory + memory,
          queued: scan.queued + queued,
          queued_max: int.max(scan.queued_max, queued),
        )
    }
  })
}

pub fn layout() -> #(List(Int), Int) {
  let used =
    list.flat_map(counter_families(), fn(entry) {
      let #(family, offset) = entry
      list.index_map(family.values, fn(_, index) { offset + index + 1 })
    })
  #([send_failures_slot, ..used], slots)
}
