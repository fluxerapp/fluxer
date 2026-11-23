// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/rollout
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type CloseCode}
import fluxer_gateway/core/session.{
  type ClientOp, type Identify, type Outbound, type ResumeError, type SessionId,
  type SessionRef, type SocketMsg, type StartError, SocketRef,
}
import fluxer_gateway/gateway/abuse_protection
import fluxer_gateway/gateway/client_ip
import fluxer_gateway/gateway/codec.{type Body, type VoiceUpdate}
import fluxer_gateway/gateway/compress.{type Context}
import fluxer_gateway/gateway/encode
import fluxer_gateway/gateway/heartbeat.{type Heartbeat}
import fluxer_gateway/gateway/identify
import fluxer_gateway/gateway/ip_connections
import fluxer_gateway/gateway/limits
import fluxer_gateway/gateway/opcode.{type Opcode}
import fluxer_gateway/gateway/rate_limit.{type RateLimits}
import fluxer_gateway/gateway/resume
import fluxer_gateway/gateway/socket_metrics.{type ConnectionState, type Hold}
import fluxer_gateway/gateway/voice_ops.{type VoiceOps}
import fluxer_gateway/gateway/ws/link.{type Link}
import fluxer_gateway/presence/protocol
import fluxer_gateway/session/manager
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/util/table
import gleam/bit_array
import gleam/bool
import gleam/bytes_tree.{type BytesTree}
import gleam/erlang/process.{type Monitor, type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import glisten.{type IpAddress}
import logging

const pause_deferred_payloads = 16

const max_deferred_payloads = 64

const max_early_messages = 4096

fn pending_identify_retry_base_ms() -> Int {
  durations.gateway().socket_pending_identify_retry_base_ms
}

fn pending_identify_retry_jitter_ms() -> Int {
  durations.gateway().socket_pending_identify_retry_jitter_ms
}

fn heartbeat_ack_fallback_ms() -> Int {
  durations.gateway().socket_heartbeat_ack_fallback_ms
}

fn reconnect_linger_ms() -> Int {
  durations.gateway().socket_reconnect_linger_ms
}

fn resume_reply_timeout_ms() -> Int {
  durations.gateway().socket_resume_reply_timeout_ms
}

pub opaque type Message {
  Outbox(SocketMsg)
  StartReply(Result(SessionRef, StartError))
  ResumeReply(Result(SessionRef, ResumeError))
  HeartbeatCheck(token: Int)
  HeartbeatAckDue(token: Int)
  StartTimeout(token: Int)
  ResumeTimeout(token: Int)
  RebindTimeout(token: Int)
  RetryIdentify(token: Int)
  RolloutChanged
  VoiceDrain(generation: Int)
  DrainReconnect
  ReconnectLinger
  Down(process.Down)
}

type Pending {
  Idle
  Starting(identify: Identify, timeout: Int)
  Resuming(timeout: Int, session: SessionId, accepted: Bool)
  Rebinding(timeout: Int)
  Holding(identify: Identify, retry: Option(Int), throttled: Bool)
}

type Acks {
  Acks(forwarded: Int, skip: Int, due: Option(Int))
}

const no_acks = Acks(forwarded: 0, skip: 0, due: None)

type Attached {
  Attached(ref: SessionRef, monitor: Option(Monitor))
}

pub opaque type Upgrade {
  Upgrade(
    api_version: Option(Int),
    peer_ip: String,
    compression: compress.Compression,
    window_log: Int,
    token_limit_per_minute: Int,
  )
}

pub opaque type Mailbox {
  Mailbox(
    self: Subject(Message),
    outbox: Subject(SocketMsg),
    start_replies: Subject(Result(SessionRef, StartError)),
    resume_replies: Subject(Result(SessionRef, ResumeError)),
    rollout_changes: Subject(rollout.RolloutChanged),
  )
}

type Config {
  Config(
    api_version: Option(Int),
    peer_ip: String,
    token_limit_per_minute: Int,
    link: Link,
    self: Subject(Message),
    outbox: Subject(SocketMsg),
    start_replies: Subject(Result(SessionRef, StartError)),
    resume_replies: Subject(Result(SessionRef, ResumeError)),
    rollout_changes: Subject(rollout.RolloutChanged),
  )
}

type Phase {
  Open
  Lingering
  Closed
}

pub opaque type State {
  State(
    config: Config,
    phase: Phase,
    connection_acquired: Bool,
    compress: Context,
    heartbeat: Heartbeat,
    heartbeat_timer: Option(Int),
    acks: Acks,
    attached: Option(Attached),
    pending: Pending,
    deferred: List(#(Opcode, Body)),
    deferred_count: Int,
    early: List(SocketMsg),
    early_count: Int,
    rates: RateLimits,
    voice: VoiceOps,
    voice_generation: Int,
    next_token: Int,
    identify_at: Option(Int),
    resume_at: Option(Int),
    reported: ConnectionState,
  )
}

pub fn upgrade(req: Request(IpAddress), settings: Settings) -> Upgrade {
  let query = case request.get_query(req) {
    Ok(query) -> query
    Error(Nil) -> []
  }
  let api_version = case list.key_find(query, "v") {
    Ok("1") -> Some(1)
    _ -> None
  }
  Upgrade(
    api_version:,
    peer_ip: client_ip.resolve(
      client_ip.header_value(req.headers, settings.client_ip_header),
      settings.trust_client_ip_header,
      peer_address(req.body),
    ),
    compression: compress.parse_compression(
      list.key_find(query, "compress") |> option.from_result,
      list.key_find(query, "stream") |> option.from_result,
    ),
    window_log: settings.zstd_window_log,
    token_limit_per_minute: settings.identify_token_limit_per_minute,
  )
}

fn peer_address(address: IpAddress) -> String {
  case address {
    glisten.IpV4(a, b, c, d) -> client_ip.from_ipv4(a, b, c, d)
    glisten.IpV6(a, b, c, d, e, f, g, h) ->
      client_ip.from_ipv6(a, b, c, d, e, f, g, h)
  }
}

pub fn mailbox() -> #(Mailbox, Selector(Message)) {
  let mailbox =
    Mailbox(
      self: process.new_subject(),
      outbox: process.new_subject(),
      start_replies: process.new_subject(),
      resume_replies: process.new_subject(),
      rollout_changes: process.new_subject(),
    )
  let selector =
    process.new_selector()
    |> process.select(mailbox.self)
    |> process.select_map(mailbox.outbox, Outbox)
    |> process.select_map(mailbox.start_replies, StartReply)
    |> process.select_map(mailbox.resume_replies, ResumeReply)
    |> process.select_map(mailbox.rollout_changes, fn(_) { RolloutChanged })
    |> process.select_monitors(Down)
  #(mailbox, selector)
}

fn registry() -> table.Table {
  table.named("fluxer_gateway_ws_sockets")
}

fn lingering_table() -> table.Table {
  table.named("fluxer_gateway_ws_sockets_lingering")
}

pub fn lingering() -> Int {
  let _ = table.ensure_public(lingering_table())
  table.to_list(lingering_table())
  |> list.filter(fn(row: #(process.Pid, Nil)) {
    case process.is_alive(row.0) {
      True -> True
      False -> {
        table.delete_object(lingering_table(), row)
        False
      }
    }
  })
  |> list.length
}

pub fn reconnect_all() -> Int {
  let sockets = live_sockets()
  list.each(sockets, process.send(_, DrainReconnect))
  list.length(sockets)
}

pub fn prune_dead() -> Nil {
  let _ = live_sockets()
  Nil
}

fn live_sockets() -> List(Subject(Message)) {
  table.to_list(registry())
  |> list.filter_map(fn(row: #(process.Pid, Subject(Message))) {
    case process.is_alive(row.0) {
      True -> Ok(row.1)
      False -> {
        table.delete_object(registry(), row)
        Error(Nil)
      }
    }
  })
}

pub fn open(upgrade: Upgrade, mailbox: Mailbox, link: Link) -> State {
  let _ = table.ensure_public(registry())
  table.insert(registry(), #(process.self(), mailbox.self))
  socket_metrics.track(process.self(), socket_metrics.AwaitingIdentify)
  let config =
    Config(
      api_version: upgrade.api_version,
      peer_ip: upgrade.peer_ip,
      token_limit_per_minute: upgrade.token_limit_per_minute,
      link:,
      self: mailbox.self,
      outbox: mailbox.outbox,
      start_replies: mailbox.start_replies,
      resume_replies: mailbox.resume_replies,
      rollout_changes: mailbox.rollout_changes,
    )
  State(
    config:,
    phase: Open,
    connection_acquired: False,
    compress: compress.new_context(upgrade.compression, upgrade.window_log),
    heartbeat: heartbeat.new(clock.system_ms()),
    heartbeat_timer: None,
    acks: no_acks,
    attached: None,
    pending: Idle,
    deferred: [],
    deferred_count: 0,
    early: [],
    early_count: 0,
    rates: rate_limit.new(),
    voice: voice_ops.new(),
    voice_generation: 0,
    next_token: 0,
    identify_at: None,
    resume_at: None,
    reported: socket_metrics.AwaitingIdentify,
  )
  |> greet
}

fn greet(state: State) -> State {
  case state.config.api_version {
    None -> {
      socket_metrics.connected(socket_metrics.BadApiVersion)
      close(state, error.InvalidApiVersion, "Invalid API version")
    }
    Some(_) ->
      case ip_connections.acquire(state.config.peer_ip) {
        Error(ip_connections.TooManyConnections) -> {
          socket_metrics.connected(socket_metrics.TooManyConnections)
          socket_metrics.rate_limited(socket_metrics.IpConnectionsLimit)
          close(state, error.CloseRateLimited, "Too many connections")
        }
        Ok(Nil) -> {
          socket_metrics.connected(socket_metrics.Accepted)
          let state = State(..state, connection_acquired: True)
          let hello = encode.hello(limits.heartbeat_interval_ms())
          case send_frame(state, hello) {
            Ok(state) -> schedule_heartbeat(state)
            Error(Nil) ->
              close(state, error.DecodeError, "Compression failed: zstd-stream")
          }
        }
      }
  }
}

pub fn receive(state: State, data: BitArray) -> State {
  after(incoming(state, data))
}

pub fn handle(state: State, message: Message) -> State {
  after(info(state, message))
}

pub fn closing(state: State) -> Bool {
  state.phase == Closed
}

pub fn close_sent(state: State) -> Bool {
  state.phase == Lingering
}

pub fn paused(state: State) -> Bool {
  awaiting_reply(state) && state.deferred_count >= pause_deferred_payloads
}

fn awaiting_reply(state: State) -> Bool {
  case state.pending {
    Starting(..) | Resuming(..) | Rebinding(..) -> True
    Idle | Holding(..) -> False
  }
}

fn after(state: State) -> State {
  let state = replay_deferred(state)
  case state.attached, state.pending {
    None, Starting(..) | None, Holding(..) -> garbage_collect()
    _, _ -> Nil
  }
  report(state)
}

fn report(state: State) -> State {
  let current = connection_state(state)
  case current == state.reported {
    True -> state
    False -> {
      socket_metrics.track(process.self(), current)
      State(..state, reported: current)
    }
  }
}

fn connection_state(state: State) -> ConnectionState {
  case state.phase, state.attached, state.pending {
    Lingering, _, _ | Closed, _, _ -> socket_metrics.Closing
    Open, Some(_), _ -> socket_metrics.Attached
    Open, None, Starting(..) -> socket_metrics.Identifying
    Open, None, Resuming(..) -> socket_metrics.Resuming
    Open, None, Holding(..) -> socket_metrics.Held
    Open, None, Idle | Open, None, Rebinding(..) ->
      socket_metrics.AwaitingIdentify
  }
}

pub fn terminate(state: State, client_closed: Bool) -> Nil {
  table.delete(registry(), process.self())
  table.delete(lingering_table(), process.self())
  socket_metrics.untrack(process.self())
  rollout.unsubscribe_changes(state.config.rollout_changes)
  case state.connection_acquired {
    True -> ip_connections.release(state.config.peer_ip)
    False -> Nil
  }
  case state.attached, client_closed {
    Some(Attached(monitor:, ..)), True -> demonitor(monitor)
    Some(Attached(ref:, monitor:)), False -> {
      demonitor(monitor)
      session.send(ref, session.SocketDown(process.self()))
    }
    None, _ -> Nil
  }
}

fn close(state: State, code: CloseCode, reason: String) -> State {
  case state.phase {
    Closed -> state
    Lingering -> State(..state, phase: Closed)
    Open -> {
      rollout_metrics.count(
        rollout_metrics.socket_closes,
        [#("reason", int.to_string(error.close_code_to_int(code)))],
        1,
      )
      link.close(state.config.link, error.close_code_to_int(code), reason)
      State(..state, phase: Closed)
    }
  }
}

fn incoming(state: State, data: BitArray) -> State {
  use <- bool.guard(state.phase == Closed, state)
  let max = limits.max_payload_size()
  case bit_array.byte_size(data) <= max {
    False -> close(state, error.DecodeError, "Payload too large")
    True ->
      case compress.decompress(state.compress, data, max) {
        Ok(#(bytes, context)) -> {
          let size = bit_array.byte_size(bytes)
          socket_metrics.received(
            compress.compression(context),
            bit_array.byte_size(data),
            size,
          )
          case size <= max {
            True -> decoded(State(..state, compress: context), bytes)
            False -> close(state, error.DecodeError, "Payload too large")
          }
        }
        Error(compress.DecompressTooLarge) ->
          close(state, error.DecodeError, "Payload too large")
        Error(compress.DecompressFailed) ->
          close(state, error.DecodeError, "Decompression failed")
      }
  }
}

fn decoded(state: State, bytes: BitArray) -> State {
  case codec.decode(bytes) {
    Error(codec.DecodeFailed) ->
      close(state, error.DecodeError, "Decode failed")
    Error(codec.MissingOp) -> close(state, error.DecodeError, "Invalid payload")
    Ok(codec.Frame(op:, body:)) -> {
      socket_metrics.op_received(op)
      case
        rate_limit.check(
          state.rates,
          op,
          state.config.peer_ip,
          session_key(state),
        )
      {
        rate_limit.Allowed(rates) ->
          dispatch_or_defer(State(..state, rates:), op, body)
        rate_limit.OpcodeLimited(rates) -> {
          socket_metrics.rate_limited(socket_metrics.OpcodeLimit)
          State(..state, rates:)
        }
        rate_limit.ConnectionLimited -> {
          socket_metrics.rate_limited(socket_metrics.ConnectionLimit)
          close(state, error.CloseRateLimited, "Rate limited")
        }
      }
    }
  }
}

fn session_key(state: State) -> Option(SessionId) {
  case state.attached, state.pending {
    Some(Attached(ref:, ..)), _ -> Some(ref.id)
    None, Resuming(session:, accepted: True, ..) -> Some(session)
    None, _ -> None
  }
}

fn holds_payloads(state: State) -> Bool {
  case state.pending {
    Starting(..) | Resuming(..) | Holding(..) | Rebinding(..) -> True
    Idle -> False
  }
}

fn defers(state: State, op: Opcode) -> Bool {
  case op, state.pending {
    opcode.Heartbeat, _ -> False
    _, Starting(..) | _, Resuming(..) | _, Rebinding(..) -> True
    opcode.Identify, Holding(..) | opcode.Resume, Holding(..) -> False
    _, Holding(..) -> True
    _, Idle -> False
  }
}

fn dispatch_or_defer(state: State, op: Opcode, body: Body) -> State {
  case defers(state, op) {
    False -> handle_op(state, body)
    True -> {
      let count = state.deferred_count + 1
      case count > max_deferred_payloads {
        True -> {
          socket_metrics.overflowed(socket_metrics.DeferredBuffer)
          close(state, error.CloseRateLimited, "Rate limited")
        }
        False ->
          State(
            ..state,
            deferred: [#(op, body), ..state.deferred],
            deferred_count: count,
          )
      }
    }
  }
}

fn replay_deferred(state: State) -> State {
  case holds_payloads(state), state.deferred {
    False, [_, ..] -> {
      let deferred = list.reverse(state.deferred)
      replay(State(..state, deferred: [], deferred_count: 0), deferred)
    }
    _, _ -> state
  }
}

fn replay(state: State, deferred: List(#(Opcode, Body))) -> State {
  case state.phase, deferred {
    Closed, _ | _, [] -> state
    _, [#(op, body), ..rest] -> replay(dispatch_or_defer(state, op, body), rest)
  }
}

fn handle_op(state: State, body: Body) -> State {
  case state.phase, body {
    Lingering, codec.IdentifyBody(_) | Lingering, codec.ResumeBody(_) -> state
    _, _ -> handle_open_op(state, body)
  }
}

fn handle_open_op(state: State, body: Body) -> State {
  case body, state.attached {
    codec.HeartbeatBody(Some(seq)), _ -> client_heartbeat(state, seq)
    codec.IdentifyBody(Some(ident)), None -> handle_identify(state, ident)
    codec.IdentifyBody(_), _
    | codec.ResumeBody(_), Some(_)
    | codec.ResumeBody(None), _
    -> close(state, error.AlreadyAuthenticated, "Already authenticated")
    codec.ResumeBody(Some(request)), None -> handle_resume(state, request)
    codec.RouteBody(Some(_)), None ->
      close(state, error.NotAuthenticated, "Not authenticated")
    codec.RouteBody(Some(route)), Some(attached) ->
      handle_route(state, attached, route)
    codec.HeartbeatBody(None), _ | codec.RouteBody(None), _ ->
      close(state, error.UnknownOpcode, "Unknown opcode")
  }
}

fn handle_route(state: State, attached: Attached, route: codec.Route) -> State {
  case route {
    codec.Forward(message) -> {
      forward(attached, message)
      state
    }
    codec.QueueVoice(update) -> voice_update(state, attached, update)
    codec.Reject(code:, reason:) -> close(state, code, reason)
    codec.Ignore -> state
  }
}

fn forward(attached: Attached, op: ClientOp) -> Nil {
  session.send(attached.ref, session.SocketOp(op))
}

fn client_heartbeat(state: State, seq: Result(Option(Int), Nil)) -> State {
  case state.attached, seq {
    None, _ | Some(_), Ok(None) -> ack_heartbeat(state)
    Some(attached), Ok(Some(seq)) -> {
      forward(attached, session.Heartbeat(Some(seq)))
      expect_ack(state)
    }
    Some(_), Error(_) -> close(state, error.InvalidSeq, "Invalid sequence")
  }
}

fn expect_ack(state: State) -> State {
  let acks = state.acks
  let forwarded = acks.forwarded + 1
  case acks.due {
    Some(_) -> State(..state, acks: Acks(..acks, forwarded:))
    None -> {
      let #(state, token) = new_token(state)
      process.send_after(
        state.config.self,
        heartbeat_ack_fallback_ms(),
        HeartbeatAckDue(token),
      )
      State(..state, acks: Acks(..acks, forwarded:, due: Some(token)))
    }
  }
}

fn heartbeat_ack_due(state: State, token: Int) -> State {
  case state.acks {
    Acks(forwarded:, skip:, due: Some(due)) if due == token -> {
      let state =
        State(
          ..state,
          acks: Acks(forwarded: 0, skip: skip + forwarded, due: None),
        )
      list.fold(list.repeat(Nil, forwarded), state, fn(state, _) {
        ack_heartbeat(state)
      })
    }
    _ -> state
  }
}

fn session_ack(state: State) -> State {
  case state.acks {
    Acks(skip:, ..) if skip > 0 ->
      State(..state, acks: Acks(..state.acks, skip: skip - 1))
    Acks(forwarded:, due:, ..) -> {
      let forwarded = int.max(forwarded - 1, 0)
      let due = case forwarded {
        0 -> None
        _ -> due
      }
      ack_heartbeat(State(..state, acks: Acks(forwarded:, skip: 0, due:)))
    }
  }
}

fn ack_heartbeat(state: State) -> State {
  case send_frame(state, encode.heartbeat_ack()) {
    Ok(state) -> State(..state, heartbeat: heartbeat.acked(clock.system_ms()))
    Error(Nil) -> state
  }
}

fn handle_identify(
  state: State,
  payload: Result(Identify, identify.IdentifyError),
) -> State {
  case abuse_protection.check_identify_rate(state.config.peer_ip) {
    Error(_) -> {
      socket_metrics.rate_limited(socket_metrics.IdentifyIpLimit)
      socket_metrics.identified(socket_metrics.IdentifyRateLimited)
      close(state, error.CloseRateLimited, "Identify rate limited")
    }
    Ok(Nil) ->
      case payload {
        Error(rejected) -> {
          socket_metrics.identified(socket_metrics.IdentifyRejected(rejected))
          identify_rejected(state, rejected)
        }
        Ok(ident) -> {
          let state =
            State(
              ..state,
              identify_at: option.or(
                state.identify_at,
                Some(clock.monotonic_ms()),
              ),
            )
          case
            abuse_protection.check_token_identify_rate(
              ident.token,
              state.config.token_limit_per_minute,
            )
          {
            Error(_) -> {
              socket_metrics.identified(socket_metrics.IdentifyThrottled)
              socket_metrics.rate_limited(socket_metrics.IdentifyTokenLimit)
              hold_with(state, ident, socket_metrics.HoldThrottled, True)
            }
            Ok(Nil) -> {
              socket_metrics.identified(socket_metrics.IdentifyAccepted)
              start_session(state, ident)
            }
          }
        }
      }
  }
}

fn identify_rejected(state: State, rejected: identify.IdentifyError) -> State {
  case rejected {
    identify.InvalidShard -> close(state, error.InvalidShard, "Invalid shard")
    identify.InvalidToken ->
      close(state, error.AuthenticationFailed, "Invalid token")
    identify.InvalidPayload ->
      close(state, error.DecodeError, "Invalid identify payload")
  }
}

fn start_session(state: State, ident: Identify) -> State {
  case rollout.session_rollout_percentage() <= 0 {
    True -> hold(state, ident, socket_metrics.HoldPaused)
    False -> start_unless_draining(state, ident)
  }
}

fn start_unless_draining(state: State, ident: Identify) -> State {
  case draining.is_draining() {
    True -> {
      drain_op7()
      send_outbound(release_hold(state), session.ReconnectFrame)
    }
    False -> {
      let state = release_hold(state)
      let #(state, token) = new_token(state)
      manager.start(
        ident,
        session.Peer(ip: state.config.peer_ip, api_version: 1),
        socket_ref(state),
        state.config.start_replies,
      )
      process.send_after(
        state.config.self,
        manager.start_timeout_ms(),
        StartTimeout(token),
      )
      State(..state, pending: Starting(identify: ident, timeout: token))
    }
  }
}

fn socket_ref(state: State) -> session.SocketRef {
  SocketRef(pid: process.self(), outbox: state.config.outbox)
}

fn release_hold(state: State) -> State {
  case state.pending {
    Holding(..) -> {
      rollout.unsubscribe_changes(state.config.rollout_changes)
      State(..state, pending: Idle)
    }
    _ -> state
  }
}

fn hold(state: State, ident: Identify, why: Hold) -> State {
  hold_with(state, ident, why, False)
}

fn hold_reason(why: Hold) -> String {
  case why {
    socket_metrics.HoldPaused -> "Session starts paused"
    socket_metrics.HoldThrottled -> "Identify rate limited"
    socket_metrics.HoldStartTimeout -> "Session start timed out"
    socket_metrics.HoldRateLimited -> "Session RPC rate limited"
    socket_metrics.HoldUnavailable -> "Gateway at capacity"
  }
}

fn hold_with(
  state: State,
  ident: Identify,
  why: Hold,
  throttled: Bool,
) -> State {
  socket_metrics.held(why)
  logging.log(
    logging.Debug,
    "Holding gateway identify: reason=" <> hold_reason(why),
  )
  let retry = case state.pending {
    Holding(retry: Some(token), ..) -> Some(token)
    Holding(retry: None, ..) -> None
    _ -> {
      rollout.subscribe_changes(state.config.rollout_changes)
      None
    }
  }
  let state =
    State(..state, pending: Holding(identify: ident, retry:, throttled:))
  case retry {
    Some(_) -> state
    None -> schedule_retry(state)
  }
}

fn schedule_retry(state: State) -> State {
  case state.pending {
    Holding(identify: ident, retry: None, throttled:) -> {
      let #(state, token) = new_token(state)
      let delay =
        pending_identify_retry_base_ms()
        + int.random(pending_identify_retry_jitter_ms())
      process.send_after(state.config.self, delay, RetryIdentify(token))
      State(
        ..state,
        pending: Holding(identify: ident, retry: Some(token), throttled:),
      )
    }
    _ -> state
  }
}

fn retry_held_identify(state: State) -> State {
  case state.pending {
    Holding(identify: ident, throttled: True, ..) ->
      case
        abuse_protection.check_token_identify_rate(
          ident.token,
          state.config.token_limit_per_minute,
        )
      {
        Error(_) -> schedule_retry(state)
        Ok(Nil) -> start_session(state, ident)
      }
    Holding(identify: ident, throttled: False, ..) ->
      case rollout.session_rollout_percentage() > 0 {
        True -> start_unless_draining(state, ident)
        False -> schedule_retry(state)
      }
    Idle | Starting(..) | Resuming(..) | Rebinding(..) -> state
  }
}

fn handle_resume(
  state: State,
  payload: Result(resume.ResumeRequest, resume.ResumeDecodeError),
) -> State {
  case abuse_protection.check_identify_rate(state.config.peer_ip) {
    Error(_) -> {
      socket_metrics.rate_limited(socket_metrics.IdentifyIpLimit)
      socket_metrics.resume_finished(socket_metrics.ResumeRateLimited)
      close(state, error.CloseRateLimited, "Resume rate limited")
    }
    Ok(Nil) ->
      case payload {
        Error(resume.InvalidResumePayload) -> {
          socket_metrics.resume_finished(socket_metrics.ResumeInvalidPayload)
          close(state, error.DecodeError, "Invalid resume payload")
        }
        Ok(resume.ResumeRequest(token:, session_id:, seq:)) ->
          case session.session_id_from_string(session_id) {
            Error(Nil) -> {
              socket_metrics.resume_finished(socket_metrics.ResumeBadSessionId)
              send_invalid_session(state, "bad_session_id")
            }
            Ok(id) -> {
              let #(state, timeout) = new_token(release_hold(state))
              manager.resume(
                id,
                token,
                seq,
                socket_ref(state),
                state.config.resume_replies,
              )
              process.send_after(
                state.config.self,
                resume_reply_timeout_ms(),
                ResumeTimeout(timeout),
              )
              State(
                ..state,
                pending: Resuming(timeout:, session: id, accepted: False),
                resume_at: Some(clock.monotonic_ms()),
              )
            }
          }
      }
  }
}

fn drain_op7() -> Nil {
  rollout_metrics.count(rollout_metrics.op7_sent, [#("reason", "drain")], 1)
}

fn send_invalid_session(state: State, reason: String) -> State {
  rollout_metrics.count(
    rollout_metrics.invalid_session,
    [#("reason", reason)],
    1,
  )
  case send_frame(state, encode.invalid_session(False)) {
    Ok(state) -> state
    Error(Nil) -> state
  }
}

fn info(state: State, message: Message) -> State {
  case state.phase, message {
    Closed, _ -> state
    Lingering, ReconnectLinger -> State(..state, phase: Closed)
    Lingering, Down(down) -> down_message(state, down)
    Lingering, _ -> state
    Open, _ -> open_info(state, message)
  }
}

fn open_info(state: State, message: Message) -> State {
  case message {
    Outbox(socket_message) -> outbox(state, socket_message)
    StartReply(reply) -> start_reply(state, reply)
    ResumeReply(reply) -> resume_reply(state, reply)
    HeartbeatCheck(token) -> heartbeat_check(state, token)
    HeartbeatAckDue(token) -> heartbeat_ack_due(state, token)
    StartTimeout(token) ->
      case state.pending {
        Starting(identify: ident, timeout:) if timeout == token -> {
          socket_metrics.start_timed_out()
          hold(
            State(..state, pending: Idle),
            ident,
            socket_metrics.HoldStartTimeout,
          )
        }
        _ -> state
      }
    RebindTimeout(token) ->
      case state.pending {
        Rebinding(timeout:) if timeout == token ->
          replay_deferred(State(..state, pending: Idle))
        _ -> state
      }
    ResumeTimeout(token) ->
      case state.pending {
        Resuming(timeout:, ..) if timeout == token -> {
          socket_metrics.resume_finished(socket_metrics.ResumeTimedOut)
          close(
            State(..state, pending: Idle),
            error.CloseUnknownError,
            "Session unavailable",
          )
        }
        _ -> state
      }
    RetryIdentify(token) ->
      case state.pending {
        Holding(identify: ident, retry: Some(retry), throttled:)
          if retry == token
        ->
          retry_held_identify(
            State(
              ..state,
              pending: Holding(identify: ident, retry: None, throttled:),
            ),
          )
        _ -> state
      }
    RolloutChanged -> retry_held_identify(state)
    VoiceDrain(generation:) if generation == state.voice_generation ->
      voice_drain(state)
    VoiceDrain(..) -> state
    DrainReconnect -> drain_reconnect(state)
    ReconnectLinger -> state
    Down(down) -> down_message(state, down)
  }
}

fn outbox(state: State, message: SocketMsg) -> State {
  case state.attached, state.pending {
    Some(Attached(ref:, ..)), _ if ref.id == message.session ->
      deliver(state, message)
    Some(_), _ -> state
    None, Resuming(timeout:, session:, ..) if session == message.session ->
      deliver(
        State(..state, pending: Resuming(timeout:, session:, accepted: True)),
        message,
      )
    None, Resuming(..) -> state
    None, _ -> hold_early(state, message)
  }
}

fn deliver(state: State, message: SocketMsg) -> State {
  case message {
    session.Send(frames:, ..) ->
      list.fold(frames, state, fn(state, frame) { send_outbound(state, frame) })
    session.Close(code:, reason:, ..) -> close(state, code, reason)
    session.Moved(from:, to:, ..) -> rebind(state, from, to)
    session.Reattached(..) ->
      case state.pending {
        Rebinding(..) -> replay_deferred(State(..state, pending: Idle))
        _ -> state
      }
  }
}

fn rebind(state: State, from: SessionRef, to: SessionRef) -> State {
  case state.attached, state.pending {
    Some(Attached(ref: old, monitor:)), Idle if old == from -> {
      demonitor(monitor)
      let monitor = case process.subject_owner(to.inbox) {
        Ok(pid) -> Some(process.monitor(pid))
        Error(Nil) -> None
      }
      session.send(old, session.SocketDown(process.self()))
      metrics.counter("fluxer_gateway_websocket_session_rebinds_total", [], 1)
      let #(state, token) = new_token(state)
      let _ =
        process.send_after(
          state.config.self,
          resume_reply_timeout_ms(),
          RebindTimeout(token),
        )
      State(
        ..state,
        attached: Some(Attached(ref: to, monitor:)),
        pending: Rebinding(timeout: token),
      )
    }
    _, _ -> state
  }
}

fn hold_early(state: State, message: SocketMsg) -> State {
  case state.early_count >= max_early_messages {
    True -> {
      socket_metrics.overflowed(socket_metrics.EarlyBuffer)
      close(state, error.CloseUnknownError, "Session unavailable")
    }
    False ->
      State(
        ..state,
        early: [message, ..state.early],
        early_count: state.early_count + 1,
      )
  }
}

fn release_early(state: State, owner: SessionId) -> State {
  let early = list.reverse(state.early)
  let state = State(..state, early: [], early_count: 0)
  list.fold(early, state, fn(state, message) {
    case message.session == owner {
      True -> deliver(state, message)
      False -> state
    }
  })
}

fn send_outbound(state: State, frame: Outbound) -> State {
  use <- bool.guard(state.phase != Open, state)
  case frame {
    session.HeartbeatAckFrame -> session_ack(state)
    _ -> send_session_frame(state, frame)
  }
}

fn send_session_frame(state: State, frame: Outbound) -> State {
  let sent = send_frame(state, encode.outbound(frame))
  case frame, sent {
    session.DispatchFrame(event:, ..), Ok(state) -> {
      metrics.counter("fluxer_gateway_websocket_dispatches_total", [], 1)
      socket_metrics.dispatch_sent(event)
      case event {
        "READY" -> {
          garbage_collect()
          State(
            ..state,
            identify_at: elapsed(
              state.identify_at,
              socket_metrics.identify_to_ready,
            ),
          )
        }
        "RESUMED" -> {
          reconnect_if_draining(state)
          State(
            ..state,
            resume_at: elapsed(
              state.resume_at,
              socket_metrics.resume_to_resumed,
            ),
          )
        }
        _ -> state
      }
    }
    session.DispatchFrame(..), Error(Nil) -> {
      metrics.counter("fluxer_gateway_websocket_dispatch_drops_total", [], 1)
      state
    }
    session.ReconnectFrame, Ok(state) -> close_for_drain(state)
    session.ReconnectFrame, Error(Nil) -> close_for_drain(state)
    _, Ok(state) -> state
    _, Error(Nil) -> state
  }
}

fn elapsed(started: Option(Int), observe: fn(Int) -> Nil) -> Option(Int) {
  case started {
    Some(at) -> observe(clock.monotonic_ms() - at)
    None -> Nil
  }
  None
}

fn drain_reconnect(state: State) -> State {
  case state.phase {
    Open -> {
      drain_op7()
      case
        send_frame(state, encode.outbound(session.ReconnectFrame)),
        state.attached
      {
        Ok(state), Some(_) -> await_client_close(state)
        Ok(state), None -> close_for_drain(state)
        Error(Nil), _ -> close_for_drain(state)
      }
    }
    Lingering | Closed -> state
  }
}

fn close_for_drain(state: State) -> State {
  close(
    state,
    error.CloseUnknownError,
    "Session drain requested; reconnect to continue",
  )
}

fn await_client_close(state: State) -> State {
  let _ = table.ensure_public(lingering_table())
  table.insert(lingering_table(), #(process.self(), Nil))
  let state = close_for_drain(state)
  process.send_after(state.config.self, reconnect_linger_ms(), ReconnectLinger)
  State(..state, phase: Lingering)
}

fn reconnect_if_draining(state: State) -> Nil {
  case draining.is_draining() {
    True -> process.send(state.config.self, DrainReconnect)
    False -> Nil
  }
}

fn demonitor(monitor: Option(Monitor)) -> Nil {
  case monitor {
    Some(monitor) -> process.demonitor_process(monitor)
    None -> Nil
  }
}

fn attach(state: State, session_ref: SessionRef) -> State {
  let monitor = case process.subject_owner(session_ref.inbox) {
    Ok(pid) -> Some(process.monitor(pid))
    Error(Nil) -> None
  }
  State(
    ..reset_voice(state),
    attached: Some(Attached(ref: session_ref, monitor:)),
    acks: no_acks,
  )
  |> release_early(session_ref.id)
}

fn reset_voice(state: State) -> State {
  State(
    ..state,
    voice: voice_ops.new(),
    voice_generation: state.voice_generation + 1,
  )
}

fn start_reply(state: State, reply: Result(SessionRef, StartError)) -> State {
  case reply, state.attached, state.pending {
    Ok(session_ref), Some(_), _ | Ok(session_ref), None, Resuming(..) -> {
      session.send(session_ref, session.Terminate(session.Replaced))
      state
    }
    Ok(session_ref), None, _ -> {
      socket_metrics.start_replied(Ok(Nil))
      attach(State(..release_hold(state), pending: Idle), session_ref)
    }
    Error(start_error), None, Starting(identify: ident, ..) -> {
      socket_metrics.start_replied(Error(start_error))
      start_failed(State(..state, pending: Idle), ident, start_error)
    }
    Error(_), _, _ -> state
  }
}

fn start_failed(
  state: State,
  ident: Identify,
  start_error: StartError,
) -> State {
  case start_error {
    session.StartAuthFailed ->
      close(state, error.AuthenticationFailed, "Invalid token")
    session.StartShardingRequired ->
      close(state, error.ShardingRequired, "Sharding required")
    session.StartTooManySessions ->
      close(state, error.CloseRateLimited, "Too many sessions")
    session.StartRateLimited(..) ->
      hold(state, ident, socket_metrics.HoldRateLimited)
    session.StartUnavailable(..) ->
      hold(state, ident, socket_metrics.HoldUnavailable)
    session.StartUnknownError ->
      close(state, error.CloseUnknownError, "Failed to start session")
  }
}

fn resume_reply(state: State, reply: Result(SessionRef, ResumeError)) -> State {
  case state.pending, state.attached {
    Resuming(..), None -> {
      let state = State(..state, pending: Idle)
      socket_metrics.resume_finished(case reply {
        Ok(_) -> socket_metrics.Resumed
        Error(rejected) -> socket_metrics.ResumeRejected(rejected)
      })
      case reply {
        Ok(session_ref) -> {
          metrics.counter("fluxer_gateway_session_resumes_total", [], 1)
          rollout_metrics.count(rollout_metrics.resumed, [], 1)
          garbage_collect()
          State(
            ..attach(state, session_ref),
            heartbeat: heartbeat.acked(clock.system_ms()),
          )
        }
        Error(session.InvalidResumeSeq) ->
          close(state, error.InvalidSeq, "Invalid sequence")
        Error(session.SessionGone) ->
          send_invalid_session(state, "session_gone")
        Error(session.ResumeAuthFailed) ->
          close(state, error.AuthenticationFailed, "Invalid token")
        Error(session.ResumeUnavailable(..)) ->
          close(state, error.CloseUnknownError, "Session unavailable")
      }
    }
    _, _ -> state
  }
}

fn down_message(state: State, down: process.Down) -> State {
  case state.attached, down {
    Some(Attached(monitor: Some(monitor), ..)),
      process.ProcessDown(monitor: down_monitor, reason:, ..)
      if monitor == down_monitor
    -> {
      case crashed(reason) {
        True ->
          rollout_metrics.count(rollout_metrics.identify_after_crash, [], 1)
        False -> Nil
      }
      let state = State(..reset_voice(state), attached: None, acks: no_acks)
      send_invalid_session(state, "session_down")
    }
    _, _ -> state
  }
}

pub fn crashed(reason: process.ExitReason) -> Bool {
  case reason {
    process.Normal -> False
    process.Killed -> True
    process.Abnormal(found) -> found != protocol.handoff_exit()
  }
}

fn heartbeat_check(state: State, token: Int) -> State {
  case state.heartbeat_timer {
    Some(current) if current == token -> {
      let state = State(..state, heartbeat_timer: None)
      let outcome = heartbeat.check(state.heartbeat, clock.system_ms())
      socket_metrics.heartbeat_checked(outcome)
      let next = case outcome {
        heartbeat.TimedOut ->
          close(state, error.SessionTimeout, "Heartbeat timeout")
        heartbeat.RequestHeartbeat(waiting) -> {
          let state = schedule_heartbeat(State(..state, heartbeat: waiting))
          case send_frame(state, encode.heartbeat_request()) {
            Ok(state) -> state
            Error(Nil) -> state
          }
        }
        heartbeat.StillAlive -> schedule_heartbeat(state)
      }
      garbage_collect()
      next
    }
    _ -> state
  }
}

fn schedule_heartbeat(state: State) -> State {
  case state.heartbeat_timer {
    Some(_) -> state
    None -> {
      let #(state, token) = new_token(state)
      process.send_after(
        state.config.self,
        heartbeat.check_interval_ms(),
        HeartbeatCheck(token),
      )
      State(..state, heartbeat_timer: Some(token))
    }
  }
}

fn voice_update(
  state: State,
  attached: Attached,
  update: VoiceUpdate,
) -> State {
  case
    voice_ops.submit(
      state.voice,
      update,
      clock.system_ms(),
      limits.rate_limits_disabled(),
    )
  {
    voice_ops.SendNow(voice) -> {
      log_voice(update, "direct")
      forward(attached, update.op)
      State(..state, voice:)
    }
    voice_ops.Queued(ops: voice, arm_timer:, queue_length:) -> {
      log_voice(update, "queued")
      logging.log(
        logging.Warning,
        "gateway_voice_state_update_queue_insert: queue_len="
          <> int.to_string(queue_length),
      )
      arm_voice_timer(state, arm_timer)
      State(..state, voice:)
    }
  }
}

fn log_voice(update: VoiceUpdate, mode: String) -> Nil {
  let level = case mode {
    "direct" -> logging.Info
    _ -> logging.Warning
  }
  logging.log(
    level,
    "gateway_voice_state_update_dispatch: guild_id="
      <> codec.describe(update.guild)
      <> " channel_id="
      <> codec.describe(update.channel)
      <> " connection_id="
      <> codec.describe(update.connection)
      <> " mode="
      <> mode,
  )
}

fn arm_voice_timer(state: State, arm: Bool) -> Nil {
  case arm {
    True -> {
      process.send_after(
        state.config.self,
        voice_ops.drain_interval_ms(),
        VoiceDrain(state.voice_generation),
      )
      Nil
    }
    False -> Nil
  }
}

fn voice_drain(state: State) -> State {
  case state.attached {
    None -> reset_voice(state)
    Some(attached) -> {
      let drained =
        voice_ops.drain(
          state.voice,
          clock.system_ms(),
          limits.rate_limits_disabled(),
        )
      case drained.send {
        Some(update) -> forward(attached, update.op)
        None -> Nil
      }
      arm_voice_timer(state, drained.arm_timer)
      State(..state, voice: drained.ops)
    }
  }
}

fn send_frame(state: State, json_bytes: BytesTree) -> Result(State, Nil) {
  use <- bool.guard(state.phase != Open, Error(Nil))
  case compress.compress(state.compress, json_bytes) {
    Error(compress.CompressFailed) -> Error(Nil)
    Ok(#(bytes, context)) -> {
      let channel = state.config.link
      let compression = compress.compression(context)
      let wire = bytes_tree.byte_size(bytes)
      let payload = case compression {
        compress.NoCompression -> wire
        compress.ZstdStream -> bytes_tree.byte_size(json_bytes)
      }
      socket_metrics.sent(compression, wire, payload)
      let sent = case compression {
        compress.NoCompression -> link.text(channel, bytes)
        compress.ZstdStream -> link.binary(channel, bytes)
      }
      case sent {
        Ok(Nil) -> Nil
        Error(Nil) -> socket_metrics.send_failed()
      }
      Ok(State(..state, compress: context))
    }
  }
}

fn new_token(state: State) -> #(State, Int) {
  let token = state.next_token + 1
  #(State(..state, next_token: token), token)
}

@external(erlang, "erlang", "garbage_collect")
fn erlang_garbage_collect() -> Bool

fn garbage_collect() -> Nil {
  erlang_garbage_collect()
  Nil
}
