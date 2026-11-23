// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router as cluster_router
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/json_scan
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/remote as guild_remote
import fluxer_gateway/guild/rpc as guild_rpc
import fluxer_gateway/guild/rpc/infra
import fluxer_gateway/guild/rpc/params as guild_params
import fluxer_gateway/nats/pool.{type NatsMessage}
import fluxer_gateway/presence/rpc as presence_rpc
import fluxer_gateway/push/rpc as push_rpc
import fluxer_gateway/rpc/metrics as rpc_metrics
import fluxer_gateway/rpc/reply
import fluxer_gateway/telemetry/rpc as telemetry_rpc
import fluxer_gateway/util/decode as edge
import fluxer_gateway/util/spawn
import fluxer_gateway/voice/rpc as voice_rpc
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

fn init_timeout_ms() -> Int {
  durations.rpc().server_init_timeout_ms
}

const default_max_handlers = 512

fn subscribe_retry_ms() -> Int {
  durations.rpc().server_subscribe_retry_ms
}

const call_timeout_message = "callee did not send reply before timeout"

type Status {
  NotRequired
  Subscribed
  Missing
}

pub opaque type Message {
  Request(NatsMessage)
  HandlerDown(process.Down)
  RetrySubscribe
  StopListening(reply: Subject(Nil))
}

type State {
  State(
    self: Subject(Message),
    inbox: Subject(NatsMessage),
    subjects: List(String),
    subscribed: List(String),
    handlers: Dict(Pid, Handler),
    max_handlers: Int,
    pool: Option(#(Pid, process.Monitor)),
    retry_armed: Bool,
  )
}

type Handler {
  Handler(method: String, reply_to: String)
}

type Key {
  FluxerGatewayRpcStatus
  FluxerGatewayRpcOverloadSenders
}

type Stored

type Atomics

type AtomicsOption {
  Signed(Bool)
}

@external(erlang, "persistent_term", "get")
fn get_status(key: Key, default: Status) -> Status

@external(erlang, "persistent_term", "put")
fn put_status(key: Key, value: Status) -> Stored

@external(erlang, "persistent_term", "get")
fn get_atomics(
  key: Key,
  default: option.Option(Atomics),
) -> option.Option(Atomics)

@external(erlang, "persistent_term", "put")
fn put_atomics(key: Key, value: option.Option(Atomics)) -> Stored

@external(erlang, "atomics", "new")
fn atomics_new(size: Int, options: List(AtomicsOption)) -> Atomics

@external(erlang, "atomics", "add_get")
fn atomics_add_get(cells: Atomics, index: Int, by: Int) -> Int

@external(erlang, "atomics", "sub")
fn atomics_sub(cells: Atomics, index: Int, by: Int) -> Stored

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway_nats_rpc")
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() { start(settings) })
}

fn start(settings: Settings) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    let self = process.new_subject()
    let subjects = case settings.nats_rpc_enabled {
      True -> reply.subjects_for_role(settings.role)
      False -> {
        logging.log(
          logging.Info,
          "Gateway NATS RPC subscription disabled (FLUXER_GATEWAY_NATS_RPC_ENABLED=false)",
        )
        []
      }
    }
    let _ = overload_senders()
    rpc_metrics.watch()
    let inbox = process.new_subject()
    let state =
      State(
        self:,
        inbox:,
        subjects:,
        subscribed: [],
        handlers: dict.new(),
        max_handlers: positive_or(
          settings.gateway_nats_rpc_max_handlers,
          default_max_handlers,
        ),
        pool: None,
        retry_armed: True,
      )
    publish_status(state)
    process.send(self, RetrySubscribe)
    let selector =
      process.new_selector()
      |> process.select(named)
      |> process.select(self)
      |> process.select_map(inbox, Request)
      |> process.select_monitors(HandlerDown)
    state
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

pub fn stop_listening() -> Nil {
  case process.named(name()) {
    Ok(_) -> {
      let reply = process.new_subject()
      process.send(process.named_subject(name()), StopListening(reply))
      let _ = process.receive(reply, init_timeout_ms())
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn nats_ready() -> Bool {
  case get_status(FluxerGatewayRpcStatus, Missing) {
    NotRequired -> True
    Subscribed -> pool.has_ready_connection()
    Missing -> False
  }
}

pub fn handler_limit_reached(count: Int, max: Int) -> Bool {
  count >= max
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    RetrySubscribe ->
      actor.continue(subscribe(State(..state, retry_armed: False)))
    Request(request) -> actor.continue(dispatch(state, request))
    StopListening(reply) -> {
      pool.unsubscribe_all(state.inbox)
      let state = State(..state, subjects: [], subscribed: [])
      process.send(reply, Nil)
      actor.continue(state)
    }
    HandlerDown(process.ProcessDown(pid:, reason:, ..)) ->
      case watched_pool(state) == Some(pid) {
        True -> actor.continue(pool_down(state))
        False -> handler_down(state, pid, reason)
      }
    HandlerDown(process.PortDown(..)) -> actor.continue(state)
  }
}

fn handler_down(
  state: State,
  pid: Pid,
  reason: process.ExitReason,
) -> actor.Next(State, Message) {
  case dict.has_key(state.handlers, pid) {
    True -> rpc_metrics.handler_finished()
    False -> Nil
  }
  case dict.get(state.handlers, pid), reason {
    Ok(Handler(method:, reply_to:)), process.Abnormal(crash) -> {
      let body = case is_call_timeout(crash) {
        True -> {
          rpc_metrics.handled(method, "timeout")
          logging.log(
            logging.Warning,
            "Gateway NATS RPC exit timeout: method=" <> method,
          )
          reply.failure("timeout")
        }
        False -> {
          rpc_metrics.handled(method, "crashed")
          logging.log(
            logging.Error,
            "Gateway NATS RPC method execution failed: method="
              <> method
              <> " reason="
              <> string.inspect(crash),
          )
          reply.internal_error()
        }
      }
      let _ = pool.publish_reply(reply_to, bit_array.from_string(body))
      Nil
    }
    Ok(Handler(method:, reply_to:)), process.Killed -> {
      rpc_metrics.handled(method, "crashed")
      logging.log(
        logging.Error,
        "Gateway NATS RPC method execution failed: method="
          <> method
          <> " reason=killed",
      )
      let _ =
        pool.publish_reply(
          reply_to,
          bit_array.from_string(reply.internal_error()),
        )
      Nil
    }
    _, _ -> Nil
  }
  actor.continue(State(..state, handlers: dict.delete(state.handlers, pid)))
}

fn is_call_timeout(crash: Dynamic) -> Bool {
  let message =
    decode.at([0], decode.at([atom.create("message")], decode.string))
  decode.run(crash, message) == Ok(call_timeout_message)
}

fn subscribe(state: State) -> State {
  let missing =
    list.filter(state.subjects, fn(subject) {
      !list.contains(state.subscribed, subject)
    })
  let state =
    list.fold(missing, state, fn(state, subject) {
      case
        pool.subscribe_every_slot(subject, Some(reply.queue_group), state.inbox)
      {
        Ok(subscription) -> {
          logging.log(
            logging.Info,
            "Gateway NATS RPC subscribed: subject=" <> subject,
          )
          record(state, subject, pool.subscription_pool(subscription))
        }
        Error(reason) -> {
          logging.log(
            logging.Warning,
            "Gateway NATS RPC failed to subscribe: subject="
              <> subject
              <> " reason="
              <> describe(reason),
          )
          state
        }
      }
    })
  let state = case list.length(state.subscribed) < list.length(state.subjects) {
    True -> arm_retry(state)
    False -> state
  }
  publish_status(state)
  state
}

fn arm_retry(state: State) -> State {
  case state.retry_armed {
    True -> state
    False -> {
      process.send_after(state.self, subscribe_retry_ms(), RetrySubscribe)
      State(..state, retry_armed: True)
    }
  }
}

fn watched_pool(state: State) -> Option(Pid) {
  option.map(state.pool, fn(watched) { watched.0 })
}

fn record(state: State, subject: String, owner: Pid) -> State {
  case state.pool {
    Some(#(current, _)) if current == owner ->
      State(..state, subscribed: [subject, ..state.subscribed])
    previous -> {
      case previous {
        Some(#(_, stale)) -> process.demonitor_process(stale)
        None -> Nil
      }
      State(
        ..state,
        subscribed: [subject],
        pool: Some(#(owner, process.monitor(owner))),
      )
    }
  }
}

fn pool_down(state: State) -> State {
  logging.log(
    logging.Warning,
    "Gateway NATS RPC lost its NATS subscriptions, resubscribing",
  )
  let state = State(..state, subscribed: [], pool: None)
  publish_status(state)
  arm_retry(state)
}

fn describe(reason: pool.NatsError) -> String {
  case reason {
    pool.NotConnected -> "not_connected"
    pool.NatsTimeout -> "timeout"
    pool.NoResponders -> "no_responders"
    pool.ConnectionClosed -> "closed"
    pool.ProtocolError(text) -> text
  }
}

fn publish_status(state: State) -> Nil {
  let status = case state.subjects {
    [] -> NotRequired
    _ ->
      case list.length(state.subscribed) == list.length(state.subjects) {
        True -> Subscribed
        False -> Missing
      }
  }
  case get_status(FluxerGatewayRpcStatus, Missing) == status {
    True -> Nil
    False -> {
      put_status(FluxerGatewayRpcStatus, status)
      Nil
    }
  }
}

fn dispatch(state: State, request: NatsMessage) -> State {
  case reply.is_rpc_subject(request.subject), request.reply_to {
    False, _ | True, None -> state
    True, Some(reply_to) ->
      case
        handler_limit_reached(dict.size(state.handlers), state.max_handlers)
      {
        True -> {
          rpc_metrics.handled(reply.method(request.subject), "overloaded")
          send_overloaded(reply_to, state.max_handlers)
          state
        }
        False -> {
          let subject = request.subject
          let payload = request.payload
          let #(pid, _) =
            spawn.monitored(fn() { run(subject, payload, reply_to) })
          rpc_metrics.handler_started()
          State(
            ..state,
            handlers: dict.insert(
              state.handlers,
              pid,
              Handler(method: reply.method(subject), reply_to:),
            ),
          )
        }
      }
  }
}

fn run(subject: String, payload: BitArray, reply_to: String) -> Nil {
  let method = reply.method(subject)
  let started = clock.monotonic_ms()
  let body = execute(method, payload)
  rpc_metrics.answered(
    method,
    reply.succeeded(body),
    clock.monotonic_ms() - started,
  )
  let _ = pool.publish_reply(reply_to, bit_array.from_string(body))
  Nil
}

pub fn guild_route(payload: BitArray) -> Result(GuildId, Nil) {
  use value <- result.try(json_scan.field(payload, "guild_id"))
  use decoded <- result.try(edge.decode_json(value))
  use found <- result.try(
    edge.validate_snowflake(decoded) |> result.replace_error(Nil),
  )
  id.guild_id(found) |> result.replace_error(Nil)
}

pub fn execute(method: String, payload: BitArray) -> String {
  case relays(method) && !cluster_router.joined() {
    True -> relay(method, payload)
    False -> execute_joined(method, payload)
  }
}

fn relays(method: String) -> Bool {
  case method {
    "guild." <> _ | "presence." <> _ | "call." <> _ | "voice." <> _ -> True
    _ -> False
  }
}

fn relay(method: String, payload: BitArray) -> String {
  let self = node.self()
  cluster_router.reachable_with(node.Guilds)
  |> list.append(node.connected())
  |> list.unique
  |> list.filter(fn(peer) { peer != self })
  |> relay_to(method, payload)
}

fn relay_to(
  peers: List(node.Node),
  method: String,
  payload: BitArray,
) -> String {
  case peers {
    [] -> execute_joined(method, payload)
    [peer, ..rest] ->
      case
        guild_remote.call(
          peer,
          "fluxer_gateway@rpc@server",
          "execute_joined",
          [guild_remote.arg(method), guild_remote.arg(payload)],
          infra.forward_timeout_ms(),
          expecting: decode.string,
        )
      {
        Ok(body) -> body
        Error(Nil) -> relay_to(rest, method, payload)
      }
  }
}

pub fn execute_joined(method: String, payload: BitArray) -> String {
  case route_raw(method, payload) {
    Ok(outcome) -> respond(outcome, method)
    Error(Nil) -> execute_decoded(method, payload)
  }
}

fn execute_decoded(method: String, payload: BitArray) -> String {
  case method {
    "guild." <> _ ->
      case guild_route(payload) {
        Error(Nil) -> guild_answer(method, payload)
        Ok(guild) ->
          execute_guild(
            method,
            payload,
            guild,
            clock.monotonic_ms() + owner_wait_ms,
          )
      }
    _ -> answer(method, payload, route)
  }
}

const owner_wait_ms = 3000

fn execute_guild(
  method: String,
  payload: BitArray,
  guild: GuildId,
  deadline: Int,
) -> String {
  case cluster_router.owner(cluster_router.GuildKey(guild), node.Guilds) {
    cluster_router.Remote(owner) ->
      infra.forward_payload(owner, method, payload)
      |> result.map(fn(body) { raw_json.wrap_string(body) |> raw_json.to_json })
      |> respond(method)
    cluster_router.OwnerUnavailable(retry_after_ms) ->
      case clock.monotonic_ms() + retry_after_ms < deadline {
        True -> {
          process.sleep(retry_after_ms)
          execute_guild(method, payload, guild, deadline)
        }
        False -> respond(Error(error.RpcTimeout), method)
      }
    cluster_router.Local -> guild_answer(method, payload)
  }
}

fn route_raw(
  method: String,
  payload: BitArray,
) -> Result(Result(Json, RpcError), Nil) {
  let raw = raw_json.wrap(payload)
  case method {
    "presence." <> _ -> presence_rpc.handle_raw(method, raw)
    "call." <> _ | "voice." <> _ ->
      object(raw) |> result.map(fn(_) { voice_rpc.handle(method, raw) })
    "process." <> _ ->
      object(raw) |> result.map(fn(_) { telemetry_rpc.handle(method, raw) })
    _ -> Error(Nil)
  }
}

fn object(raw: raw_json.RawJson) -> Result(Nil, Nil) {
  raw_json.fields(raw) |> result.replace(Nil)
}

fn route(method: String, params: Dynamic) -> Result(Json, RpcError) {
  case method {
    "presence." <> _ -> presence_rpc.handle(method, params)
    "push." <> _ -> push_rpc.handle(method, params)
    _ -> Error(error.UnknownMethod(method))
  }
}

pub fn execute_owned(method: String, payload: BitArray) -> #(Bool, BitArray) {
  guild_rpc.handle_owned(method, raw_json.wrap(payload))
}

fn guild_answer(method: String, payload: BitArray) -> String {
  case guild_params.of_object(raw_json.wrap(payload)) {
    Error(Nil) -> bad_params(method)
    Ok(params) -> respond(guild_rpc.handle_local(method, params), method)
  }
}

fn bad_params(method: String) -> String {
  logging.log(
    logging.Error,
    "Gateway NATS RPC method execution failed: method="
      <> method
      <> " reason=badarg",
  )
  reply.internal_error()
}

fn params(payload: BitArray) -> Result(Dynamic, Nil) {
  use decoded <- result.try(edge.decode_json(payload))
  case is_map(decoded) {
    True -> Ok(decoded)
    False -> Error(Nil)
  }
}

@external(erlang, "erlang", "is_map")
fn is_map(value: Dynamic) -> Bool

fn answer(
  method: String,
  payload: BitArray,
  route: fn(String, Dynamic) -> Result(Json, RpcError),
) -> String {
  case params(payload) {
    Error(Nil) -> bad_params(method)
    Ok(decoded) -> respond(route(method, decoded), method)
  }
}

fn respond(outcome: Result(Json, RpcError), method: String) -> String {
  case outcome {
    Ok(value) -> reply.ok(value)
    Error(reason) -> {
      let message = error.rpc_error_message(reason)
      case reply.is_quiet(message) {
        True -> Nil
        False ->
          logging.log(
            logging.Warning,
            "Gateway NATS RPC throw error. method="
              <> method
              <> " error="
              <> message,
          )
      }
      reply.failure(message)
    }
  }
}

fn overload_senders() -> Atomics {
  case get_atomics(FluxerGatewayRpcOverloadSenders, None) {
    Some(found) -> found
    None -> {
      let created = atomics_new(1, [Signed(True)])
      put_atomics(FluxerGatewayRpcOverloadSenders, Some(created))
      created
    }
  }
}

fn send_overloaded(reply_to: String, max_senders: Int) -> Nil {
  let senders = overload_senders()
  case atomics_add_get(senders, 1, 1) > max_senders {
    True -> {
      atomics_sub(senders, 1, 1)
      Nil
    }
    False -> {
      process.spawn_unlinked(fn() {
        let _ =
          pool.publish_reply(
            reply_to,
            bit_array.from_string(reply.overloaded()),
          )
        atomics_sub(senders, 1, 1)
      })
      Nil
    }
  }
}

fn positive_or(value: Int, default: Int) -> Int {
  case value > 0 {
    True -> value
    False -> default
  }
}
