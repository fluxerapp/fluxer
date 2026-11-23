// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/config/cgroup
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/rollout/hub
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/nats/pool.{type NatsMessage, type SubscriptionId}
import fluxer_gateway/telemetry/watchdog
import fluxer_gateway/util/decode as edge_decode
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

pub type Percentage {
  WholePercentage(Int)
  FractionalPercentage(Float)
}

pub type RolloutMode {
  Modulo
  Random
}

pub type VoiceE2eeScope {
  GuildFeatureOnly
  PlatformWide
}

pub opaque type RolloutConfig {
  RolloutConfig(
    session_percentage: Percentage,
    session_mode: RolloutMode,
    guild_percentage: Percentage,
    rpc_timeout_ms: Int,
    max_session_starts: Int,
    max_guild_starts: Int,
    relay_shards: Int,
    relay_max_queue: Int,
    voice_e2ee_scope: VoiceE2eeScope,
  )
}

pub type RolloutChanged {
  RolloutChanged(version: Int)
}

pub opaque type Message {
  SubscribeNats
  Fetch(token: Int)
  NatsReceived(NatsMessage)
  PoolDown(process.Down)
}

type State {
  State(
    timers: Subject(Message),
    nats: Subject(NatsMessage),
    subscription: Option(SubscriptionId),
    pool: Option(Pid),
    version: Int,
    fetch_token: Int,
  )
}

type Change {
  Updated
  Unchanged
  Rejected
}

type Source {
  FromApi
  FromNats
}

type ConfigKey {
  FluxerGatewayRolloutConfig
}

type CeilingKey {
  FluxerGatewaySessionStartCeiling
}

type ProcessFlag {
  FullsweepAfter
}

type Stored

@external(erlang, "persistent_term", "put")
fn put_config(key: ConfigKey, value: Option(RolloutConfig)) -> Stored

@external(erlang, "persistent_term", "get")
fn get_config(
  key: ConfigKey,
  default: Option(RolloutConfig),
) -> Option(RolloutConfig)

@external(erlang, "persistent_term", "put")
fn put_ceiling(key: CeilingKey, value: Option(Int)) -> Stored

@external(erlang, "persistent_term", "get")
fn get_ceiling(key: CeilingKey, default: Option(Int)) -> Option(Int)

@external(erlang, "erlang", "phash2")
fn phash2(term: String, range: Int) -> Int

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: ProcessFlag, value: Int) -> Int

const nats_subject = "config.gateway.rollout"

fn fetch_delay_ms() -> Int {
  durations.config().rollout_fetch_delay_ms
}

fn fetch_retry_ms() -> Int {
  durations.config().rollout_fetch_retry_ms
}

fn reconcile_interval_ms() -> Int {
  durations.config().rollout_reconcile_interval_ms
}

fn nats_subscribe_retry_ms() -> Int {
  durations.config().rollout_nats_subscribe_retry_ms
}

fn init_timeout_ms() -> Int {
  durations.config().rollout_init_timeout_ms
}

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway@config@rollout")
}

fn changes_name() -> process.Name(hub.Message(RolloutChanged)) {
  env.process_name("fluxer_gateway@config@rollout_changes")
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

pub fn supervised_changes(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() { hub.start(changes_name(), init_timeout_ms()) })
}

fn start() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), initialise)
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

pub fn default_config() -> RolloutConfig {
  RolloutConfig(
    session_percentage: WholePercentage(100),
    session_mode: Modulo,
    guild_percentage: WholePercentage(100),
    rpc_timeout_ms: 10_000,
    max_session_starts: 512,
    max_guild_starts: 256,
    relay_shards: 32,
    relay_max_queue: 50_000,
    voice_e2ee_scope: GuildFeatureOnly,
  )
}

fn current() -> RolloutConfig {
  get_config(FluxerGatewayRolloutConfig, None)
  |> option.lazy_unwrap(default_config)
}

pub fn is_guild_eligible(guild: GuildId) -> Bool {
  let config = current()
  eligible(id.guild_id_to_string(guild), config.guild_percentage, Modulo)
}

pub fn is_session_eligible(user: UserId) -> Bool {
  let config = current()
  eligible(
    id.user_id_to_string(user),
    config.session_percentage,
    config.session_mode,
  )
}

pub fn session_rollout_percentage() -> Int {
  case current().session_percentage {
    WholePercentage(whole) -> whole
    FractionalPercentage(fraction) ->
      fraction |> float.ceiling |> float.truncate
  }
}

pub fn voice_e2ee_scope() -> VoiceE2eeScope {
  current().voice_e2ee_scope
}

pub fn max_concurrent_session_starts() -> Int {
  let configured = current().max_session_starts
  get_ceiling(FluxerGatewaySessionStartCeiling, None)
  |> option.map(int.min(configured, _))
  |> option.unwrap(configured)
}

pub fn max_concurrent_guild_starts() -> Int {
  current().max_guild_starts
}

pub fn dispatch_relay_shards() -> Int {
  int.max(1, current().relay_shards)
}

pub fn dispatch_relay_max_queue() -> Int {
  int.max(0, current().relay_max_queue)
}

pub fn rpc_request_timeout_ms() -> Int {
  current().rpc_timeout_ms
}

pub fn subscribe_changes(to subscriber: Subject(RolloutChanged)) -> Nil {
  hub.subscribe(changes_name(), subscriber)
}

pub fn unsubscribe_changes(to subscriber: Subject(RolloutChanged)) -> Nil {
  hub.unsubscribe(changes_name(), subscriber)
}

pub fn eligible(id: String, percentage: Percentage, mode: RolloutMode) -> Bool {
  case at_least(percentage, 100), at_most(percentage, 0), mode {
    True, _, _ -> True
    False, True, _ -> False
    False, False, Modulo -> below(phash2(id, 100), percentage)
    False, False, Random -> within(int.random(100) + 1, percentage)
  }
}

pub fn validate(
  fields: Dict(String, Dynamic),
) -> Result(RolloutConfig, String) {
  let d = default_config()
  use session_percentage <- result.try(field(
    fields,
    "session_rollout_percentage",
    d.session_percentage,
    percentage_decoder(),
  ))
  use session_mode <- result.try(field(
    fields,
    "session_rollout_mode",
    d.session_mode,
    rollout_mode_decoder(),
  ))
  use guild_percentage <- result.try(field(
    fields,
    "guild_rollout_percentage",
    d.guild_percentage,
    percentage_decoder(),
  ))
  use rpc_timeout_ms <- result.try(field(
    fields,
    "rpc_request_timeout_ms",
    d.rpc_timeout_ms,
    bounded_int(1000, 60_000),
  ))
  use max_session_starts <- result.try(field(
    fields,
    "max_concurrent_session_starts",
    d.max_session_starts,
    bounded_int(1, 10_000),
  ))
  use max_guild_starts <- result.try(field(
    fields,
    "max_concurrent_guild_starts",
    d.max_guild_starts,
    bounded_int(1, 10_000),
  ))
  use relay_shards <- result.try(field(
    fields,
    "gateway_dispatch_relay_shards",
    d.relay_shards,
    bounded_int(1, id.max_id),
  ))
  use relay_max_queue <- result.try(field(
    fields,
    "gateway_dispatch_relay_max_queue",
    d.relay_max_queue,
    bounded_int(1, watchdog.kill_threshold_len()),
  ))
  use voice_e2ee_scope <- result.try(field(
    fields,
    "voice_e2ee_scope",
    d.voice_e2ee_scope,
    voice_e2ee_scope_decoder(),
  ))
  Ok(RolloutConfig(
    session_percentage:,
    session_mode:,
    guild_percentage:,
    rpc_timeout_ms:,
    max_session_starts:,
    max_guild_starts:,
    relay_shards:,
    relay_max_queue:,
    voice_e2ee_scope:,
  ))
}

pub fn payload_config(payload: BitArray) -> Result(Dict(String, Dynamic), Nil) {
  use json <- result.try(edge_decode.decode_json(payload))
  use fields <- result.try(
    decode.run(json, object_decoder()) |> result.replace_error(Nil),
  )
  case dict.get(fields, "config") {
    Ok(config) ->
      decode.run(config, object_decoder()) |> result.replace_error(Nil)
    Error(Nil) -> Ok(fields)
  }
}

fn initialise(
  self: Subject(Message),
) -> Result(actor.Initialised(State, Message, Nil), String) {
  process_flag(FullsweepAfter, 50)
  init_session_start_ceiling(cgroup.memory_limit())
  put_config(FluxerGatewayRolloutConfig, Some(current()))
  let nats = process.new_subject()
  let timers = process.new_subject()
  process.send(timers, SubscribeNats)
  process.send_after(timers, fetch_delay_ms(), Fetch(0))
  let selector =
    process.new_selector()
    |> process.select(self)
    |> process.select(timers)
    |> process.select_map(nats, NatsReceived)
    |> process.select_monitors(PoolDown)
  State(
    timers:,
    nats:,
    subscription: None,
    pool: None,
    version: 0,
    fetch_token: 0,
  )
  |> actor.initialised
  |> actor.selecting(selector)
  |> Ok
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    SubscribeNats -> actor.continue(subscribe_nats(state))
    Fetch(token) if token == state.fetch_token -> {
      let change = fetch_from_api()
      let delay = case change {
        Rejected -> fetch_retry_ms()
        Updated | Unchanged -> reconcile_interval_ms()
      }
      actor.continue(schedule_fetch(notify_if(state, change), delay))
    }
    Fetch(_) -> actor.continue(state)
    NatsReceived(received) if received.subject == nats_subject ->
      actor.continue(notify_if(state, apply_payload(received.payload)))
    NatsReceived(_) -> actor.continue(state)
    PoolDown(process.ProcessDown(pid:, ..)) ->
      case state.pool == Some(pid) {
        True -> actor.continue(pool_down(state))
        False -> actor.continue(state)
      }
    PoolDown(process.PortDown(..)) -> actor.continue(state)
  }
}

fn subscribe_nats(state: State) -> State {
  case state.subscription {
    Some(_) -> state
    None ->
      case pool.subscribe(nats_subject, None, state.nats) {
        Ok(subscription) -> {
          logging.log(
            logging.Info,
            "Gateway rollout config subscribed to NATS subject " <> nats_subject,
          )
          let owner = pool.subscription_pool(subscription)
          let _ = process.monitor(owner)
          State(..state, subscription: Some(subscription), pool: Some(owner))
          |> schedule_fetch(fetch_delay_ms())
        }
        Error(reason) -> {
          logging.log(
            logging.Debug,
            "Gateway rollout config waiting for NATS subscription: "
              <> string.inspect(reason),
          )
          process.send_after(
            state.timers,
            nats_subscribe_retry_ms(),
            SubscribeNats,
          )
          state
        }
      }
  }
}

fn pool_down(state: State) -> State {
  logging.log(
    logging.Warning,
    "Gateway rollout config lost its NATS subscription, resubscribing",
  )
  process.send_after(state.timers, nats_subscribe_retry_ms(), SubscribeNats)
  State(..state, subscription: None, pool: None)
}

fn schedule_fetch(state: State, delay_ms: Int) -> State {
  let token = state.fetch_token + 1
  process.send_after(state.timers, delay_ms, Fetch(token))
  State(..state, fetch_token: token)
}

fn notify_if(state: State, change: Change) -> State {
  case change {
    Updated -> {
      let version = state.version + 1
      hub.notify(changes_name(), RolloutChanged(version:))
      State(..state, version:)
    }
    Unchanged | Rejected -> state
  }
}

fn fetch_from_api() -> Change {
  let deadline = clock.deadline_in(rpc_request_timeout_ms())
  case api.call("get_gateway_rollout_config", [], deadline) {
    Ok(data) ->
      case decode.run(data, decode.at(["config"], object_decoder())) {
        Ok(fields) -> store(fields, FromApi)
        Error(_) -> {
          logging.log(
            logging.Warning,
            "Gateway rollout config: unexpected API response format",
          )
          Rejected
        }
      }
    Error(reason) -> {
      logging.log(
        logging.Warning,
        "Gateway rollout config failed to fetch from API: "
          <> string.inspect(reason),
      )
      Rejected
    }
  }
}

fn apply_payload(payload: BitArray) -> Change {
  case payload_config(payload) {
    Ok(fields) -> store(fields, FromNats)
    Error(Nil) -> {
      logging.log(
        logging.Warning,
        "Gateway rollout config: unexpected NATS payload format",
      )
      Rejected
    }
  }
}

fn store(fields: Dict(String, Dynamic), source: Source) -> Change {
  case validate(fields) {
    Ok(new) -> {
      let old = current()
      case new == old {
        True -> Unchanged
        False -> {
          log_transitions(old, new)
          put_config(FluxerGatewayRolloutConfig, Some(new))
          logging.log(
            logging.Info,
            "Gateway rollout config updated from " <> source_name(source),
          )
          Updated
        }
      }
    }
    Error(reason) -> {
      logging.log(
        logging.Warning,
        "Gateway rollout config rejected invalid config: " <> reason,
      )
      Rejected
    }
  }
}

fn init_session_start_ceiling(limit: Option(Int)) -> Nil {
  case limit {
    None -> {
      put_ceiling(FluxerGatewaySessionStartCeiling, None)
      logging.log(
        logging.Notice,
        "Session start ceiling: no memory limit detected",
      )
    }
    Some(limit_bytes) -> {
      let ceiling = cgroup.session_start_ceiling(limit_bytes)
      put_ceiling(FluxerGatewaySessionStartCeiling, Some(ceiling))
      logging.log(
        logging.Notice,
        "Session start ceiling: memory_limit_bytes="
          <> int.to_string(limit_bytes)
          <> " max_concurrent_session_starts="
          <> int.to_string(int.min(ceiling, max_concurrent_session_starts())),
      )
    }
  }
}

fn log_transitions(old: RolloutConfig, new: RolloutConfig) -> Nil {
  [
    #(
      "session_rollout_percentage",
      percentage_text(old.session_percentage),
      percentage_text(new.session_percentage),
    ),
    #(
      "guild_rollout_percentage",
      percentage_text(old.guild_percentage),
      percentage_text(new.guild_percentage),
    ),
    #(
      "rpc_request_timeout_ms",
      int.to_string(old.rpc_timeout_ms),
      int.to_string(new.rpc_timeout_ms),
    ),
  ]
  |> list.each(fn(transition) {
    let #(key, old_value, new_value) = transition
    case old_value == new_value {
      True -> Nil
      False ->
        logging.log(
          logging.Notice,
          "Gateway rollout config transition: key="
            <> key
            <> " old="
            <> old_value
            <> " new="
            <> new_value,
        )
    }
  })
}

fn field(
  fields: Dict(String, Dynamic),
  key: String,
  default: a,
  decoder: Decoder(a),
) -> Result(a, String) {
  case dict.get(fields, key) {
    Error(Nil) -> Ok(default)
    Ok(value) ->
      decode.run(value, decoder)
      |> result.replace_error("invalid field " <> key)
  }
}

fn object_decoder() -> Decoder(Dict(String, Dynamic)) {
  decode.dict(decode.string, decode.dynamic)
}

fn percentage_decoder() -> Decoder(Percentage) {
  decode.one_of(decode.int |> decode.map(WholePercentage), [
    decode.float |> decode.map(FractionalPercentage),
  ])
  |> decode.then(fn(percentage) {
    case at_least(percentage, 0) && at_most(percentage, 100) {
      True -> decode.success(percentage)
      False -> decode.failure(percentage, "percentage")
    }
  })
}

fn bounded_int(min: Int, max: Int) -> Decoder(Int) {
  decode.int
  |> decode.then(fn(value) {
    case value >= min && value <= max {
      True -> decode.success(value)
      False -> decode.failure(value, "bounded integer")
    }
  })
}

fn rollout_mode_decoder() -> Decoder(RolloutMode) {
  decode.string
  |> decode.then(fn(text) {
    case text {
      "modulo" -> decode.success(Modulo)
      "random" -> decode.success(Random)
      _ -> decode.failure(Modulo, "rollout mode")
    }
  })
}

fn voice_e2ee_scope_decoder() -> Decoder(VoiceE2eeScope) {
  decode.string
  |> decode.then(fn(text) {
    case text {
      "guild_feature_only" -> decode.success(GuildFeatureOnly)
      "platform_wide" -> decode.success(PlatformWide)
      _ -> decode.failure(GuildFeatureOnly, "voice e2ee scope")
    }
  })
}

fn source_name(source: Source) -> String {
  case source {
    FromApi -> "api"
    FromNats -> "nats"
  }
}

fn percentage_text(percentage: Percentage) -> String {
  case percentage {
    WholePercentage(whole) -> int.to_string(whole)
    FractionalPercentage(fraction) -> float.to_string(fraction)
  }
}

fn within(value: Int, percentage: Percentage) -> Bool {
  case percentage {
    WholePercentage(whole) -> value <= whole
    FractionalPercentage(fraction) -> int.to_float(value) <=. fraction
  }
}

fn below(value: Int, percentage: Percentage) -> Bool {
  case percentage {
    WholePercentage(whole) -> value < whole
    FractionalPercentage(fraction) -> int.to_float(value) <. fraction
  }
}

fn at_least(percentage: Percentage, bound: Int) -> Bool {
  case percentage {
    WholePercentage(whole) -> whole >= bound
    FractionalPercentage(fraction) -> fraction >=. int.to_float(bound)
  }
}

fn at_most(percentage: Percentage, bound: Int) -> Bool {
  case percentage {
    WholePercentage(whole) -> whole <= bound
    FractionalPercentage(fraction) -> fraction <=. int.to_float(bound)
  }
}
