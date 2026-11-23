// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type GatewayError, type RpcError}
import fluxer_gateway/core/guild.{type GuildFacts}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/core/voice_key
import fluxer_gateway/core/voice_state.{type VoiceState}
import fluxer_gateway/perms
import fluxer_gateway/voice/client_update
import fluxer_gateway/voice/connection
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_rpc
import fluxer_gateway/voice/guild_state.{
  type Ctx, type Request, type Step, State,
}
import fluxer_gateway/voice/lifecycle
import fluxer_gateway/voice/member as voice_member
import fluxer_gateway/voice/metrics as voice_metrics
import fluxer_gateway/voice/p2p
import fluxer_gateway/voice/region
import fluxer_gateway/voice/registry
import fluxer_gateway/voice/store.{type Store}
import fluxer_gateway/voice/virtual_access
import gleam/bool
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

fn handshake_timeout_ms() -> Int {
  durations.voice().guild_handler_handshake_timeout_ms
}

fn sweep_interval_ms() -> Int {
  durations.voice().guild_handler_sweep_interval_ms
}

fn p2p_check_interval_ms() -> Int {
  durations.voice().guild_handler_p2p_check_interval_ms
}

fn fetch_ttl_ms() -> Int {
  durations.voice().guild_handler_fetch_ttl_ms
}

fn relay_lifetime_ms() -> Int {
  durations.voice().guild_handler_relay_lifetime_ms
}

pub opaque type GuildVoice {
  GuildVoice(
    state: guild_state.State,
    registered: Option(Subject(VoiceRequest)),
    predecessor: Option(Subject(Subject(VoiceRequest))),
  )
}

pub opaque type Successor {
  Successor(Subject(Subject(Result(Subject(VoiceRequest), Nil))))
}

type RelayEvent {
  Arrived(Subject(VoiceRequest))
  Asked(Subject(Result(Subject(VoiceRequest), Nil)))
  OwnerGone
}

pub type VoiceContext {
  VoiceContext(
    facts: GuildFacts,
    reader: perms.Reader,
    member: fn(UserId) -> Option(Member),
    channel: fn(ChannelId) -> Option(Channel),
    sessions_of: fn(UserId) -> List(String),
    self_voice: Subject(VoiceRequest),
    now_ms: Int,
  )
}

pub opaque type VoiceRequest {
  VoiceRequest(Request)
}

pub fn is_read_only(request: VoiceRequest) -> Bool {
  let VoiceRequest(inner) = request
  case inner {
    guild_state.CollectStates(..) -> True
    guild_state.ClientUpdate(..)
    | guild_state.Signal(..)
    | guild_state.TokenReady(..)
    | guild_state.Rpc(..)
    | guild_state.Sweep
    | guild_state.P2pCheck
    | guild_state.CleanupAccess(..) -> False
  }
}

pub type VoiceEffect {
  Broadcast(event: String, payload: RawJson, channel: Option(ChannelId))
  BroadcastMove(
    event: String,
    payload: RawJson,
    left: RawJson,
    from: ChannelId,
    to: ChannelId,
  )
  SendToSession(session: SessionId, event: String, payload: RawJson)
  SendError(session: SessionId, error: GatewayError)
  VirtualAccessSet(user: UserId, channels: List(ChannelId))
}

pub fn new(facts: GuildFacts) -> GuildVoice {
  GuildVoice(
    state: guild_state.new(guild.id(facts)),
    registered: None,
    predecessor: None,
  )
}

pub fn handoff(voice: GuildVoice) -> #(GuildVoice, Successor) {
  let #(link, asks) = start_relay()
  #(
    GuildVoice(
      state: State(..voice.state, ticking: False)
        |> lifecycle.drop_orphan_transitions,
      registered: None,
      predecessor: Some(link),
    ),
    Successor(asks),
  )
}

pub fn await_successor(
  successor: Successor,
  timeout: Int,
) -> Result(Subject(VoiceRequest), Nil) {
  let Successor(asks) = successor
  let reply = process.new_subject()
  process.send(asks, reply)
  process.receive(reply, timeout) |> result.flatten
}

fn start_relay() -> #(
  Subject(Subject(VoiceRequest)),
  Subject(Subject(Result(Subject(VoiceRequest), Nil))),
) {
  let owner = process.self()
  let handshake = process.new_subject()
  process.spawn_unlinked(fn() {
    let link = process.new_subject()
    let asks = process.new_subject()
    let monitor = process.monitor(owner)
    process.send(handshake, #(link, asks))
    process.new_selector()
    |> process.select_map(link, Arrived)
    |> process.select_map(asks, Asked)
    |> process.select_specific_monitor(monitor, fn(_) { OwnerGone })
    |> relay(None, [], relay_lifetime_ms())
  })
  case process.receive(handshake, handshake_timeout_ms()) {
    Ok(pair) -> pair
    Error(Nil) -> #(process.new_subject(), process.new_subject())
  }
}

fn relay(
  events: process.Selector(RelayEvent),
  found: Option(Subject(VoiceRequest)),
  waiting: List(Subject(Result(Subject(VoiceRequest), Nil))),
  remaining: Int,
) -> Nil {
  let started = clock.monotonic_ms()
  case process.selector_receive(events, remaining) {
    Error(Nil) | Ok(OwnerGone) ->
      list.each(waiting, process.send(_, Error(Nil)))
    Ok(Arrived(target)) -> {
      list.each(waiting, process.send(_, Ok(target)))
      relay(events, Some(target), [], left(remaining, started))
    }
    Ok(Asked(reply)) ->
      case found {
        Some(target) -> {
          process.send(reply, Ok(target))
          relay(events, found, waiting, left(remaining, started))
        }
        None ->
          relay(events, found, [reply, ..waiting], left(remaining, started))
      }
  }
}

fn left(remaining: Int, started: Int) -> Int {
  int.max(remaining - { clock.monotonic_ms() - started }, 0)
}

pub fn follows_handoff(request: VoiceRequest) -> Bool {
  let VoiceRequest(inner) = request
  case inner {
    guild_state.ClientUpdate(..)
    | guild_state.Signal(..)
    | guild_state.TokenReady(..)
    | guild_state.Rpc(..) -> True
    guild_state.CollectStates(..)
    | guild_state.Sweep
    | guild_state.P2pCheck
    | guild_state.CleanupAccess(..) -> False
  }
}

pub fn resume(voice: GuildVoice, ctx: VoiceContext) -> GuildVoice {
  list.each(store.values(voice.state.states), count_table.upsert)
  let voice = register(voice, ctx)
  let ictx = internal(ctx)
  case voice.predecessor {
    Some(link) -> process.send(link, ctx.self_voice)
    None -> Nil
  }
  case virtual_access.granted_users(voice.state.access) {
    [] -> Nil
    users -> ictx.notify(guild_state.CleanupAccess(users))
  }
  let fetches =
    dict.map_values(voice.state.fetches, fn(_nonce, fetch) {
      guild_state.spawn_fetch(ictx.notify, fetch.job, fetch.request)
      guild_state.Fetch(..fetch, started_at: ctx.now_ms)
    })
  GuildVoice(
    ..voice,
    predecessor: None,
    state: ensure_ticking(State(..voice.state, fetches:), ictx),
  )
}

pub fn release(voice: GuildVoice) -> Nil {
  count_table.drop_owner(process.self())
  case voice.registered {
    Some(subject) -> unregister(voice.state.guild, subject)
    None -> Nil
  }
}

fn unregister(guild: id.GuildId, subject: Subject(VoiceRequest)) -> Nil {
  let table = registry.guilds_table()
  case registry.exists(table) {
    True -> {
      ets_delete_object(table, #(id.guild_id_to_int(guild), subject))
      Nil
    }
    False -> Nil
  }
}

pub fn voice_states(voice: GuildVoice) -> List(VoiceState) {
  voice.state.states
  |> store.values
  |> list.map(entry.state)
}

pub fn issued_states(voice: GuildVoice) -> List(VoiceState) {
  voice.state.pending
  |> store.values
  |> list.map(fn(pending) { entry.state(pending.entry) })
  |> list.append(voice_states(voice), _)
}

pub fn issued_count(voice: GuildVoice) -> Int {
  store.size(voice.state.pending) + store.size(voice.state.states)
}

pub fn connected_channels(voice: GuildVoice, user: UserId) -> List(ChannelId) {
  store.keys_of_user(voice.state.states, user)
  |> list.map(voice_key.channel)
  |> list.unique
}

pub fn has_virtual_access(
  voice: GuildVoice,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  virtual_access.has_access(voice.state.access, user, channel)
}

pub fn preserve_view(
  voice: GuildVoice,
  user: UserId,
  channel: ChannelId,
) -> GuildVoice {
  GuildVoice(
    ..voice,
    state: State(
      ..voice.state,
      access: virtual_access.grant_view_only(voice.state.access, user, channel),
    ),
  )
}

pub fn virtual_channels(voice: GuildVoice, user: UserId) -> List(ChannelId) {
  virtual_access.channels(voice.state.access, user)
}

pub fn virtual_grants(voice: GuildVoice) -> List(#(UserId, List(ChannelId))) {
  virtual_access.all(voice.state.access)
}

pub fn voice_state_json(voice: GuildVoice, state: VoiceState) -> Json {
  let found = case
    guild_state.connected(voice.state, voice_state.connection_id(state))
  {
    Ok(live) ->
      case entry.state(live) == state {
        True -> live
        False -> entry.from_state(state)
      }
    Error(Nil) -> entry.from_state(state)
  }
  entry.to_json(found, entry.Broadcast)
}

pub fn decode_op(
  ref: SessionRef,
  op: session.ClientOp,
) -> Result(Option(VoiceRequest), GatewayError) {
  case op {
    session.VoiceStateUpdate(data) ->
      decode.run(data, client_update.decoder())
      |> result.unwrap(client_update.empty())
      |> update(ref, _)
      |> result.map(fn(request) { Some(VoiceRequest(request)) })
    session.VoiceSignal(data) ->
      client_update.signal(data)
      |> option.from_result
      |> option.map(fn(found) { VoiceRequest(signal(ref, found)) })
      |> Ok
    _ -> Ok(None)
  }
}

fn signal(session: SessionRef, signal: client_update.Signal) -> Request {
  guild_state.Signal(
    session:,
    channel: signal.channel,
    to: signal.to,
    data: signal.data,
  )
}

fn update(
  session: SessionRef,
  update: client_update.Update,
) -> Result(Request, GatewayError) {
  use channel <- result.try(case update.channel {
    Ok(None) -> Ok(guild_state.Leave)
    Ok(Some(channel)) -> Ok(guild_state.Join(channel))
    Error(Nil) -> Error(error.ValidationInvalidSnowflake)
  })
  Ok(
    guild_state.ClientUpdate(guild_state.Update(
      session:,
      channel:,
      connection_id: update.connection_id,
      flags: update.flags,
      viewer_keys: update.viewer_keys,
      geo: update.geo,
      e2ee_capable: update.e2ee_capable,
      bot: update.bot,
      p2p: update.p2p,
      country_code: update.country_code,
    )),
  )
}

pub fn handle(
  voice: GuildVoice,
  ctx: VoiceContext,
  request: VoiceRequest,
) -> #(GuildVoice, List(VoiceEffect)) {
  let VoiceRequest(inner) = request
  run(voice, ctx, fn(step, ictx) { dispatch(step, ictx, inner) })
}

pub fn on_member_change(
  voice: GuildVoice,
  ctx: VoiceContext,
  user: UserId,
) -> #(GuildVoice, List(VoiceEffect)) {
  run(voice, ctx, fn(step, ictx) {
    voice_member.on_member_change(step, ictx, user)
  })
}

pub fn on_permissions_change(
  voice: GuildVoice,
  ctx: VoiceContext,
) -> #(GuildVoice, List(VoiceEffect)) {
  run(voice, ctx, voice_member.on_permissions_change)
}

pub fn on_channels_change(
  voice: GuildVoice,
  ctx: VoiceContext,
  channels: List(ChannelId),
) -> #(GuildVoice, List(VoiceEffect)) {
  run(voice, ctx, fn(step, ictx) {
    voice_member.on_channels_change(step, ictx, channels)
  })
}

pub fn on_users_change(
  voice: GuildVoice,
  ctx: VoiceContext,
  users: List(UserId),
) -> #(GuildVoice, List(VoiceEffect)) {
  run(voice, ctx, fn(step, ictx) {
    voice_member.on_users_change(step, ictx, users)
  })
}

pub fn on_channel_delete(
  voice: GuildVoice,
  ctx: VoiceContext,
  channel: ChannelId,
) -> #(GuildVoice, List(VoiceEffect)) {
  run(voice, ctx, fn(step, ictx) {
    step
    |> region.cancel_reissues(ictx, fn(job) {
      guild_state.job_target(job).1 == channel
    })
    |> lifecycle.disconnect_channel(ictx, channel)
  })
}

pub fn on_unavailable(
  voice: GuildVoice,
  ctx: VoiceContext,
  keep: fn(UserId) -> Bool,
) -> #(GuildVoice, List(VoiceEffect)) {
  run(voice, ctx, fn(step, ictx) {
    voice_users(step.state)
    |> list.filter(fn(user) { !keep(user) })
    |> list.fold(step, fn(acc, user) {
      connection.disconnect_user(acc, ictx, user, None)
    })
  })
}

fn voice_users(state: guild_state.State) -> List(UserId) {
  let connected = store.users(state.states)
  let pending = store.users(state.pending)
  let fetching =
    state.fetches
    |> dict.values
    |> list.map(fn(fetch) { guild_state.job_target(fetch.job).0 })
  list.flatten([connected, pending, fetching]) |> list.unique
}

fn internal(ctx: VoiceContext) -> Ctx {
  let target = ctx.self_voice
  guild_state.Ctx(
    facts: ctx.facts,
    reader: ctx.reader,
    member: ctx.member,
    channel: ctx.channel,
    sessions_of: ctx.sessions_of,
    now_ms: ctx.now_ms,
    notify: fn(request) { process.send(target, VoiceRequest(request)) },
    notify_after: fn(request, delay) {
      process.send_after(target, delay, VoiceRequest(request))
      Nil
    },
  )
}

fn run(
  voice: GuildVoice,
  ctx: VoiceContext,
  body: fn(Step, Ctx) -> Step,
) -> #(GuildVoice, List(VoiceEffect)) {
  let voice = register(voice, ctx)
  let ictx = internal(ctx)
  let before = voice.state.states
  let step = body(guild_state.start(ensure_ticking(voice.state, ictx)), ictx)
  let after = step.state.states
  sync_counts(before, after)
  count_effects(step.effects)
  let effects =
    step.effects
    |> list.reverse
    |> list.filter_map(convert)
    |> list.append(
      virtual_access.granted_changes(voice.state.access, step.state.access)
      |> list.map(fn(pair) { VirtualAccessSet(pair.0, pair.1) }),
    )
  #(GuildVoice(..voice, state: step.state), effects)
}

fn ensure_ticking(state: guild_state.State, ctx: Ctx) -> guild_state.State {
  case state.ticking {
    True -> state
    False -> {
      ctx.notify_after(guild_state.Sweep, sweep_interval_ms())
      ctx.notify_after(guild_state.P2pCheck, p2p_check_interval_ms())
      State(..state, ticking: True)
    }
  }
}

fn convert(effect: guild_state.Effect) -> Result(VoiceEffect, Nil) {
  case effect {
    guild_state.Broadcast(event, payload, channel) ->
      Ok(Broadcast(event, payload, channel))
    guild_state.BroadcastMove(event, payload, left, from, to) ->
      Ok(BroadcastMove(event, payload, left, from, to))
    guild_state.ToSession(target, event, payload) ->
      session.session_id_from_string(target)
      |> result.map(SendToSession(_, event, payload))
    guild_state.ErrorTo(target, reason) ->
      session.session_id_from_string(target)
      |> result.map(SendError(_, reason))
  }
}

fn count_effects(effects: List(guild_state.Effect)) -> Nil {
  let #(updates, errors) =
    list.fold(effects, #(0, 0), fn(acc, effect) {
      case effect {
        guild_state.ToSession(_, "VOICE_SERVER_UPDATE", _) -> #(
          acc.0 + 1,
          acc.1,
        )
        guild_state.ErrorTo(..) -> #(acc.0, acc.1 + 1)
        guild_state.ToSession(..)
        | guild_state.Broadcast(..)
        | guild_state.BroadcastMove(..) -> acc
      }
    })
  voice_metrics.server_updates(voice_metrics.GuildScope, updates)
  voice_metrics.guild_errors(errors)
}

fn sync_counts(before: Store(Entry), after: Store(Entry)) -> Nil {
  case before == after {
    True -> Nil
    False -> {
      let previous = store.to_dict(before)
      let current = store.to_dict(after)
      voice_metrics.transitions(voice_metrics.GuildScope, previous, current)
      dict.each(previous, fn(key, _value) {
        case dict.has_key(current, key) {
          True -> Nil
          False -> count_table.remove(key)
        }
      })
      dict.each(current, fn(key, value) {
        case dict.get(previous, key) {
          Ok(old) if old == value -> Nil
          _ -> count_table.upsert(value)
        }
      })
    }
  }
}

fn dispatch(step: Step, ctx: Ctx, request: Request) -> Step {
  voice_metrics.guild_request(request_kind(request))
  case request {
    guild_state.ClientUpdate(update) -> connection.update(step, ctx, update)
    guild_state.Signal(session, channel, to, data) ->
      connection.signal(
        step,
        guild_state.session_text(session),
        channel,
        to,
        data,
      )
    guild_state.TokenReady(job, outcome) ->
      connection.token_ready(step, ctx, job, outcome)
    guild_state.Rpc(call, reply) -> {
      let #(step, result) = guild_rpc.handle(step, ctx, call)
      respond(reply, result)
      step
    }
    guild_state.CollectStates(reply) -> {
      process.send(reply, guild_state.entries(step.state))
      step
    }
    guild_state.Sweep -> {
      ctx.notify_after(guild_state.Sweep, sweep_interval_ms())
      step
      |> guild_state.map_state(lifecycle.sweep(_, ctx.now_ms))
      |> expire_fetches(ctx)
      |> lifecycle.sweep_pending(ctx)
    }
    guild_state.P2pCheck -> {
      ctx.notify_after(guild_state.P2pCheck, p2p_check_interval_ms())
      p2p_check(step, ctx)
    }
    guild_state.CleanupAccess(users) ->
      list.fold(users, step, fn(acc, user) {
        lifecycle.cleanup_access(acc, ctx, user)
      })
  }
}

fn request_kind(request: Request) -> voice_metrics.GuildRequest {
  case request {
    guild_state.ClientUpdate(..) -> voice_metrics.ClientUpdateRequest
    guild_state.Signal(..) -> voice_metrics.SignalRequest
    guild_state.TokenReady(..) -> voice_metrics.TokenReadyRequest
    guild_state.Rpc(..) -> voice_metrics.RpcRequest
    guild_state.CollectStates(..) -> voice_metrics.CollectRequest
    guild_state.Sweep -> voice_metrics.SweepRequest
    guild_state.P2pCheck -> voice_metrics.P2pCheckRequest
    guild_state.CleanupAccess(..) -> voice_metrics.CleanupAccessRequest
  }
}

fn expire_fetches(step: Step, ctx: Ctx) -> Step {
  step.state.fetches
  |> dict.values
  |> list.filter(fn(fetch) { ctx.now_ms - fetch.started_at >= fetch_ttl_ms() })
  |> list.fold(step, fn(acc, fetch) {
    connection.token_ready(acc, ctx, fetch.job, Error(error.VoiceTokenFailed))
  })
}

fn p2p_check(step: Step, ctx: Ctx) -> Step {
  let gone =
    step.state.states
    |> store.to_list
    |> list.filter(fn(pair) { p2p.session_gone(pair.1, ctx.sessions_of) })
  let #(confirmed, fresh) =
    list.partition(gone, fn(pair) {
      set.contains(step.state.p2p_suspects, pair.0)
    })
  let step =
    guild_state.map_state(step, fn(state) {
      State(
        ..state,
        p2p_suspects: fresh |> list.map(fn(pair) { pair.0 }) |> set.from_list,
      )
    })
  list.fold(confirmed, step, fn(acc, pair) {
    let #(key, value) = pair
    case store.has_key(acc.state.states, key) {
      True -> connection.remove_one(acc, ctx, key, value)
      False -> acc
    }
  })
}

fn respond(reply: guild_state.Replier, result: Result(Json, RpcError)) -> Nil {
  let guild_state.ReplyTo(subject) = reply
  process.send(subject, result)
}

fn register(voice: GuildVoice, ctx: VoiceContext) -> GuildVoice {
  case voice.registered == Some(ctx.self_voice) {
    True -> voice
    False ->
      case registry.exists(registry.guilds_table()) {
        True -> {
          ets_insert(registry.guilds_table(), #(
            id.guild_id_to_int(voice.state.guild),
            ctx.self_voice,
          ))
          GuildVoice(..voice, registered: Some(ctx.self_voice))
        }
        False -> voice
      }
  }
}

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: #(Int, Subject(VoiceRequest))) -> Bool

@external(erlang, "ets", "delete_object")
fn ets_delete_object(table: Atom, row: #(Int, Subject(VoiceRequest))) -> Bool

@external(erlang, "ets", "tab2list")
fn ets_rows(table: Atom) -> List(#(Int, Subject(VoiceRequest)))

pub fn rpc_request(
  call: guild_state.RpcCall,
  reply: Subject(Result(Json, RpcError)),
) -> VoiceRequest {
  VoiceRequest(guild_state.Rpc(call, guild_state.ReplyTo(reply)))
}

pub fn collect_all(
  timeout: Int,
) -> List(#(process.Pid, Result(List(Entry), Nil))) {
  let table = registry.guilds_table()
  use <- bool.guard(!registry.exists(table), [])
  let deadline = clock.monotonic_ms() + timeout
  ets_rows(table)
  |> list.filter_map(fn(row) {
    case process.subject_owner(row.1) {
      Ok(pid) ->
        case process.is_alive(pid) {
          True -> {
            let reply = process.new_subject()
            process.send(row.1, VoiceRequest(guild_state.CollectStates(reply)))
            Ok(#(pid, reply))
          }
          False -> {
            ets_delete_object(table, row)
            Error(Nil)
          }
        }
      Error(Nil) -> {
        ets_delete_object(table, row)
        Error(Nil)
      }
    }
  })
  |> list.map(fn(pending) {
    let wait = int.max(deadline - clock.monotonic_ms(), 0)
    #(pending.0, process.receive(pending.1, wait))
  })
}
