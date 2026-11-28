// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/message.{type MoveId} as _
import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/guild.{
  type GuildFacts, type GuildRequest, type JoinReply, type JoinRequest, GuildRef,
} as core_guild
import fluxer_gateway/core/id.{
  type ChannelId, type GuildId, type RoleId, type UserId,
}
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/guild/availability
import fluxer_gateway/guild/cleanup
import fluxer_gateway/guild/data.{type Loaded, Loaded}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/dispatch
import fluxer_gateway/guild/dispatch/broadcast
import fluxer_gateway/guild/dispatch/model
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/handoff/catch_up.{type CatchUp}
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/handoff/copy
import fluxer_gateway/guild/handoff/hold.{type Hold}
import fluxer_gateway/guild/handoff/roster
import fluxer_gateway/guild/handoff/signal.{type Signal}
import fluxer_gateway/guild/handoff/transfer
import fluxer_gateway/guild/health
import fluxer_gateway/guild/inbox.{type Control, type Handle, Handle}
import fluxer_gateway/guild/init
import fluxer_gateway/guild/join/admission.{type Gate}
import fluxer_gateway/guild/maintenance
import fluxer_gateway/guild/members
import fluxer_gateway/guild/members/search
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/guild/presence/reconcile
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/push/dispatch as push_dispatch
import fluxer_gateway/guild/push/engaged
import fluxer_gateway/guild/query
import fluxer_gateway/guild/remains.{type Remains}
import fluxer_gateway/guild/requests
import fluxer_gateway/guild/requests/online
import fluxer_gateway/guild/sessions.{type Sessions} as guild_sessions
import fluxer_gateway/guild/sessions/connect
import fluxer_gateway/guild/sessions/dm_partners
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/sessions/passive_sync
import fluxer_gateway/guild/sessions/presence as session_presence
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/subscriptions
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/guild/user_data
import fluxer_gateway/guild/visitors
import fluxer_gateway/guild/visitors/dispatch as visitor_dispatch
import fluxer_gateway/guild/visitors/table as visitor_table
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move as handoff_move
import fluxer_gateway/handoff/precopy
import fluxer_gateway/member_list
import fluxer_gateway/perms
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/session/control
import fluxer_gateway/telemetry/memory_stats
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/threads/message.{type ThreadMsg}
import fluxer_gateway/threads/store.{type Handoff}
import fluxer_gateway/util/decode as id_decode
import fluxer_gateway/voice/guild_handler.{
  type GuildVoice, type VoiceContext, type VoiceEffect, type VoiceRequest,
}
import fluxer_gateway/voice/p2p
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/set.{type Set}
import logging

fn down_jitter_ms() -> Int {
  durations.guild().server_down_jitter_ms
}

pub fn init_timeout_ms() -> Int {
  durations.guild().server_init_timeout_ms
}

const fullsweep_after = 100

const large_guild_members = 10_000

const large_guild_min_bin_vheap_words = 16_000_000

fn auto_stop_grace_ms() -> Int {
  durations.guild().server_auto_stop_grace_ms
}

fn idle_start_grace_ms() -> Int {
  durations.guild().server_idle_start_grace_ms
}

fn stale_perms_release_ms() -> Int {
  durations.guild().server_stale_perms_release_ms
}

fn successor_timeout_ms() -> Int {
  durations.guild().server_successor_timeout_ms
}

pub type StartArgs {
  StartArgs(
    guild: GuildId,
    loaded: Loaded,
    threads: Option(Handoff),
    voice: Option(GuildVoice),
    rejoining: List(SessionId),
    arriving: Option(Sessions),
  )
  StageArgs(guild: GuildId, move: MoveId, bulk: BitArray)
}

type Mode {
  Serving
  Staged(move: MoveId, round: Int)
  Frozen(move: MoveId, version: Int, epoch: Int, held: List(Message))
  Handing(
    move: MoveId,
    version: Int,
    epoch: Int,
    target: Handle,
    waiting: Set(String),
    sealed: Bool,
  )
  Arriving(applied: Int, early: Dict(Int, BitArray), stash: List(Message))
}

type Timer {
  CountsRefresh
  AvailabilityRecheck
  DrainJoins
  PassiveSync
  Reconcile
  AutoStop(token: Int)
  ReplayWindowClosed
  FreezeDeadline(token: Int)
  ReleasePerms(stale: perms.Perms, since_ms: Int)
  AckMoves
}

type Copy {
  Copy(move: MoveId, round: Int)
}

type Parts {
  Parts(
    handle: Handle,
    voice: Subject(VoiceRequest),
    timers: Subject(Timer),
    reconciled: Subject(reconcile.Mismatches),
  )
}

type Message {
  Public(GuildRequest)
  Private(Control)
  Voice(VoiceRequest)
  Push(push_dispatch.PushGuildMsg)
  Threads(ThreadMsg)
  Reconciled(reconcile.Mismatches)
  Monitor(process.Down)
  Tick(Timer)
  Moving(Signal)
  Copying(copy.Request)
}

type Server {
  Server(
    state: GuildState,
    handle: Handle,
    gate: Gate,
    drain_scheduled: Bool,
    timers: Subject(Timer),
    reconciled: Subject(reconcile.Mismatches),
    published: Int,
    frozen: Option(Frozen),
    auto_stop: Option(Int),
    next_token: Int,
    awaiting_replay: Option(List(#(String, Dynamic))),
    remains: Remains,
    catch_up: CatchUp,
    mode: Mode,
    copy: Option(Copy),
    moves: List(#(SessionRef, Bool)),
  )
}

type Frozen =
  Hold(VoiceRequest, guild_handler.Successor)

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: atom.Atom, value: Int) -> Int

pub fn start(args: StartArgs) -> actor.StartResult(Handle) {
  actor.new_with_initialiser(init_timeout_ms(), fn(_self) {
    case args {
      StartArgs(guild:, loaded:, threads:, voice:, rejoining:, arriving:) ->
        initialise(guild, loaded, threads, voice, rejoining, arriving)
      StageArgs(guild:, move:, bulk:) -> stage(guild, move, bulk)
    }
  })
  |> actor.on_message(handle_message)
  |> actor.start
}

fn tune(member_count: Int) -> Nil {
  let _ = process_flag(atom.create("fullsweep_after"), fullsweep_after)
  case member_count >= large_guild_members {
    True -> {
      let _ =
        process_flag(
          atom.create("min_bin_vheap_size"),
          large_guild_min_bin_vheap_words,
        )
      Nil
    }
    False -> Nil
  }
}

fn new_parts(guild: GuildId) -> Parts {
  Parts(
    handle: Handle(
      ref: GuildRef(guild:, inbox: process.new_subject()),
      control: process.new_subject(),
      pid: process.self(),
    ),
    voice: process.new_subject(),
    timers: process.new_subject(),
    reconciled: process.new_subject(),
  )
}

fn init_failure(error: init.InitError) -> String {
  case error {
    init.PermsLoadFailed -> "guild_perms_load_failed"
    init.MemberListLoadFailed -> "guild_member_list_load_failed"
  }
}

fn initialise(
  guild: GuildId,
  loaded: Loaded,
  threads: Option(Handoff),
  voice: Option(GuildVoice),
  rejoining: List(SessionId),
  arriving: Option(Sessions),
) -> Result(actor.Initialised(Server, Message, Handle), String) {
  tune(list.length(loaded.members))
  let parts = new_parts(guild)
  case
    init.build(
      parts.handle.ref,
      parts.handle.control,
      parts.voice,
      loaded,
      threads,
      voice,
      arriving
        |> option.map(guild_sessions.rehome(_, process.monitor))
        |> option.unwrap(guild_sessions.new()),
    )
  {
    Error(error) -> Error(init_failure(error))
    Ok(state) -> {
      let state = rows.restore_engagement(state)
      let state = case voice {
        Some(_) -> resume_voice(state)
        None -> state
      }
      let #(server, selector) =
        go_live(
          new_server(state, parts, Serving, rejoining),
          parts,
          rejoining,
          option.is_some(voice),
          option.is_some(arriving),
        )
      actor.initialised(server)
      |> actor.selecting(selector)
      |> actor.returning(parts.handle)
      |> Ok
    }
  }
}

fn resume_voice(state: GuildState) -> GuildState {
  guild_state.set_voice(
    state,
    guild_handler.resume(
      guild_state.voice(state),
      guild_state.voice_context(state, clock.system_ms()),
    ),
  )
}

fn new_server(
  state: GuildState,
  parts: Parts,
  mode: Mode,
  rejoining: List(SessionId),
) -> Server {
  Server(
    state:,
    handle: parts.handle,
    gate: admission.new(admission.default_config(), clock.monotonic_ms()),
    drain_scheduled: False,
    timers: parts.timers,
    reconciled: parts.reconciled,
    published: guild_state.version(state),
    frozen: None,
    auto_stop: Some(1),
    next_token: 2,
    awaiting_replay: None,
    remains: remains.new(),
    catch_up: catch_up.new(
      rejoining,
      clock.monotonic_ms(),
      transfer.awaiting_replay_ms(),
    ),
    mode:,
    copy: None,
    moves: [],
  )
}

fn go_live(
  server: Server,
  parts: Parts,
  rejoining: List(SessionId),
  resumed: Bool,
  arriving: Bool,
) -> #(Server, process.Selector(Message)) {
  let state = server.state
  let handle = parts.handle
  let timers = parts.timers
  let guild = guild_state.guild_id(state)
  health.register(guild, handle.control)
  let carried = guild_sessions.session_refs(guild_state.sessions(state))
  list.each(carried, fn(ref) {
    health.put_session(guild, ref)
    health.send_current(guild, ref)
  })
  roster.put_many(
    guild,
    list.fold(carried, rejoining, fn(ids, ref) { [ref.id, ..ids] }),
  )
  maintenance.publish_view(state, handle)
  maintenance.publish_counts(state)
  let _ = availability.publish(guild_state.facts(state))
  process.send_after(
    timers,
    maintenance.count_refresh_interval_ms(),
    CountsRefresh,
  )
  process.send_after(timers, passive_sync.interval_ms(), PassiveSync)
  process.send_after(timers, reconcile.interval_ms(), Reconcile)
  schedule_recheck(state, timers)
  process.send_after(timers, idle_start_grace_ms(), AutoStop(1))
  let awaiting_replay = case resumed, arriving {
    _, True -> None
    True, False -> {
      process.send_after(
        timers,
        transfer.awaiting_replay_ms(),
        ReplayWindowClosed,
      )
      Some([])
    }
    False, False -> None
  }
  let mode = case arriving {
    True -> {
      guild_metrics.enter(guild_metrics.Arriving)
      Arriving(applied: 0, early: dict.new(), stash: [])
    }
    False -> {
      guild_metrics.enter(guild_metrics.Serving)
      Serving
    }
  }
  let server =
    Server(
      ..server,
      published: guild_state.version(state),
      awaiting_replay:,
      mode:,
    )
    |> remember_voice
  #(server, live_selector(state, parts))
}

fn live_selector(state: GuildState, parts: Parts) -> process.Selector(Message) {
  let self = process.self()
  process.new_selector()
  |> process.select_map(signal.subject(self), Moving)
  |> process.select_map(copy.subject(self), Copying)
  |> process.select_map(parts.handle.ref.inbox, Public)
  |> process.select_map(parts.handle.control, Private)
  |> process.select_map(parts.voice, Voice)
  |> process.select_map(push_dispatch.subject(guild_state.push(state)), Push)
  |> process.select_map(guild_state.thread_inbox(state), Threads)
  |> process.select_map(parts.timers, Tick)
  |> process.select_map(parts.reconciled, Reconciled)
  |> process.select_monitors(Monitor)
}

fn staged_ttl_ms() -> Int {
  durations.guild().server_staged_ttl_ms
}

fn stage(
  guild: GuildId,
  move: MoveId,
  bulk: BitArray,
) -> Result(actor.Initialised(Server, Message, Handle), String) {
  use #(_, loaded) <- result.try(
    handoff_codec.decode(guild_codec.bulk(), bulk)
    |> result.replace_error("guild bulk could not be decoded"),
  )
  use <- bool.guard(
    data.guild_id(loaded.data) != guild,
    Error("guild bulk is for another guild"),
  )
  tune(list.length(loaded.members))
  let parts = new_parts(guild)
  let state =
    init.prepare(
      parts.handle.ref,
      parts.handle.control,
      parts.voice,
      loaded,
      None,
      guild_sessions.new(),
    )
  let staging = copy.subject(process.self())
  process.send_after(staging, staged_ttl_ms(), copy.Expire)
  guild_metrics.enter(guild_metrics.Staged)
  new_server(state, parts, Staged(move:, round: 0), [])
  |> actor.initialised
  |> actor.selecting(
    process.new_selector() |> process.select_map(staging, Copying),
  )
  |> actor.returning(parts.handle)
  |> Ok
}

pub fn stage_activate(
  move: MoveId,
  tail: guild_codec.Tail,
) -> Result(Handle, String) {
  use pid <- result.try(
    precopy.take(move) |> result.replace_error("guild was not staged"),
  )
  let reply = process.new_subject()
  let monitor = process.monitor(pid)
  process.send(copy.subject(pid), copy.Activate(tail:, reply:))
  let answer =
    process.new_selector()
    |> process.select(reply)
    |> process.select_specific_monitor(monitor, fn(_) {
      Error("staged guild stopped")
    })
    |> process.selector_receive(init_timeout_ms())
  process.demonitor_process(monitor)
  case answer {
    Ok(found) -> found
    Error(Nil) -> {
      process.send(copy.subject(pid), copy.Discard)
      Error("staged guild did not activate in time")
    }
  }
}

fn schedule_recheck(state: GuildState, timers: Subject(Timer)) -> Nil {
  case availability.mode(guild_state.facts(state)) {
    availability.Available -> Nil
    _ -> {
      process.send_after(
        timers,
        availability.recheck_interval_ms(),
        AvailabilityRecheck,
      )
      Nil
    }
  }
}

fn handle_message(
  server: Server,
  message: Message,
) -> actor.Next(Server, Message) {
  case message, server.mode {
    Moving(found), _ -> on_signal(server, found)
    Copying(request), _ -> on_copy(server, request)
    Private(inbox.Disown), Serving
    | Private(inbox.ResyncSessions(..)), Serving
    -> next(run(server, message))
    Private(inbox.Disown), _ -> actor.continue(server)
    Private(inbox.ResyncSessions(reply)), _ -> {
      process.send(reply, 0)
      actor.continue(server)
    }
    _, Staged(..) -> actor.continue(server)
    _, Serving -> next(run(server, message))
    _, Arriving(applied:, early:, stash:) ->
      actor.continue(
        Server(
          ..server,
          mode: Arriving(applied:, early:, stash: [message, ..stash]),
        ),
      )
    _, Frozen(..) -> actor.continue(hold_message(server, message))
    _, Handing(..) -> actor.continue(pass_on(server, message))
  }
}

fn run(server: Server, message: Message) -> Step {
  let occupied = occupancy(server.state)
  case route(server, message) {
    Continue(next) -> settle_perms(after_route(catch_up_joined(next), occupied))
    other -> other
  }
}

fn settle_perms(server: Server) -> Step {
  let #(state, repair) = guild_state.take_repair(server.state)
  case repair {
    guild_state.Intact -> publish_live(server)
    guild_state.Rebuilt(stale) -> {
      let since_ms = clock.monotonic_ms()
      list.each(stale, fn(handle) {
        process.send_after(
          server.timers,
          stale_perms_release_ms(),
          ReleasePerms(handle, since_ms),
        )
      })
      publish_live(with_state(server, state))
    }
    guild_state.Unrepairable -> Crash(server, "guild_model_rebuild_failed")
  }
}

fn publish_live(server: Server) -> Step {
  case perms.status(guild_state.perms_handle(server.state)) {
    Ok(Nil) -> Continue(publish(server))
    Error(_) -> Crash(server, "guild_perms_broken")
  }
}

fn next(step: Step) -> actor.Next(Server, Message) {
  case step {
    Continue(server) -> actor.continue(server)
    Stop(server) -> {
      cleanup.release(server.state)
      remains.forget(guild_state.guild_id(server.state), process.self())
      roster.clear(guild_state.guild_id(server.state), process.self())
      actor.stop()
    }
    Crash(server, reason) -> {
      cleanup.release(server.state)
      actor.stop_abnormal(reason)
    }
  }
}

fn run_all(server: Server, messages: List(Message)) -> Step {
  case messages {
    [] -> Continue(server)
    [message, ..rest] ->
      case run(server, message) {
        Continue(server) -> run_all(server, rest)
        other -> other
      }
  }
}

fn on_signal(server: Server, found: Signal) -> actor.Next(Server, Message) {
  case found, server.mode {
    signal.Freeze(move:, version:, epoch:, reply:), Serving ->
      actor.continue(freeze(server, move, version, epoch, reply))
    signal.Freeze(reply:, ..), _ -> {
      process.send(reply, Error("guild is not serving"))
      actor.continue(server)
    }
    signal.Commit(target:, reply:), Frozen(move:, version:, epoch:, ..) -> {
      process.send(reply, True)
      actor.continue(commit(server, move, version, epoch, target))
    }
    signal.Commit(reply:, ..), _ -> {
      process.send(reply, False)
      actor.continue(server)
    }
    signal.Thaw(reply:), Frozen(held:, ..) -> {
      process.send(reply, True)
      guild_metrics.enter(guild_metrics.Serving)
      next(run_all(Server(..server, mode: Serving), list.reverse(held)))
    }
    signal.Thaw(reply:), _ -> {
      process.send(reply, False)
      actor.continue(server)
    }
    signal.Repointed(session), Handing(waiting:, sealed:, ..) as mode -> {
      let waiting = set.delete(waiting, session.session_id_to_string(session))
      let server = Server(..server, mode: Handing(..mode, waiting:))
      case set.is_empty(waiting), sealed {
        True, False -> actor.continue(seal(server))
        _, _ -> actor.continue(server)
      }
    }
    signal.SealDue, Handing(sealed: False, ..) -> actor.continue(seal(server))
    signal.ForwardGraceOver, Handing(..) -> {
      guild_metrics.stopped("handed_off")
      next(Stop(server))
    }
    signal.Release(seq:, bytes:), Arriving(applied:, early:, stash:) ->
      case seq > applied {
        False -> actor.continue(server)
        True ->
          next(drain_releases(
            Server(
              ..server,
              mode: Arriving(
                applied:,
                early: dict.insert(early, seq, bytes),
                stash:,
              ),
            ),
          ))
      }
    signal.Seal(last_seq:, reply:), Arriving(applied:, stash:, ..) -> {
      process.send(reply, applied >= last_seq)
      guild_metrics.enter(guild_metrics.Serving)
      next(run_all(open(Server(..server, mode: Serving)), list.reverse(stash)))
    }
    signal.Probe(reply:), Frozen(..) | signal.Probe(reply:), Handing(..) -> {
      process.send(reply, True)
      actor.continue(server)
    }
    signal.Probe(reply:), _ -> {
      process.send(reply, False)
      actor.continue(server)
    }
    signal.AbortArrival, Arriving(..)
    | signal.AbortArrival, Frozen(..)
    | signal.AbortArrival, Handing(..)
    -> {
      guild_metrics.stopped("aborted")
      next(Stop(server))
    }
    _, _ -> actor.continue(server)
  }
}

fn on_copy(
  server: Server,
  request: copy.Request,
) -> actor.Next(Server, Message) {
  case request, server.mode {
    copy.Snapshot(move:, version:, reply:), Serving ->
      actor.continue(snapshot(server, move, version, reply))
    copy.Snapshot(reply:, ..), _ -> {
      process.send(reply, Error("guild is not serving"))
      actor.continue(server)
    }
    copy.Changes(move:, version:, round:, reply:), Serving ->
      actor.continue(changes(server, move, version, round, reply))
    copy.Changes(reply:, ..), _ -> {
      process.send(reply, Error("guild is not serving"))
      actor.continue(server)
    }
    copy.Cancel(move), _ ->
      case server.copy {
        Some(Copy(move: current, ..)) if current == move ->
          actor.continue(stop_copy(server))
        _ -> actor.continue(server)
      }
    copy.Restage(round:, bytes:, reply:), Staged(move:, round: applied) ->
      case restage(server, move, applied, round, bytes) {
        Ok(server) -> {
          process.send(reply, Ok(Nil))
          actor.continue(Server(..server, mode: Staged(move:, round:)))
        }
        Error(reason) -> {
          process.send(reply, Error(reason))
          actor.continue(server)
        }
      }
    copy.Restage(reply:, ..), _ -> {
      process.send(reply, Error("guild is not staged"))
      actor.continue(server)
    }
    copy.Activate(tail:, reply:), Staged(move:, round:) ->
      case activate(server, move, round, tail) {
        Ok(#(server, selector)) -> {
          process.send(reply, Ok(server.handle))
          actor.continue(server) |> actor.with_selector(selector)
        }
        Error(reason) -> {
          process.send(reply, Error(reason))
          cleanup.release_staged(server.state)
          actor.stop()
        }
      }
    copy.Activate(reply:, ..), _ -> {
      process.send(reply, Error("guild is not staged"))
      actor.continue(server)
    }
    copy.Discard, Staged(..) | copy.Expire, Staged(..) -> {
      cleanup.release_staged(server.state)
      actor.stop()
    }
    copy.Discard, _ | copy.Expire, _ -> actor.continue(server)
  }
}

fn snapshot(
  server: Server,
  move: MoveId,
  version: Int,
  reply: Subject(Result(BitArray, String)),
) -> Server {
  let server = stop_copy(server)
  let table = members.track(guild_state.members_table(server.state))
  let state = guild_state.set_members_table(server.state, table)
  let loaded =
    Loaded(data: guild_state.guild_data(state), members: members.to_list(table))
  case handoff_codec.encode(guild_codec.bulk(), loaded, version, 0) {
    Ok(bytes) -> {
      process.send(reply, Ok(bytes))
      Server(..server, state:, copy: Some(Copy(move:, round: 0)))
    }
    Error(_) -> {
      process.send(reply, Error("guild bulk could not be encoded"))
      Server(
        ..server,
        state: guild_state.set_members_table(state, members.untrack(table)),
      )
    }
  }
}

fn changes(
  server: Server,
  move: MoveId,
  version: Int,
  round: Int,
  reply: Subject(Result(#(BitArray, Int), String)),
) -> Server {
  case server.copy {
    Some(Copy(move: current, round: previous))
      if current == move && round == previous + 1
    -> {
      let found = members.take_changes(guild_state.members_table(server.state))
      let delta =
        guild_codec.Delta(
          move:,
          round:,
          changed: Loaded(
            data: guild_state.guild_data(server.state),
            members: found.upserted,
          ),
          removed: found.removed,
        )
      case handoff_codec.encode(guild_codec.delta(), delta, version, 0) {
        Ok(bytes) -> {
          process.send(
            reply,
            Ok(#(
              bytes,
              list.length(found.upserted) + list.length(found.removed),
            )),
          )
          Server(..server, copy: Some(Copy(move:, round:)))
        }
        Error(_) -> {
          process.send(reply, Error("guild delta could not be encoded"))
          stop_copy(server)
        }
      }
    }
    _ -> {
      process.send(reply, Error("no precopy for this move"))
      server
    }
  }
}

fn stop_copy(server: Server) -> Server {
  let table = guild_state.members_table(server.state)
  Server(
    ..server,
    state: guild_state.set_members_table(server.state, members.untrack(table)),
    copy: None,
  )
}

fn restage(
  server: Server,
  move: MoveId,
  applied: Int,
  round: Int,
  bytes: BitArray,
) -> Result(Server, String) {
  use #(_, delta) <- result.try(
    handoff_codec.decode(guild_codec.delta(), bytes)
    |> result.replace_error("guild delta could not be decoded"),
  )
  use <- bool.guard(
    delta.move != move || delta.round != round || round != applied + 1,
    Error("guild delta is out of order"),
  )
  Ok(with_state(server, apply_delta(server.state, delta.changed, delta.removed)))
}

fn apply_delta(
  state: GuildState,
  changed: Loaded,
  removed: List(Int),
) -> GuildState {
  let table = guild_state.members_table(state)
  members.put_many(table, changed.members)
  list.each(removed, fn(user) {
    case id.user_id(user) {
      Ok(found) -> members.remove(table, found)
      Error(_) -> Nil
    }
  })
  guild_state.set_guild_data(state, changed.data)
  |> guild_state.set_push(init.push_info(guild_state.push(state), changed.data))
}

fn activate(
  server: Server,
  move: MoveId,
  round: Int,
  tail: guild_codec.Tail,
) -> Result(#(Server, process.Selector(Message)), String) {
  use staged <- result.try(option.to_result(
    tail.staged,
    "guild tail is not staged",
  ))
  use <- bool.guard(
    staged.move != move || staged.round != round,
    Error("guild staging is behind its tail"),
  )
  let state =
    apply_delta(server.state, tail.loaded, staged.removed)
    |> guild_state.set_sessions(
      option.unwrap(tail.table, guild_sessions.new())
      |> guild_sessions.rehome(process.monitor),
    )
    |> rows.restore_engagement
    |> guild_state.set_voice(tail.voice)
  use state <- result.map(
    init.load(state, members.to_list(guild_state.members_table(state)))
    |> result.map_error(init_failure),
  )
  let state =
    guild_threads.install(state, Some(tail.threads))
    |> resume_voice
  let parts =
    Parts(
      handle: server.handle,
      voice: guild_state.voice_inbox(state),
      timers: server.timers,
      reconciled: server.reconciled,
    )
  go_live(Server(..server, state:), parts, [], True, True)
}

fn freeze(
  server: Server,
  move: MoveId,
  version: Int,
  epoch: Epoch,
  reply: Subject(Result(BitArray, String)),
) -> Server {
  let server = drain_joins(end_replay_wait(server))
  let server = case server.moves {
    [] -> server
    _ -> ack_moves(server)
  }
  let state = server.state
  let #(voice, _) = guild_handler.handoff(guild_state.voice(state))
  let table = guild_state.members_table(state)
  let #(rows, staged) = case server.copy {
    Some(Copy(move: copying, round:))
      if copying == move && version >= guild_codec.precopy_from
    -> {
      let found = members.take_changes(table)
      #(
        found.upserted,
        Some(guild_codec.Staged(move:, round:, removed: found.removed)),
      )
    }
    _ -> #(members.to_list(table), None)
  }
  let server = stop_copy(server)
  let tail =
    guild_codec.Tail(
      loaded: Loaded(data: guild_state.guild_data(state), members: rows),
      sessions: guild_sessions.session_refs(guild_state.sessions(state)),
      threads: store.export_handoff(guild_state.threads(state)),
      voice:,
      table: Some(guild_state.sessions(state)),
      staged:,
    )
  case
    handoff_codec.encode(guild_codec.tail(), tail, version, epoch.to_int(epoch))
  {
    Ok(bytes) -> {
      flush_broadcast(state)
      process.send(reply, Ok(bytes))
      guild_metrics.enter(guild_metrics.Frozen)
      Server(
        ..server,
        mode: Frozen(move:, version:, epoch: epoch.to_int(epoch), held: []),
      )
    }
    Error(_) -> {
      process.send(reply, Error("guild tail could not be encoded"))
      server
    }
  }
}

fn flush_broadcast(state: GuildState) -> Nil {
  case guild_sessions.broadcaster(guild_state.sessions(state)) {
    Some(found) -> broadcast.flush(found)
    None -> relay.settle(1000)
  }
}

fn crossing(message: Message) -> Result(guild_codec.Inbound, Nil) {
  case message {
    Public(request) -> Ok(guild_codec.Public(request))
    Private(control) -> Ok(guild_codec.Private(control))
    Voice(request) -> Ok(guild_codec.VoiceOp(request))
    _ -> Error(Nil)
  }
}

fn forward(move: MoveId, version: Int, epoch: Int, message: Message) -> Nil {
  case crossing(message) {
    Error(Nil) -> Nil
    Ok(portable) ->
      case handoff_codec.encode(guild_codec.inbox(), portable, version, epoch) {
        Ok(bytes) -> handoff_move.forward(move, bytes)
        Error(_) -> {
          metrics.counter("fluxer_gateway_handoff_forward_dropped_total", [], 1)
          stub_dropped()
        }
      }
  }
}

fn stub_hit() -> Nil {
  rollout_metrics.count(
    rollout_metrics.stub_hits,
    [#("kind", "guild"), #("outcome", "forwarded")],
    1,
  )
}

fn stub_dropped() -> Nil {
  rollout_metrics.count(
    rollout_metrics.stub_hits,
    [#("kind", "guild"), #("outcome", "dropped")],
    1,
  )
}

fn hold_message(server: Server, message: Message) -> Server {
  case server.mode {
    Frozen(move:, version:, epoch:, held:) -> {
      forward(move, version, epoch, message)
      Server(
        ..server,
        mode: Frozen(move:, version:, epoch:, held: [message, ..held]),
      )
    }
    _ -> server
  }
}

fn commit(
  server: Server,
  move: MoveId,
  version: Int,
  epoch: Int,
  target: Handle,
) -> Server {
  let sessions = guild_state.sessions(server.state)
  let refs =
    list.append(
      guild_sessions.session_refs(sessions),
      list.map(
        visitor_table.visitors(guild_sessions.visitors(sessions)),
        fn(visitor) { visitor.ref },
      ),
    )
  list.each(refs, control.send(
    _,
    control.GuildRebind(target.ref, process.self()),
  ))
  guild_metrics.enter(guild_metrics.Handing)
  let server =
    Server(
      ..server,
      mode: Handing(
        move:,
        version:,
        epoch:,
        target:,
        waiting: refs
          |> list.map(fn(ref) { session.session_id_to_string(ref.id) })
          |> set.from_list,
        sealed: False,
      ),
    )
  case refs {
    [] -> seal(server)
    _ -> {
      process.send_after(
        signal.subject(process.self()),
        durations.guild().server_seal_wait_ms,
        signal.SealDue,
      )
      server
    }
  }
}

fn seal(server: Server) -> Server {
  case server.mode {
    Handing(move:, ..) as mode -> {
      handoff_move.repointed(move)
      process.send_after(
        signal.subject(process.self()),
        durations.guild().server_forward_grace_ms,
        signal.ForwardGraceOver,
      )
      Server(..server, mode: Handing(..mode, sealed: True))
    }
    _ -> server
  }
}

fn pass_on(server: Server, message: Message) -> Server {
  case server.mode, message {
    Handing(sealed: False, move:, version:, epoch:, ..), _ -> {
      forward(move, version, epoch, message)
      server
    }
    Handing(sealed: True, ..), Public(core_guild.MemberPresence(..))
    | Handing(sealed: True, ..), Public(core_guild.MemberUserUpdate(..))
    -> server
    Handing(sealed: True, target:, ..), Public(request) -> {
      stub_hit()
      process.send(target.ref.inbox, request)
      server
    }
    Handing(sealed: True, target:, ..), Private(found) -> {
      stub_hit()
      process.send(target.control, found)
      server
    }
    Handing(sealed: True, target:, ..), Voice(request) -> {
      stub_hit()
      process.send(target.control, inbox.VoiceRpc(request))
      server
    }
    _, _ -> server
  }
}

fn drain_releases(server: Server) -> Step {
  case server.mode {
    Arriving(applied:, early:, stash:) ->
      case dict.get(early, applied + 1) {
        Error(Nil) -> Continue(server)
        Ok(bytes) -> {
          let server =
            Server(
              ..server,
              mode: Arriving(
                applied: applied + 1,
                early: dict.delete(early, applied + 1),
                stash:,
              ),
            )
          case released(bytes) {
            Ok(message) ->
              case run(server, message) {
                Continue(server) -> drain_releases(server)
                other -> other
              }
            Error(Nil) -> {
              metrics.counter(
                "fluxer_gateway_handoff_forward_dropped_total",
                [],
                1,
              )
              stub_dropped()
              drain_releases(server)
            }
          }
        }
      }
    _ -> Continue(server)
  }
}

fn released(bytes: BitArray) -> Result(Message, Nil) {
  case handoff_codec.decode(guild_codec.inbox(), bytes) {
    Ok(#(_, guild_codec.Public(request))) -> Ok(Public(request))
    Ok(#(_, guild_codec.Private(found))) -> Ok(Private(found))
    Ok(#(_, guild_codec.VoiceOp(request))) -> Ok(Voice(request))
    Error(_) -> Error(Nil)
  }
}

fn open(server: Server) -> Server {
  let state = server.state
  let sessions = guild_state.sessions(state)
  case
    guild_sessions.interested(sessions)
    |> list.sort(int.compare)
  {
    [] -> server
    users -> {
      let packed = packed_ids.from_list(users)
      presence_client.register_guild_interest(
        guild_state.guild_id(state),
        packed,
        guild_state.self_ref(state).inbox,
      )
      let #(state, changes) =
        presence_client.lookup_many(packed)
        |> list.fold(#(state, []), fn(acc, found) {
          let #(state, changes) = acc
          let #(state, more) = rows.apply(state, found, rows.Deferred)
          #(state, list.append(more, changes))
        })
      with_state(server, rows.apply_engagement(state, changes))
    }
  }
}

type Step {
  Continue(Server)
  Stop(Server)
  Crash(Server, String)
}

fn publish(server: Server) -> Server {
  let version = guild_state.version(server.state)
  case version == server.published {
    True -> server
    False -> {
      maintenance.publish_view(server.state, server.handle)
      Server(..server, published: version)
    }
  }
}

fn route(server: Server, message: Message) -> Step {
  case message {
    Public(request) -> Continue(public(server, request))
    Private(control) -> private(server, control)
    Voice(request) -> Continue(voice_request(server, request))
    Push(request) -> Continue(push_message(server, request))
    Threads(request) ->
      Continue(with_state(server, guild_threads.handle(server.state, request)))
    Reconciled(mismatches) -> {
      let #(state, changes) = reconcile.apply(server.state, mismatches)
      Continue(with_state(server, rows.apply_engagement(state, changes)))
    }
    Monitor(process.ProcessDown(monitor:, ..)) ->
      Continue(process_down(server, monitor))
    Monitor(_) -> Continue(server)
    Tick(timer) -> tick(server, timer)
    Moving(_) -> Continue(server)
    Copying(_) -> Continue(server)
  }
}

fn process_down(server: Server, monitor: process.Monitor) -> Server {
  let owner = case server.frozen {
    Some(frozen) -> hold.owner_down(frozen, monitor)
    None -> hold.NotOwner
  }
  case owner {
    hold.NotOwner ->
      maybe_auto_stop(with_state(server, connect.on_down(server.state, monitor)))
    hold.StillHeld(frozen) -> Server(..server, frozen: Some(frozen))
    hold.Abandoned(frozen) -> thaw(server, frozen)
  }
}

fn thaw(server: Server, frozen: Frozen) -> Server {
  guild_metrics.enter(guild_metrics.Serving)
  list.fold(
    hold.release(frozen),
    remember_voice(Server(..server, frozen: None)),
    run_voice_request,
  )
}

fn with_state(server: Server, state: GuildState) -> Server {
  Server(..server, state:)
}

fn public(server: Server, request: GuildRequest) -> Server {
  case request {
    core_guild.Join(join, reply) -> offer(server, join, reply)
    core_guild.Leave(session_id, pid) ->
      Server(
        ..with_state(server, connect.leave_owned(server.state, session_id, pid)),
        gate: admission.withdraw(server.gate, session_id, pid),
      )
      |> maybe_auto_stop
    core_guild.SessionOp(ref, op) -> session_op(server, ref, op)
    core_guild.MemberPresence(found) -> presence_update(server, found)
    core_guild.MemberUserUpdate(user_wire) ->
      member_user_update(server, user_wire)
    core_guild.SessionPushHold(session_id, hold) ->
      push_hold(server, session_id, hold)
    core_guild.UpdateDmPartners(session_id, partners) ->
      with_state(server, dm_partners.update(server.state, session_id, partners))
    core_guild.DropMemberLists(session_id) -> {
      case
        member_list.unsubscribe(
          guild_state.member_list(server.state),
          session_id,
        )
      {
        Ok(dispatches) -> send.list_dispatches(server.state, dispatches)
        Error(_) -> Nil
      }
      server
    }
    core_guild.SessionMoved(to, from) -> session_moved(server, to, from)
    core_guild.CountsQuery(user, reply) -> {
      let #(state, online_count) = online.mutual_online(server.state, user)
      process.send(
        reply,
        core_guild.GuildCounts(
          guild: guild_state.guild_id(state),
          member_count: guild_state.member_count(state),
          online_count:,
        ),
      )
      with_state(server, state)
    }
  }
}

fn session_moved(server: Server, to: SessionRef, from: SessionRef) -> Server {
  let #(state, kept) = connect.moved(server.state, to, from)
  case server.moves {
    [] -> process.send(server.timers, AckMoves)
    _ -> Nil
  }
  Server(..with_state(server, state), moves: [#(from, kept), ..server.moves])
}

fn ack_moves(server: Server) -> Server {
  flush_broadcast(server.state)
  let guild = guild_state.guild_id(server.state)
  list.each(list.reverse(server.moves), fn(pair) {
    relay.send([pair.0], session.GuildRebound(guild, pair.1))
  })
  Server(..server, moves: [])
}

fn offer(
  server: Server,
  request: JoinRequest,
  reply: Subject(JoinReply),
) -> Server {
  let #(gate, effects) =
    admission.offer(server.gate, request, reply, clock.monotonic_ms())
  run_join_effects(effects)
  let server = Server(..server, gate:)
  case server.drain_scheduled {
    True -> server
    False -> {
      process.send(server.timers, DrainJoins)
      Server(..server, drain_scheduled: True)
    }
  }
}

fn run_join_effects(effects: List(admission.Effect)) -> Nil {
  list.each(effects, fn(effect) {
    case effect {
      admission.Reply(to, reply) -> process.send(to, reply)
      admission.Dropped(_, admission.Expired) ->
        metrics.counter("gateway_guild_joins_expired_total", [], 1)
      admission.Dropped(_, admission.Stale) ->
        metrics.counter("gateway_guild_joins_stale_total", [], 1)
    }
  })
}

fn drain_joins(server: Server) -> Server {
  let now = clock.monotonic_ms()
  let #(gate, batch, effects) = admission.next_batch(server.gate, now)
  run_join_effects(effects)
  case batch {
    None -> settle_auto_stop(Server(..server, gate:, drain_scheduled: False))
    Some(found) -> {
      let #(state, gate) =
        list.fold(found.members, #(server.state, gate), fn(acc, waiting) {
          let #(state, gate) = acc
          let now = clock.monotonic_ms()
          case admission.deliverable(waiting, now) {
            True -> {
              guild_metrics.join_admitted(
                waiting.request.class,
                now - waiting.enqueued_at,
              )
              #(connect.join(state, waiting.request, waiting.reply), gate)
            }
            False -> {
              guild_metrics.join_expired(now - waiting.enqueued_at)
              let #(gate, effects) = admission.expire(gate, waiting, now)
              run_join_effects(effects)
              #(state, gate)
            }
          }
        })
      let finished = clock.monotonic_ms()
      guild_metrics.join_batch(finished - found.started_at)
      let gate = admission.batch_done(gate, found, finished)
      drain_joins(Server(..server, state:, gate:))
    }
  }
}

fn session_op(server: Server, ref: SessionRef, op: session.ClientOp) -> Server {
  case op {
    session.VoiceStateUpdate(_) | session.VoiceSignal(_) ->
      case guild_handler.decode_op(ref, op) {
        Ok(Some(request)) -> voice_request(server, request)
        Ok(None) -> server
        Error(reason) -> {
          send_error(server.state, ref.id, reason)
          server
        }
      }
    session.LazyRequest(_) -> {
      let started = clock.monotonic_ms()
      let state = subscriptions.handle(server.state, ref, op)
      metrics.observe_ms(
        "fluxer_gateway_guild_lazy_request_ms",
        [],
        clock.monotonic_ms() - started,
      )
      with_state(server, state)
    }
    _ -> with_state(server, requests.handle(server.state, ref, op))
  }
}

fn member_user_update(server: Server, user_wire: BitArray) -> Server {
  case
    user_data.refresh_user(
      guild_state.members_table(server.state),
      guild_state.guild_id(server.state),
      user_wire,
    )
  {
    Some(payload) -> dispatch_event(server, "GUILD_MEMBER_UPDATE", payload)
    None -> server
  }
}

fn presence_update(server: Server, found: Presence) -> Server {
  let #(state, changes) = rows.on_presence(server.state, found)
  with_state(server, rows.apply_engagement(state, changes))
}

fn push_hold(server: Server, session_id: SessionId, hold: Bool) -> Server {
  case guild_sessions.get(guild_state.sessions(server.state), session_id) {
    Some(_) -> {
      let push =
        engaged.set_session_hold(
          guild_state.push(server.state),
          session_id,
          hold,
        )
      with_state(server, guild_state.set_push(server.state, push))
    }
    None -> server
  }
}

fn push_message(server: Server, request: push_dispatch.PushGuildMsg) -> Server {
  let state = server.state
  with_state(
    server,
    guild_state.set_push(
      state,
      push_dispatch.on_permit(
        guild_state.push(state),
        guild_state.member_list(state),
        fn(session_id) {
          option.is_some(guild_sessions.get(
            guild_state.sessions(state),
            session_id,
          ))
        },
        request,
      ),
    ),
  )
}

fn voice_request(server: Server, request: VoiceRequest) -> Server {
  case server.frozen, guild_handler.is_read_only(request) {
    Some(frozen), False ->
      Server(..server, frozen: Some(hold.hold_voice(frozen, request)))
    _, _ -> run_voice_request(server, request)
  }
}

fn run_voice_request(server: Server, request: VoiceRequest) -> Server {
  use voice, context <- step_voice(server)
  guild_handler.handle(voice, context, request)
}

fn hand_over_voice(frozen: Frozen) -> Nil {
  case list.filter(hold.release(frozen), guild_handler.follows_handoff) {
    [] -> Nil
    requests ->
      case
        guild_handler.await_successor(
          hold.successor(frozen),
          successor_timeout_ms(),
        )
      {
        Ok(target) -> list.each(requests, process.send(target, _))
        Error(Nil) -> Nil
      }
  }
}

fn step_voice(
  server: Server,
  step: fn(GuildVoice, VoiceContext) -> #(GuildVoice, List(VoiceEffect)),
) -> Server {
  let context = guild_state.voice_context(server.state, clock.system_ms())
  let #(voice, effects) = step(guild_state.voice(server.state), context)
  with_state(server, guild_state.set_voice(server.state, voice))
  |> remember_voice
  |> run_voice(effects)
}

fn remember_voice(server: Server) -> Server {
  let connections =
    remains.connections_of(
      guild_handler.issued_states(guild_state.voice(server.state)),
    )
  Server(
    ..server,
    remains: remains.with_connections(
      guild_state.guild_id(server.state),
      server.remains,
      connections,
      frozen: option.is_some(server.frozen),
    ),
  )
}

fn run_voice(server: Server, effects: List(VoiceEffect)) -> Server {
  list.fold(effects, server, fn(acc, effect) {
    case effect {
      guild_handler.Broadcast(event, payload, channel) -> {
        dispatch.broadcast(acc.state, event, payload, channel)
        acc
      }
      guild_handler.BroadcastMove(event, payload, left, from, to) -> {
        dispatch.broadcast_move(acc.state, event, payload, left, from, to)
        acc
      }
      guild_handler.SendToSession(session_id, event, payload) -> {
        send_to_session(acc.state, session_id, event, payload)
        acc
      }
      guild_handler.SendError(session_id, reason) -> {
        send_error(acc.state, session_id, reason)
        acc
      }
      guild_handler.VirtualAccessSet(user, channels) ->
        with_state(
          acc,
          model.set_virtual_access(acc.state, user, channels)
            |> dm_partners.reevaluate_user(user),
        )
    }
  })
}

fn send_to_session(
  state: GuildState,
  session_id: SessionId,
  event: String,
  payload: RawJson,
) -> Nil {
  case guild_sessions.get(guild_state.sessions(state), session_id) {
    Some(found) ->
      session.send(
        entry.ref(found),
        session.guild_dispatch(guild_state.guild_id(state), event, payload),
      )
    None -> Nil
  }
}

fn send_error(
  state: GuildState,
  session_id: SessionId,
  reason: GatewayError,
) -> Nil {
  case
    p2p.reports_rejection(reason),
    guild_sessions.get(guild_state.sessions(state), session_id)
  {
    True, Some(found) ->
      control.send(entry.ref(found), control.VoiceRejected(reason))
    _, _ -> Nil
  }
}

fn private(server: Server, control: Control) -> Step {
  case control {
    inbox.Dispatch(event, payload) ->
      Continue(incoming(server, [#(event, payload)]))
    inbox.DispatchMany(events) -> Continue(incoming(server, events))
    inbox.Replay(events) ->
      list.fold(events, server, fn(acc, pair) {
        apply_event(acc, pair.0, pair.1, dispatch.replay_event)
      })
      |> end_replay_wait
      |> Continue
    inbox.Ask(asked, deadline_ms, reply) ->
      case clock.monotonic_ms() > deadline_ms {
        True -> Continue(server)
        False -> {
          let #(server, answer) = ask(server, asked)
          process.send(reply, answer)
          Continue(server)
        }
      }
    inbox.AskThreadBits(user, thread, deadline_ms, reply) ->
      case clock.monotonic_ms() > deadline_ms {
        True -> Continue(server)
        False -> {
          process.send(reply, thread_bits(server, user, thread))
          Continue(server)
        }
      }
    inbox.Reload(loaded, reply) -> {
      let step = reload(server, loaded)
      process.send(reply, Nil)
      step
    }
    inbox.Shutdown(reply) -> {
      process.send(reply, Nil)
      guild_metrics.stopped("shutdown")
      Stop(server)
    }
    inbox.Freeze(owners, reply) -> {
      let server = end_replay_wait(server)
      let #(moved, successor) = export_state(server.state)
      let token = server.next_token
      process.send_after(
        server.timers,
        transfer.freeze_deadline_ms(),
        FreezeDeadline(token),
      )
      let frozen = hold.freeze(server.frozen, owners, successor, token)
      guild_metrics.enter(guild_metrics.Frozen)
      let server =
        remember_voice(
          Server(..server, frozen: Some(frozen), next_token: token + 1),
        )
      process.send(reply, Ok(moved))
      Continue(server)
    }
    inbox.Unfreeze ->
      case server.frozen {
        Some(frozen) -> Continue(thaw(server, frozen))
        None -> Continue(server)
      }
    inbox.Moved(reply) -> {
      let events = case server.frozen {
        Some(frozen) -> {
          hand_over_voice(frozen)
          hold.events(frozen)
        }
        None -> []
      }
      process.send(reply, events)
      let guild = guild_state.guild_id(server.state)
      guild_state.sessions(server.state)
      |> guild_sessions.session_refs
      |> list.each(fn(ref) {
        session.send(
          ref,
          session.GuildDown(guild, int.random(down_jitter_ms())),
        )
      })
      guild_metrics.stopped("moved")
      Stop(server)
    }
    inbox.ResyncSessions(reply) -> {
      let #(state, count) = availability.resync_all(server.state)
      process.send(reply, count)
      Continue(with_state(server, state))
    }
    inbox.Disown -> {
      let guild = guild_state.guild_id(server.state)
      guild_state.sessions(server.state)
      |> guild_sessions.session_refs
      |> list.each(fn(ref) {
        control.guild_lost(ref, guild, int.random(down_jitter_ms()))
      })
      guild_metrics.stopped("unowned")
      Stop(server)
    }
    inbox.VoiceRpc(request) -> Continue(voice_request(server, request))
    inbox.HealthProbe(token, pong) -> {
      pong(token, clock.monotonic_ms())
      Continue(server)
    }
    inbox.FlushMemberList ->
      Continue(with_state(server, rows.flush_list(server.state)))
    inbox.Describe(reply) -> {
      process.send(reply, describe(server.state))
      Continue(server)
    }
  }
}

fn incoming(server: Server, events: List(#(String, Dynamic))) -> Server {
  case server.awaiting_replay {
    Some(queued) ->
      Server(
        ..server,
        awaiting_replay: Some(list.append(list.reverse(events), queued)),
      )
    None ->
      list.fold(events, server, fn(acc, pair) {
        dispatch_event(acc, pair.0, pair.1)
      })
  }
}

fn end_replay_wait(server: Server) -> Server {
  case server.awaiting_replay {
    Some(queued) ->
      list.fold(
        list.reverse(queued),
        Server(..server, awaiting_replay: None),
        fn(acc, pair) { dispatch_event(acc, pair.0, pair.1) },
      )
    None -> server
  }
}

fn describe(state: GuildState) -> memory_stats.GuildDescription {
  let guild_data = guild_state.guild_data(state)
  let sessions = guild_state.sessions(state)
  memory_stats.GuildDescription(
    name: string_property(guild_data, "name") |> option.unwrap("Unknown"),
    icon: string_property(guild_data, "icon"),
    member_count: guild_state.member_count(state),
    session_count: guild_sessions.count(sessions),
    presence_count: guild_sessions.presence_count(sessions),
  )
}

fn string_property(guild_data: data.GuildData, key: String) -> Option(String) {
  data.property(guild_data, key)
  |> option.then(fn(raw) {
    raw_json.read(raw, decode.string) |> option.from_result
  })
}

fn export_state(
  state: GuildState,
) -> #(transfer.Transfer, guild_handler.Successor) {
  let #(voice, successor) = guild_handler.handoff(guild_state.voice(state))
  #(
    transfer.Transfer(
      loaded: Loaded(
        data: guild_state.guild_data(state),
        members: members.to_list(guild_state.members_table(state)),
      ),
      sessions: guild_sessions.session_refs(guild_state.sessions(state)),
      threads: store.export_handoff(guild_state.threads(state)),
      voice:,
    ),
    successor,
  )
}

fn ask(server: Server, asked: inbox.Query) -> #(Server, inbox.QueryReply) {
  case asked {
    inbox.EvictVisitors(keys) -> {
      let #(users, state) = visitors.evict_ips(server.state, keys)
      #(
        with_state(server, state),
        Ok(
          json.object([
            #("user_ids", json.array(users, id.json)),
          ]),
        ),
      )
    }
    inbox.MentionSources(page) -> {
      let state = server.state
      let connected =
        guild_sessions.connected_list(guild_state.sessions(state))
        |> set.from_list
      #(
        server,
        Ok(
          search.page(
            guild_state.members_table(state),
            page,
            connected,
            guild_threads.mention_universe(state, page.channel),
            fn(user, target) { guild_threads.can_view(state, user, target) },
          )
          |> search.to_json,
        ),
      )
    }
    _ -> #(server, answer_from_view(server, asked))
  }
}

fn thread_bits(
  server: Server,
  user: UserId,
  thread: ChannelId,
) -> Result(Int, inbox.QueryError) {
  case guild_threads.permissions(server.state, user, thread) {
    Some(bits) -> Ok(bits)
    None ->
      query.thread_bits(
        maintenance.view(server.state, server.handle),
        user,
        thread,
      )
  }
}

fn answer_from_view(server: Server, asked: inbox.Query) -> inbox.QueryReply {
  query.answer(maintenance.view(server.state, server.handle), asked)
}

fn dispatch_event(server: Server, event: String, payload: Dynamic) -> Server {
  let server =
    Server(
      ..server,
      catch_up: catch_up.record(
        server.catch_up,
        event,
        payload,
        clock.monotonic_ms(),
      ),
    )
  apply_event(server, event, payload, dispatch.handle_event)
}

fn catch_up_joined(server: Server) -> Server {
  case catch_up.is_waiting(server.catch_up) {
    False -> server
    True -> {
      let now = clock.monotonic_ms()
      let current = catch_up.expire(server.catch_up, now)
      let sessions = guild_state.sessions(server.state)
      catch_up.waiting(current)
      |> list.filter(fn(session) {
        option.is_some(guild_sessions.get(sessions, session))
      })
      |> list.fold(Server(..server, catch_up: current), fn(acc, session) {
        let taken = catch_up.take(acc.catch_up, session, now)
        case taken.complete {
          True -> Nil
          False ->
            logging.log(
              logging.Warning,
              "guild handoff catch-up overflowed for a rejoining session",
            )
        }
        let state = dispatch.catch_up(acc.state, session, taken.events)
        Server(..acc, state:, catch_up: taken.catch_up)
      })
    }
  }
}

fn apply_event(
  server: Server,
  event: String,
  payload: Dynamic,
  handle: fn(GuildState, String, Dynamic) -> GuildState,
) -> Server {
  let server = case server.frozen {
    Some(frozen) ->
      Server(..server, frozen: Some(hold.record_event(frozen, event, payload)))
    None -> server
  }
  let old_facts = guild_state.facts(server.state)
  let role_users_before = changed_role_users(server.state, event, payload)
  let state =
    handle(server.state, event, payload)
    |> maintenance.prune_member_subscriptions(event, payload)
  let state = case event {
    "GUILD_UPDATE" -> availability.transition(old_facts, state)
    "GUILD_MEMBER_ADD" | "GUILD_MEMBER_REMOVE" ->
      maintenance.update_counts(state)
    _ -> state
  }
  let server =
    with_state(server, state)
    |> voice_after_event(event, payload, role_users_before)
    |> voice_after_availability(old_facts)
  case event {
    "GUILD_UPDATE" -> {
      schedule_recheck(server.state, server.timers)
      server
    }
    _ -> server
  }
}

fn voice_after_availability(server: Server, old_facts: GuildFacts) -> Server {
  case availability.voice_keep(old_facts, server.state) {
    Some(keep) -> {
      use voice, context <- step_voice(server)
      guild_handler.on_unavailable(voice, context, keep)
    }
    None -> server
  }
}

fn voice_after_event(
  server: Server,
  event: String,
  payload: Dynamic,
  role_users_before: Option(RoleUsers),
) -> Server {
  case event {
    "GUILD_MEMBER_UPDATE" | "GUILD_MEMBER_REMOVE" ->
      case data_decode.user_id_of(payload, ["user", "id"]) {
        Ok(user) -> {
          use voice, context <- step_voice(server)
          guild_handler.on_member_change(voice, context, user)
        }
        Error(Nil) -> server
      }
    _ ->
      case voice_scope(server.state, event, payload, role_users_before) {
        Some(scope) -> {
          use voice, context <- step_voice(server)
          case scope {
            VoiceAll -> guild_handler.on_permissions_change(voice, context)
            VoiceChannels(channels) ->
              guild_handler.on_channels_change(voice, context, channels)
            VoiceUsers(users) ->
              guild_handler.on_users_change(voice, context, users)
            VoiceChannelDeleted(channel) ->
              guild_handler.on_channel_delete(voice, context, channel)
          }
        }
        None -> server
      }
  }
}

type VoiceScope {
  VoiceAll
  VoiceChannels(List(ChannelId))
  VoiceUsers(List(UserId))
  VoiceChannelDeleted(ChannelId)
}

type RoleUsers {
  EveryoneRole
  RoleMembers(List(RoleId), List(Int))
}

fn changed_role_users(
  state: GuildState,
  event: String,
  payload: Dynamic,
) -> Option(RoleUsers) {
  let roles = case event {
    "GUILD_ROLE_UPDATE" -> Some(snowflake_at(payload, ["role", "id"]))
    "GUILD_ROLE_UPDATE_BULK" -> Some(snowflakes_in(payload, "roles"))
    "GUILD_ROLE_DELETE" -> Some(snowflake_at(payload, ["role_id"]))
    _ -> None
  }
  use role_ints <- option.map(roles)
  let guild_int = id.guild_id_to_int(guild_state.guild_id(state))
  case list.contains(role_ints, guild_int) {
    True -> EveryoneRole
    False -> {
      let role_ids =
        list.filter_map(role_ints, fn(role) {
          id.role_id(role) |> result.replace_error(Nil)
        })
      RoleMembers(
        role_ids,
        members.with_any_role(guild_state.members_table(state), role_ids),
      )
    }
  }
}

fn voice_scope(
  state: GuildState,
  event: String,
  payload: Dynamic,
  role_users_before: Option(RoleUsers),
) -> Option(VoiceScope) {
  case event, role_users_before {
    "CHANNEL_UPDATE", _ ->
      Some(VoiceChannels(channel_ids(snowflake_at(payload, ["id"]))))
    "CHANNEL_UPDATE_BULK", _ ->
      Some(VoiceChannels(channel_ids(snowflakes_in(payload, "channels"))))
    "CHANNEL_DELETE", _ ->
      case channel_ids(snowflake_at(payload, ["id"])) {
        [channel] -> Some(VoiceChannelDeleted(channel))
        _ -> None
      }
    _, Some(EveryoneRole) -> Some(VoiceAll)
    _, Some(RoleMembers(role_ids, before)) ->
      members.with_any_role(guild_state.members_table(state), role_ids)
      |> list.append(before)
      |> list.unique
      |> list.filter_map(fn(user) {
        id.user_id(user) |> result.replace_error(Nil)
      })
      |> VoiceUsers
      |> Some
    _, None -> None
  }
}

fn channel_ids(values: List(Int)) -> List(ChannelId) {
  list.filter_map(values, fn(value) {
    id.channel_id(value) |> result.replace_error(Nil)
  })
}

fn snowflake_at(payload: Dynamic, path: List(String)) -> List(Int) {
  decode.run(payload, decode.at(path, id_decode.snowflake()))
  |> result.map(list.wrap)
  |> result.unwrap([])
}

fn snowflakes_in(payload: Dynamic, key: String) -> List(Int) {
  decode.run(payload, decode.at([key], decode.list(decode.dynamic)))
  |> result.map(list.flat_map(_, snowflake_at(_, ["id"])))
  |> result.unwrap([])
}

fn reload(server: Server, loaded: Loaded) -> Step {
  let state = server.state
  let old_facts = guild_state.facts(state)
  let old_data = guild_state.guild_data(state)
  let table = guild_state.members_table(state)
  let old_members = members.sorted_user_ints(table)
  members.replace_all(table, loaded.members)
  let current = set.from_list(members.sorted_user_ints(table))
  let state =
    guild_sessions.entries(guild_state.sessions(state))
    |> list.map(entry.user_int)
    |> list.unique
    |> list.filter(fn(user) { !set.contains(current, user) })
    |> list.fold(state, connect.remove_user)
  let gone = list.filter(old_members, fn(user) { !set.contains(current, user) })
  let state =
    guild_state.set_sessions(
      state,
      list.fold(gone, guild_state.sessions(state), guild_sessions.delete_row),
    )
    |> session_presence.unwatch(gone)
  let next_data = data.set_member_count(loaded.data, members.count(table))
  let state =
    guild_state.set_guild_data(state, next_data)
    |> guild_state.set_push(init.push_info(guild_state.push(state), next_data))
  let snapshot =
    init.reload_snapshot(
      next_data,
      table,
      guild_handler.virtual_grants(guild_state.voice(state)),
    )
  let #(fresh_perms, fresh_reader) =
    perms.new(guild_state.guild_id(state), process.self())
  case perms.load(fresh_perms, snapshot) {
    Error(_) -> {
      perms.release(fresh_perms)
      Crash(with_state(server, state), "guild_reload_perms_failed")
    }
    Ok(_) ->
      case
        member_list.rebuild(
          guild_state.member_list(state),
          fresh_perms,
          init.list_snapshot(loaded, guild_state.sessions(state)),
          rows.lookup(state),
        )
      {
        Error(_) -> {
          perms.release(fresh_perms)
          Crash(with_state(server, state), "guild_reload_member_list_failed")
        }
        Ok(#(rebuilt, counts, dispatches)) -> {
          let stale_perms = guild_state.perms_handle(state)
          let state =
            guild_state.set_member_list(state, rebuilt, counts)
            |> guild_state.set_perms(fresh_perms, fresh_reader)
          maintenance.publish_view(state, server.handle)
          process.send_after(
            server.timers,
            stale_perms_release_ms(),
            ReleasePerms(stale_perms, clock.monotonic_ms()),
          )
          send.list_dispatches(state, dispatches)
          let state = maintenance.prune_all(state)
          let state = availability.transition(old_facts, state)
          let update = guild_update_payload(state)
          dispatch.broadcast(state, "GUILD_UPDATE", update, None)
          let state =
            visitor_dispatch.after(
              state,
              old_data,
              "GUILD_UPDATE",
              json_tree.from_raw(update),
            )
            |> dm_partners.reevaluate_all
          let server =
            list.filter_map(gone, id.user_id)
            |> list.fold(with_state(server, state), fn(acc, user) {
              use voice, context <- step_voice(acc)
              guild_handler.on_member_change(voice, context, user)
            })
            |> voice_after_availability(old_facts)
          case maintenance.refresh_counts(server.state) {
            Ok(state) -> Continue(with_state(server, state))
            Error(_) -> Crash(server, "guild_members_table_lost")
          }
        }
      }
  }
}

fn guild_update_payload(state: GuildState) -> RawJson {
  let guild = guild_state.guild_id(state)
  data.properties_json(guild_state.guild_data(state), [
    #("guild_id", id.guild_json(guild)),
  ])
  |> raw_json.from_json
}

fn tick(server: Server, timer: Timer) -> Step {
  case timer {
    DrainJoins -> Continue(drain_joins(server))
    AckMoves -> Continue(ack_moves(server))
    ReleasePerms(stale:, since_ms:) -> {
      case push_busy(server.state, since_ms) {
        True -> {
          process.send_after(
            server.timers,
            stale_perms_release_ms(),
            ReleasePerms(stale:, since_ms:),
          )
          Nil
        }
        False -> perms.release(stale)
      }
      Continue(server)
    }
    PassiveSync -> {
      process.send_after(server.timers, passive_sync.interval_ms(), PassiveSync)
      Continue(with_state(server, passive_sync.tick(server.state)))
    }
    Reconcile -> {
      process.send_after(server.timers, reconcile.interval_ms(), Reconcile)
      let reconciled = server.reconciled
      reconcile.start(server.state, fn(mismatches) {
        process.send(reconciled, mismatches)
      })
      Continue(server)
    }
    CountsRefresh -> {
      process.send_after(
        server.timers,
        maintenance.count_refresh_interval_ms(),
        CountsRefresh,
      )
      case maintenance.refresh_counts(server.state) {
        Ok(state) -> Continue(with_state(server, state))
        Error(_) -> Crash(server, "guild_members_table_lost")
      }
    }
    AvailabilityRecheck -> {
      let state = availability.recheck(server.state)
      schedule_recheck(state, server.timers)
      Continue(with_state(server, state))
    }
    AutoStop(token) ->
      case
        server.auto_stop == Some(token),
        is_empty(server.state) && !server.drain_scheduled
      {
        False, _ -> Continue(server)
        True, True ->
          case
            mailbox_empty() && !push_busy(server.state, clock.monotonic_ms())
          {
            True -> {
              guild_metrics.stopped("idle")
              Stop(server)
            }
            False ->
              Continue(maybe_auto_stop(Server(..server, auto_stop: None)))
          }
        True, False -> Continue(Server(..server, auto_stop: None))
      }
    ReplayWindowClosed -> Continue(end_replay_wait(server))
    FreezeDeadline(token) ->
      case server.frozen {
        Some(frozen) ->
          case hold.token(frozen) == token {
            True -> Continue(thaw(server, frozen))
            False -> Continue(server)
          }
        None -> Continue(server)
      }
  }
}

type QueueItem {
  MessageQueueLen
}

@external(erlang, "erlang", "process_info")
fn queue_info(pid: process.Pid, item: QueueItem) -> #(QueueItem, Int)

fn mailbox_empty() -> Bool {
  queue_info(process.self(), MessageQueueLen).1 == 0
}

fn push_busy(state: GuildState, admitted_by_ms: Int) -> Bool {
  push_dispatch.busy(guild_state.push(state), admitted_by: admitted_by_ms)
}

fn occupancy(state: GuildState) -> Int {
  guild_sessions.count(guild_state.sessions(state))
  + visitors.count(state)
  + guild_handler.issued_count(guild_state.voice(state))
}

fn is_empty(state: GuildState) -> Bool {
  occupancy(state) == 0
}

fn after_route(server: Server, occupied: Int) -> Server {
  case occupied > 0 && is_empty(server.state) {
    True -> maybe_auto_stop(server)
    False -> server
  }
}

fn settle_auto_stop(server: Server) -> Server {
  case is_empty(server.state) {
    True -> maybe_auto_stop(server)
    False -> Server(..server, auto_stop: None)
  }
}

fn maybe_auto_stop(server: Server) -> Server {
  case is_empty(server.state), server.auto_stop {
    True, None -> {
      let token = server.next_token
      process.send_after(server.timers, auto_stop_grace_ms(), AutoStop(token))
      Server(..server, auto_stop: Some(token), next_token: token + 1)
    }
    _, _ -> server
  }
}
