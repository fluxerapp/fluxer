// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/message.{type MoveId}
import fluxer_gateway/api/client
import fluxer_gateway/cluster/remote as cluster_remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/config/rollout
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type Unavailable, Unavailable}
import fluxer_gateway/core/guild.{
  type JoinReply, type JoinRequest, JoinNotEligible, JoinRequest,
  JoinUnavailable, NotMember,
} as core_guild
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/gateway/start_gate
import fluxer_gateway/guild/counts_cache/shard as counts_shard
import fluxer_gateway/guild/data.{type Loaded}
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/handoff/roster
import fluxer_gateway/guild/handoff/signal
import fluxer_gateway/guild/handoff/transfer.{type Transfer}
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager/fetch
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/guild/names
import fluxer_gateway/guild/read_model
import fluxer_gateway/guild/remains
import fluxer_gateway/guild/server
import fluxer_gateway/guild/sessions.{type Sessions} as guild_sessions
import fluxer_gateway/threads/store.{type Handoff}
import fluxer_gateway/util/spawn
import fluxer_gateway/voice/guild_handler.{type GuildVoice}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/factory_supervisor
import gleam/result
import logging

fn init_timeout_ms() -> Int {
  durations.guild().shard_init_timeout_ms
}

fn api_unavailable_retry_ms() -> Int {
  durations.guild().shard_api_unavailable_retry_ms
}

fn gate_full_retry_ms() -> Int {
  durations.guild().shard_gate_full_retry_ms
}

fn stop_timeout_ms() -> Int {
  durations.guild().shard_stop_timeout_ms
}

fn unavailable_retry_ms() -> Int {
  durations.guild().shard_unavailable_retry_ms
}

fn join_transit_ms() -> Int {
  durations.guild().join_admission_transit_ms
}

fn reload_timeout_ms() -> Int {
  durations.guild().shard_reload_timeout_ms
}

fn retire_timeout_ms() -> Int {
  durations.guild().shard_retire_timeout_ms
}

pub type LookupError {
  NotEligible
  LookupUnavailable(Unavailable)
}

type Waiter {
  WantsHandle(reply: Subject(Result(Handle, LookupError)))
  WantsJoin(request: JoinRequest, reply: Subject(JoinReply), deadline_ms: Int)
  WantsArrival(reply: Subject(Result(Handle, Nil)))
}

pub type ReloadResult {
  Reloaded
  NotRunning
  ReloadFailed
}

pub type Message {
  StartOrLookup(guild: GuildId, reply: Subject(Result(Handle, LookupError)))
  Lookup(guild: GuildId, reply: Subject(Result(Handle, Nil)))
  Join(guild: GuildId, request: JoinRequest, reply: Subject(JoinReply))
  JoinsExpired(guild: GuildId)
  StopGuild(guild: GuildId, reply: Subject(Bool))
  ReloadGuild(guild: GuildId, reply: Subject(ReloadResult))
  StartTransferred(
    guild: GuildId,
    moved: Transfer,
    window_ms: Int,
    reply: Subject(Bool),
  )
  AbortTransfer(guild: GuildId, reply: Subject(Bool))
  TransferExpired(guild: GuildId, reply: Subject(Bool))
  InstallArriving(
    guild: GuildId,
    tail: guild_codec.Tail,
    reply: Subject(Result(Handle, Nil)),
  )
  Started(guild: GuildId, token: Int, result: Result(Handle, StartFailure))
  RunningElsewhere(guild: GuildId, token: Int, handle: Handle)
  LocalGuilds(reply: Subject(List(GuildId)))
  GuildDown(process.Down)
}

pub type StartFailure {
  StartGateFull
  FetchFailed(fetch.FetchError)
  InitFailed
  StartCrashed
}

type Entry {
  Running(handle: Handle, monitor: Monitor, origin: Origin)
  Loading(
    token: Int,
    waiters: List(Waiter),
    worker: process.Pid,
    origin: Origin,
    transfer: Option(PendingTransfer),
    expiry_at: Option(Int),
  )
}

type Origin {
  Fetched
  Arrived
  Transferred(reply: Subject(Bool))
  Abandoned
}

type PendingTransfer {
  PendingTransfer(moved: Transfer, reply: Subject(Bool))
}

type Crashed {
  Crashed(sessions: List(SessionId), at_ms: Int)
}

type State {
  State(
    index: Int,
    self: Subject(Message),
    guilds: Dict(Int, Entry),
    crashed: Dict(Int, Crashed),
    next_token: Int,
  )
}

pub fn factory_name(
  index: Int,
) -> process.Name(factory_supervisor.Message(server.StartArgs, Handle)) {
  names.factory(index)
}

pub fn start(index: Int) -> actor.StartResult(Subject(Message)) {
  actor.new_with_initialiser(init_timeout_ms(), fn(subject) {
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select_monitors(GuildDown)
    actor.initialised(State(
      index:,
      self: subject,
      guilds: adopt_running(index),
      crashed: dict.new(),
      next_token: unique_integer([Positive, Monotonic]),
    ))
    |> actor.selecting(selector)
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.named(names.shard(index))
  |> actor.on_message(handle)
  |> actor.start
}

type UniqueOption {
  Positive
  Monotonic
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

fn adopt_running(index: Int) -> Dict(Int, Entry) {
  registry.entries()
  |> list.filter(fn(entry) { names.shard_of(entry.0) == index })
  |> list.fold(dict.new(), fn(guilds, entry) {
    let #(guild, handle) = entry
    dict.insert(
      guilds,
      id.guild_id_to_int(guild),
      Running(handle, process.monitor(handle.pid), Fetched),
    )
  })
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    StartOrLookup(guild, reply) ->
      actor.continue(start_or_wait(state, guild, WantsHandle(reply)))
    Join(guild, request, reply) ->
      actor.continue(join(state, guild, request, reply))
    JoinsExpired(guild) -> actor.continue(expire_joins(state, guild))
    Lookup(guild, reply) -> actor.continue(lookup(state, guild, reply))
    StopGuild(guild, reply) -> {
      let key = id.guild_id_to_int(guild)
      case dict.get(state.guilds, key) {
        Ok(Running(handle:, monitor:, ..)) -> {
          process.demonitor_process(monitor)
          registry.remove(guild, handle)
          stop_running(guild, handle, reply)
          actor.continue(State(..state, guilds: dict.delete(state.guilds, key)))
        }
        _ -> {
          process.send(reply, True)
          actor.continue(state)
        }
      }
    }
    ReloadGuild(guild, reply) -> {
      case running(state, guild) {
        Ok(handle) -> {
          process.spawn_unlinked(fn() { reload(guild, handle, reply) })
          Nil
        }
        Error(Nil) -> process.send(reply, NotRunning)
      }
      actor.continue(state)
    }
    StartTransferred(guild, moved, window_ms, reply) ->
      actor.continue(start_transferred(state, guild, moved, window_ms, reply))
    AbortTransfer(guild, reply) ->
      actor.continue(abandon(state, guild, reply, True))
    TransferExpired(guild, reply) ->
      actor.continue(abandon(state, guild, reply, False))
    InstallArriving(guild, tail, reply) ->
      actor.continue(install_arriving(state, guild, tail, reply))
    Started(guild, token, result) ->
      actor.continue(started(state, guild, token, result))
    RunningElsewhere(guild, token, handle) ->
      actor.continue(running_elsewhere(state, guild, token, handle))
    LocalGuilds(reply) -> {
      process.send(
        reply,
        dict.fold(state.guilds, [], fn(acc, key, found) {
          case found, id.guild_id(key) {
            Running(..), Ok(guild) -> [guild, ..acc]
            _, _ -> acc
          }
        }),
      )
      actor.continue(state)
    }
    GuildDown(process.ProcessDown(pid:, reason:, ..)) ->
      actor.continue(forget_pid(state, pid, reason))
    GuildDown(_) -> actor.continue(state)
  }
}

fn running(state: State, guild: GuildId) -> Result(Handle, Nil) {
  case dict.get(state.guilds, id.guild_id_to_int(guild)) {
    Ok(Running(handle:, ..)) ->
      case process.is_alive(handle.pid) {
        True -> Ok(handle)
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn lookup(
  state: State,
  guild: GuildId,
  reply: Subject(Result(Handle, Nil)),
) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key) {
    Ok(Loading(origin: Transferred(_), waiters:, ..) as loading)
    | Ok(Loading(transfer: Some(_), waiters:, ..) as loading) ->
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(..loading, waiters: [WantsArrival(reply), ..waiters]),
        ),
      )
    _ -> {
      process.send(reply, running(state, guild))
      state
    }
  }
}

fn join(
  state: State,
  guild: GuildId,
  request: JoinRequest,
  reply: Subject(JoinReply),
) -> State {
  let deadline_ms = clock.monotonic_ms() + request.remaining_ms
  start_or_wait(state, guild, WantsJoin(request, reply, deadline_ms))
  |> arm_expiry(guild, Some(deadline_ms - join_transit_ms()))
}

fn arm_expiry(state: State, guild: GuildId, at: Option(Int)) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key), at {
    Ok(Loading(expiry_at:, ..) as loading), Some(at) -> {
      let now = clock.monotonic_ms()
      let at = int.max(at, now)
      let covered = case expiry_at {
        Some(armed) -> armed - join_transit_ms() <= at
        None -> False
      }
      case covered {
        True -> state
        False -> {
          process.send_after(state.self, at - now, JoinsExpired(guild))
          State(
            ..state,
            guilds: dict.insert(
              state.guilds,
              key,
              Loading(..loading, expiry_at: Some(at)),
            ),
          )
        }
      }
    }
    _, _ -> state
  }
}

fn earliest_expiry(waiters: List(Waiter)) -> Option(Int) {
  list.fold(waiters, None, fn(earliest, waiter) {
    case waiter, earliest {
      WantsJoin(deadline_ms:, ..), None -> Some(deadline_ms - join_transit_ms())
      WantsJoin(deadline_ms:, ..), Some(at) ->
        Some(int.min(at, deadline_ms - join_transit_ms()))
      _, _ -> earliest
    }
  })
}

fn expire_joins(state: State, guild: GuildId) -> State {
  let key = id.guild_id_to_int(guild)
  let now = clock.monotonic_ms()
  case dict.get(state.guilds, key) {
    Ok(Loading(expiry_at: Some(armed), ..)) if armed > now -> state
    Ok(Loading(waiters:, ..) as loading) -> {
      let #(expired, kept) =
        list.partition(waiters, fn(waiter) {
          case waiter {
            WantsJoin(deadline_ms:, ..) ->
              deadline_ms - join_transit_ms() <= now
            WantsHandle(_) | WantsArrival(_) -> False
          }
        })
      guild_metrics.loading_joins_dropped(list.length(expired))
      list.each(list.reverse(expired), fn(waiter) {
        serve(
          waiter,
          Error(Unavailable(retry_after_ms: unavailable_retry_ms())),
        )
      })
      let next =
        earliest_expiry(kept)
        |> option.map(int.max(_, now + join_transit_ms()))
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(..loading, waiters: kept, expiry_at: None),
        ),
      )
      |> arm_expiry(guild, next)
    }
    _ -> state
  }
}

fn start_or_wait(state: State, guild: GuildId, waiter: Waiter) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key) {
    Ok(Running(handle:, monitor:, ..)) ->
      case process.is_alive(handle.pid) {
        True -> {
          serve(waiter, Ok(handle))
          state
        }
        False -> {
          let sessions = forget_running(guild, handle, monitor)
          State(..state, guilds: dict.delete(state.guilds, key))
          |> remember_crash(guild, sessions)
          |> cold_start(guild, waiter)
        }
      }
    Ok(Loading(waiters:, ..) as loading) ->
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(..loading, waiters: [waiter, ..waiters]),
        ),
      )
    Error(Nil) -> cold_start(state, guild, waiter)
  }
}

fn cold_start(state: State, guild: GuildId, waiter: Waiter) -> State {
  case rollout.is_guild_eligible(guild), router.start_allowed(guild) {
    False, _ -> {
      refuse(waiter)
      state
    }
    True, False -> {
      serve(waiter, Error(Unavailable(retry_after_ms: unavailable_retry_ms())))
      state
    }
    True, True -> {
      let token = state.next_token
      let shard = state.self
      let index = state.index
      let rejoining = crashed_sessions(state, guild)
      case rejoining {
        [] -> Nil
        _ -> guild_metrics.crash_restarted()
      }
      let worker =
        watched(fn() {
          case incumbent(guild) {
            Ok(handle) ->
              process.send(shard, RunningElsewhere(guild, token, handle))
            Error(Nil) ->
              process.send(
                shard,
                Started(guild, token, load_and_start(index, guild, rejoining)),
              )
          }
        })
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          id.guild_id_to_int(guild),
          Loading(token, [waiter], worker, Fetched, None, None),
        ),
        next_token: token + 1,
      )
    }
  }
}

fn incumbent(guild: GuildId) -> Result(Handle, Nil) {
  let self = node.self()
  router.reachable_with(node.Guilds)
  |> list.append(router.expected_with(node.Guilds))
  |> list.unique
  |> list.filter(fn(peer) { peer != self })
  |> running_on(guild, _, incumbent_lookup_timeout_ms())
}

pub fn running_on(
  guild: GuildId,
  peers: List(node.Node),
  timeout_ms: Int,
) -> Result(Handle, Nil) {
  let reply = process.new_subject()
  let asked =
    list.fold(peers, 0, fn(count, peer) {
      case
        cluster_remote.send_named(
          peer,
          names.shard(names.shard_of(guild)),
          Lookup(guild, reply),
        )
      {
        Ok(Nil) -> count + 1
        Error(_) -> count
      }
    })
  first_running(reply, asked, clock.deadline_in(timeout_ms))
}

fn first_running(
  reply: Subject(Result(Handle, Nil)),
  left: Int,
  deadline: clock.Deadline,
) -> Result(Handle, Nil) {
  case left {
    0 -> Error(Nil)
    _ ->
      case process.receive(reply, clock.remaining_ms(deadline)) {
        Ok(Ok(handle)) -> Ok(handle)
        Ok(Error(Nil)) -> first_running(reply, left - 1, deadline)
        Error(Nil) -> Error(Nil)
      }
  }
}

fn incumbent_lookup_timeout_ms() -> Int {
  durations.guild().manager_incumbent_lookup_timeout_ms
}

fn running_elsewhere(
  state: State,
  guild: GuildId,
  token: Int,
  handle: Handle,
) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key) {
    Ok(Loading(expected, waiters, _, origin, transfer, _))
      if expected == token
    -> {
      let state = State(..state, guilds: dict.delete(state.guilds, key))
      case origin, transfer {
        _, Some(pending) -> {
          decline(origin)
          launch_transfer(state, guild, pending.moved, pending.reply, waiters)
        }
        Abandoned, None -> {
          fail_waiters(waiters, retry_after(InitFailed))
          state
        }
        _, None -> {
          decline(origin)
          list.each(list.reverse(waiters), fn(waiter) {
            serve(waiter, Ok(handle))
          })
          state
        }
      }
    }
    _ -> state
  }
}

fn load_and_start(
  index: Int,
  guild: GuildId,
  rejoining: List(SessionId),
) -> Result(Handle, StartFailure) {
  case start_gate.try_guild_start() {
    Error(Nil) -> Error(StartGateFull)
    Ok(permit) -> {
      let began = clock.monotonic_ms()
      let loaded =
        fetch.load(guild, clock.deadline_in(fetch.fetch_timeout_ms()))
      guild_metrics.loaded(result.is_ok(loaded), clock.monotonic_ms() - began)
      let result = case loaded {
        Error(error) -> Error(FetchFailed(error))
        Ok(found) ->
          spawn_guild(index, guild, found, None, None, rejoining, None)
      }
      start_gate.release_guild_start(permit)
      result
    }
  }
}

fn spawn_guild(
  index: Int,
  guild: GuildId,
  loaded: Loaded,
  threads: Option(Handoff),
  voice: Option(GuildVoice),
  rejoining: List(SessionId),
  arriving: Option(Sessions),
) -> Result(Handle, StartFailure) {
  factory_supervisor.start_child(
    factory_supervisor.get_by_name(factory_name(index)),
    server.StartArgs(guild:, loaded:, threads:, voice:, rejoining:, arriving:),
  )
  |> result.map(fn(started) { started.data })
  |> result.replace_error(InitFailed)
}

pub fn stage(
  guild: GuildId,
  move: MoveId,
  bulk: BitArray,
) -> Result(Handle, Nil) {
  factory_supervisor.start_child(
    factory_supervisor.get_by_name(factory_name(names.shard_of(guild))),
    server.StageArgs(guild:, move:, bulk:),
  )
  |> result.map(fn(started) { started.data })
  |> result.replace_error(Nil)
}

fn start_transferred(
  state: State,
  guild: GuildId,
  moved: Transfer,
  window_ms: Int,
  reply: Subject(Bool),
) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key) {
    Ok(Running(origin: Transferred(current), ..)) if current == reply -> {
      process.send(reply, True)
      state
    }
    Ok(Loading(origin: Transferred(current), ..)) if current == reply -> state
    Ok(Loading(transfer:, ..) as loading) -> {
      case transfer {
        Some(previous) -> process.send(previous.reply, False)
        None -> Nil
      }
      expire_later(state, guild, window_ms, reply)
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(..loading, transfer: Some(PendingTransfer(moved, reply))),
        ),
      )
    }
    Ok(Running(handle, monitor, _)) -> {
      retire_running(guild, handle, monitor)
      expire_later(state, guild, window_ms, reply)
      launch_transfer(
        State(..state, guilds: dict.delete(state.guilds, key)),
        guild,
        moved,
        reply,
        [],
      )
    }
    Error(Nil) -> {
      expire_later(state, guild, window_ms, reply)
      launch_transfer(state, guild, moved, reply, [])
    }
  }
}

fn install_arriving(
  state: State,
  guild: GuildId,
  tail: guild_codec.Tail,
  reply: Subject(Result(Handle, Nil)),
) -> State {
  let key = id.guild_id_to_int(guild)
  let entry = case dict.get(state.guilds, key) {
    Ok(Running(handle:, monitor:, ..)) ->
      case leaving(handle) {
        True -> {
          process.demonitor_process(monitor)
          registry.remove(guild, handle)
          Error(Nil)
        }
        False -> Ok(Nil)
      }
    Ok(_) -> Ok(Nil)
    Error(Nil) -> Error(Nil)
  }
  case entry {
    Error(Nil) -> {
      let token = state.next_token
      let shard = state.self
      let index = state.index
      let worker =
        watched(fn() {
          process.send(
            shard,
            Started(guild, token, case tail.staged {
              Some(staged) ->
                server.stage_activate(staged.move, tail)
                |> result.replace_error(InitFailed)
              None ->
                spawn_guild(
                  index,
                  guild,
                  tail.loaded,
                  Some(tail.threads),
                  Some(tail.voice),
                  [],
                  Some(option.unwrap(tail.table, guild_sessions.new())),
                )
            }),
          )
        })
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(token, [WantsArrival(reply)], worker, Arrived, None, None),
        ),
        next_token: token + 1,
      )
    }
    Ok(_) -> {
      process.send(reply, Error(Nil))
      state
    }
  }
}

fn leaving(handle: Handle) -> Bool {
  let reply = process.new_subject()
  let monitor = process.monitor(handle.pid)
  process.send(signal.subject(handle.pid), signal.Probe(reply))
  let answer =
    process.new_selector()
    |> process.select(reply)
    |> process.select_specific_monitor(monitor, fn(_) { False })
    |> process.selector_receive(probe_timeout_ms)
  process.demonitor_process(monitor)
  answer == Ok(True)
}

const probe_timeout_ms = 2000

fn expire_later(
  state: State,
  guild: GuildId,
  window_ms: Int,
  reply: Subject(Bool),
) -> Nil {
  process.send_after(state.self, window_ms, TransferExpired(guild, reply))
  Nil
}

fn abandon(
  state: State,
  guild: GuildId,
  reply: Subject(Bool),
  aborted: Bool,
) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key) {
    Ok(Loading(transfer: Some(pending), ..) as loading)
      if pending.reply == reply
    -> {
      process.send(reply, False)
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(..loading, transfer: None),
        ),
      )
    }
    Ok(Loading(origin: Transferred(current), ..) as loading)
      if current == reply
    -> {
      process.send(reply, False)
      State(
        ..state,
        guilds: dict.insert(
          state.guilds,
          key,
          Loading(..loading, origin: Abandoned),
        ),
      )
    }
    Ok(Running(handle, monitor, Transferred(current)))
      if aborted && current == reply
    -> {
      retire_running(guild, handle, monitor)
      State(..state, guilds: dict.delete(state.guilds, key))
    }
    _ -> state
  }
}

fn launch_transfer(
  state: State,
  guild: GuildId,
  moved: Transfer,
  reply: Subject(Bool),
  waiters: List(Waiter),
) -> State {
  let token = state.next_token
  let shard = state.self
  let index = state.index
  let worker =
    watched(fn() {
      process.send(
        shard,
        Started(
          guild,
          token,
          spawn_guild(
            index,
            guild,
            moved.loaded,
            Some(moved.threads),
            Some(moved.voice),
            list.map(moved.sessions, fn(ref) { ref.id }),
            None,
          ),
        ),
      )
    })
  State(
    ..state,
    guilds: dict.insert(
      state.guilds,
      id.guild_id_to_int(guild),
      Loading(token, waiters, worker, Transferred(reply), None, None),
    ),
    next_token: token + 1,
  )
  |> arm_expiry(guild, earliest_expiry(waiters))
}

fn started(
  state: State,
  guild: GuildId,
  token: Int,
  result: Result(Handle, StartFailure),
) -> State {
  let key = id.guild_id_to_int(guild)
  case dict.get(state.guilds, key) {
    Ok(Loading(expected, waiters, _, origin, transfer, _))
      if expected == token
    -> {
      let state = State(..state, guilds: dict.delete(state.guilds, key))
      case result, origin, transfer {
        Ok(handle), _, Some(pending) -> {
          decline(origin)
          retire(guild, handle)
          launch_transfer(state, guild, pending.moved, pending.reply, waiters)
        }
        Ok(handle), Abandoned, None -> {
          retire(guild, handle)
          fail_waiters(waiters, retry_after(InitFailed))
          state
        }
        Ok(handle), _, None -> {
          case origin {
            Transferred(reply) -> {
              guild_metrics.started(guild_metrics.FromTransfer)
              process.send(reply, True)
            }
            Arrived -> guild_metrics.started(guild_metrics.FromArrival)
            Fetched | Abandoned -> guild_metrics.started(guild_metrics.FromApi)
          }
          let monitor = process.monitor(handle.pid)
          registry.put(guild, handle)
          list.each(list.reverse(waiters), fn(waiter) {
            serve(waiter, Ok(handle))
          })
          State(
            ..state,
            guilds: dict.insert(
              state.guilds,
              key,
              Running(handle, monitor, origin),
            ),
            crashed: dict.delete(state.crashed, key),
          )
        }
        Error(failure), _, Some(pending) -> {
          log_failure(guild, failure)
          decline(origin)
          launch_transfer(state, guild, pending.moved, pending.reply, waiters)
        }
        Error(failure), _, None -> {
          log_failure(guild, failure)
          decline(origin)
          case is_unknown_guild(failure) {
            True -> drop_waiters(waiters, retry_after(failure))
            False -> fail_waiters(waiters, retry_after(failure))
          }
          state
        }
      }
    }
    _ -> {
      case result {
        Ok(handle) -> retire(guild, handle)
        Error(_) -> Nil
      }
      state
    }
  }
}

fn decline(origin: Origin) -> Nil {
  case origin {
    Transferred(reply) -> process.send(reply, False)
    Fetched | Arrived | Abandoned -> Nil
  }
}

fn fail_waiters(waiters: List(Waiter), retry: Int) -> Nil {
  list.each(list.reverse(waiters), fn(waiter) {
    serve(waiter, Error(Unavailable(retry_after_ms: retry)))
  })
}

fn is_unknown_guild(failure: StartFailure) -> Bool {
  case failure {
    FetchFailed(fetch.FetchApi(client.ApiRejected(404, _))) -> True
    _ -> False
  }
}

fn drop_waiters(waiters: List(Waiter), retry: Int) -> Nil {
  list.each(list.reverse(waiters), fn(waiter) {
    case waiter {
      WantsJoin(request, reply, _) ->
        process.send(reply, NotMember(request.attempt))
      _ -> serve(waiter, Error(Unavailable(retry_after_ms: retry)))
    }
  })
}

fn retire_running(guild: GuildId, handle: Handle, monitor: Monitor) -> Nil {
  process.demonitor_process(monitor)
  registry.remove(guild, handle)
  retire(guild, handle)
}

fn retire(guild: GuildId, handle: Handle) -> Nil {
  process.spawn_unlinked(fn() {
    let _ = shut_down(handle, retire_timeout_ms())
    roster.clear(guild, handle.pid)
    counts_shard.delete_owned(guild, handle.pid)
    read_model.delete(guild, handle.pid)
    remains.forget(guild, handle.pid)
  })
  Nil
}

fn stop_running(guild: GuildId, handle: Handle, reply: Subject(Bool)) -> Nil {
  process.spawn_unlinked(fn() {
    let graceful = shut_down(handle, stop_timeout_ms())
    roster.clear(guild, handle.pid)
    counts_shard.delete_owned(guild, handle.pid)
    read_model.delete(guild, handle.pid)
    remains.settle(guild, handle.pid)
    process.send(reply, graceful)
  })
  Nil
}

fn shut_down(handle: Handle, wait_ms: Int) -> Bool {
  let watch = process.monitor(handle.pid)
  let down =
    process.new_selector()
    |> process.select_specific_monitor(watch, fn(_) { Nil })
  process.send(handle.control, inbox.Shutdown(process.new_subject()))
  case process.selector_receive(down, wait_ms) {
    Ok(Nil) -> True
    Error(Nil) -> {
      process.kill(handle.pid)
      let _ = process.selector_receive(down, wait_ms)
      False
    }
  }
}

fn retry_after(failure: StartFailure) -> Int {
  let base = case failure {
    FetchFailed(fetch.FetchApi(client.ApiUnavailable)) ->
      api_unavailable_retry_ms()
    StartGateFull -> gate_full_retry_ms()
    _ -> unavailable_retry_ms()
  }
  base + int.random(base / 2 + 1)
}

fn log_failure(guild: GuildId, failure: StartFailure) -> Nil {
  let #(kind, reason) = case failure {
    StartGateFull -> #("start_gate_full", "start_gate_full")
    FetchFailed(fetch.FetchTimeout) -> #("fetch_timeout", "fetch_timeout")
    FetchFailed(fetch.FetchCrashed(name)) -> #(
      "fetch_crashed",
      "fetch_crashed " <> name,
    )
    FetchFailed(fetch.BadCollection(name)) -> #(
      "bad_collection",
      "bad_collection " <> name,
    )
    FetchFailed(fetch.FetchApi(client.ApiRejected(404, _))) -> #(
      "unknown_guild",
      "unknown_guild",
    )
    FetchFailed(fetch.FetchApi(_)) -> #("api_error", "api_error")
    InitFailed -> #("init_failed", "init_failed")
    StartCrashed -> #("start_crashed", "start_crashed")
  }
  guild_metrics.start_failed(kind)
  logging.log(
    logging.Warning,
    "guild start failed: guild_id="
      <> id.guild_id_to_string(guild)
      <> " reason="
      <> reason,
  )
}

fn serve(waiter: Waiter, outcome: Result(Handle, Unavailable)) -> Nil {
  case waiter, outcome {
    WantsHandle(reply), _ ->
      process.send(reply, result.map_error(outcome, LookupUnavailable))
    WantsJoin(request, reply, deadline_ms), Ok(handle) ->
      process.send(
        handle.ref.inbox,
        core_guild.Join(
          JoinRequest(
            ..request,
            remaining_ms: deadline_ms - clock.monotonic_ms(),
          ),
          reply,
        ),
      )
    WantsJoin(request, reply, _), Error(failure) ->
      process.send(
        reply,
        JoinUnavailable(failure.retry_after_ms, request.attempt),
      )
    WantsArrival(reply), _ ->
      process.send(reply, result.replace_error(outcome, Nil))
  }
}

fn refuse(waiter: Waiter) -> Nil {
  case waiter {
    WantsHandle(reply) -> process.send(reply, Error(NotEligible))
    WantsJoin(request, reply, _) ->
      process.send(reply, JoinNotEligible(request.attempt))
    WantsArrival(reply) -> process.send(reply, Error(Nil))
  }
}

fn watched(work: fn() -> Nil) -> process.Pid {
  let #(pid, _) = spawn.monitored(work)
  pid
}

fn forget_pid(
  state: State,
  pid: process.Pid,
  reason: process.ExitReason,
) -> State {
  dict.fold(state.guilds, state, fn(state, key, found) {
    case found, id.guild_id(key) {
      Running(handle:, monitor:, ..), Ok(guild) if handle.pid == pid -> {
        case reason {
          process.Normal -> Nil
          process.Killed | process.Abnormal(_) ->
            guild_metrics.stopped("crashed")
        }
        let sessions = forget_running(guild, handle, monitor)
        State(..state, guilds: dict.delete(state.guilds, key))
        |> remember_crash(guild, sessions)
      }
      Loading(waiters:, worker:, origin:, transfer:, ..), Ok(guild)
        if worker == pid
      -> {
        log_failure(guild, StartCrashed)
        decline(origin)
        let state = State(..state, guilds: dict.delete(state.guilds, key))
        case transfer {
          Some(pending) ->
            launch_transfer(state, guild, pending.moved, pending.reply, waiters)
          None -> {
            fail_waiters(waiters, retry_after(StartCrashed))
            state
          }
        }
      }
      _, _ -> state
    }
  })
}

fn forget_running(
  guild: GuildId,
  handle: Handle,
  monitor: Monitor,
) -> List(SessionId) {
  process.demonitor_process(monitor)
  registry.remove(guild, handle)
  counts_shard.delete_owned(guild, handle.pid)
  read_model.delete(guild, handle.pid)
  remains.settle(guild, handle.pid)
  roster.take(guild, handle.pid)
}

fn remember_crash(
  state: State,
  guild: GuildId,
  sessions: List(SessionId),
) -> State {
  let now = clock.monotonic_ms()
  let fresh = dict.filter(state.crashed, fn(_, found) { is_fresh(found, now) })
  let key = id.guild_id_to_int(guild)
  let crashed = case sessions {
    [] -> dict.delete(fresh, key)
    _ -> dict.insert(fresh, key, Crashed(sessions:, at_ms: now))
  }
  State(..state, crashed:)
}

fn crashed_sessions(state: State, guild: GuildId) -> List(SessionId) {
  case dict.get(state.crashed, id.guild_id_to_int(guild)) {
    Ok(found) ->
      case is_fresh(found, clock.monotonic_ms()) {
        True -> found.sessions
        False -> []
      }
    Error(Nil) -> []
  }
}

fn is_fresh(found: Crashed, now: Int) -> Bool {
  now - found.at_ms <= transfer.awaiting_replay_ms()
}

fn reload(guild: GuildId, handle: Handle, reply: Subject(ReloadResult)) -> Nil {
  case fetch.load(guild, clock.deadline_in(fetch.fetch_timeout_ms())) {
    Error(_) -> process.send(reply, ReloadFailed)
    Ok(loaded) -> {
      let done = process.new_subject()
      process.send(handle.control, inbox.Reload(loaded, done))
      case process.receive(done, reload_timeout_ms()) {
        Ok(Nil) -> process.send(reply, Reloaded)
        Error(Nil) -> process.send(reply, ReloadFailed)
      }
    }
  }
}
