// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event
import fluxer_gateway/presence/fanout
import fluxer_gateway/presence/gate
import fluxer_gateway/presence/leftovers
import fluxer_gateway/presence/owner
import fluxer_gateway/presence/pids.{type Inbox}
import fluxer_gateway/presence/protocol.{
  type Handoff, type Input, type ManagerMsg, type StartRequest, HandoffRunning,
  HandoffSpawned, LinkExit, Lookup, ManagerTick, ManagerTopology, MoveTo,
  PresenceDown, StartHandoff, StartOrLookup,
}
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/server
import fluxer_gateway/presence/version
import fluxer_gateway/telemetry/process_registry
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{
  type Monitor, type Name, type Pid, type Subject, type Timer,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn init_timeout_ms() -> Int {
  durations.presence().manager_init_timeout_ms
}

type Shard {
  Shard(
    index: Int,
    inbox: Subject(ManagerMsg),
    parent: Pid,
    monitors: Dict(Monitor, #(Int, Inbox)),
    tick: Int,
    timer: Timer,
  )
}

fn check_interval_ms() -> Int {
  durations.presence().manager_check_interval_ms
}

fn topology_settle_ms() -> Int {
  durations.presence().manager_topology_settle_ms
}

fn move_spread_ms() -> Int {
  durations.presence().manager_move_spread_ms
}

fn shard_name(shard: Int) -> Name(ManagerMsg) {
  route.name(route.Manager, shard)
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  route.shards()
  |> list.fold(
    static_supervisor.new(static_supervisor.OneForOne)
      |> static_supervisor.add(
        owner.tables(list.append(pids.tables(), leftovers.tables())),
      ),
    fn(builder, shard) { static_supervisor.add(builder, shard_unit(shard)) },
  )
  |> static_supervisor.supervised
  |> supervision.map_data(fn(_) { Nil })
}

fn shard_unit(shard: Int) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    let parent = process.self()
    actor.new_with_initialiser(init_timeout_ms(), fn(inbox) {
      process.trap_exits(True)
      let built = process.new_subject()
      route.subscribe_built(built)
      let monitors =
        pids.shard_entries(shard)
        |> list.fold(dict.new(), fn(acc, entry) {
          let #(user, presence_inbox) = entry
          case process.subject_owner(presence_inbox) {
            Ok(pid) ->
              case process.is_alive(pid) {
                True ->
                  dict.insert(acc, process.monitor(pid), #(user, presence_inbox))
                False -> {
                  registry.bury(user)
                  pids.delete(user)
                  gate.open_all(user)
                  process_registry.unregister_pid(
                    process_registry.presences,
                    user,
                    pid,
                  )
                  server.recover_crashed(user, pid)
                  publish_killed_offline(user)
                  acc
                }
              }
            Error(Nil) -> acc
          }
        })
      let selector =
        process.new_selector()
        |> process.select(inbox)
        |> process.select_monitors(PresenceDown)
        |> process.select_trapped_exits(LinkExit)
        |> process.select_map(built, fn(_) { ManagerTopology })
      let timer = process.send_after(inbox, check_interval_ms(), ManagerTick(0))
      Ok(
        actor.initialised(Shard(
          index: shard,
          inbox:,
          parent:,
          monitors:,
          tick: 0,
          timer:,
        ))
        |> actor.selecting(selector),
      )
    })
    |> actor.named(shard_name(shard))
    |> actor.on_message(handle)
    |> actor.start
    |> owner.started_nil
  })
}

fn handle(shard: Shard, message: ManagerMsg) -> actor.Next(Shard, ManagerMsg) {
  case message {
    StartOrLookup(request:, first:, reply:) -> {
      let #(shard, started) = start_local(shard, request)
      case started {
        Ok(inbox) -> process.send(inbox, first)
        Error(Nil) -> Nil
      }
      process.send(reply, started)
      actor.continue(shard)
    }
    StartHandoff(request:, reply:) -> {
      let #(shard, started) = accept_handoff(shard, request)
      process.send(reply, started)
      actor.continue(shard)
    }
    protocol.InstallArriving(request:, leaving:, reply:) -> {
      let #(shard, started) = install_arriving(shard, request, leaving)
      process.send(reply, started)
      actor.continue(shard)
    }
    Lookup(user, reply) -> {
      process.send(reply, pids.lookup(user))
      actor.continue(shard)
    }
    PresenceDown(down) -> actor.continue(presence_down(shard, down))
    LinkExit(process.ExitMessage(pid:, reason:)) if pid == shard.parent ->
      shut_down(reason)
    LinkExit(_) -> actor.continue(shard)
    ManagerTopology -> actor.continue(arm_tick(shard, topology_settle_ms()))
    ManagerTick(tick) if tick == shard.tick -> {
      check_owners(shard)
      counters.local_presences(shard.index, dict.size(shard.monitors))
      actor.continue(arm_tick(shard, check_interval_ms()))
    }
    ManagerTick(_) -> actor.continue(shard)
  }
}

@external(erlang, "erlang", "exit")
fn exit_with(reason: reason) -> actor.Next(Shard, ManagerMsg)

fn shut_down(reason: process.ExitReason) -> actor.Next(Shard, ManagerMsg) {
  process.trap_exits(False)
  case reason {
    process.Abnormal(detail) -> exit_with(detail)
    process.Normal -> exit_with(atom.create("shutdown"))
    process.Killed -> exit_with(atom.create("killed"))
  }
}

fn arm_tick(shard: Shard, after_ms: Int) -> Shard {
  let _ = process.cancel_timer(shard.timer)
  let tick = shard.tick + 1
  let timer = process.send_after(shard.inbox, after_ms, ManagerTick(tick))
  Shard(..shard, tick:, timer:)
}

fn start_local(
  shard: Shard,
  request: StartRequest,
) -> #(Shard, Result(Inbox, Nil)) {
  case live_presence(request.user) {
    Ok(inbox) -> #(shard, Ok(inbox))
    Error(Nil) -> spawn_presence(shard, request)
  }
}

fn accept_handoff(
  shard: Shard,
  request: StartRequest,
) -> #(Shard, Result(Handoff, Nil)) {
  case route.owner(request.user) == Ok(remote.self_atom()) {
    False -> #(shard, Error(Nil))
    True ->
      case live_presence(request.user) {
        Ok(inbox) -> #(shard, Ok(HandoffRunning(inbox)))
        Error(Nil) -> {
          let #(shard, started) = spawn_presence(shard, request)
          #(shard, result.map(started, HandoffSpawned))
        }
      }
  }
}

fn install_arriving(
  shard: Shard,
  request: StartRequest,
  leaving: Option(Inbox),
) -> #(Shard, Result(Inbox, Nil)) {
  let started = case live_presence(request.user) {
    Ok(found) if Some(found) != leaving -> Error(Nil)
    _ -> {
      use user <- result.try(
        id.user_id(request.user) |> result.replace_error(Nil),
      )
      server.start_arriving(request, user) |> result.replace_error(Nil)
    }
  }
  case started {
    Ok(actor.Started(pid:, data: inbox)) -> {
      let _ =
        process_registry.register(process_registry.presences, request.user, pid)
      let monitors =
        dict.insert(shard.monitors, process.monitor(pid), #(request.user, inbox))
      #(Shard(..shard, monitors:), Ok(inbox))
    }
    Error(Nil) -> #(shard, Error(Nil))
  }
}

fn live_presence(user: Int) -> Result(Inbox, Nil) {
  use inbox <- result.try(pids.lookup(user))
  use pid <- result.try(process.subject_owner(inbox))
  case process.is_alive(pid) {
    True -> Ok(inbox)
    False -> Error(Nil)
  }
}

fn spawn_presence(
  shard: Shard,
  request: StartRequest,
) -> #(Shard, Result(Inbox, Nil)) {
  let prior = pids.lookup(request.user)
  let started = {
    use user <- result.try(
      id.user_id(request.user) |> result.replace_error(Nil),
    )
    server.start(request, user) |> result.replace_error(Nil)
  }
  case started {
    Ok(actor.Started(pid:, data: inbox)) -> {
      let _ =
        process_registry.register(process_registry.presences, request.user, pid)
      let monitors =
        dict.insert(shard.monitors, process.monitor(pid), #(request.user, inbox))
      #(Shard(..shard, monitors:), Ok(inbox))
    }
    Error(Nil) -> {
      case prior {
        Ok(inbox) -> pids.put(request.user, inbox)
        Error(Nil) -> pids.delete(request.user)
      }
      #(shard, Error(Nil))
    }
  }
}

fn presence_down(shard: Shard, down: process.Down) -> Shard {
  case down {
    process.PortDown(..) -> shard
    process.ProcessDown(monitor:, pid:, reason:) ->
      case dict.get(shard.monitors, monitor) {
        Error(Nil) -> shard
        Ok(#(user, inbox)) -> {
          process_registry.unregister_pid(process_registry.presences, user, pid)
          let owned = case pids.lookup(user) {
            Ok(current) if current == inbox -> {
              case reason {
                process.Normal -> Nil
                process.Killed | process.Abnormal(_) -> registry.bury(user)
              }
              pids.delete(user)
              gate.open_all(user)
              True
            }
            _ -> False
          }
          registry.release(user)
          case reason {
            process.Normal -> leftovers.release(user, pid)
            process.Killed | process.Abnormal(_) -> {
              server.recover_crashed(user, pid)
              case owned {
                True -> publish_killed_offline(user)
                False -> Nil
              }
            }
          }
          Shard(..shard, monitors: dict.delete(shard.monitors, monitor))
        }
      }
  }
}

fn publish_killed_offline(user: Int) -> Nil {
  case cache.take_local(user) {
    Some(row) ->
      case display.is_visible(row.display) {
        True -> {
          counters.incr(counters.KilledOffline)
          let _ =
            fanout.publish(event.status_changed(
              user:,
              version: version.next(row.version),
              display: display.offline(),
              custom_status: <<"null":utf8>>,
              user_wire: row.user_wire,
            ))
          Nil
        }
        False -> Nil
      }
    None -> Nil
  }
}

fn check_owners(shard: Shard) -> Nil {
  let self = remote.self_atom()
  dict.each(shard.monitors, fn(_, entry) {
    let #(user, inbox) = entry
    case route.owner(user) {
      Ok(target) if target != self -> {
        let _ =
          process.send_after(
            inbox,
            int.random(move_spread_ms()),
            MoveTo(target),
          )
        Nil
      }
      _ -> Nil
    }
  })
}

pub fn install_arriving_here(
  request: StartRequest,
  leaving: Option(Inbox),
  timeout_ms: Int,
) -> Result(Inbox, Nil) {
  remote.call(
    remote.self_atom(),
    shard_name(route.shard(request.user)),
    timeout_ms,
    fn(reply) { protocol.InstallArriving(request, leaving, reply) },
  )
  |> result.replace_error(Nil)
  |> result.flatten
}

pub fn start_or_lookup(
  request: StartRequest,
  first: Input(Monitor),
  timeout_ms: Int,
) -> Result(Inbox, remote.CallError) {
  let started = clock.monotonic_ms()
  let #(outcome, inbox) = start_or_lookup_timed(request, first, timeout_ms)
  counters.started(outcome, clock.monotonic_ms() - started)
  inbox
}

fn start_or_lookup_timed(
  request: StartRequest,
  first: Input(Monitor),
  timeout_ms: Int,
) -> #(counters.StartOutcome, Result(Inbox, remote.CallError)) {
  use target <- unavailable_start(route.owner(request.user))
  let found =
    case target == remote.self_atom() {
      True -> live_presence(request.user)
      False -> Error(Nil)
    }
    |> result.lazy_or(fn() {
      stale_holders(request.user, target)
      |> list.find_map(lookup_on(_, request.user, timeout_ms))
    })
  case found {
    Ok(inbox) -> {
      process.send(inbox, first)
      #(counters.StartFound, Ok(inbox))
    }
    Error(Nil) -> {
      let called =
        remote.call_from_disposable(
          target,
          shard_name(route.shard(request.user)),
          timeout_ms,
          fn(reply) { StartOrLookup(request, first, reply) },
        )
        |> result.try(result.replace_error(_, remote.CallUnavailable))
      #(start_outcome(called), called)
    }
  }
}

fn unavailable_start(
  target: Result(Atom, error),
  next: fn(Atom) -> #(counters.StartOutcome, Result(Inbox, remote.CallError)),
) -> #(counters.StartOutcome, Result(Inbox, remote.CallError)) {
  case target {
    Ok(target) -> next(target)
    Error(_) -> #(counters.StartUnavailable, Error(remote.CallUnavailable))
  }
}

fn start_outcome(
  called: Result(Inbox, remote.CallError),
) -> counters.StartOutcome {
  case called {
    Ok(_) -> counters.StartCalled
    Error(remote.CallTimeout) -> counters.StartTimeout
    Error(remote.CallUnavailable) -> counters.StartUnavailable
    Error(remote.CallBusy) -> counters.StartBusy
  }
}

pub type LookupError {
  NotFound
  OwnerUnavailable
  LookupFailed(remote.CallError)
}

pub fn lookup_all(
  user: Int,
  timeout_ms: Int,
) -> Result(List(Inbox), LookupError) {
  let started = clock.monotonic_ms()
  let found = lookup_everywhere(user, timeout_ms)
  let outcome = case found {
    Ok(_) -> counters.LookupFound
    Error(NotFound) -> counters.LookupNotFound
    Error(OwnerUnavailable) -> counters.LookupOwnerUnavailable
    Error(LookupFailed(_)) -> counters.LookupFailed
  }
  counters.looked_up(outcome, clock.monotonic_ms() - started)
  found
}

fn lookup_everywhere(
  user: Int,
  timeout_ms: Int,
) -> Result(List(Inbox), LookupError) {
  use target <- result.try(
    route.owner(user)
    |> result.replace_error(OwnerUnavailable),
  )
  use primary <- result.try(case lookup_on(target, user, timeout_ms) {
    Ok(inbox) -> Ok([inbox])
    Error(NotFound) -> Ok([])
    Error(failed) -> Error(failed)
  })
  let stale =
    stale_holders(user, target)
    |> list.filter_map(lookup_on(_, user, timeout_ms))
  case list.unique(list.append(primary, stale)) {
    [] -> Error(NotFound)
    found -> Ok(found)
  }
}

fn stale_holders(user: Int, target: Atom) -> List(Atom) {
  let previous = case route.previous_owner(user) {
    Some(previous) -> [previous]
    None -> []
  }
  list.append(previous, [remote.self_atom()])
  |> list.unique
  |> list.filter(fn(node) { node != target })
}

fn lookup_on(
  node: Atom,
  user: Int,
  timeout_ms: Int,
) -> Result(Inbox, LookupError) {
  case node == remote.self_atom() {
    True -> live_presence(user) |> result.replace_error(NotFound)
    False ->
      remote.call(node, shard_name(route.shard(user)), timeout_ms, fn(reply) {
        Lookup(user, reply)
      })
      |> result.map_error(LookupFailed)
      |> result.try(result.replace_error(_, NotFound))
  }
}
