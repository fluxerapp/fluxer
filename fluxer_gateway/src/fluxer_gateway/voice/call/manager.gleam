// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/voice/call/handoff/codec as call_codec
import fluxer_gateway/voice/call/server.{type Message as CallMessage}
import fluxer_gateway/voice/call/state.{type Call, type Init}
import fluxer_gateway/voice/metrics as voice_metrics
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Name, type Pid, type Subject}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string

fn init_timeout_ms() -> Int {
  durations.voice().manager_init_timeout_ms
}

fn export_timeout_ms() -> Int {
  durations.voice().manager_export_timeout_ms
}

fn list_timeout_ms() -> Int {
  durations.voice().manager_list_timeout_ms
}

fn lookup_timeout_ms() -> Int {
  durations.voice().manager_lookup_timeout_ms
}

fn create_timeout_ms() -> Int {
  durations.voice().manager_create_timeout_ms
}

fn adopt_timeout_ms() -> Int {
  durations.voice().manager_adopt_timeout_ms
}

const adopt_confirm_attempts = 5

pub type CallRef =
  Subject(CallMessage)

pub type LookupError {
  NotFound
  Unavailable
}

pub type CreateError {
  AlreadyExists
  CreateFailed
  CreateUnavailable
}

pub opaque type Message {
  Create(
    channel: ChannelId,
    init: Init,
    reply: Subject(Result(CallRef, CreateError)),
  )
  Lookup(channel: ChannelId, reply: Subject(Result(CallRef, Nil)))
  LookupAdopted(channel: ChannelId, reply: Subject(Result(CallRef, Nil)))
  Expect(channel: ChannelId, call: CallRef)
  Adopt(channel: ChannelId, call: Call, reply: Subject(Result(CallRef, Nil)))
  ListCalls(reply: Subject(List(#(ChannelId, CallRef))))
  Install(
    channel: ChannelId,
    tail: call_codec.Tail,
    reply: Subject(Result(CallRef, Nil)),
  )
  Forget(channel: ChannelId, call: Pid)
  Promote(call: Pid)
  CallDown(process.Down)
  CallExit(process.ExitMessage)
}

type Tracked {
  Tracked(channel: ChannelId, call: CallRef, monitor: Monitor)
}

type Manager {
  Manager(
    calls: Dict(Int, Tracked),
    incoming: Dict(Int, Tracked),
    arriving: Dict(Int, Tracked),
    parent: Pid,
  )
}

fn name() -> Name(Message) {
  env.process_name("fluxer_voice_call_manager")
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    let parent = process.self()
    actor.new_with_initialiser(init_timeout_ms(), fn(self) {
      process.trap_exits(True)
      let selector =
        process.new_selector()
        |> process.select(self)
        |> process.select_monitors(CallDown)
        |> process.select_trapped_exits(CallExit)
      let _ = process_registry.clear(process_registry.calls)
      actor.initialised(Manager(
        calls: dict.new(),
        incoming: dict.new(),
        arriving: dict.new(),
        parent:,
      ))
      |> actor.selecting(selector)
      |> actor.returning(Nil)
      |> Ok
    })
    |> actor.on_message(handle)
    |> actor.named(name())
    |> actor.start
  })
}

fn handle(manager: Manager, message: Message) -> actor.Next(Manager, Message) {
  case message {
    Create(channel, init, reply) ->
      case resolve(manager, channel) {
        Ok(_) -> {
          process.send(reply, Error(AlreadyExists))
          actor.continue(manager)
        }
        Error(Nil) -> {
          let #(manager, result) =
            start_call(manager, channel, server.Fresh(init))
          process.send(reply, result |> result.replace_error(CreateFailed))
          actor.continue(manager)
        }
      }
    Lookup(channel, reply) -> {
      process.send(reply, resolve(manager, channel))
      actor.continue(manager)
    }
    LookupAdopted(channel, reply) -> {
      process.send(reply, find(manager, channel))
      actor.continue(manager)
    }
    Expect(channel, call) -> actor.continue(expect(manager, channel, call))
    Adopt(channel, call, reply) -> {
      let manager = forget_incoming(manager, channel)
      case find(manager, channel) {
        Ok(existing) -> {
          server.absorb(existing, call)
          process.send(reply, Ok(existing))
          actor.continue(manager)
        }
        Error(Nil) -> {
          let #(manager, result) =
            start_call(manager, channel, server.Restored(call))
          process.send(reply, result)
          actor.continue(manager)
        }
      }
    }
    Install(channel, tail, reply) -> {
      let #(manager, result) = install(manager, channel, tail)
      process.send(reply, result)
      actor.continue(manager)
    }
    Promote(call) -> actor.continue(promote(manager, call))
    Forget(channel, call) -> {
      let key = id.channel_id_to_int(channel)
      case dict.get(manager.calls, key) {
        Ok(tracked) ->
          case process.subject_owner(tracked.call) == Ok(call) {
            True -> actor.continue(drop_tracked(manager, key, tracked, call))
            False -> actor.continue(manager)
          }
        Error(Nil) -> actor.continue(manager)
      }
    }
    ListCalls(reply) -> {
      manager.calls
      |> dict.values
      |> list.map(fn(tracked) { #(tracked.channel, tracked.call) })
      |> process.send(reply, _)
      actor.continue(manager)
    }
    CallDown(process.ProcessDown(monitor:, pid:, ..)) -> {
      let #(gone, kept) =
        manager.calls
        |> dict.to_list
        |> list.partition(fn(pair) {
          let #(_, tracked) = pair
          tracked.monitor == monitor
        })
      list.each(gone, fn(pair) {
        let #(_, tracked) = pair
        process_registry.unregister_pid(
          process_registry.calls,
          id.channel_id_to_int(tracked.channel),
          pid,
        )
      })
      let incoming =
        dict.filter(manager.incoming, fn(_key, tracked) {
          tracked.monitor != monitor
        })
      let arriving =
        dict.filter(manager.arriving, fn(_key, tracked) {
          tracked.monitor != monitor
        })
      actor.continue(
        Manager(..manager, calls: dict.from_list(kept), incoming:, arriving:),
      )
    }
    CallDown(_) -> actor.continue(manager)
    CallExit(process.ExitMessage(pid, reason)) ->
      case pid == manager.parent, reason {
        False, _ -> actor.continue(manager)
        True, process.Normal -> actor.stop()
        True, process.Killed -> actor.stop_abnormal("killed")
        True, process.Abnormal(reason) ->
          actor.stop_abnormal(string.inspect(reason))
      }
  }
}

fn install(
  manager: Manager,
  channel: ChannelId,
  tail: call_codec.Tail,
) -> #(Manager, Result(CallRef, Nil)) {
  let key = id.channel_id_to_int(channel)
  let settled = case find(manager, channel) {
    Error(Nil) -> True
    Ok(existing) ->
      case process.subject_owner(existing) {
        Ok(pid) -> server.is_leaving(pid) == Ok(True)
        Error(Nil) -> True
      }
  }
  case settled && !dict.has_key(manager.arriving, key) {
    False -> #(manager, Error(Nil))
    True ->
      case server.start(server.Arrived(tail)) {
        Error(_) -> #(manager, Error(Nil))
        Ok(started) -> {
          let tracked =
            Tracked(
              channel:,
              call: started.data,
              monitor: process.monitor(started.pid),
            )
          #(
            Manager(
              ..manager,
              arriving: dict.insert(manager.arriving, key, tracked),
            ),
            Ok(started.data),
          )
        }
      }
  }
}

fn promote(manager: Manager, call: Pid) -> Manager {
  let found =
    dict.to_list(manager.arriving)
    |> list.find(fn(pair) { process.subject_owner({ pair.1 }.call) == Ok(call) })
  case found {
    Error(Nil) -> manager
    Ok(#(key, tracked)) -> {
      let manager = case dict.get(manager.calls, key) {
        Ok(previous) ->
          case process.subject_owner(previous.call) {
            Ok(pid) -> drop_tracked(manager, key, previous, pid)
            Error(Nil) -> manager
          }
        Error(Nil) -> manager
      }
      let _ = process_registry.register(process_registry.calls, key, call)
      Manager(
        ..manager,
        calls: dict.insert(manager.calls, key, tracked),
        arriving: dict.delete(manager.arriving, key),
      )
    }
  }
}

fn drop_tracked(
  manager: Manager,
  key: Int,
  tracked: Tracked,
  pid: Pid,
) -> Manager {
  process.demonitor_process(tracked.monitor)
  process_registry.unregister_pid(process_registry.calls, key, pid)
  Manager(..manager, calls: dict.delete(manager.calls, key))
}

fn resolve(manager: Manager, channel: ChannelId) -> Result(CallRef, Nil) {
  let key = id.channel_id_to_int(channel)
  case dict.get(manager.incoming, key) {
    Ok(tracked) -> Ok(tracked.call)
    Error(Nil) ->
      case find(manager, channel) {
        Ok(found) -> Ok(found)
        Error(Nil) ->
          dict.get(manager.arriving, key)
          |> result.map(fn(found) { found.call })
      }
  }
}

fn expect(manager: Manager, channel: ChannelId, call: CallRef) -> Manager {
  case process.subject_owner(call) {
    Ok(pid) -> {
      let manager = forget_incoming(manager, channel)
      let tracked = Tracked(channel:, call:, monitor: process.monitor(pid))
      Manager(
        ..manager,
        incoming: dict.insert(
          manager.incoming,
          id.channel_id_to_int(channel),
          tracked,
        ),
      )
    }
    Error(Nil) -> manager
  }
}

fn forget_incoming(manager: Manager, channel: ChannelId) -> Manager {
  let key = id.channel_id_to_int(channel)
  case dict.get(manager.incoming, key) {
    Ok(tracked) -> {
      process.demonitor_process(tracked.monitor)
      Manager(..manager, incoming: dict.delete(manager.incoming, key))
    }
    Error(Nil) -> manager
  }
}

fn find(manager: Manager, channel: ChannelId) -> Result(CallRef, Nil) {
  use tracked <- result.try(dict.get(
    manager.calls,
    id.channel_id_to_int(channel),
  ))
  use pid <- result.try(process.subject_owner(tracked.call))
  case process.is_alive(pid) {
    True -> Ok(tracked.call)
    False -> Error(Nil)
  }
}

fn start_call(
  manager: Manager,
  channel: ChannelId,
  start: server.Start,
) -> #(Manager, Result(CallRef, Nil)) {
  case server.start(start) {
    Ok(started) -> {
      let monitor = process.monitor(started.pid)
      let _ =
        process_registry.register(
          process_registry.calls,
          id.channel_id_to_int(channel),
          started.pid,
        )
      #(
        Manager(
          ..manager,
          calls: dict.insert(
            manager.calls,
            id.channel_id_to_int(channel),
            Tracked(channel:, call: started.data, monitor:),
          ),
        ),
        Ok(started.data),
      )
    }
    Error(_) -> #(manager, Error(Nil))
  }
}

fn local_ready() -> Bool {
  process.named(name()) |> result.is_ok
}

fn send_to(target: Node, message: Message) -> Result(Nil, Nil) {
  case target == node.self() {
    True ->
      case local_ready() {
        True -> Ok(process.send(process.named_subject(name()), message))
        False -> Error(Nil)
      }
    False ->
      remote.send_named(target, name(), message)
      |> result.replace_error(Nil)
  }
}

fn request(
  target: Node,
  timeout: Int,
  make: fn(Subject(a)) -> Message,
) -> Result(a, Nil) {
  let reply = process.new_subject()
  use Nil <- result.try(send_to(target, make(reply)))
  process.receive(reply, timeout)
}

fn owner_node(channel: ChannelId) -> Result(Node, Nil) {
  case router.owner(router.CallKey(channel), node.Calls) {
    router.Local -> Ok(node.self())
    router.Remote(target) -> Ok(target)
    router.OwnerUnavailable(_) -> Error(Nil)
  }
}

pub fn lookup(channel: ChannelId) -> Result(CallRef, LookupError) {
  let started = clock.monotonic_ms()
  let found = lookup_owner(channel)
  let outcome = case found {
    Ok(_) -> voice_metrics.CallFound
    Error(NotFound) -> voice_metrics.CallNotFound
    Error(Unavailable) -> voice_metrics.CallUnavailable
  }
  voice_metrics.call_lookup(outcome, clock.monotonic_ms() - started)
  found
}

fn lookup_owner(channel: ChannelId) -> Result(CallRef, LookupError) {
  use target <- result.try(
    owner_node(channel) |> result.replace_error(Unavailable),
  )
  case request(target, lookup_timeout_ms(), Lookup(channel, _)) {
    Ok(Ok(subject)) -> Ok(subject)
    Ok(Error(Nil)) -> lookup_local(target, channel)
    Error(Nil) -> Error(Unavailable)
  }
}

fn lookup_local(
  target: Node,
  channel: ChannelId,
) -> Result(CallRef, LookupError) {
  case target == node.self() {
    True -> Error(NotFound)
    False ->
      case request(node.self(), lookup_timeout_ms(), Lookup(channel, _)) {
        Ok(Ok(subject)) -> Ok(subject)
        _ -> Error(NotFound)
      }
  }
}

pub fn create(channel: ChannelId, init: Init) -> Result(CallRef, CreateError) {
  let started = clock.monotonic_ms()
  let created = create_on_owner(channel, init)
  let outcome = case created {
    Ok(_) -> voice_metrics.CallCreated
    Error(AlreadyExists) -> voice_metrics.CallExists
    Error(CreateFailed) -> voice_metrics.CallFailed
    Error(CreateUnavailable) -> voice_metrics.CallCreateUnavailable
  }
  voice_metrics.call_create(outcome, clock.monotonic_ms() - started)
  created
}

fn create_on_owner(
  channel: ChannelId,
  init: Init,
) -> Result(CallRef, CreateError) {
  use target <- result.try(
    owner_node(channel) |> result.replace_error(CreateUnavailable),
  )
  request(target, create_timeout_ms(), Create(channel, init, _))
  |> result.replace_error(CreateUnavailable)
  |> result.flatten
}

pub fn local_calls() -> List(#(ChannelId, CallRef)) {
  request(node.self(), list_timeout_ms(), ListCalls) |> result.unwrap([])
}

pub fn install_arriving(
  channel: ChannelId,
  tail: call_codec.Tail,
) -> Result(CallRef, Nil) {
  request(node.self(), create_timeout_ms(), Install(channel, tail, _))
  |> result.flatten
}

pub fn forget_local(channel: ChannelId, call: Pid) -> Nil {
  let _ = send_to(node.self(), Forget(channel, call))
  Nil
}

pub fn promote_local(call: Pid) -> Nil {
  let _ = send_to(node.self(), Promote(call))
  Nil
}

pub fn export_for(nodes: List(Node)) -> Int {
  let moves =
    local_calls()
    |> list.filter_map(fn(pair) {
      let #(channel, subject) = pair
      case owner_node(channel) {
        Ok(target) ->
          case target != node.self() && list.contains(nodes, target) {
            True -> Ok(#(channel, subject, target))
            False -> Error(Nil)
          }
        Error(Nil) -> Error(Nil)
      }
    })
  list.each(moves, fn(move) {
    let #(channel, subject, target) = move
    let _ = send_to(target, Expect(channel, subject))
    Nil
  })
  list.count(moves, fn(move) {
    let #(channel, subject, target) = move
    handoff(channel, subject, target)
  })
}

fn handoff(channel: ChannelId, subject: CallRef, target: Node) -> Bool {
  let adopted = {
    use call <- result.try(server.export(subject, export_timeout_ms()))
    adopt(target, channel, call)
  }
  case adopted {
    Ok(successor) -> {
      server.stop_for_handoff(subject, successor)
      True
    }
    Error(Nil) -> {
      server.thaw(subject)
      False
    }
  }
}

fn adopt(target: Node, channel: ChannelId, call: Call) -> Result(CallRef, Nil) {
  let reply = process.new_subject()
  use Nil <- result.try(send_to(target, Adopt(channel, call, reply)))
  case process.receive(reply, adopt_timeout_ms()) {
    Ok(adopted) -> adopted
    Error(Nil) -> adopted_after_timeout(target, channel, adopt_confirm_attempts)
  }
}

fn adopted_after_timeout(
  target: Node,
  channel: ChannelId,
  attempts: Int,
) -> Result(CallRef, Nil) {
  case request(target, lookup_timeout_ms(), LookupAdopted(channel, _)) {
    Ok(found) -> found
    Error(Nil) ->
      case attempts > 1 {
        True -> adopted_after_timeout(target, channel, attempts - 1)
        False -> Error(Nil)
      }
  }
}
