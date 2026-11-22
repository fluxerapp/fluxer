// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/config/durations
import fluxer_gateway/core/node
import fluxer_gateway/guild/remote as guild_remote
import fluxer_gateway/handoff/adapter.{
  type Adapter, type AdapterError, type Arrival, type Frozen, Adapter, Arrival,
  Frozen,
}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/presence/handoff/codec
import fluxer_gateway/presence/handoff/signal
import fluxer_gateway/presence/manager
import fluxer_gateway/presence/pids
import fluxer_gateway/presence/protocol.{type Input}
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn timeout_ms() -> Int {
  durations.presence().server_handoff_timeout_ms
}

pub fn adapter() -> Adapter {
  Adapter(
    ns: key.presence,
    codecs: codec.range(),
    entities_in:,
    freeze:,
    install:,
    release:,
    seal:,
    abort_arrival:,
    committed:,
    thaw:,
    abandon:,
    precopy: None,
  )
}

pub fn entities_in(bucket: Int) -> List(Int) {
  route.shards()
  |> list.flat_map(pids.shard_entries)
  |> list.filter_map(fn(entry) {
    case key.bucket(entry.0) == bucket {
      True -> Ok(entry.0)
      False -> Error(Nil)
    }
  })
  |> list.sort(int.compare)
}

pub fn freeze(
  entity: Key,
  move: MoveId,
  version: Int,
  at: Epoch,
) -> Result(Frozen, AdapterError) {
  use pid <- result.try(
    pids.lookup(entity.id)
    |> result.try(process.subject_owner)
    |> result.replace_error(adapter.EntityMissing(entity)),
  )
  case
    ask(pid, fn(reply) { signal.Freeze(move:, version:, epoch: at, reply:) })
  {
    Ok(Ok(tail)) -> Ok(Frozen(tail:, stub: pid))
    Ok(Error(signal.Busy)) -> Error(adapter.EntityBusy(entity))
    Ok(Error(signal.Unencodable(reason))) -> Error(adapter.EncodeFailed(reason))
    Error(Nil) -> {
      process.send(signal.subject(pid), signal.Thaw(process.new_subject()))
      Error(adapter.EntityMissing(entity))
    }
  }
}

pub fn install(
  entity: Key,
  source: WorkerId,
  at: Epoch,
  tail: BitArray,
) -> Result(Arrival, AdapterError) {
  let _ = at
  adopt_interest(entity.id, source)
  use #(_, request) <- result.try(
    handoff_codec.decode(codec.tail(), tail)
    |> result.replace_error(adapter.DecodeFailed("presence tail")),
  )
  use Nil <- result.try(case request.user == entity.id {
    True -> Ok(Nil)
    False -> Error(adapter.DecodeFailed("presence tail names another user"))
  })
  use leaving <- result.try(leaving(entity))
  use inbox <- result.try(
    manager.install_arriving_here(request, leaving, timeout_ms())
    |> result.replace_error(adapter.EntityBusy(entity)),
  )
  use pid <- result.map(
    process.subject_owner(inbox)
    |> result.replace_error(adapter.InstallFailed("presence has no owner")),
  )
  Arrival(entity: pid, target_ref: codec.encode_target(inbox))
}

fn leaving(
  entity: Key,
) -> Result(Option(Subject(Input(Monitor))), AdapterError) {
  case pids.lookup(entity.id) {
    Error(Nil) -> Ok(None)
    Ok(inbox) ->
      case process.subject_owner(inbox) {
        Error(Nil) -> Ok(None)
        Ok(pid) ->
          case ask(pid, fn(reply) { signal.Probe(reply) }) {
            Ok(True) -> Ok(Some(inbox))
            Ok(False) -> Error(adapter.EntityBusy(entity))
            Error(Nil) -> Ok(None)
          }
      }
  }
}

fn adopt_interest(user: Int, source: WorkerId) -> Nil {
  case source.node == remote.self_atom() {
    True -> Nil
    False ->
      guild_remote.call(
        node.from_atom(source.node),
        "fluxer_gateway@presence@registry",
        "interest",
        [guild_remote.arg(user)],
        timeout_ms(),
        expecting: decode.list(atom.decoder()),
      )
      |> result.unwrap([])
      |> registry.adopt_interest(user, _)
  }
}

pub fn release(
  entity: Pid,
  seq: Int,
  bytes: BitArray,
) -> Result(Nil, AdapterError) {
  process.send(signal.subject(entity), signal.Release(seq:, bytes:))
  Ok(Nil)
}

pub fn seal(entity: Pid, last_seq: Int) -> Result(Nil, AdapterError) {
  case ask(entity, fn(reply) { signal.Seal(last_seq:, reply:) }) {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(adapter.InstallFailed("forwarded input missing"))
    Error(Nil) -> Error(adapter.InstallFailed("presence did not seal"))
  }
}

pub fn abort_arrival(entity: Pid) -> Nil {
  process.send(signal.subject(entity), signal.AbortArrival)
}

pub fn committed(
  stub: Pid,
  target_ref: BitArray,
  at: Epoch,
) -> Result(Nil, AdapterError) {
  use target <- result.try(
    codec.decode_target(target_ref)
    |> result.replace_error(adapter.DecodeFailed("presence target")),
  )
  case ask(stub, fn(reply) { signal.Commit(target:, epoch: at, reply:) }) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("presence stub was not frozen"))
  }
}

pub fn thaw(
  stub: Pid,
  returned: List(#(Int, BitArray)),
) -> Result(Nil, AdapterError) {
  let _ = returned
  case ask(stub, fn(reply) { signal.Thaw(reply:) }) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("presence stub was not frozen"))
  }
}

pub fn abandon(entity: Key) -> Nil {
  let _ = entity
  Nil
}

type Answer(reply) {
  Answered(reply)
  Gone
}

fn ask(
  pid: Pid,
  build: fn(Subject(reply)) -> signal.Signal,
) -> Result(reply, Nil) {
  let reply = process.new_subject()
  let monitor = process.monitor(pid)
  process.send(signal.subject(pid), build(reply))
  let answer =
    process.new_selector()
    |> process.select_map(reply, Answered)
    |> process.select_specific_monitor(monitor, fn(_) { Gone })
    |> process.selector_receive(timeout_ms())
  process.demonitor_process(monitor)
  case answer {
    Ok(Answered(found)) -> Ok(found)
    Ok(Gone) | Error(Nil) -> Error(Nil)
  }
}
