// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/config/durations
import fluxer_gateway/core/id
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/handoff/copy
import fluxer_gateway/guild/handoff/signal.{type Signal}
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/names
import fluxer_gateway/guild/server
import fluxer_gateway/handoff/adapter.{
  type Adapter, type AdapterError, type Arrival, type Frozen, Adapter, Arrival,
  Frozen, Precopy,
}
import fluxer_gateway/handoff/codec as handoff_codec
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result

const ask_timeout_ms = 10_000

pub fn adapter() -> Adapter {
  Adapter(
    ns: key.guild,
    codecs: guild_codec.range(),
    entities_in:,
    freeze:,
    install:,
    release:,
    seal:,
    abort_arrival:,
    committed:,
    thaw:,
    abandon:,
    precopy: Some(Precopy(
      from_version: guild_codec.precopy_from,
      snapshot:,
      changes:,
      cancel:,
      stage:,
      restage:,
      unstage:,
    )),
  )
}

pub fn snapshot(
  entity: Key,
  move: MoveId,
  version: Int,
) -> Result(BitArray, AdapterError) {
  use pid <- result.try(running(entity))
  case ask_copy(pid, fn(reply) { copy.Snapshot(move:, version:, reply:) }) {
    Ok(Ok(bytes)) -> Ok(bytes)
    Ok(Error(reason)) -> Error(adapter.EncodeFailed(reason))
    Error(Nil) -> Error(adapter.EntityMissing(entity))
  }
}

pub fn changes(
  entity: Key,
  move: MoveId,
  version: Int,
  round: Int,
) -> Result(#(BitArray, Int), AdapterError) {
  use pid <- result.try(running(entity))
  case
    ask_copy(pid, fn(reply) { copy.Changes(move:, version:, round:, reply:) })
  {
    Ok(Ok(found)) -> Ok(found)
    Ok(Error(reason)) -> Error(adapter.EncodeFailed(reason))
    Error(Nil) -> Error(adapter.EntityMissing(entity))
  }
}

pub fn cancel(entity: Key, move: MoveId) -> Nil {
  case running(entity) {
    Ok(pid) -> process.send(copy.subject(pid), copy.Cancel(move))
    Error(_) -> Nil
  }
}

pub fn stage(
  entity: Key,
  move: MoveId,
  bulk: BitArray,
) -> Result(Pid, AdapterError) {
  use guild <- result.try(
    id.guild_id(entity.id)
    |> result.replace_error(adapter.DecodeFailed("guild id")),
  )
  shard.stage(guild, move, bulk)
  |> result.map(fn(handle) { handle.pid })
  |> result.replace_error(adapter.InstallFailed("guild did not stage"))
}

pub fn restage(
  staged: Pid,
  round: Int,
  bytes: BitArray,
) -> Result(Nil, AdapterError) {
  case ask_copy(staged, fn(reply) { copy.Restage(round:, bytes:, reply:) }) {
    Ok(Ok(Nil)) -> Ok(Nil)
    Ok(Error(reason)) -> Error(adapter.InstallFailed(reason))
    Error(Nil) -> Error(adapter.InstallFailed("staged guild stopped"))
  }
}

pub fn unstage(staged: Pid) -> Nil {
  process.send(copy.subject(staged), copy.Discard)
}

fn running(entity: Key) -> Result(Pid, AdapterError) {
  use guild <- result.try(
    id.guild_id(entity.id)
    |> result.replace_error(adapter.EntityMissing(entity)),
  )
  registry.lookup(guild)
  |> result.map(fn(handle) { handle.pid })
  |> result.replace_error(adapter.EntityMissing(entity))
}

pub fn entities_in(bucket: Int) -> List(Int) {
  registry.guild_ints()
  |> list.filter(fn(guild) { key.bucket(guild) == bucket })
  |> list.unique
  |> list.sort(int.compare)
}

pub fn freeze(
  entity: Key,
  move: MoveId,
  version: Int,
  at: Epoch,
) -> Result(Frozen, AdapterError) {
  use guild <- result.try(
    id.guild_id(entity.id)
    |> result.replace_error(adapter.EntityMissing(entity)),
  )
  use handle <- result.try(
    registry.lookup(guild)
    |> result.replace_error(adapter.EntityMissing(entity)),
  )
  case
    ask_within(
      handle.pid,
      durations.guild().move_freeze_ask_timeout_ms,
      fn(reply) { signal.Freeze(move:, version:, epoch: at, reply:) },
    )
  {
    Ok(Ok(tail)) -> Ok(Frozen(tail:, stub: handle.pid))
    Ok(Error(reason)) -> Error(adapter.EncodeFailed(reason))
    Error(Nil) -> {
      process.send(
        signal.subject(handle.pid),
        signal.Thaw(process.new_subject()),
      )
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
  let _ = #(source, at)
  use guild <- result.try(
    id.guild_id(entity.id)
    |> result.replace_error(adapter.DecodeFailed("guild id")),
  )
  use #(_, decoded) <- result.try(
    handoff_codec.decode(guild_codec.tail(), tail)
    |> result.replace_error(adapter.DecodeFailed("guild tail")),
  )
  let reply = process.new_subject()
  process.send(
    process.named_subject(names.shard(names.shard_of(guild))),
    shard.InstallArriving(guild, decoded, reply),
  )
  case process.receive(reply, server.init_timeout_ms()) {
    Ok(Ok(handle)) ->
      Ok(Arrival(
        entity: handle.pid,
        target_ref: handoff_codec.term_body(handle),
      ))
    Ok(Error(Nil)) -> Error(adapter.EntityBusy(entity))
    Error(Nil) -> Error(adapter.InstallFailed("guild did not start in time"))
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
    Error(Nil) -> Error(adapter.InstallFailed("guild did not seal"))
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
  let _ = at
  use target <- result.try(
    target(target_ref)
    |> result.replace_error(adapter.DecodeFailed("guild target")),
  )
  case ask(stub, fn(reply) { signal.Commit(target:, reply:) }) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("guild stub was not frozen"))
  }
}

fn target(bytes: BitArray) -> Result(Handle, Nil) {
  handoff_codec.term_of(bytes) |> result.replace_error(Nil)
}

pub fn thaw(
  stub: Pid,
  returned: List(#(Int, BitArray)),
) -> Result(Nil, AdapterError) {
  let _ = returned
  case ask(stub, fn(reply) { signal.Thaw(reply:) }) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("guild stub was not frozen"))
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

fn ask(pid: Pid, build: fn(Subject(reply)) -> Signal) -> Result(reply, Nil) {
  ask_within(pid, ask_timeout_ms, build)
}

fn ask_within(
  pid: Pid,
  timeout_ms: Int,
  build: fn(Subject(reply)) -> Signal,
) -> Result(reply, Nil) {
  ask_with(pid, timeout_ms, fn(reply) {
    process.send(signal.subject(pid), build(reply))
  })
}

fn ask_copy(
  pid: Pid,
  build: fn(Subject(reply)) -> copy.Request,
) -> Result(reply, Nil) {
  ask_with(pid, ask_timeout_ms, fn(reply) {
    process.send(copy.subject(pid), build(reply))
  })
}

fn ask_with(
  pid: Pid,
  timeout_ms: Int,
  send: fn(Subject(reply)) -> Nil,
) -> Result(reply, Nil) {
  let reply = process.new_subject()
  let monitor = process.monitor(pid)
  send(reply)
  let answer =
    process.new_selector()
    |> process.select_map(reply, Answered)
    |> process.select_specific_monitor(monitor, fn(_) { Gone })
    |> process.selector_receive(timeout_ms)
  process.demonitor_process(monitor)
  case answer {
    Ok(Answered(found)) -> Ok(found)
    Ok(Gone) | Error(Nil) -> Error(Nil)
  }
}
