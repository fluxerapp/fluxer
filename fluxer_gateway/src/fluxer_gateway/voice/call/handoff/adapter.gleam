// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/handoff/adapter.{
  type Adapter, type AdapterError, type Arrival, type Frozen, Adapter, Arrival,
  Frozen,
}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/voice/call/handoff/codec
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/result

pub fn adapter() -> Adapter {
  Adapter(
    ns: key.call,
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
  manager.local_calls()
  |> list.filter_map(fn(pair) {
    let channel = id.channel_id_to_int(pair.0)
    case key.bucket(channel) == bucket {
      True -> Ok(channel)
      False -> Error(Nil)
    }
  })
  |> list.unique
  |> list.sort(int.compare)
}

fn channel_of(entity: Key) -> Result(ChannelId, AdapterError) {
  id.channel_id(entity.id)
  |> result.replace_error(adapter.EntityMissing(entity))
}

fn local_pid(entity: Key) -> Result(Pid, AdapterError) {
  use channel <- result.try(channel_of(entity))
  manager.local_calls()
  |> list.find(fn(pair) { pair.0 == channel })
  |> result.try(fn(pair) { process.subject_owner(pair.1) })
  |> result.replace_error(adapter.EntityMissing(entity))
}

pub fn freeze(
  entity: Key,
  move: MoveId,
  version: Int,
  at: Epoch,
) -> Result(Frozen, AdapterError) {
  use pid <- result.try(local_pid(entity))
  case server.freeze_for_move(pid, move, version, at) {
    Ok(Ok(tail)) -> Ok(Frozen(tail:, stub: pid))
    Ok(Error(server.MoveBusy)) -> Error(adapter.EntityBusy(entity))
    Ok(Error(server.MoveUnencodable)) ->
      Error(adapter.EncodeFailed("call tail"))
    Error(Nil) -> {
      server.thaw_late(pid)
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
  let _ = source
  let _ = at
  use channel <- result.try(channel_of(entity))
  use #(_, decoded) <- result.try(
    handoff_codec.decode(codec.tail(), tail)
    |> result.replace_error(adapter.DecodeFailed("call tail")),
  )
  use Nil <- result.try(case decoded.call.channel == channel {
    True -> Ok(Nil)
    False -> Error(adapter.DecodeFailed("call tail names another channel"))
  })
  use call <- result.try(
    manager.install_arriving(channel, decoded)
    |> result.replace_error(adapter.EntityBusy(entity)),
  )
  use pid <- result.map(
    process.subject_owner(call)
    |> result.replace_error(adapter.InstallFailed("call has no owner")),
  )
  Arrival(entity: pid, target_ref: codec.encode_target(call))
}

pub fn release(
  entity: Pid,
  seq: Int,
  bytes: BitArray,
) -> Result(Nil, AdapterError) {
  server.release_moved(entity, seq, bytes)
  Ok(Nil)
}

pub fn seal(entity: Pid, last_seq: Int) -> Result(Nil, AdapterError) {
  let sealed = server.seal_moved(entity, last_seq)
  case sealed {
    Ok(_) -> manager.promote_local(entity)
    Error(Nil) -> Nil
  }
  case sealed {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(adapter.InstallFailed("forwarded call input missing"))
    Error(Nil) -> Error(adapter.InstallFailed("call did not seal"))
  }
}

pub fn abort_arrival(entity: Pid) -> Nil {
  server.abort_move(entity)
}

pub fn committed(
  stub: Pid,
  target_ref: BitArray,
  at: Epoch,
) -> Result(Nil, AdapterError) {
  let _ = at
  use target <- result.try(
    codec.decode_target(target_ref)
    |> result.replace_error(adapter.DecodeFailed("call target")),
  )
  case server.commit_move(stub, target) {
    Ok(Ok(channel)) -> {
      manager.forget_local(channel, stub)
      Ok(Nil)
    }
    Ok(Error(Nil)) | Error(Nil) ->
      Error(adapter.InstallFailed("call stub was not frozen"))
  }
}

pub fn thaw(
  stub: Pid,
  returned: List(#(Int, BitArray)),
) -> Result(Nil, AdapterError) {
  let _ = returned
  case server.thaw_move(stub) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("call stub was not frozen"))
  }
}

pub fn abandon(entity: Key) -> Nil {
  let _ = entity
  Nil
}
