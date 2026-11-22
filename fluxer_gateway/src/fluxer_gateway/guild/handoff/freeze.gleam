// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote as cluster_remote
import fluxer_gateway/config/durations
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/guild/handoff/transfer.{type Transfer}
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/names
import fluxer_gateway/guild/remains
import fluxer_gateway/guild/remote
import fluxer_gateway/guild/server
import fluxer_gateway/util/spawn
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/result

fn freeze_timeout_ms() -> Int {
  durations.guild().freeze_timeout_ms
}

fn ack_margin_ms() -> Int {
  durations.guild().freeze_ack_margin_ms
}

fn resync_timeout_ms() -> Int {
  durations.guild().freeze_resync_timeout_ms
}

fn replay_timeout_ms() -> Int {
  durations.guild().freeze_replay_timeout_ms
}

pub type MoveError {
  NotRunning
  FreezeTimeout
  TargetUnreachable
  TargetRejected
  MoverCrashed
}

type Progress {
  Started
  Finished(Result(Nil, MoveError))
}

type Watched {
  Reported(Progress)
  MoverDown
}

pub fn move(guild: GuildId, target: Node) -> Result(Nil, MoveError) {
  case registry.lookup(guild) {
    Error(Nil) -> Error(NotRunning)
    Ok(handle) -> {
      let supervisor = process.self()
      supervise(
        handle,
        fn(started) { run_move(guild, target, handle, supervisor, started) },
        fn() { hand_off(target, guild, handle) },
      )
    }
  }
}

pub fn supervise(
  handle: inbox.Handle,
  run: fn(fn() -> Nil) -> Result(Nil, MoveError),
  complete: fn() -> Nil,
) -> Result(Nil, MoveError) {
  let progress = process.new_subject()
  let #(_, watch) =
    spawn.monitored(fn() {
      let outcome = run(fn() { process.send(progress, Started) })
      process.send(progress, Finished(outcome))
    })
  await_mover(progress, watch, handle, complete, False)
}

fn await_mover(
  progress: Subject(Progress),
  watch: process.Monitor,
  handle: inbox.Handle,
  complete: fn() -> Nil,
  started: Bool,
) -> Result(Nil, MoveError) {
  let next =
    process.new_selector()
    |> process.select_map(progress, Reported)
    |> process.select_specific_monitor(watch, fn(_) { MoverDown })
    |> process.selector_receive_forever
  case next, started {
    Reported(Started), _ -> await_mover(progress, watch, handle, complete, True)
    Reported(Finished(outcome)), _ -> {
      process.demonitor_process(watch)
      outcome
    }
    MoverDown, True -> {
      complete()
      Ok(Nil)
    }
    MoverDown, False -> {
      abandon(handle)
      Error(MoverCrashed)
    }
  }
}

fn run_move(
  guild: GuildId,
  target: Node,
  handle: inbox.Handle,
  supervisor: process.Pid,
  started: fn() -> Nil,
) -> Result(Nil, MoveError) {
  let frozen = process.new_subject()
  process.send(
    handle.control,
    inbox.Freeze([supervisor, process.self()], frozen),
  )
  case process.receive(frozen, freeze_timeout_ms()) {
    Ok(Ok(moved)) ->
      case start_on(target, guild, moved) {
        Ok(Nil) -> {
          started()
          hand_off(target, guild, handle)
          Ok(Nil)
        }
        Error(reason) -> {
          abandon(handle)
          Error(reason)
        }
      }
    _ -> {
      abandon(handle)
      Error(FreezeTimeout)
    }
  }
}

fn abandon(handle: inbox.Handle) -> Nil {
  process.send(handle.control, inbox.Unfreeze)
  remains.release(handle.ref.guild, handle.pid)
}

fn hand_off(target: Node, guild: GuildId, handle: inbox.Handle) -> Nil {
  let buffered =
    await_buffered(
      handle.pid,
      fn(reply) { process.send(handle.control, inbox.Moved(reply)) },
      transfer.moved_timeout_ms(),
    )
  let events = result.unwrap(buffered, [])
  let replayed = replay_on(target, guild, events)
  remains.forget(guild, handle.pid)
  case replayed && result.is_ok(buffered) {
    True -> Nil
    False -> resync(target, guild)
  }
}

pub fn await_buffered(
  source: process.Pid,
  request: fn(Subject(List(#(String, Dynamic)))) -> Nil,
  timeout_ms: Int,
) -> Result(List(#(String, Dynamic)), Nil) {
  let reply = process.new_subject()
  let watch = process.monitor(source)
  request(reply)
  let outcome =
    process.new_selector()
    |> process.select_map(reply, Ok)
    |> process.select_specific_monitor(watch, fn(_) { Error(Nil) })
    |> process.selector_receive(timeout_ms)
    |> result.flatten
  process.demonitor_process(watch)
  outcome
}

fn resync(target: Node, guild: GuildId) -> Nil {
  process.spawn_unlinked(fn() {
    let reply = process.new_subject()
    case
      cluster_remote.send_named(
        target,
        shard_name(guild),
        shard.ReloadGuild(guild, reply),
      )
    {
      Ok(Nil) -> {
        let _ = process.receive(reply, resync_timeout_ms())
        Nil
      }
      Error(_) -> Nil
    }
  })
  Nil
}

fn shard_name(guild: GuildId) -> process.Name(shard.Message) {
  names.shard(names.shard_of(guild))
}

fn start_on(
  target: Node,
  guild: GuildId,
  moved: Transfer,
) -> Result(Nil, MoveError) {
  let ack = process.new_subject()
  let name = shard_name(guild)
  let window_ms = server.init_timeout_ms()
  case
    cluster_remote.send_named(
      target,
      name,
      shard.StartTransferred(guild, moved, window_ms, ack),
    )
  {
    Error(_) -> Error(TargetUnreachable)
    Ok(Nil) ->
      case process.receive(ack, window_ms + ack_margin_ms()) {
        Ok(True) -> Ok(Nil)
        Ok(False) -> Error(TargetRejected)
        Error(Nil) -> {
          let _ =
            cluster_remote.send_named(
              target,
              name,
              shard.AbortTransfer(guild, ack),
            )
          Error(TargetUnreachable)
        }
      }
  }
}

fn replay_on(
  target: Node,
  guild: GuildId,
  events: List(#(String, Dynamic)),
) -> Bool {
  remote.call(
    target,
    "fluxer_gateway@guild@handoff@freeze",
    "replay_local",
    [remote.arg(id.guild_id_to_int(guild)), remote.arg(events)],
    replay_timeout_ms(),
    expecting: decode.bool,
  )
  |> result.unwrap(False)
}

pub fn replay_local(guild_int: Int, events: List(#(String, Dynamic))) -> Bool {
  let found = {
    use guild <- result.try(id.guild_id(guild_int) |> result.replace_error(Nil))
    use handle <- result.map(registry.lookup(guild))
    process.send(handle.control, inbox.Replay(events))
  }
  result.is_ok(found)
}
