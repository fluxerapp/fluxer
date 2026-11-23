// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/handoff/signal
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/handoff/adapter.{type Arrival, type Frozen}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/precopy
import fluxer_gateway/session/control
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/isolation

type Sink {
  Sink(pid: Pid, seen: Subject(move.Command))
}

fn start_sink() -> Sink {
  let seen = process.new_subject()
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      claim_engine_name()
      process.send(ready, Nil)
      sink_loop(process.named_subject(move.engine_name()), seen)
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  Sink(pid:, seen:)
}

fn claim_engine_name() -> Nil {
  case process.register(process.self(), move.engine_name()) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      process.sleep(5)
      claim_engine_name()
    }
  }
}

fn sink_loop(inbox: Subject(move.Command), seen: Subject(move.Command)) -> Nil {
  process.send(seen, process.receive_forever(inbox))
  sink_loop(inbox, seen)
}

fn stop_pid(pid: Pid) -> Nil {
  let monitor = process.monitor(pid)
  process.kill(pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(1000)
  Nil
}

fn short_timers() -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      guild: settings.GuildDurations(
        ..d.guild,
        server_seal_wait_ms: 2000,
        server_forward_grace_ms: 300,
      ),
    )
  })
}

fn move_id(serial: Int) -> message.MoveId {
  MoveId(
    source: WorkerId(
      node: atom.create("member_list_move_source"),
      incarnation: 1,
    ),
    serial: 8_200_000 + serial,
  )
}

fn at(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

fn crowd() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
  |> fixture.with_member(4, [])
}

fn list_op() -> session.ClientOp {
  session.LazyRequest(
    fixture.to_dynamic(
      json.object([
        #(
          "member_list_channels",
          json.object([
            #("10", json.preprocessed_array([json.array([0, 99], json.int)])),
          ]),
        ),
      ]),
    ),
  )
}

fn forwarded_until_sealed(
  sink: Sink,
  acc: List(#(Int, BitArray)),
) -> #(List(#(Int, BitArray)), Int) {
  let assert Ok(found) = process.receive(sink.seen, 3000)
  case found {
    move.Forwarded(bytes:, ..) ->
      forwarded_until_sealed(sink, [#(list.length(acc) + 1, bytes), ..acc])
    move.Repointed(..) -> #(list.reverse(acc), list.length(acc))
    _ -> forwarded_until_sealed(sink, acc)
  }
}

fn stage(found: fixture.Guild, serial: Int) -> Nil {
  let entity = Key(key.guild, found.id)
  let move = move_id(serial)
  let reply = process.new_subject()
  let assert Ok(bulk) = guild_adapter.snapshot(entity, move, 3)
  process.send(
    process.named_subject(precopy.receiver_name()),
    precopy.Bulk(move:, entity:, blob: bulk, reply:),
  )
  assert process.receive(reply, 30_000) == Ok(precopy.Staged(move, 0))
}

fn rebound(found: fake.FakeSession) -> Bool {
  fake.drain(found, 200)
  |> list.any(fn(seen) {
    case seen {
      fake.Control(control.GuildRebind(..)) -> True
      _ -> False
    }
  })
}

fn move_guild(
  found: fixture.Guild,
  serial: Int,
  watcher: fake.FakeSession,
  replay: Bool,
) -> Handle {
  let entity = Key(key.guild, found.id)
  let sink = start_sink()
  let assert Ok(frozen) =
    guild_adapter.freeze(entity, move_id(serial), 3, at(5))
  let assert Ok(arrival) =
    guild_adapter.install(entity, move_id(serial).source, at(6), frozen.tail)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  finish(sink, frozen, arrival, target, watcher, replay)
  stop_pid(sink.pid)
  fixture.sync(target)
  target
}

fn finish(
  sink: Sink,
  frozen: Frozen,
  arrival: Arrival,
  target: Handle,
  watcher: fake.FakeSession,
  replay: Bool,
) -> Nil {
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  assert rebound(watcher)
  case replay {
    True -> fake.op(watcher, target, list_op())
    False -> Nil
  }
  signal.repointed(frozen.stub, watcher.ref.id)
  let #(entries, last) = forwarded_until_sealed(sink, [])
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, last)
  Nil
}

fn list_update(found: fake.FakeSession, wait_ms: Int) -> Option(Dynamic) {
  fake.await_event(found, "GUILD_MEMBER_LIST_UPDATE", wait_ms)
}

fn online(payload: Dynamic) -> Result(Int, Nil) {
  decode.run(payload, decode.at(["online_count"], decode.int))
  |> option.from_result
  |> option.to_result(Nil)
}

fn ops(payload: Dynamic) -> List(String) {
  decode.run(
    payload,
    decode.at(["ops"], decode.list(decode.at(["op"], decode.string))),
  )
  |> option.from_result
  |> option.unwrap([])
}

fn sidebar_follows_the_move(precopied: Bool, serial: Int, replay: Bool) -> Nil {
  short_timers()
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  let _ = fake.drain(owner, 50)
  fake.op(owner, handle, list_op())
  let assert Some(sync) = list_update(owner, 1000)
  assert ops(sync) == ["SYNC"]
  let _ = fake.drain(owner, 200)
  let receiver = case precopied {
    True -> {
      let assert Ok(started) = precopy.start([guild_adapter.adapter()])
      process.unlink(started.pid)
      stage(found, serial)
      Some(started.pid)
    }
    False -> None
  }
  let target = move_guild(found, serial, owner, replay)
  let _ = fake.drain(owner, 200)
  let member = fake.start(3)
  assert fake.is_joined(fake.join(member, target))
  let assert Some(joined) = list_update(owner, 1000)
  assert online(joined) == Ok(2)
  option.map(receiver, stop_pid)
  fixture.stop(found)
}

pub fn member_list_follows_a_full_tail_move_after_the_session_replays_test_() {
  use <- fixture.slow(30)
  sidebar_follows_the_move(False, 1, True)
}

pub fn member_list_follows_a_precopied_move_after_the_session_replays_test_() {
  use <- fixture.slow(30)
  sidebar_follows_the_move(True, 2, True)
}
