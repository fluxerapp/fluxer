// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/id
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/handoff/signal
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move
import fluxer_gateway/session/control
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/option
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/isolation
import support/wait

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

fn stop_sink(sink: Sink) -> Nil {
  let monitor = process.monitor(sink.pid)
  process.kill(sink.pid)
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
        health_degraded_ms: 100,
      ),
    )
  })
}

@external(erlang, "erlang", "suspend_process")
fn suspend(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume(pid: Pid) -> Bool

fn move_id(serial: Int) -> message.MoveId {
  MoveId(
    source: WorkerId(node: atom.create("guild_arrival_source"), incarnation: 1),
    serial:,
  )
}

fn at(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn send_message(handle: Handle, message_id: Int) -> Nil {
  fixture.dispatch(
    handle,
    "MESSAGE_CREATE",
    json.object([
      #("id", json.string(int_text(message_id))),
      #("channel_id", json.string(int_text(10))),
      #("content", json.string("hi")),
      #("author", fixture.user(1)),
    ]),
  )
}

fn seen_ids(seen: List(fake.Seen)) -> List(String) {
  fake.only_dispatches(seen)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "MESSAGE_CREATE" -> option.to_result(fake.text_at(pair.1, ["id"]), Nil)
      _ -> Error(Nil)
    }
  })
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

fn move_guild(
  found: fixture.Guild,
  member: fake.FakeSession,
) -> #(Handle, List(fake.Seen)) {
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let sink = start_sink()
  let assert Ok(frozen) =
    guild_adapter.freeze(Key(key.guild, guild), move_id(guild), 2, at(5))
  let assert Ok(arrival) =
    guild_adapter.install(
      Key(key.guild, guild),
      move_id(0).source,
      at(6),
      frozen.tail,
    )
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  let committed = fake.drain(member, 100)
  signal.repointed(frozen.stub, member.ref.id)
  let #(entries, last) = forwarded_until_sealed(sink, [])
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, last)
  assert wait.until(2000, fn() { !process.is_alive(frozen.stub) })
  stop_sink(sink)
  fixture.sync(target)
  #(target, list.append(committed, fake.drain(member, 50)))
}

fn joined_guild(
  api: guild_api.FakeApi,
) -> #(fixture.Guild, Handle, fake.FakeSession) {
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  #(found, handle, member)
}

fn health_updates(seen: List(fake.Seen)) -> List(Bool) {
  list.filter_map(seen, fn(found) {
    case found {
      fake.Control(control.GuildHealth(_, degraded)) -> Ok(degraded)
      _ -> Error(Nil)
    }
  })
}

fn await_degraded(member: fake.FakeSession, deadline: Int) -> Bool {
  case deadline <= 0 {
    True -> False
    False ->
      case list.contains(health_updates(fake.drain(member, 100)), True) {
        True -> True
        False -> await_degraded(member, deadline - 100)
      }
  }
}

pub fn moved_guild_tells_carried_sessions_when_it_degrades_test() {
  short_timers()
  use api <- guild_api.with_api
  let #(found, _, member) = joined_guild(api)
  let #(target, _) = move_guild(found, member)
  assert suspend(target.pid)
  send_message(target, 1)
  let degraded = await_degraded(member, 3000)
  assert resume(target.pid)
  assert degraded
  fixture.stop(found)
}

pub fn moved_guild_that_crashes_catches_up_carried_sessions_test() {
  short_timers()
  use api <- guild_api.with_api
  let #(found, _, member) = joined_guild(api)
  let #(target, _) = move_guild(found, member)
  send_message(target, 1)
  assert seen_ids(fake.drain(member, 100)) == ["1"]
  process.send_abnormal_exit(target.pid, "injected")
  assert wait.until(3000, fn() { !process.is_alive(target.pid) })
  let again = fixture.started(api, found)
  assert again.pid != target.pid
  send_message(again, 2)
  fixture.sync(again)
  let _ = fake.drain(member, 50)
  assert fake.is_joined(fake.join(member, again))
  fixture.sync(again)
  assert seen_ids(fake.drain(member, 150)) == ["2"]
  fixture.stop(found)
}

pub fn moved_guild_clears_a_degraded_state_left_by_the_source_test() {
  short_timers()
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      guild: settings.GuildDurations(..d.guild, health_recovered_ms: 0),
    )
  })
  use api <- guild_api.with_api
  let #(found, source, member) = joined_guild(api)
  assert suspend(source.pid)
  send_message(source, 1)
  let degraded = await_degraded(member, 3000)
  assert resume(source.pid)
  assert degraded
  fixture.sync(source)
  let _ = fake.drain(member, 50)
  let #(_, seen) = move_guild(found, member)
  assert list.last(health_updates(seen)) == Ok(False)
  fixture.stop(found)
}
