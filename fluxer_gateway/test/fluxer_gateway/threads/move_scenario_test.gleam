// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/handoff/signal
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/handoff/adapter.{type Arrival, type Frozen}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/precopy
import fluxer_gateway/threads/scenario/kit
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{None, Some}
import support/guild_api.{type FakeApi}
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
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
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
    source: WorkerId(node: atom.create("thread_move_source"), incarnation: 1),
    serial: 8_100_000 + serial,
  )
}

fn at(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
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

fn ready(api: FakeApi, found: fixture.Guild) -> Handle {
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  assert kit.threads_loaded(handle)
  fixture.sync(handle)
  handle
}

fn joined(handle: Handle, user: Int) -> fake.FakeSession {
  let found = fake.start(user)
  assert fake.is_joined(fake.join(found, handle))
  let _ = fake.drain(found, 50)
  found
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

fn move_guild(
  found: fixture.Guild,
  serial: Int,
  sessions: List(fake.FakeSession),
) -> Handle {
  let entity = Key(key.guild, found.id)
  let sink = start_sink()
  let assert Ok(frozen) =
    guild_adapter.freeze(entity, move_id(serial), 3, at(5))
  let assert Ok(arrival) =
    guild_adapter.install(entity, move_id(serial).source, at(6), frozen.tail)
  finish(sink, frozen, arrival, sessions)
  stop_pid(sink.pid)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  fixture.sync(target)
  list.each(sessions, fn(member) { fake.drain(member, 50) })
  target
}

fn finish(
  sink: Sink,
  frozen: Frozen,
  arrival: Arrival,
  sessions: List(fake.FakeSession),
) -> Nil {
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  list.each(sessions, fn(member) {
    signal.repointed(frozen.stub, member.ref.id)
  })
  let #(entries, last) = forwarded_until_sealed(sink, [])
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, last)
  Nil
}

fn message_ids(found: fake.FakeSession, channel: Int) -> List(String) {
  fake.dispatches(found, 150)
  |> kit.named("MESSAGE_CREATE")
  |> list.filter(fn(payload) {
    fake.text_at(payload, ["channel_id"]) == Some(kit.text(channel))
  })
  |> list.filter_map(fn(payload) {
    option.to_result(fake.text_at(payload, ["id"]), Nil)
  })
}

fn thread_lists(found: fake.FakeSession) -> List(List(String)) {
  fake.dispatches(found, 150)
  |> kit.named("THREAD_MEMBER_LIST_UPDATE")
  |> list.map(kit.list_user_ids)
}

fn watching(handle: Handle) -> fake.FakeSession {
  let watcher = joined(handle, 4)
  kit.subscribe(watcher, handle, Some(True), None)
  let _ = fake.drain(watcher, 100)
  watcher
}

fn thread_events_survive(precopied: Bool, serial: Int) -> Nil {
  short_timers()
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let member = joined(handle, 2)
  let watcher = watching(handle)
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.open_thread, 901))
  assert message_ids(member, kit.open_thread) == ["901"]
  assert message_ids(watcher, kit.open_thread) == ["901"]
  let receiver = case precopied {
    True -> {
      let assert Ok(started) = precopy.start([guild_adapter.adapter()])
      process.unlink(started.pid)
      stage(found, serial)
      Some(started.pid)
    }
    False -> None
  }
  let target = move_guild(found, serial, [member, watcher])
  kit.dispatch(target, "MESSAGE_CREATE", kit.message(kit.open_channel, 902))
  assert message_ids(member, kit.open_channel) == ["902"]
  kit.dispatch(target, "MESSAGE_CREATE", kit.message(kit.open_thread, 903))
  assert message_ids(member, kit.open_thread) == ["903"]
  kit.subscribe(watcher, target, Some(True), None)
  let _ = fake.drain(watcher, 100)
  kit.dispatch(target, "MESSAGE_CREATE", kit.message(kit.open_thread, 904))
  assert message_ids(watcher, kit.open_thread) == ["904"]
  kit.subscribe(member, target, None, Some([kit.open_thread]))
  assert thread_lists(member) == [["2", "3"]]
  option.map(receiver, stop_pid)
  fixture.stop(found)
}

pub fn thread_events_reach_sessions_after_a_full_tail_move_test_() {
  use <- fixture.slow(30)
  thread_events_survive(False, 1)
}

pub fn thread_events_reach_sessions_after_a_precopied_move_test_() {
  use <- fixture.slow(30)
  thread_events_survive(True, 2)
}

pub fn thread_events_reach_sessions_that_rejoin_a_restarted_guild_test_() {
  use <- fixture.slow(30)
  short_timers()
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let member = joined(handle, 2)
  process.kill(handle.pid)
  let again = ready(api, found)
  assert again.pid != handle.pid
  assert fake.is_joined(fake.join(member, again))
  kit.subscribe(member, again, Some(True), Some([kit.open_thread]))
  let _ = fake.drain(member, 150)
  kit.dispatch(again, "MESSAGE_CREATE", kit.message(kit.open_thread, 911))
  assert message_ids(member, kit.open_thread) == ["911"]
  fixture.stop(found)
}
