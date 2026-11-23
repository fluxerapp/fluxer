// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/handoff/signal
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/handoff/adapter.{type Arrival, type Frozen}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move
import fluxer_gateway/session/control
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/option
import support/guild_api
import support/guild_engagement
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
    source: WorkerId(node: atom.create("guild_move_source"), incarnation: 1),
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

fn message(message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(10))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn send_message(handle: Handle, message_id: Int) -> Nil {
  fixture.dispatch(handle, "MESSAGE_CREATE", message(message_id))
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

fn freeze(guild: Int, serial: Int) -> Frozen {
  let assert Ok(frozen) =
    guild_adapter.freeze(Key(key.guild, guild), move_id(serial), 2, at(5))
  frozen
}

fn install(guild: Int, frozen: Frozen) -> Arrival {
  let assert Ok(arrival) =
    guild_adapter.install(
      Key(key.guild, guild),
      move_id(0).source,
      at(6),
      frozen.tail,
    )
  arrival
}

fn rebinds(seen: List(fake.Seen)) -> List(Pid) {
  list.filter_map(seen, fn(found) {
    case found {
      fake.Control(control.GuildRebind(_, stub)) -> Ok(stub)
      _ -> Error(Nil)
    }
  })
}

fn downs(seen: List(fake.Seen)) -> Int {
  list.count(seen, fn(found) {
    case found {
      fake.Inbox(session.GuildDown(..)) -> True
      _ -> False
    }
  })
}

fn creates(seen: List(fake.Seen)) -> Int {
  fake.only_dispatches(seen)
  |> list.count(fn(pair) { pair.0 == "GUILD_CREATE" })
}

fn wait_down(pid: Pid, timeout_ms: Int) -> Bool {
  let monitor = process.monitor(pid)
  process.new_selector()
  |> process.select_specific_monitor(monitor, fn(_) { True })
  |> process.selector_receive(timeout_ms)
  == Ok(True)
}

pub fn committed_guild_move_keeps_event_order_without_a_rejoin_test() {
  short_timers()
  use api <- guild_api.with_api
  let owner = 7_920_000_000_000_001
  let user = 7_920_000_000_000_002
  let found =
    fixture.new(owner) |> fixture.with_text(10) |> fixture.with_member(user, [])
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let member = fake.start(user)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  send_message(handle, 1)
  assert seen_ids(fake.drain(member, 100)) == ["1"]
  let sink = start_sink()
  let frozen = freeze(guild, 1)
  send_message(handle, 2)
  send_message(handle, 3)
  assert seen_ids(fake.drain(member, 100)) == []
  let arrival = install(guild, frozen)
  assert arrival.entity != frozen.stub
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  send_message(target, 4)
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  let seen = fake.drain(member, 100)
  assert rebinds(seen) == [frozen.stub]
  signal.repointed(frozen.stub, member.ref.id)
  let #(entries, last) = forwarded_until_sealed(sink, [])
  assert last == 2
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, last)
  let after = fake.drain(member, 150)
  assert seen_ids(after) == ["2", "3", "4"]
  assert creates(after) == 0
  assert downs(list.append(seen, after)) == 0
  assert wait_down(frozen.stub, 2000)
  send_message(target, 5)
  assert seen_ids(fake.drain(member, 100)) == ["5"]
  stop_sink(sink)
  fixture.stop(found)
}

pub fn thawed_guild_replays_what_it_held_and_keeps_serving_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let sink = start_sink()
  let frozen = freeze(guild, 2)
  send_message(handle, 7)
  assert seen_ids(fake.drain(member, 100)) == []
  let assert Ok(Nil) = guild_adapter.thaw(frozen.stub, [])
  send_message(handle, 8)
  assert seen_ids(fake.drain(member, 150)) == ["7", "8"]
  assert process.is_alive(handle.pid)
  stop_sink(sink)
  fixture.stop(found)
}

pub fn aborted_arrival_stops_without_output_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let sink = start_sink()
  let frozen = freeze(guild, 3)
  let arrival = install(guild, frozen)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  send_message(target, 9)
  guild_adapter.abort_arrival(arrival.entity)
  assert wait_down(arrival.entity, 2000)
  assert seen_ids(fake.drain(member, 100)) == []
  let assert Ok(Nil) = guild_adapter.thaw(frozen.stub, [])
  assert process.is_alive(handle.pid)
  stop_sink(sink)
  process.kill(handle.pid)
}

pub fn a_freeze_that_answers_after_the_mover_gave_up_does_not_stay_frozen_test() {
  short_timers()
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      guild: settings.GuildDurations(..d.guild, move_freeze_ask_timeout_ms: 50),
    )
  })
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let sink = start_sink()
  assert suspend(handle.pid)
  let assert Error(_) =
    guild_adapter.freeze(Key(key.guild, guild), move_id(4), 2, at(5))
  assert resume(handle.pid)
  send_message(handle, 11)
  assert seen_ids(fake.drain(member, 200)) == ["11"]
  stop_sink(sink)
  fixture.stop(found)
}

pub fn a_moved_guild_keeps_its_engaged_members_for_push_test() {
  short_timers()
  use api <- guild_api.with_api
  let owner = 7_930_000_000_000_001
  let user = 7_930_000_000_000_002
  let found =
    fixture.new(owner) |> fixture.with_text(10) |> fixture.with_member(user, [])
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let member = fake.start(user)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  assert guild_engagement.engaged(handle.pid, user)
  let sink = start_sink()
  let frozen = freeze(guild, 5)
  let arrival = install(guild, frozen)
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  assert rebinds(fake.drain(member, 100)) == [frozen.stub]
  signal.repointed(frozen.stub, member.ref.id)
  let #(entries, last) = forwarded_until_sealed(sink, [])
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, last)
  assert guild_engagement.engaged(arrival.entity, user)
  stop_sink(sink)
  fixture.stop(found)
}

fn private_message(message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(20))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

pub fn moved_guild_gates_private_channels_by_the_target_role_sets_test() {
  short_timers()
  use api <- guild_api.with_api
  let owner = 7_920_000_000_000_001
  let allowed = 7_920_000_000_000_002
  let denied = 7_920_000_000_000_003
  let base = fixture.new(owner)
  let found =
    base
    |> fixture.with_role(300, 0, 1)
    |> fixture.with_role(400, 0, 2)
    |> fixture.with_text(10)
    |> fixture.with_channel(20, 0, option.None, [
      fixture.RoleDeny(base.id, fixture.view_channel),
      fixture.RoleAllow(300, fixture.view_channel),
    ])
    |> fixture.with_member(allowed, [300])
    |> fixture.with_member(denied, [400])
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let reader = fake.start(allowed)
  let outsider = fake.start(denied)
  assert fake.is_joined(fake.join(reader, handle))
  assert fake.is_joined(fake.join(outsider, handle))
  let _ = fake.drain(reader, 50)
  let _ = fake.drain(outsider, 50)
  fixture.dispatch(handle, "MESSAGE_CREATE", private_message(1))
  assert seen_ids(fake.drain(reader, 100)) == ["1"]
  assert seen_ids(fake.drain(outsider, 100)) == []
  let sink = start_sink()
  let frozen = freeze(guild, 5)
  let arrival = install(guild, frozen)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  let _ = fake.drain(reader, 100)
  let _ = fake.drain(outsider, 100)
  signal.repointed(frozen.stub, reader.ref.id)
  signal.repointed(frozen.stub, outsider.ref.id)
  let #(entries, last) = forwarded_until_sealed(sink, [])
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, last)
  let _ = fake.drain(reader, 150)
  let _ = fake.drain(outsider, 150)
  fixture.dispatch(target, "MESSAGE_CREATE", private_message(2))
  let delivered = #(
    seen_ids(fake.drain(reader, 100)),
    seen_ids(fake.drain(outsider, 100)),
  )
  assert delivered == #(["2"], [])
  stop_sink(sink)
  fixture.stop(found)
}
