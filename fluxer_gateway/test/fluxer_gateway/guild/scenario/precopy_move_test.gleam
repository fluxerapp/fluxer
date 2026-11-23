// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{type Key, Key}
import bouncer_proto/message.{type MoveId, MoveId}
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/handoff/signal
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/handoff/adapter.{type Frozen, type Precopy}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/precopy
import fluxer_gateway/session/control
import gleam/bit_array
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import support/guild_api
import support/guild_engagement
import support/guild_fixture as fixture
import support/guild_session as fake
import support/isolation

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

type Sink {
  Sink(pid: Pid, seen: Subject(move.Command))
}

fn start_sink() -> Sink {
  let seen = process.new_subject()
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      claim(process.self())
      process.send(ready, Nil)
      sink_loop(process.named_subject(move.engine_name()), seen)
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  Sink(pid:, seen:)
}

fn claim(pid: Pid) -> Nil {
  case process.register(pid, move.engine_name()) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      process.sleep(5)
      claim(pid)
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

fn start_receiver() -> Pid {
  let assert Ok(started) = precopy.start([guild_adapter.adapter()])
  process.unlink(started.pid)
  started.pid
}

fn me() -> WorkerId {
  WorkerId(node: node.to_atom(node.self()), incarnation: 1)
}

fn move_id(serial: Int) -> MoveId {
  MoveId(source: me(), serial: 7_000_000 + serial)
}

fn hooks() -> Precopy {
  let assert Some(found) = guild_adapter.adapter().precopy
  found
}

fn at(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

fn envelope(message: precopy.Envelope) -> Nil {
  process.send(process.named_subject(precopy.receiver_name()), message)
}

fn stage_bulk(entity: Key, move: MoveId) -> Subject(precopy.Ack) {
  let reply = process.new_subject()
  let assert Ok(bulk) = guild_adapter.snapshot(entity, move, 3)
  envelope(precopy.Bulk(move:, entity:, blob: bulk, reply:))
  assert process.receive(reply, 30_000) == Ok(precopy.Staged(move, 0))
  reply
}

fn ship_round(
  entity: Key,
  move: MoveId,
  round: Int,
  reply: Subject(precopy.Ack),
) -> Int {
  let assert Ok(#(blob, changes)) =
    guild_adapter.changes(entity, move, 3, round)
  envelope(precopy.Round(move:, round:, blob:, reply:))
  assert process.receive(reply, 10_000) == Ok(precopy.Staged(move, round))
  changes
}

fn member_event(user: Int, nick: String) -> json.Json {
  json.object([
    #("user", fixture.user(user)),
    #("nick", json.string(nick)),
    #("roles", json.array([], json.string)),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

fn removal(found: fixture.Guild, user: Int) -> json.Json {
  json.object([
    #("guild_id", json.string(int.to_string(found.id))),
    #("user", fixture.user(user)),
  ])
}

fn member_answers(handle: Handle, users: List(Int)) -> List(String) {
  list.map(users, fn(user) {
    let assert Ok(found) = id.user_id(user)
    case fixture.ask(handle, inbox.GetMember(found)) {
      Ok(answer) -> json.to_string(answer)
      Error(_) -> "missing"
    }
  })
}

fn message(message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(message_id))),
    #("channel_id", json.string("10")),
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

fn creates(seen: List(fake.Seen)) -> Int {
  fake.only_dispatches(seen)
  |> list.count(fn(pair) { pair.0 == "GUILD_CREATE" })
}

fn downs(seen: List(fake.Seen)) -> Int {
  list.count(seen, fn(found) {
    case found {
      fake.Inbox(session.GuildDown(..)) -> True
      _ -> False
    }
  })
}

fn rebound(seen: List(fake.Seen)) -> Bool {
  list.any(seen, fn(found) {
    case found {
      fake.Control(control.GuildRebind(..)) -> True
      _ -> False
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

fn finish_move(
  sink: Sink,
  frozen: Frozen,
  arrival: adapter.Arrival,
  sessions: List(fake.FakeSession),
) -> Nil {
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  open_target(sink, frozen, arrival, sessions)
}

fn open_target(
  sink: Sink,
  frozen: Frozen,
  arrival: adapter.Arrival,
  sessions: List(fake.FakeSession),
) -> Nil {
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

fn decoded(frozen: Frozen) -> guild_codec.Tail {
  let assert Ok(#(_, tail)) =
    handoff_codec.decode(guild_codec.tail(), frozen.tail)
  tail
}

pub fn precopied_move_carries_changes_made_during_the_copy_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
    |> fixture.with_member(4, [])
    |> fixture.with_member(5, [])
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  assert guild_engagement.engaged(handle.pid, 2)
  let receiver = start_receiver()
  let sink = start_sink()
  let move = move_id(1)
  let reply = stage_bulk(entity, move)
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(9, []))
  fixture.dispatch(handle, "GUILD_MEMBER_REMOVE", removal(found, 3))
  fixture.sync(handle)
  assert ship_round(entity, move, 1, reply) == 2
  fixture.dispatch(handle, "GUILD_MEMBER_UPDATE", member_event(4, "renamed"))
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(10, []))
  send_message(handle, 1)
  assert seen_ids(fake.drain(member, 100)) == ["1"]
  let users = [1, 2, 3, 4, 5, 9, 10]
  let expected = member_answers(handle, users)
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(5))
  let tail = decoded(frozen)
  assert tail.staged == Some(guild_codec.Staged(move:, round: 1, removed: []))
  assert list.length(tail.loaded.members) == 2
  send_message(handle, 2)
  let assert Ok(arrival) =
    guild_adapter.install(entity, me(), at(6), frozen.tail)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  assert arrival.entity != frozen.stub
  send_message(target, 3)
  finish_move(sink, frozen, arrival, [member])
  let after = fake.drain(member, 150)
  assert seen_ids(after) == ["2", "3"]
  assert creates(after) == 0
  assert downs(after) == 0
  assert rebound(after)
  assert member_answers(target, users) == expected
  assert guild_engagement.engaged(arrival.entity, 2)
  assert precopy.take(move) == Error(Nil)
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
}

pub fn staging_behind_its_tail_refuses_the_install_and_the_source_thaws_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let receiver = start_receiver()
  let sink = start_sink()
  let move = move_id(2)
  let _ = stage_bulk(entity, move)
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(9, []))
  fixture.sync(handle)
  let assert Ok(#(_, 1)) = guild_adapter.changes(entity, move, 3, 1)
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(5))
  send_message(handle, 4)
  let assert Error(_) = guild_adapter.install(entity, me(), at(6), frozen.tail)
  assert precopy.take(move) == Error(Nil)
  let assert Ok(Nil) = guild_adapter.thaw(frozen.stub, [])
  send_message(handle, 5)
  let seen = fake.drain(member, 150)
  assert seen_ids(seen) == ["4", "5"]
  assert creates(seen) == 0
  assert process.is_alive(handle.pid)
  let assert Ok(user) = id.user_id(9)
  let assert Ok(_) = fixture.ask(handle, inbox.GetMember(user))
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
}

pub fn dropped_or_cancelled_precopy_falls_back_to_a_full_tail_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let receiver = start_receiver()
  let sink = start_sink()
  let move = move_id(3)
  let _ = stage_bulk(entity, move)
  envelope(precopy.Drop(move))
  let hooks = hooks()
  hooks.cancel(entity, move)
  fixture.sync(handle)
  assert precopy.take(move) == Error(Nil)
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(5))
  let tail = decoded(frozen)
  assert tail.staged == None
  assert list.length(tail.loaded.members) == 2
  let assert Ok(Nil) = guild_adapter.thaw(frozen.stub, [])
  assert process.is_alive(handle.pid)
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
}

pub fn precopy_driver_settles_and_the_move_commits_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let receiver = start_receiver()
  let sink = start_sink()
  let move = move_id(4)
  let assert Ok(copied) =
    precopy.run(
      hooks(),
      entity,
      move,
      3,
      me(),
      precopy.default_plan(),
      clock.monotonic_ms,
    )
  assert copied.rounds == 1
  assert copied.last_changes == 0
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(5))
  let assert Some(_) = decoded(frozen).staged
  let assert Ok(arrival) =
    guild_adapter.install(entity, me(), at(6), frozen.tail)
  finish_move(sink, frozen, arrival, [])
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  let assert Ok(user) = id.user_id(2)
  let assert Ok(_) = fixture.ask(target, inbox.GetMember(user))
  let _ = handle
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
}

pub type Measured {
  Measured(
    members: Int,
    precopy_ms: Int,
    freeze_ms: Int,
    tail_bytes: Int,
    bulk_bytes: Int,
  )
}

pub fn measure(size: Int, sessions: Int, late_changes: Int) -> Measured {
  let base = fixture.new(1) |> fixture.with_text(10)
  let rows =
    int.range(from: size, to: 1, with: [], run: fn(acc, user) {
      [#(user, fixture.member(user, [])), ..acc]
    })
  let found =
    fixture.Guild(..base, members: [#(1, fixture.member(1, [])), ..rows])
  let api = guild_api.start()
  guild_api.set_page_size(api, 1000)
  let previous = settings.get()
  let _ =
    settings.store(
      settings.Settings(..previous, api_rpc_endpoint: Some(api.url)),
    )
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let joined =
    int.range(from: 2, to: sessions + 2, with: [], run: fn(acc, user) {
      let member = fake.start(user)
      assert fake.is_joined(fake.join(member, handle))
      [member, ..acc]
    })
  let receiver = start_receiver()
  let sink = start_sink()
  let move = move_id(100 + size)
  let started = clock.monotonic_ms()
  let assert Ok(copied) =
    precopy.run(
      hooks(),
      entity,
      move,
      3,
      me(),
      precopy.default_plan(),
      clock.monotonic_ms,
    )
  let precopy_ms = clock.monotonic_ms() - started
  int.range(from: 2, to: late_changes + 2, with: Nil, run: fn(_, user) {
    fixture.dispatch(
      handle,
      "GUILD_MEMBER_UPDATE",
      member_event(user, "late" <> int.to_string(user)),
    )
  })
  fixture.sync(handle)
  let probe = [1, 2, size / 2, size]
  let expected = member_answers(handle, probe)
  let frozen_at = clock.monotonic_ms()
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(5))
  let assert Ok(arrival) =
    guild_adapter.install(entity, me(), at(6), frozen.tail)
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  let freeze_ms = clock.monotonic_ms() - frozen_at
  open_target(sink, frozen, arrival, joined)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  assert member_answers(target, probe) == expected
  let measured =
    Measured(
      members: size,
      precopy_ms:,
      freeze_ms:,
      tail_bytes: bit_array.byte_size(frozen.tail),
      bulk_bytes: copied.bulk_bytes,
    )
  io.println(
    "precopy guild members="
    <> int.to_string(size)
    <> " sessions="
    <> int.to_string(sessions)
    <> " late_changes="
    <> int.to_string(late_changes)
    <> " bulk_bytes="
    <> int.to_string(measured.bulk_bytes)
    <> " precopy_ms="
    <> int.to_string(precopy_ms)
    <> " tail_bytes="
    <> int.to_string(measured.tail_bytes)
    <> " freeze_ms="
    <> int.to_string(freeze_ms),
  )
  list.each(joined, fake.stop)
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
  let _ = settings.store(previous)
  measured
}

pub fn a_150k_member_guild_freezes_well_inside_the_bound_test_() {
  fixture.slow(600, fn() {
    short_timers()
    let large = measure(150_000, 200, 500)
    assert large.freeze_ms < 1000
    assert large.tail_bytes * 20 < large.bulk_bytes
    let medium = measure(50_000, 200, 500)
    assert medium.freeze_ms < 500
  })
}

pub type RoundTrip {
  RoundTrip(members: Int, first: Measured, second: Measured)
}

fn timed_move(
  handle: Handle,
  entity: Key,
  serial: Int,
  from_epoch: Int,
  late_changes: Int,
  sink: Sink,
  joined: List(fake.FakeSession),
) -> #(Measured, Handle) {
  let move = move_id(serial)
  let started = clock.monotonic_ms()
  let assert Ok(copied) =
    precopy.run(
      hooks(),
      entity,
      move,
      3,
      me(),
      precopy.default_plan(),
      clock.monotonic_ms,
    )
  let precopy_ms = clock.monotonic_ms() - started
  int.range(from: 2, to: late_changes + 2, with: Nil, run: fn(_, user) {
    fixture.dispatch(
      handle,
      "GUILD_MEMBER_UPDATE",
      member_event(user, "late" <> int.to_string(serial + user)),
    )
  })
  fixture.sync(handle)
  let frozen_at = clock.monotonic_ms()
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(from_epoch))
  let assert Ok(arrival) =
    guild_adapter.install(entity, me(), at(from_epoch + 1), frozen.tail)
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, at(from_epoch + 1))
  let freeze_ms = clock.monotonic_ms() - frozen_at
  open_target(sink, frozen, arrival, joined)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  #(
    Measured(
      members: 0,
      precopy_ms:,
      freeze_ms:,
      tail_bytes: bit_array.byte_size(frozen.tail),
      bulk_bytes: copied.bulk_bytes,
    ),
    target,
  )
}

pub fn round_trip(size: Int, sessions: Int, late_changes: Int) -> RoundTrip {
  let base = fixture.new(1) |> fixture.with_text(10)
  let rows =
    int.range(from: size, to: 1, with: [], run: fn(acc, user) {
      [#(user, fixture.member(user, [])), ..acc]
    })
  let found =
    fixture.Guild(..base, members: [#(1, fixture.member(1, [])), ..rows])
  let api = guild_api.start()
  guild_api.set_page_size(api, 1000)
  let previous = settings.get()
  let _ =
    settings.store(
      settings.Settings(..previous, api_rpc_endpoint: Some(api.url)),
    )
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let joined =
    int.range(from: 2, to: sessions + 2, with: [], run: fn(acc, user) {
      let member = fake.start(user)
      assert fake.is_joined(fake.join(member, handle))
      [member, ..acc]
    })
  let receiver = start_receiver()
  let sink = start_sink()
  let probe = [1, 2, size / 2, size]
  assert !list.contains(member_answers(handle, probe), "missing")
  let #(first, canary) =
    timed_move(handle, entity, 300 + size, 5, late_changes, sink, joined)
  let #(second, back) =
    timed_move(canary, entity, 400 + size, 6, late_changes, sink, joined)
  assert !list.contains(member_answers(back, probe), "missing")
  let seen = list.flat_map(joined, fake.drain(_, 50))
  assert creates(seen) == 0
  assert downs(seen) == 0
  let show = fn(label: String, found: Measured) {
    label
    <> " bulk_bytes="
    <> int.to_string(found.bulk_bytes)
    <> " precopy_ms="
    <> int.to_string(found.precopy_ms)
    <> " tail_bytes="
    <> int.to_string(found.tail_bytes)
    <> " freeze_ms="
    <> int.to_string(found.freeze_ms)
  }
  io.println(
    "abort round trip members="
    <> int.to_string(size)
    <> " sessions="
    <> int.to_string(sessions)
    <> " late_changes="
    <> int.to_string(late_changes)
    <> " "
    <> show("first", first)
    <> " "
    <> show("back", second),
  )
  list.each(joined, fake.stop)
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
  let _ = settings.store(previous)
  RoundTrip(members: size, first:, second:)
}

pub fn an_aborted_canary_moves_a_150k_member_guild_back_inside_the_bound_test_() {
  fixture.slow(600, fn() {
    short_timers()
    let large = round_trip(150_000, 200, 500)
    assert large.first.freeze_ms < 1000
    assert large.second.freeze_ms < 1000
    assert large.second.tail_bytes * 20 < large.second.bulk_bytes
  })
}

pub fn a_staged_copy_nobody_takes_or_drops_is_unstaged_test() {
  short_timers()
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let assert Ok(started) =
    precopy.start_expiring([guild_adapter.adapter()], 200)
  process.unlink(started.pid)
  let move = move_id(9)
  let _ = stage_bulk(entity, move)
  hooks().cancel(entity, move)
  process.sleep(600)
  let taken = precopy.take(move)
  case taken {
    Ok(pid) -> stop_pid(pid)
    Error(Nil) -> Nil
  }
  assert taken == Error(Nil)
  stop_pid(started.pid)
  let _ = handle
  fixture.stop(found)
}

fn private_message(message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(message_id))),
    #("channel_id", json.string("20")),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

pub fn precopied_move_gates_private_channels_by_the_target_role_sets_test() {
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
    |> fixture.with_channel(20, 0, None, [
      fixture.RoleDeny(base.id, fixture.view_channel),
      fixture.RoleAllow(300, fixture.view_channel),
    ])
    |> fixture.with_member(allowed, [300])
    |> fixture.with_member(denied, [400])
  let handle = fixture.started(api, found)
  let entity = Key(key.guild, found.id)
  let reader = fake.start(allowed)
  let outsider = fake.start(denied)
  assert fake.is_joined(fake.join(reader, handle))
  assert fake.is_joined(fake.join(outsider, handle))
  let _ = fake.drain(reader, 50)
  let _ = fake.drain(outsider, 50)
  let receiver = start_receiver()
  let sink = start_sink()
  let move = move_id(7)
  let _ = stage_bulk(entity, move)
  fixture.dispatch(handle, "MESSAGE_CREATE", private_message(1))
  assert seen_ids(fake.drain(reader, 100)) == ["1"]
  assert seen_ids(fake.drain(outsider, 100)) == []
  let assert Ok(frozen) = guild_adapter.freeze(entity, move, 3, at(5))
  assert decoded(frozen).staged
    == Some(guild_codec.Staged(move:, round: 0, removed: []))
  let assert Ok(arrival) =
    guild_adapter.install(entity, me(), at(6), frozen.tail)
  let assert Ok(target) = handoff_codec.term_of(arrival.target_ref)
  let target: Handle = target
  finish_move(sink, frozen, arrival, [reader, outsider])
  let _ = fake.drain(reader, 150)
  let _ = fake.drain(outsider, 150)
  fixture.dispatch(target, "MESSAGE_CREATE", private_message(2))
  let delivered = #(
    seen_ids(fake.drain(reader, 100)),
    seen_ids(fake.drain(outsider, 100)),
  )
  assert delivered == #(["2"], [])
  stop_pid(sink.pid)
  stop_pid(receiver)
  fixture.stop(found)
}
