// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Dnd, Idle, Offline, Online}
import fluxer_gateway/handoff/adapter.{type Arrival, type Frozen}
import fluxer_gateway/handoff/move
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client
import fluxer_gateway/presence/handoff/adapter as presence_adapter
import fluxer_gateway/presence/handoff/signal
import fluxer_gateway/session/control
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{None}
import gleam/result
import support/isolation
import support/presence_scenario as scn

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
  process.kill(sink.pid)
  let _ = scn.wait_down(sink.pid, 1000)
  Nil
}

fn short_timers() -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      presence: settings.PresenceDurations(
        ..d.presence,
        server_seal_wait_ms: 300,
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
    source: WorkerId(node: atom.create("presence_move_source"), incarnation: 1),
    serial:,
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

fn freeze(user: Int, serial: Int) -> Frozen {
  let assert Ok(frozen) =
    presence_adapter.freeze(Key(key.presence, user), move_id(serial), 1, at(5))
  frozen
}

fn install(user: Int, frozen: Frozen) -> Arrival {
  let assert Ok(arrival) =
    presence_adapter.install(
      Key(key.presence, user),
      move_id(0).source,
      at(6),
      frozen.tail,
    )
  arrival
}

fn complete(sink: Sink, frozen: Frozen, arrival: Arrival) -> Int {
  let assert Ok(Nil) =
    presence_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  let #(entries, last) = forwarded_until_sealed(sink, [])
  list.each(entries, fn(entry) {
    let assert Ok(Nil) =
      presence_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = presence_adapter.seal(arrival.entity, last)
  last
}

fn statuses(observer, user: Int, quiet_ms: Int) -> List(presence.Status) {
  scn.presences(observer, user, quiet_ms) |> list.map(presence.status)
}

fn cached(user: Int) -> List(presence.Status) {
  cache.lookup_many(packed_ids.from_list([user])) |> list.map(presence.status)
}

fn rebinds(seen: List(scn.Seen)) -> List(#(Subject(_), Pid)) {
  list.filter_map(seen, fn(found) {
    case found {
      scn.Notice(control.PresenceRebind(to, stub)) -> Ok(#(to, stub))
      _ -> Error(Nil)
    }
  })
}

pub fn committed_move_applies_frozen_updates_on_target_without_offline_test() {
  short_timers()
  let user = 7_910_000_000_000_001
  let guild = 7_910_000_000_000_901
  let sink = start_sink()
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  assert statuses(observer, user, 150) == [Online]
  let _ = scn.drain_seen(session, 50)
  let frozen = freeze(user, 1)
  client.session_update(link, Idle, False, False, None)
  client.session_update(link, Dnd, False, False, None)
  scn.sync(user)
  assert statuses(observer, user, 100) == []
  let arrival = install(user, frozen)
  assert arrival.entity != frozen.stub
  let last = complete(sink, frozen, arrival)
  assert last == 2
  assert scn.presence_pid(user) == Ok(arrival.entity)
  let seen = statuses(observer, user, 200)
  assert list.last(seen) == Ok(Dnd)
  assert !list.contains(seen, Offline)
  assert cached(user) == [Dnd]
  let assert [#(to, stub)] = rebinds(scn.drain_seen(session, 100))
  assert stub == frozen.stub
  assert process.subject_owner(to) == Ok(arrival.entity)
  assert scn.wait_down(frozen.stub, 2000)
  assert process.is_alive(arrival.entity)
  assert statuses(observer, user, 150) == []
  stop_sink(sink)
  scn.exit_session(session, scn.closed_reason())
  assert scn.wait_down(arrival.entity, 2000)
  assert statuses(observer, user, 200) == [Offline]
  scn.stop_observing(guild)
}

pub fn every_session_ack_seals_without_waiting_test() {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      presence: settings.PresenceDurations(
        ..d.presence,
        server_seal_wait_ms: 60_000,
        server_forward_grace_ms: 300,
      ),
    )
  })
  let user = 7_910_000_000_000_002
  let sink = start_sink()
  let first = scn.fake_session(user, 1)
  let second = scn.fake_session(user, 2)
  let _ = scn.connect(first, Online, [])
  let _ = scn.connect(second, Online, [])
  let frozen = freeze(user, 2)
  let arrival = install(user, frozen)
  let assert Ok(Nil) =
    presence_adapter.committed(frozen.stub, arrival.target_ref, at(6))
  assert process.receive(sink.seen, 200) |> result.is_error
  list.each([first, second], fn(fake) {
    let assert [#(_, stub)] = rebinds(scn.drain_seen(fake, 100))
    signal.repointed(stub, fake.ref.id)
  })
  let assert Ok(move.Repointed(..)) = process.receive(sink.seen, 1000)
  let assert Ok(Nil) = presence_adapter.seal(arrival.entity, 0)
  assert scn.presence_pid(user) == Ok(arrival.entity)
  stop_sink(sink)
}

pub fn aborted_move_thaws_source_with_held_input_in_order_test() {
  short_timers()
  let user = 7_910_000_000_000_003
  let guild = 7_910_000_000_000_903
  let sink = start_sink()
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  assert statuses(observer, user, 150) == [Online]
  let frozen = freeze(user, 3)
  client.session_update(link, Idle, False, False, None)
  client.session_update(link, Dnd, False, False, None)
  let arrival = install(user, frozen)
  presence_adapter.abort_arrival(arrival.entity)
  assert scn.wait_down(arrival.entity, 1000)
  let assert Ok(Nil) = presence_adapter.thaw(frozen.stub, [])
  scn.sync(user)
  assert scn.presence_pid(user) == Ok(frozen.stub)
  let seen = statuses(observer, user, 200)
  assert list.last(seen) == Ok(Dnd)
  assert !list.contains(seen, Offline)
  assert cached(user) == [Dnd]
  assert rebinds(scn.drain_seen(session, 50)) == []
  assert presence_adapter.thaw(frozen.stub, []) |> result.is_error
  stop_sink(sink)
  scn.exit_session(session, scn.closed_reason())
  assert scn.wait_down(frozen.stub, 2000)
  scn.stop_observing(guild)
}

pub fn freeze_refuses_missing_and_frozen_presences_test() {
  let user = 7_910_000_000_000_004
  let sink = start_sink()
  assert presence_adapter.freeze(Key(key.presence, user), move_id(4), 1, at(5))
    == Error(adapter.EntityMissing(Key(key.presence, user)))
  let session = scn.fake_session(user, 1)
  let _ = scn.connect(session, Online, [])
  let frozen = freeze(user, 4)
  assert presence_adapter.freeze(Key(key.presence, user), move_id(5), 1, at(5))
    == Error(adapter.EntityBusy(Key(key.presence, user)))
  assert presence_adapter.install(
      Key(key.presence, user + 1),
      move_id(0).source,
      at(6),
      frozen.tail,
    )
    |> result.is_error
  let assert Ok(Nil) = presence_adapter.thaw(frozen.stub, [])
  assert presence_adapter.entities_in(key.bucket(user)) |> list.contains(user)
  stop_sink(sink)
  scn.exit_session(session, scn.closed_reason())
}

pub fn messages_reaching_the_stub_after_seal_go_straight_to_target_test() {
  short_timers()
  let user = 7_910_000_000_000_005
  let guild = 7_910_000_000_000_905
  let sink = start_sink()
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  assert statuses(observer, user, 150) == [Online]
  let frozen = freeze(user, 6)
  let arrival = install(user, frozen)
  let _ = complete(sink, frozen, arrival)
  client.session_update(link, Idle, False, False, None)
  let seen = statuses(observer, user, 200)
  assert list.last(seen) == Ok(Idle)
  assert scn.presence_pid(user) == Ok(arrival.entity)
  stop_sink(sink)
  scn.exit_session(session, scn.closed_reason())
  assert scn.wait_down(arrival.entity, 2000)
  scn.stop_observing(guild)
}

pub fn a_fenced_stub_stops_without_publishing_offline_test() {
  let user = 7_910_000_000_000_006
  let guild = 7_910_000_000_000_906
  let sink = start_sink()
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let _ = scn.connect(session, Online, [])
  assert statuses(observer, user, 150) == [Online]
  let frozen = freeze(user, 7)
  presence_adapter.abort_arrival(frozen.stub)
  assert scn.wait_down(frozen.stub, 1000)
  assert statuses(observer, user, 200) == []
  assert scn.presence_inbox(user) |> result.is_error
  stop_sink(sink)
  scn.stop_observing(guild)
}

pub fn a_freeze_that_answers_after_the_mover_gave_up_does_not_stay_frozen_test() {
  short_timers()
  let user = 7_910_000_000_000_011
  let guild = 7_910_000_000_000_911
  let sink = start_sink()
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  assert statuses(observer, user, 150) == [Online]
  let assert Ok(pid) = scn.presence_pid(user)
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      presence: settings.PresenceDurations(
        ..d.presence,
        server_handoff_timeout_ms: 50,
      ),
    )
  })
  assert suspend(pid)
  let assert Error(_) =
    presence_adapter.freeze(Key(key.presence, user), move_id(11), 1, at(5))
  assert resume(pid)
  client.session_update(link, Idle, False, False, None)
  assert statuses(observer, user, 300) == [Idle]
  stop_sink(sink)
  scn.exit_session(session, scn.closed_reason())
  assert scn.wait_down(pid, 2000)
  scn.stop_observing(guild)
}
