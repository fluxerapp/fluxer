// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key.{Key}
import fluxer_gateway/config/settings.{Durations, VoiceDurations}
import fluxer_gateway/core/raw_json
import fluxer_gateway/handoff/adapter.{type Arrival, type Frozen}
import fluxer_gateway/session/control
import fluxer_gateway/voice/call/handoff/adapter as call_adapter
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/call/state as call_state
import fluxer_gateway/voice/client
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/rpc
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import support/isolation
import support/move_sink as sink
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake

@external(erlang, "erlang", "suspend_process")
fn suspend(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume(pid: Pid) -> Bool

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> #(Atom, Int)

@external(erlang, "erlang", "process_info")
fn monitors_of(pid: Pid, item: Atom) -> #(Atom, List(Pid))

fn monitored_by(pid: Pid, watcher: Pid) -> Bool {
  list.contains(monitors_of(pid, atom.create("monitored_by")).1, watcher)
}

@external(erlang, "sys", "get_state")
fn get_state(pid: Pid) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn to_entry(value: Dynamic) -> Entry

fn queued(pid: Pid) -> Int {
  process_info(pid, atom.create("message_queue_len")).1
}

fn dm_pid(session: fake.Fake) -> Pid {
  let assert Ok(subject) = dm_server.lookup(session.ref)
  let assert Ok(pid) = process.subject_owner(subject)
  pid
}

fn dm_connections(session: fake.Fake) -> List(String) {
  let assert Ok(states) =
    decode.run(
      get_state(dm_pid(session)),
      decode.at([5, 1], decode.dict(decode.dynamic, decode.dynamic)),
    )
  states |> dict.values |> list.map(to_entry) |> list.map(voice_ids.of)
}

fn short_grace() -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      voice: VoiceDurations(..d.voice, server_forward_grace_ms: 200),
    )
  })
}

fn dm_channel(recipients: List(Int)) -> api.Rule {
  api.Reply(
    api.Data(
      json.object([
        #(
          "channel",
          json.object([
            #("type", json.int(1)),
            #(
              "recipients",
              json.array(recipients, fn(user) {
                json.object([#("id", json.string(int.to_string(user)))])
              }),
            ),
          ]),
        ),
      ]),
    ),
  )
}

fn create_call(channel: Int, recipients: List(Int)) -> Nil {
  let assert Ok(_) =
    rpc.handle(
      "call.create",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("message_id", json.string(int.to_string(channel + 1000))),
          #("region", json.null()),
          #("ringing", json.array([], json.string)),
          #(
            "recipients",
            json.array(recipients, fn(user) { json.string(int.to_string(user)) }),
          ),
        ]),
      ),
    )
  Nil
}

fn view(channel: Int) -> call_state.View {
  let assert Ok(call) = manager.lookup(g.channel_id(channel))
  let assert Ok(found) = call_server.get_state(call, 2000)
  found
}

fn call_pid(channel: Int) -> Result(Pid, Nil) {
  manager.lookup(g.channel_id(channel))
  |> result.replace_error(Nil)
  |> result.try(process.subject_owner)
}

fn connection_ids(channel: Int) -> List(String) {
  case manager.lookup(g.channel_id(channel)) {
    Ok(call) ->
      case call_server.get_state(call, 2000) {
        Ok(found) -> list.map(found.connections, voice_ids.of)
        Error(Nil) -> []
      }
    Error(_) -> []
  }
}

fn send(session: fake.Fake, data: Json) -> Nil {
  client.dm_voice_update(session.ref, g.client_update(data), calls: [])
}

fn joined(session: fake.Fake, channel: Int) -> String {
  send(session, g.join(channel))
  let assert True =
    api.eventually(fn() {
      fake.dispatched(session, "VOICE_SERVER_UPDATE") != []
    })
  let assert Ok(update) =
    list.last(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert api.eventually(fn() { list.contains(connection_ids(channel), conn) })
  conn
}

fn confirm_text(channel: Int, conn: String) -> String {
  let assert Ok(reply) =
    rpc.handle(
      "call.confirm_connection",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("connection_id", json.string(conn)),
        ]),
      ),
    )
  json.to_string(reply)
}

fn key_of(channel: Int) -> key.Key {
  Key(key.call, channel)
}

fn freeze(channel: Int, serial: Int) -> Frozen {
  let assert Ok(frozen) =
    call_adapter.freeze(key_of(channel), sink.move_id(serial), 1, sink.at(5))
  frozen
}

fn install(channel: Int, frozen: Frozen) -> Arrival {
  let assert Ok(arrival) =
    call_adapter.install(
      key_of(channel),
      sink.source(),
      sink.at(6),
      frozen.tail,
    )
  arrival
}

fn complete(moves: sink.Sink, frozen: Frozen, arrival: Arrival) -> Int {
  let assert Ok(Nil) =
    call_adapter.committed(frozen.stub, arrival.target_ref, sink.at(6))
  let entries = sink.forwarded_until_sealed(moves)
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = call_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = call_adapter.seal(arrival.entity, list.length(entries))
  list.length(entries)
}

fn wait_down(pid: Pid) -> Bool {
  api.eventually(fn() { !process.is_alive(pid) })
}

fn dispatch_names(session: fake.Fake) -> List(String) {
  fake.seen(session).dispatches |> list.map(fn(pair) { pair.0 })
}

fn call_monitors(session: fake.Fake) -> List(Pid) {
  fake.controls(session)
  |> list.filter_map(fn(found) {
    case found {
      control.CallMonitor(call:, ..) -> Ok(call)
      _ -> Error(Nil)
    }
  })
}

fn regions(stub: api.Api) -> List(String) {
  api.calls_of(stub, "voice_get_token")
  |> list.filter_map(fn(found) {
    api.field(found, "rtc_region") |> option.to_result(Nil)
  })
  |> list.unique
  |> list.sort(string.compare)
}

fn server_updates(session: fake.Fake) -> Int {
  list.length(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
}

fn leave_all(channel: Int, sessions: List(fake.Fake)) -> Nil {
  list.each(sessions, fn(session) { send(session, g.update(None, None, [])) })
  assert api.eventually(fn() {
    manager.lookup(g.channel_id(channel)) == Error(manager.NotFound)
  })
  list.each(sessions, fake.stop)
}

pub fn an_active_dm_call_moves_with_no_client_visible_change_test() {
  short_grace()
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5301, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, 5301)
  let c2 = joined(two, 5301)
  assert string.contains(confirm_text(5301, c1), "\"success\":true")
  let assert Ok(call) = manager.lookup(g.channel_id(5301))
  let assert Ok(room_key) = call_server.room_key(call, 2000)
  api.settle()
  let before = view(5301)
  let seen_one = dispatch_names(one)
  let seen_two = dispatch_names(two)
  let moves = sink.start()
  let frozen = freeze(5301, 1)
  let arrival = install(5301, frozen)
  assert arrival.entity != frozen.stub
  assert complete(moves, frozen, arrival) == 0
  assert call_pid(5301) == Ok(arrival.entity)
  assert view(5301) == before
  let assert Ok(moved) = manager.lookup(g.channel_id(5301))
  assert call_server.room_key(moved, 2000) == Ok(room_key)
  assert api.eventually(fn() {
    list.contains(call_monitors(one), arrival.entity)
    && list.contains(call_monitors(two), arrival.entity)
  })
  assert wait_down(frozen.stub)
  let confirmed = confirm_text(5301, c1)
  assert string.contains(confirmed, "\"already_confirmed\":true")
  let pending = confirm_text(5301, c2)
  assert string.contains(pending, "\"success\":true")
  assert !string.contains(pending, "already_confirmed")
  api.settle()
  assert dispatch_names(one) == seen_one
  assert dispatch_names(two) == seen_two
  assert process.is_alive(arrival.entity)
  sink.stop(moves)
  leave_all(5301, [one, two])
  api.stop(stub)
}

pub fn calls_ops_held_by_the_frozen_call_apply_once_on_the_target_in_order_test() {
  short_grace()
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5302, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, 5302)
  let _ = joined(two, 5302)
  api.settle()
  let assert Ok(source) = manager.lookup(g.channel_id(5302))
  let moves = sink.start()
  let frozen = freeze(5302, 2)
  let first =
    process.spawn(fn() {
      let _ = call_server.update_region(source, Some("us-east"), 10_000)
      Nil
    })
  let held_first = sink.next_forwarded(moves)
  let second =
    process.spawn(fn() {
      let _ = call_server.update_region(source, Some("eu-west"), 10_000)
      Nil
    })
  let held_second = sink.next_forwarded(moves)
  assert regions(stub) == []
  let arrival = install(5302, frozen)
  let assert Ok(Nil) =
    call_adapter.committed(frozen.stub, arrival.target_ref, sink.at(6))
  let assert [] = sink.forwarded_until_sealed(moves)
  let assert Ok(Nil) = call_adapter.release(arrival.entity, 1, held_first)
  let assert Ok(Nil) = call_adapter.release(arrival.entity, 2, held_second)
  let assert Ok(Nil) = call_adapter.seal(arrival.entity, 2)
  assert wait_down(first) && wait_down(second)
  assert view(5302).region == Some("eu-west")
  assert api.eventually(fn() {
    server_updates(one) == 3 && server_updates(two) == 3
  })
  api.settle()
  assert regions(stub) == ["eu-west", "us-east"]
  assert server_updates(one) == 3
  assert server_updates(two) == 3
  assert call_pid(5302) == Ok(arrival.entity)
  sink.stop(moves)
  leave_all(5302, [one, two])
  api.stop(stub)
}

pub fn an_aborted_call_move_thaws_the_source_with_its_held_ops_test() {
  short_grace()
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5303, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, 5303)
  let _ = joined(two, 5303)
  api.settle()
  let assert Ok(source) = manager.lookup(g.channel_id(5303))
  let moves = sink.start()
  let frozen = freeze(5303, 3)
  let asker =
    process.spawn(fn() {
      let _ = call_server.update_region(source, Some("us-east"), 10_000)
      Nil
    })
  let arrival = install(5303, frozen)
  call_adapter.abort_arrival(arrival.entity)
  assert wait_down(arrival.entity)
  let assert Ok(Nil) = call_adapter.thaw(frozen.stub, [])
  assert wait_down(asker)
  assert call_pid(5303) == Ok(frozen.stub)
  assert view(5303).region == Some("us-east")
  assert api.eventually(fn() {
    server_updates(one) == 2 && server_updates(two) == 2
  })
  assert regions(stub) == ["us-east"]
  assert !list.contains(call_monitors(one), arrival.entity)
  assert call_adapter.thaw(frozen.stub, []) |> result.is_error
  sink.stop(moves)
  leave_all(5303, [one, two])
  api.stop(stub)
}

pub fn a_second_freeze_of_a_frozen_call_is_refused_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5304, [1, 2])
  let one = fake.start(1)
  let _ = joined(one, 5304)
  assert call_adapter.entities_in(key.bucket(5304)) |> list.contains(5304)
  let frozen = freeze(5304, 4)
  assert call_adapter.freeze(key_of(5304), sink.move_id(5), 1, sink.at(5))
    == Error(adapter.EntityBusy(key_of(5304)))
  assert call_adapter.install(
      key_of(5305),
      sink.source(),
      sink.at(6),
      frozen.tail,
    )
    |> result.is_error
  let assert Ok(Nil) = call_adapter.thaw(frozen.stub, [])
  leave_all(5304, [one])
  api.stop(stub)
}

pub fn a_freeze_that_answers_after_the_mover_gave_up_does_not_stay_frozen_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5306, [1, 2])
  let one = fake.start(1)
  let _ = joined(one, 5306)
  api.settle()
  let assert Ok(pid) = call_pid(5306)
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      voice: VoiceDurations(..d.voice, server_handoff_timeout_ms: 50),
    )
  })
  assert suspend(pid)
  let assert Error(_) =
    call_adapter.freeze(key_of(5306), sink.move_id(6), 1, sink.at(5))
  assert resume(pid)
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      voice: VoiceDurations(..d.voice, server_handoff_timeout_ms: 2000),
    )
  })
  assert call_server.is_leaving(pid) == Ok(False)
  assert call_pid(5306) == Ok(pid)
  leave_all(5306, [one])
  api.stop(stub)
}

pub fn a_join_report_that_lands_after_the_call_moved_keeps_the_dm_connection_test() {
  short_grace()
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5307, [1, 2])
  let one = fake.start(1)
  let assert Ok(call) = manager.lookup(g.channel_id(5307))
  let assert Ok(_) = call_server.export(call, 2000)
  send(one, g.join(5307))
  assert api.eventually(fn() { dm_connections(one) != [] })
  let dm = dm_pid(one)
  assert suspend(dm)
  call_server.thaw(call)
  assert api.eventually(fn() { server_updates(one) == 1 })
  let assert [conn] = connection_ids(5307)
  let moves = sink.start()
  let frozen = freeze(5307, 7)
  let arrival = install(5307, frozen)
  let _ = complete(moves, frozen, arrival)
  assert wait_down(frozen.stub)
  assert call_pid(5307) == Ok(arrival.entity)
  assert api.eventually(fn() { queued(dm) == 1 })
  assert resume(dm)
  assert api.eventually(fn() { monitored_by(arrival.entity, dm) })
  assert dm_connections(one) == [conn]
  assert connection_ids(5307) == [conn]
  sink.stop(moves)
  leave_all(5307, [one])
  api.stop(stub)
}
