// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key.{Key}
import fluxer_gateway/config/settings.{Durations, VoiceDurations}
import fluxer_gateway/core/id
import fluxer_gateway/session/state.{type State, Lost, Watched}
import fluxer_gateway/voice/call/handoff/adapter as call_adapter
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/state as call_state
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/option.{None}
import gleeunit/should
import support/isolation
import support/move_sink as sink
import support/session_harness.{Frame} as h

fn dm_api(user: Int, partner: Int, channel: Int) -> h.Responder {
  h.session_api(
    h.session_data_with(user, False, [], [
      #(
        "private_channels",
        json.preprocessed_array([
          json.object([
            #("id", json.string(int.to_string(channel))),
            #("type", json.int(1)),
            #(
              "recipients",
              json.preprocessed_array([h.user_json(partner, False)]),
            ),
          ]),
        ]),
      ),
    ]),
  )
}

fn start_call(channel: Int, user: Int) -> Pid {
  let assert Ok(channel_id) = id.channel_id(channel)
  let assert Ok(message_id) = id.message_id(channel + 1)
  let assert Ok(user_id) = id.user_id(user)
  let assert Ok(call) =
    manager.create(
      channel_id,
      call_state.Init(
        channel: channel_id,
        message: message_id,
        region: None,
        ringing: [],
        recipients: [user_id],
        caller: None,
      ),
    )
  let assert Ok(pid) = process.subject_owner(call)
  pid
}

fn slot(state: State, channel: Int) -> Result(state.CallSlot, Nil) {
  dict.get(state.calls, channel)
}

fn channel_of(payload) -> Result(String, Nil) {
  decode.run(payload, decode.at(["channel_id"], decode.string))
  |> option_result
}

fn option_result(result: Result(a, b)) -> Result(a, Nil) {
  case result {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
  }
}

pub fn ready_announces_live_dm_call_and_watches_it_test() {
  let user = h.unique_user()
  let channel = h.unique_user()
  let call = start_call(channel, user)
  use <- h.with_api(dm_api(user, h.unique_user(), channel))
  let #(ref, socket, _) = h.ready_session("tok-c1")
  let assert Ok(Frame(_, "CALL_CREATE", payload)) =
    h.await_event(socket, "CALL_CREATE", 3000)
  channel_of(payload) |> should.equal(Ok(int.to_string(channel)))
  h.wait_until(
    fn() {
      case slot(h.state_of(ref), channel) {
        Ok(Watched(watched, _)) -> watched == call
        _ -> False
      }
    },
    2000,
  )
  |> should.be_true
  h.terminate(ref)
  process.kill(call)
}

pub fn crashed_call_is_reported_and_rewatched_when_back_test() {
  let user = h.unique_user()
  let channel = h.unique_user()
  let call = start_call(channel, user)
  use <- h.with_api(dm_api(user, h.unique_user(), channel))
  let #(ref, socket, _) = h.ready_session("tok-c2")
  let assert Ok(_) = h.await_event(socket, "CALL_CREATE", 3000)
  h.wait_until(fn() { slot(h.state_of(ref), channel) |> is_watched }, 2000)
  |> should.be_true

  process.kill(call)
  let assert Ok(Frame(_, "CALL_DELETE", deleted)) =
    h.await_event(socket, "CALL_DELETE", 2000)
  decode.run(deleted, decode.at(["unavailable"], decode.bool))
  |> should.equal(Ok(True))
  slot(h.state_of(ref), channel) |> should.equal(Ok(Lost))

  let again = start_call(channel, user)
  h.wait_until(
    fn() { watched_pid(h.state_of(ref), channel) == Ok(again) },
    3000,
  )
  |> should.be_true
  h.terminate(ref)
  process.kill(again)
}

pub fn handed_off_call_is_rewatched_without_delete_test() {
  let user = h.unique_user()
  let channel = h.unique_user()
  let call = start_call(channel, user)
  use <- h.with_api(dm_api(user, h.unique_user(), channel))
  let #(ref, socket, _) = h.ready_session("tok-c4")
  let assert Ok(_) = h.await_event(socket, "CALL_CREATE", 3000)
  h.wait_until(fn() { slot(h.state_of(ref), channel) |> is_watched }, 2000)
  |> should.be_true
  h.exit_handoff(call)
  h.wait_until(fn() { slot(h.state_of(ref), channel) == Ok(Lost) }, 1000)
  |> should.be_true
  let again = start_call(channel, user)
  h.wait_until(
    fn() { watched_pid(h.state_of(ref), channel) == Ok(again) },
    2500,
  )
  |> should.be_true
  h.await_event(socket, "CALL_DELETE", 200) |> should.be_error
  h.terminate(ref)
  process.kill(again)
}

pub fn a_moved_call_is_rewatched_without_any_call_frame_test() {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      voice: VoiceDurations(..d.voice, server_forward_grace_ms: 200),
    )
  })
  let user = h.unique_user()
  let channel = h.unique_user()
  let call = start_call(channel, user)
  use <- h.with_api(dm_api(user, h.unique_user(), channel))
  let #(ref, socket, _) = h.ready_session("tok-c5")
  let assert Ok(_) = h.await_event(socket, "CALL_CREATE", 3000)
  h.wait_until(fn() { watched_pid(h.state_of(ref), channel) == Ok(call) }, 2000)
  |> should.be_true
  let moves = sink.start()
  let entity = Key(key.call, channel)
  let assert Ok(frozen) =
    call_adapter.freeze(entity, sink.move_id(11), 1, sink.at(5))
  let assert Ok(arrival) =
    call_adapter.install(entity, sink.source(), sink.at(6), frozen.tail)
  let assert Ok(Nil) =
    call_adapter.committed(frozen.stub, arrival.target_ref, sink.at(6))
  let assert [] = sink.forwarded_until_sealed(moves)
  let assert Ok(Nil) = call_adapter.seal(arrival.entity, 0)
  h.wait_until(
    fn() { watched_pid(h.state_of(ref), channel) == Ok(arrival.entity) },
    3000,
  )
  |> should.be_true
  process.is_alive(call) |> should.be_false
  h.await_event(socket, "CALL_CREATE", 300) |> should.be_error
  h.await_event(socket, "CALL_DELETE", 100) |> should.be_error
  sink.stop(moves)
  h.terminate(ref)
  process.kill(arrival.entity)
}

fn watched_pid(state: State, channel: Int) -> Result(Pid, Nil) {
  case slot(state, channel) {
    Ok(Watched(pid, _)) -> Ok(pid)
    _ -> Error(Nil)
  }
}

fn is_watched(found: Result(state.CallSlot, Nil)) -> Bool {
  case found {
    Ok(Watched(_, _)) -> True
    _ -> False
  }
}
