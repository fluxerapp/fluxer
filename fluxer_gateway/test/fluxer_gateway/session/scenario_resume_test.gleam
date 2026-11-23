// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{
  Dispatch, InvalidResumeSeq, ReconnectFrame, ResumeAuthFailed, SessionGone,
}
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/state.{Detached, ResumeExpired}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import support/session_harness.{Frame, Other} as h

fn message(content: String) -> raw_json.RawJson {
  raw_json.from_json(json.object([#("content", json.string(content))]))
}

fn api() -> h.Responder {
  h.session_api(h.session_data(h.unique_user(), False, []))
}

pub fn resume_within_window_replays_missed_events_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r1")
  session.send(ref, Dispatch("MESSAGE_CREATE", message("one")))
  let assert Ok(Frame(one, "MESSAGE_CREATE", _)) =
    h.await_event(socket, "MESSAGE_CREATE", 2000)
  one |> should.equal(ready_seq + 1)
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  session.send(ref, Dispatch("MESSAGE_CREATE", message("two")))
  session.send(ref, Dispatch("MESSAGE_CREATE", message("three")))

  let fresh = h.fake_socket()
  h.resume(ref, "tok-r1", one, fresh) |> should.equal(Ok(ref))
  let frames = h.collect(fresh, 300)
  frames
  |> h.events
  |> should.equal(["MESSAGE_CREATE", "MESSAGE_CREATE", "RESUMED"])
  let assert [Frame(two, _, _), Frame(three, _, _), Frame(resumed, _, _)] =
    frames
  #(two, three, resumed) |> should.equal(#(one + 1, one + 2, one + 2))

  session.send(ref, Dispatch("MESSAGE_CREATE", message("four")))
  let assert Ok(Frame(four, _, _)) =
    h.await_event(fresh, "MESSAGE_CREATE", 2000)
  four |> should.equal(one + 3)
  h.terminate(ref)
}

pub fn resume_rejects_wrong_token_and_future_seq_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r2")
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  h.resume(ref, "other", ready_seq, h.fake_socket())
  |> should.equal(Error(ResumeAuthFailed))
  h.resume(ref, "tok-r2", ready_seq + 5, h.fake_socket())
  |> should.equal(Error(InvalidResumeSeq))
  let fresh = h.fake_socket()
  h.resume(ref, "tok-r2", ready_seq, fresh) |> should.equal(Ok(ref))
  h.await_event(fresh, "RESUMED", 2000) |> should.be_ok
  h.terminate(ref)
}

pub fn resume_after_window_is_session_gone_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r3")
  let pid = h.session_pid(ref)
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  let current = h.state_of(ref)
  let assert Detached(generation) = current.socket
  process.send(current.internal, ResumeExpired(generation - 1))
  process.sleep(50)
  process.is_alive(pid) |> should.be_true
  process.send(current.internal, ResumeExpired(generation))
  h.await_exit(pid, 2000) |> should.be_true
  h.wait_until(
    fn() { registry.lookup(session.session_id_to_string(ref.id)) == Error(Nil) },
    2000,
  )
  |> should.be_true
  h.resume(ref, "tok-r3", ready_seq, h.fake_socket())
  |> should.equal(Error(SessionGone))
}

pub fn resume_onto_live_socket_reconnects_the_old_one_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r4")
  let fresh = h.fake_socket()
  h.resume(ref, "tok-r4", ready_seq, fresh) |> should.equal(Ok(ref))
  h.await_frame(socket, 2000, fn(frame) { frame == Other(ReconnectFrame) })
  |> should.be_ok
  h.await_event(fresh, "RESUMED", 2000) |> should.be_ok
  h.drop_socket(socket)
  process.sleep(50)
  h.is_detached(ref) |> should.be_false
  h.terminate(ref)
}

pub fn expired_timer_of_old_generation_spares_resumed_session_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r5")
  let pid = h.session_pid(ref)
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  let detached = h.state_of(ref)
  let assert Detached(generation) = detached.socket
  let fresh = h.fake_socket()
  h.resume(ref, "tok-r5", ready_seq, fresh) |> should.equal(Ok(ref))
  process.send(detached.internal, ResumeExpired(generation))
  process.send(detached.internal, state.OfflineGrace(generation))
  process.sleep(100)
  process.is_alive(pid) |> should.be_true
  h.state_of(ref).status |> should.not_equal(h.offline())
  h.terminate(ref)
}

pub fn client_close_ends_session_at_once_test() {
  use <- h.with_api(api())
  let #(ref, socket, _) = h.ready_session("tok-r6")
  let pid = h.session_pid(ref)
  h.close_socket_by_client(socket)
  h.await_exit(pid, 2000) |> should.be_true
}

pub fn resume_queued_behind_expiry_answers_session_gone_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r7")
  let pid = h.session_pid(ref)
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  let current = h.state_of(ref)
  let assert Detached(generation) = current.socket
  h.suspend(pid)
  process.send(current.internal, ResumeExpired(generation))
  let reply = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(reply, h.resume(ref, "tok-r7", ready_seq, h.fake_socket()))
  })
  process.sleep(100)
  h.resume_process(pid)
  let started = h.now_ms()
  process.receive(reply, 5000) |> should.equal(Ok(Error(SessionGone)))
  { h.now_ms() - started < 2000 } |> should.be_true
}

pub fn status_goes_offline_after_grace_and_returns_on_resume_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r8")
  let assert Ok(dnd) =
    json.parse(
      json.to_string(json.object([#("status", json.string("dnd"))])),
      decode.dynamic,
    )
  session.send(ref, session.SocketOp(session.PresenceUpdate(dnd)))
  h.wait_until(fn() { h.state_of(ref).status == presence.Dnd }, 1000)
  |> should.be_true
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  let detached = h.state_of(ref)
  let assert Detached(generation) = detached.socket
  process.send(detached.internal, state.OfflineGrace(generation))
  h.wait_until(fn() { h.state_of(ref).status == presence.Offline }, 1000)
  |> should.be_true
  h.state_of(ref).resume_status |> should.equal(presence.Dnd)
  h.resume(ref, "tok-r8", ready_seq, h.fake_socket()) |> should.equal(Ok(ref))
  h.state_of(ref).status |> should.equal(presence.Dnd)
  h.terminate(ref)
}

pub fn heartbeat_ack_trims_replay_and_blocks_older_resume_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r9")
  session.send(ref, Dispatch("MESSAGE_CREATE", message("one")))
  session.send(ref, Dispatch("MESSAGE_CREATE", message("two")))
  let assert Ok(Frame(_, _, _)) = h.await_event(socket, "MESSAGE_CREATE", 1000)
  let assert Ok(Frame(two, _, _)) =
    h.await_event(socket, "MESSAGE_CREATE", 1000)
  session.send(ref, session.SocketOp(session.Heartbeat(option.Some(two))))
  h.await_frame(socket, 1000, fn(frame) {
    frame == Other(session.HeartbeatAckFrame)
  })
  |> should.be_ok
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  h.resume(ref, "tok-r9", ready_seq, h.fake_socket())
  |> should.equal(Error(InvalidResumeSeq))
  let fresh = h.fake_socket()
  h.resume(ref, "tok-r9", two, fresh) |> should.equal(Ok(ref))
  h.collect(fresh, 200) |> h.events |> should.equal(["RESUMED"])
  h.terminate(ref)
}

pub fn overflowed_replay_turns_old_resume_into_session_gone_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r10")
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  list.each(list.repeat(Nil, 4106), fn(_) {
    session.send(ref, Dispatch("MESSAGE_CREATE", message("x")))
  })
  let last = ready_seq + 4106
  h.wait_until(fn() { h.state_of(ref).seq == last }, 3000) |> should.be_true
  h.resume(ref, "tok-r10", ready_seq, h.fake_socket())
  |> should.equal(Error(SessionGone))
  let fresh = h.fake_socket()
  h.resume(ref, "tok-r10", last - 3, fresh) |> should.equal(Ok(ref))
  h.collect(fresh, 200)
  |> h.events
  |> should.equal([
    "MESSAGE_CREATE",
    "MESSAGE_CREATE",
    "MESSAGE_CREATE",
    "RESUMED",
  ])
  h.terminate(ref)
}

pub fn oversized_event_is_delivered_but_not_replayable_test() {
  use <- h.with_api(api())
  let #(ref, socket, ready_seq) = h.ready_session("tok-r11")
  session.send(
    ref,
    Dispatch("MESSAGE_CREATE", message(string.repeat("y", 2_200_000))),
  )
  let assert Ok(Frame(big, _, _)) =
    h.await_event(socket, "MESSAGE_CREATE", 2000)
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  h.resume(ref, "tok-r11", ready_seq, h.fake_socket())
  |> should.equal(Error(SessionGone))
  h.resume(ref, "tok-r11", big, h.fake_socket()) |> should.equal(Ok(ref))
  h.terminate(ref)
}
