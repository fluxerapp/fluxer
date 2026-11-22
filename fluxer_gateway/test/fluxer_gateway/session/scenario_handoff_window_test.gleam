// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{SessionDurations}
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{Dispatch, ReconnectFrame}
import fluxer_gateway/session/control
import fluxer_gateway/session/manager/registry
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option
import gleeunit/should
import support/isolation
import support/session_harness.{Frame, Other} as h
import support/session_transfer_tap as tap

fn message(content: String) -> raw_json.RawJson {
  raw_json.from_json(json.object([#("content", json.string(content))]))
}

fn api() -> h.Responder {
  h.session_api(h.session_data(h.unique_user(), False, []))
}

fn reconnected(frames: List(h.Frame)) -> Bool {
  list.contains(frames, Other(ReconnectFrame))
}

fn stand_in(ref: session.SessionRef) -> session.SessionRef {
  session.SessionRef(..ref, inbox: process.new_subject())
}

fn hand_off(ref: session.SessionRef) -> process.Subject(Bool) {
  let answer = process.new_subject()
  control.send(ref, control.HandOff(node.self(), answer))
  answer
}

pub fn source_serves_until_the_target_confirms_adoption_test() {
  use <- h.with_api(api())
  let #(ref, socket, _) = h.ready_session("tok-w1")
  let source = h.session_pid(ref)
  let key = session.session_id_to_string(ref.id)
  let transfer = tap.start()
  let answer = hand_off(ref)
  let assert Ok(#(exported, adopted)) = tap.committing(transfer, 2000)
  exported.id |> should.equal(ref.id)

  process.sleep(150)
  process.is_alive(source) |> should.be_true
  registry.lookup(key) |> should.equal(Ok(ref))
  h.collect(socket, 50) |> reconnected |> should.be_false
  process.receive(answer, 0) |> should.equal(Error(Nil))

  process.send(adopted, Ok(stand_in(ref)))
  process.receive(answer, 2000) |> should.equal(Ok(True))
  h.await_frame(socket, 2000, fn(frame) { frame == h.Rebound(exported.seq) })
  |> should.be_ok
  h.collect(socket, 50) |> reconnected |> should.be_false
  h.await_exit(source, 2000) |> should.be_true
  tap.release(transfer)
}

pub fn refused_adoption_keeps_the_session_serving_test() {
  use <- h.with_api(api())
  let #(ref, socket, _) = h.ready_session("tok-w2")
  let source = h.session_pid(ref)
  let transfer = tap.start()
  let answer = hand_off(ref)
  let assert Ok(#(_, adopted)) = tap.committing(transfer, 2000)
  process.send(adopted, Error(Nil))
  process.receive(answer, 2000) |> should.equal(Ok(False))
  tap.release(transfer)

  session.send(ref, Dispatch("MESSAGE_CREATE", message("still here")))
  let assert Ok(Frame(_, "MESSAGE_CREATE", _)) =
    h.await_event(socket, "MESSAGE_CREATE", 2000)
  h.collect(socket, 100) |> reconnected |> should.be_false
  process.is_alive(source) |> should.be_true
  h.state_of(ref).fenced |> should.be_false
  h.terminate(ref)
}

fn short_commit() -> Nil {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      session: SessionDurations(
        ..d.session,
        transfer_push_timeout_ms: 400,
        transfer_locate_timeout_ms: 300,
      ),
    )
  })
}

fn unanswered_commit(
  token: String,
  abort_answer: Result(Bool, Nil),
) -> #(session.SessionRef, h.FakeSocket, Result(Bool, Nil)) {
  short_commit()
  use <- h.with_api(api())
  let #(ref, socket, _) = h.ready_session(token)
  let transfer = tap.start()
  let started = h.now_ms()
  let answer = hand_off(ref)
  let assert Ok(#(_, _ignored)) = tap.committing(transfer, 2000)
  let assert Ok(abort) = tap.aborting(transfer, 2000)
  case abort_answer {
    Ok(aborted) -> process.send(abort, aborted)
    Error(Nil) -> Nil
  }
  let outcome = process.receive(answer, 2000)
  { h.now_ms() - started < 1500 } |> should.be_true
  tap.release(transfer)
  #(ref, socket, outcome)
}

pub fn unanswered_commit_and_abort_fence_after_the_deadline_test() {
  let #(ref, socket, outcome) = unanswered_commit("tok-w3", Error(Nil))
  outcome |> should.equal(Ok(True))
  h.await_frame(socket, 2000, fn(frame) { frame == Other(ReconnectFrame) })
  |> should.be_ok
  h.await_exit(h.session_pid(ref), 2000) |> should.be_true
}

pub fn commit_consumed_by_the_target_fences_the_source_test() {
  let #(ref, socket, outcome) = unanswered_commit("tok-w5", Ok(False))
  outcome |> should.equal(Ok(True))
  h.await_frame(socket, 2000, fn(frame) { frame == Other(ReconnectFrame) })
  |> should.be_ok
  h.await_exit(h.session_pid(ref), 2000) |> should.be_true
}

pub fn lost_commit_is_aborted_and_the_source_keeps_serving_test() {
  let #(ref, socket, outcome) = unanswered_commit("tok-w6", Ok(True))
  outcome |> should.equal(Ok(False))
  h.collect(socket, 100) |> reconnected |> should.be_false
  session.send(ref, Dispatch("MESSAGE_CREATE", message("kept")))
  h.await_event(socket, "MESSAGE_CREATE", 2000) |> should.be_ok
  h.state_of(ref).fenced |> should.be_false
  h.terminate(ref)
}

pub fn messages_queued_during_the_commit_wait_reach_the_target_test() {
  use <- h.with_api(api())
  let #(ref, socket, _) = h.ready_session("tok-w4")
  session.send(ref, Dispatch("MESSAGE_CREATE", message("before")))
  let assert Ok(Frame(before, _, _)) =
    h.await_event(socket, "MESSAGE_CREATE", 2000)
  let transfer = tap.start()
  let answer = hand_off(ref)
  let assert Ok(#(exported, adopted)) = tap.committing(transfer, 2000)
  session.send(ref, Dispatch("MESSAGE_CREATE", message("late")))
  session.send(ref, session.SocketOp(session.Heartbeat(option.None)))
  let terminate_reply = process.new_subject()
  control.send(ref, control.TerminateMatching(["aGFzaA"], terminate_reply))
  let second = hand_off(ref)
  let target = stand_in(ref)
  process.send(adopted, Ok(target))
  process.receive(answer, 2000) |> should.equal(Ok(True))
  process.receive(second, 2000) |> should.equal(Ok(False))
  exported.seq |> should.equal(before)
  let frames = h.collect(socket, 200)
  frames |> h.events |> list.contains("MESSAGE_CREATE") |> should.be_false
  frames |> list.contains(h.Rebound(before)) |> should.be_true
  frames |> reconnected |> should.be_false
  let relayed = control.subject(process.self())
  process.receive(relayed, 1000)
  |> should.equal(
    Ok(control.Relayed(Dispatch("MESSAGE_CREATE", message("late")))),
  )
  process.receive(relayed, 1000)
  |> should.equal(
    Ok(control.Relayed(session.SocketOp(session.Heartbeat(option.None)))),
  )
  let assert Ok(control.RelayedControl(control.TerminateMatching(["aGFzaA"], _))) =
    process.receive(relayed, 1000)
  let assert Ok(control.Reattach(_, reattach_seq)) =
    process.receive(relayed, 2000)
  reattach_seq |> should.equal(before)
  let assert Ok(control.MoveDone([], _)) = process.receive(relayed, 2000)
  process.receive(target.inbox, 100) |> should.equal(Error(Nil))
  tap.release(transfer)
}
