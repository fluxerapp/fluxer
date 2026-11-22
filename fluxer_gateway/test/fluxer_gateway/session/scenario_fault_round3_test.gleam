// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/core/session.{
  type SessionRef, type SocketMsg, type StartError, DispatchFrame, Send,
  SocketRef,
}
import fluxer_gateway/session/control
import fluxer_gateway/session/manager
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/result
import gleeunit/should
import support/session_harness as h
import support/session_transfer_tap as tap

@external(erlang, "sys", "get_state")
fn sys_get_state(pid: Pid) -> Dynamic

@external(erlang, "erlang", "map_size")
fn map_size(map: Dynamic) -> Int

fn token_of(body: Dynamic) -> String {
  decode.run(body, decode.at(["token"], decode.string)) |> result.unwrap("")
}

fn shard_pids() -> List(Pid) {
  list.repeat(Nil, shards.count())
  |> list.index_map(fn(_, index) { index })
  |> list.filter_map(fn(index) { process.named(names.shard(index)) })
}

fn pending_starts(pid: Pid) -> Int {
  decode.run(sys_get_state(pid), decode.at([3], decode.dynamic))
  |> result.map(map_size)
  |> result.unwrap(0)
}

fn shard_with_a_pending_start() -> Result(Pid, Nil) {
  list.find(shard_pids(), fn(pid) { pending_starts(pid) > 0 })
}

fn shard_of(ref: SessionRef) -> Pid {
  let key = session.session_id_to_string(ref.id)
  let assert Ok(pid) = process.named(names.shard(shards.index(key)))
  pid
}

fn peer() -> session.Peer {
  session.Peer(ip: "127.0.0.1", api_version: 1)
}

pub fn shard_killed_during_the_start_fetch_answers_unavailable_at_once_test() {
  let user = h.unique_user()
  let fetching = process.new_subject()
  use <- h.with_api(fn(kind, body) {
    case kind, token_of(body) {
      "session", "tok-r3-slow" -> {
        process.send(fetching, Nil)
        process.sleep(3000)
      }
      _, _ -> Nil
    }
    h.session_api(h.session_data(user, False, []))(kind, body)
  })
  let socket = h.fake_socket()
  let reply = process.new_subject()
  manager.start(h.identify("tok-r3-slow"), peer(), socket.ref, reply)
  process.receive(fetching, 3000) |> should.equal(Ok(Nil))
  let assert Ok(shard) = shard_with_a_pending_start()
  let started = h.now_ms()
  process.kill(shard)
  let assert Ok(Error(session.StartUnavailable(_))) =
    process.receive(reply, 1500)
  { h.now_ms() - started < 1500 } |> should.be_true
  process.receive(reply, 3500) |> should.equal(Error(Nil))
  h.wait_until(fn() { h.user_sessions(user) == 0 }, 2000) |> should.be_true
  let #(again, _, _) = h.ready_session("tok-r3-fast")
  h.terminate(again)
}

type First {
  ReplyFirst(SessionRef)
  FrameFirst
  Silent
}

fn first_of_one_start(
  outbox: Subject(SocketMsg),
  replies: Subject(Result(SessionRef, StartError)),
) -> First {
  manager.start(
    h.identify("tok-r3-bot"),
    peer(),
    SocketRef(pid: process.self(), outbox:),
    replies,
  )
  let first =
    process.new_selector()
    |> process.select_map(replies, fn(answer) {
      case answer {
        Ok(ref) -> ReplyFirst(ref)
        Error(_) -> Silent
      }
    })
    |> process.select_map(outbox, fn(_) { FrameFirst })
    |> process.selector_receive(5000)
    |> result.unwrap(Silent)
  case first {
    ReplyFirst(ref) -> {
      let ready = await_ready(outbox, ref, h.now_ms() + 5000)
      h.terminate(ref)
      case ready {
        True -> first
        False -> Silent
      }
    }
    FrameFirst | Silent -> first
  }
}

fn await_ready(
  outbox: Subject(SocketMsg),
  ref: SessionRef,
  until: Int,
) -> Bool {
  case process.receive(outbox, int.max(until - h.now_ms(), 0)) {
    Error(Nil) -> False
    Ok(Send(sender, frames)) ->
      sender == ref.id
      && list.any(frames, fn(frame) {
        case frame {
          DispatchFrame(_, "READY", _) -> True
          _ -> False
        }
      })
      || await_ready(outbox, ref, until)
    Ok(_) -> await_ready(outbox, ref, until)
  }
}

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

fn stutter(pids: List(Pid), stop: Subject(Nil)) -> Nil {
  list.each(pids, suspend_process)
  let stopped = process.receive(stop, 3)
  list.each(pids, resume_process)
  case stopped {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      process.sleep(1)
      stutter(pids, stop)
    }
  }
}

pub fn start_reply_reaches_the_socket_before_any_frame_of_its_session_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), True, [])))
  let shards_now = shard_pids()
  let handle = process.new_subject()
  process.spawn_unlinked(fn() {
    let stop = process.new_subject()
    process.send(handle, stop)
    stutter(shards_now, stop)
  })
  let assert Ok(stop) = process.receive(handle, 1000)
  let report = process.new_subject()
  process.spawn_unlinked(fn() {
    let outbox = process.new_subject()
    let replies = process.new_subject()
    let firsts =
      list.map(list.repeat(Nil, 40), fn(_) {
        first_of_one_start(outbox, replies)
      })
    process.send(report, firsts)
  })
  let assert Ok(firsts) = process.receive(report, 60_000)
  process.send(stop, Nil)
  list.filter(firsts, fn(first) {
    case first {
      ReplyFirst(_) -> False
      FrameFirst | Silent -> True
    }
  })
  |> should.equal([])
}

pub fn shard_killed_with_a_resume_in_flight_answers_session_gone_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, _socket, seq) = h.ready_session("tok-r3-resume")
  let pid = h.session_pid(ref)
  let shard = shard_of(ref)
  h.suspend(pid)
  let answer = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(answer, h.resume(ref, "tok-r3-resume", seq, h.fake_socket()))
  })
  process.sleep(150)
  process.receive(answer, 0) |> should.equal(Error(Nil))
  process.kill(shard)
  h.await_exit(pid, 2000) |> should.be_true
  process.receive(answer, 2000) |> should.equal(Ok(Error(session.SessionGone)))
  registry.lookup(session.session_id_to_string(ref.id))
  |> should.equal(Error(Nil))
}

pub fn session_killed_mid_hand_off_answers_the_drain_at_once_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, _socket, _) = h.ready_session("tok-r3-handoff")
  let pid = h.session_pid(ref)
  let transfer = tap.start()
  let answer = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(answer, control.hand_off(ref, node.self(), 8000))
  })
  let assert Ok(_) = tap.committing(transfer, 2000)
  let started = h.now_ms()
  process.kill(pid)
  process.receive(answer, 1000) |> should.equal(Ok(False))
  { h.now_ms() - started < 1000 } |> should.be_true
  tap.release(transfer)
}

pub fn hand_off_of_a_session_that_already_died_fails_at_once_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, _socket, _) = h.ready_session("tok-r3-dead")
  let pid = h.session_pid(ref)
  process.kill(pid)
  h.await_exit(pid, 1000) |> should.be_true
  let started = h.now_ms()
  control.hand_off(ref, node.self(), 8000) |> should.be_false
  { h.now_ms() - started < 500 } |> should.be_true
}

pub fn resume_racing_a_shard_restart_never_hangs_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, _socket, seq) = h.ready_session("tok-r3-race")
  let shard = shard_of(ref)
  let answers = process.new_subject()
  list.each(list.repeat(Nil, 8), fn(_) {
    process.spawn_unlinked(fn() {
      process.send(answers, h.resume(ref, "tok-r3-race", seq, h.fake_socket()))
    })
  })
  process.kill(shard)
  list.repeat(Nil, 8)
  |> list.map(fn(_) { process.receive(answers, 3000) })
  |> list.filter(fn(outcome) {
    case outcome {
      Ok(Ok(_))
      | Ok(Error(session.SessionGone))
      | Ok(Error(session.ResumeUnavailable(_))) -> False
      _ -> True
    }
  })
  |> should.equal([])
}
