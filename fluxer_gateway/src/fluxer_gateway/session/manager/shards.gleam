// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/session.{
  type Identify, type Peer, type ResumeError, type SessionId, type SessionRef,
  type SocketRef, type StartError,
}
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/seed.{type Seed}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Down, type Name, type Pid, type Subject}

pub type Message {
  StartSession(
    session_id: SessionId,
    identify: Identify,
    peer: Peer,
    socket: SocketRef,
    reply: Subject(Result(SessionRef, StartError)),
    settled: Subject(Nil),
  )
  ResumeSession(
    session_id: String,
    token: String,
    seq: Int,
    socket: SocketRef,
    reply: Subject(Result(SessionRef, ResumeError)),
    settled: Subject(Nil),
  )
  Adopt(seed: Seed, reply: Subject(Result(SessionRef, Nil)))
  Fetched(token: Int, result: Result(Seed, StartError))
  NotFoundElsewhere(
    session_id: String,
    token: String,
    seq: Int,
    socket: SocketRef,
    reply: Subject(Result(SessionRef, ResumeError)),
    settled: Subject(Nil),
  )
  Watched(Down)
}

pub type Delivery {
  Settled
  NoShard
  ShardDown
  Unanswered
}

pub fn count() -> Int {
  case system_info(atom.create("schedulers_online")) {
    count if count > 0 -> count
    _ -> 1
  }
}

@external(erlang, "erlang", "system_info")
fn system_info(item: Atom) -> Int

@external(erlang, "erlang", "phash2")
fn phash2(term: String, range: Int) -> Int

pub fn index(session_id: String) -> Int {
  phash2(session_id, count())
}

@external(erlang, "erlang", "send")
fn raw_send(
  pid: Pid,
  message: #(Name(Message), Message),
) -> #(Name(Message), Message)

pub fn whereis(session_id: String) -> Result(Pid, Nil) {
  process.named(names.shard(index(session_id)))
}

pub fn send_to(pid: Pid, session_id: String, message: Message) -> Nil {
  raw_send(pid, #(names.shard(index(session_id)), message))
  Nil
}

pub fn send(session_id: String, message: Message) -> Result(Nil, Nil) {
  case whereis(session_id) {
    Ok(pid) -> {
      send_to(pid, session_id, message)
      Ok(Nil)
    }
    Error(Nil) -> Error(Nil)
  }
}

pub fn deliver(
  session_id: String,
  message: fn(Subject(Nil)) -> Message,
) -> Delivery {
  case whereis(session_id) {
    Error(Nil) -> NoShard
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      let settled = process.new_subject()
      send_to(pid, session_id, message(settled))
      let outcome =
        process.new_selector()
        |> process.select_map(settled, fn(_) { Settled })
        |> process.select_specific_monitor(monitor, fn(_) { ShardDown })
        |> process.selector_receive(durations.session().manager_start_timeout)
      process.demonitor_process(monitor)
      case outcome {
        Ok(delivery) -> delivery
        Error(Nil) -> Unanswered
      }
    }
  }
}
