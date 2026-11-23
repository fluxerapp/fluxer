// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/otp/actor
import gleam/result

fn init_timeout_ms() -> Int {
  durations.guild().broadcast_init_timeout_ms
}

const max_mailbox = 500

pub opaque type Broadcaster {
  Broadcaster(pid: Pid, inbox: Subject(Message))
}

type Message {
  Batch(sessions: List(SessionRef), message: SessionInbox)
  Sync(reply: Subject(Nil))
  OwnerDown
}

pub fn start(owner: Pid) -> Result(Broadcaster, Nil) {
  actor.new_with_initialiser(init_timeout_ms(), fn(subject) {
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select_specific_monitor(process.monitor(owner), fn(_) {
        OwnerDown
      })
    actor.initialised(Nil)
    |> actor.selecting(selector)
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.on_message(fn(state, message) {
    case message {
      Batch(sessions:, message:) -> {
        relay.send(sessions, message)
        actor.continue(state)
      }
      Sync(reply) -> {
        process.send(reply, Nil)
        actor.continue(state)
      }
      OwnerDown -> actor.stop()
    }
  })
  |> actor.start
  |> result.map(fn(started) { Broadcaster(started.pid, started.data) })
  |> result.replace_error(Nil)
}

pub fn flush(broadcaster: Broadcaster) -> Nil {
  let reply = process.new_subject()
  let monitor = process.monitor(broadcaster.pid)
  process.send(broadcaster.inbox, Sync(reply))
  let _ =
    process.new_selector()
    |> process.select(reply)
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(flush_timeout_ms)
  process.demonitor_process(monitor)
  relay.settle(flush_timeout_ms)
}

const flush_timeout_ms = 1000

pub fn is_alive(broadcaster: Broadcaster) -> Bool {
  process.is_alive(broadcaster.pid)
}

pub fn cast(
  broadcaster: Broadcaster,
  sessions: List(SessionRef),
  message: SessionInbox,
) -> Bool {
  case mailbox_len(broadcaster.pid) {
    Ok(length) if length < max_mailbox -> {
      force(broadcaster, sessions, message)
      True
    }
    _ -> False
  }
}

pub fn force(
  broadcaster: Broadcaster,
  sessions: List(SessionRef),
  message: SessionInbox,
) -> Nil {
  process.send(broadcaster.inbox, Batch(sessions:, message:))
}

fn mailbox_len(pid: Pid) -> Result(Int, Nil) {
  process_info(pid, atom.create("message_queue_len"))
  |> decode.run(decode.at([1], decode.int))
  |> result.replace_error(Nil)
}

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> Dynamic
