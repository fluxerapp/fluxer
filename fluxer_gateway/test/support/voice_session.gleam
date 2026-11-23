// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import fluxer_gateway/session/control.{type Control}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/otp/actor
import gleam/result

pub type Fake {
  Fake(ref: SessionRef, pid: Pid, log: Subject(Message))
}

pub type Seen {
  Seen(dispatches: List(#(String, Dynamic)), controls: List(Control))
}

pub opaque type Message {
  Inbox(SessionInbox)
  Controlled(Control)
  Read(reply: Subject(Seen))
}

pub fn start(user: Int) -> Fake {
  let assert Ok(user_id) = id.user_id(user)
  let assert Ok(started) =
    actor.new_with_initialiser(5000, fn(self) {
      let inbox = process.new_subject()
      let controls = control.subject(process.self())
      actor.initialised(Seen([], []))
      |> actor.selecting(
        process.new_selector()
        |> process.select(self)
        |> process.select_map(inbox, Inbox)
        |> process.select_map(controls, Controlled),
      )
      |> actor.returning(#(self, inbox))
      |> Ok
    })
    |> actor.on_message(handle)
    |> actor.start
  process.unlink(started.pid)
  let #(log, inbox) = started.data
  Fake(
    ref: session.SessionRef(id: session.new_session_id(), user: user_id, inbox:),
    pid: started.pid,
    log:,
  )
}

pub fn stop(fake: Fake) -> Nil {
  process.kill(fake.pid)
}

pub fn exit_normally(fake: Fake) -> Nil {
  process.send_exit(fake.pid)
}

pub fn text(fake: Fake) -> String {
  session.session_id_to_string(fake.ref.id)
}

fn handle(seen: Seen, message: Message) -> actor.Next(Seen, Message) {
  case message {
    Inbox(session.Dispatch(event, payload)) -> {
      let decoded =
        json.parse_bits(raw_json.to_bits(payload), decode.dynamic)
        |> result.unwrap(dynamic.nil())
      actor.continue(
        Seen(..seen, dispatches: [#(event, decoded), ..seen.dispatches]),
      )
    }
    Inbox(_) -> actor.continue(seen)
    Controlled(found) ->
      actor.continue(Seen(..seen, controls: [found, ..seen.controls]))
    Read(reply) -> {
      process.send(
        reply,
        Seen(
          dispatches: list.reverse(seen.dispatches),
          controls: list.reverse(seen.controls),
        ),
      )
      actor.continue(seen)
    }
  }
}

pub fn seen(fake: Fake) -> Seen {
  process.call(fake.log, 5000, Read)
}

pub fn dispatched(fake: Fake, event: String) -> List(Dynamic) {
  seen(fake).dispatches
  |> list.filter_map(fn(pair) {
    case pair.0 == event {
      True -> Ok(pair.1)
      False -> Error(Nil)
    }
  })
}

pub fn controls(fake: Fake) -> List(Control) {
  seen(fake).controls
}
