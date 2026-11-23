// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{
  type JoinMode, type JoinReply, type JoinRequest, AsMember, Focused,
  JoinRequest,
}
import fluxer_gateway/core/guild_create as created
import fluxer_gateway/core/id
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/session/control.{type Control}
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Seen {
  Inbox(SessionInbox)
  Control(Control)
}

pub type FakeSession {
  FakeSession(ref: SessionRef, pid: Pid, seen: Subject(Seen))
}

pub fn start(user: Int) -> FakeSession {
  let seen = process.new_subject()
  let ready = process.new_subject()
  let assert Ok(user_id) = id.user_id(user)
  let session_id = session.new_session_id()
  let pid =
    process.spawn_unlinked(fn() {
      let inbox = process.new_subject()
      process.send(ready, inbox)
      let selector =
        process.new_selector()
        |> process.select_map(inbox, Inbox)
        |> process.select_map(control.subject(process.self()), Control)
      forward(selector, seen)
    })
  let assert Ok(inbox) = process.receive(ready, 1000)
  FakeSession(ref: session.SessionRef(session_id, user_id, inbox), pid:, seen:)
}

fn forward(selector: process.Selector(Seen), seen: Subject(Seen)) -> Nil {
  process.send(seen, process.selector_receive_forever(selector))
  forward(selector, seen)
}

pub fn stop(found: FakeSession) -> Nil {
  process.kill(found.pid)
}

pub fn request(
  found: FakeSession,
  class: guild.JoinClass,
  mode: JoinMode,
) -> JoinRequest {
  JoinRequest(
    session: found.ref,
    flags: session.IdentifyFlags(
      debounce_message_reactions: False,
      channel_threads: True,
      guild_visits: True,
    ),
    class:,
    mode:,
    presence: online(found.ref.user),
    bot: False,
    staff: False,
    remaining_ms: 10_000,
    attempt: 1,
  )
}

pub fn online(user: id.UserId) -> presence.Presence {
  let text = id.user_id_to_string(user)
  presence.new(
    user:,
    status: presence.Online,
    mobile: False,
    afk: False,
    custom_status: None,
    wire: raw_json.from_json(
      json.object([
        #("user", json.object([#("id", json.string(text))])),
        #("status", json.string("online")),
      ]),
    ),
  )
}

pub fn join(found: FakeSession, handle: Handle) -> JoinReply {
  join_with(found, handle, request(found, Focused, AsMember))
}

pub fn join_with(
  _found: FakeSession,
  handle: Handle,
  join_request: JoinRequest,
) -> JoinReply {
  let reply = process.new_subject()
  guild.send(handle.ref, guild.Join(join_request, reply))
  let assert Ok(answer) = process.receive(reply, 3000)
  answer
}

pub fn leave(found: FakeSession, handle: Handle) -> Nil {
  guild.send(handle.ref, guild.Leave(found.ref.id, found.pid))
}

pub fn op(
  found: FakeSession,
  handle: Handle,
  client_op: session.ClientOp,
) -> Nil {
  guild.send(handle.ref, guild.SessionOp(found.ref, client_op))
}

pub fn drain(found: FakeSession, wait_ms: Int) -> List(Seen) {
  collect(found.seen, wait_ms, [])
}

fn collect(seen: Subject(Seen), wait_ms: Int, acc: List(Seen)) -> List(Seen) {
  case process.receive(seen, wait_ms) {
    Ok(item) -> collect(seen, wait_ms, [item, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

pub fn dispatches(
  found: FakeSession,
  wait_ms: Int,
) -> List(#(String, Dynamic)) {
  drain(found, wait_ms) |> only_dispatches
}

pub fn only_dispatches(seen: List(Seen)) -> List(#(String, Dynamic)) {
  list.filter_map(seen, fn(item) {
    case item {
      Inbox(session.GuildDispatch(_, event, payload, _))
      | Inbox(session.Dispatch(event, payload)) -> Ok(#(event, parse(payload)))
      _ -> Error(Nil)
    }
  })
}

pub fn events(found: FakeSession, wait_ms: Int) -> List(String) {
  dispatches(found, wait_ms) |> list.map(fn(pair) { pair.0 })
}

pub fn await_event(
  found: FakeSession,
  event: String,
  wait_ms: Int,
) -> Option(Dynamic) {
  case process.receive(found.seen, wait_ms) {
    Error(Nil) -> None
    Ok(Inbox(session.GuildDispatch(_, name, payload, _)))
      | Ok(Inbox(session.Dispatch(name, payload)))
      if name == event
    -> Some(parse(payload))
    Ok(_) -> await_event(found, event, wait_ms)
  }
}

pub fn parse(payload: RawJson) -> Dynamic {
  id_decode.decode_json(raw_json.to_bits(payload))
  |> result.unwrap(dynamic.nil())
}

pub fn text_at(value: Dynamic, path: List(String)) -> Option(String) {
  decode.run(value, decode.at(path, decode.string)) |> option.from_result
}

pub fn ids_at(value: Dynamic, path: List(String), key: String) -> List(String) {
  decode.run(
    value,
    decode.at(path, decode.list(decode.at([key], decode.string))),
  )
  |> result.unwrap([])
}

pub fn guild_create(reply: JoinReply) -> Option(Dynamic) {
  case reply {
    guild.Joined(guild_create:, ..) -> Some(parse(created.to_raw(guild_create)))
    _ -> None
  }
}

pub fn channel_ids(reply: JoinReply) -> List(String) {
  case guild_create(reply) {
    Some(value) -> ids_at(value, ["channels"], "id")
    None -> []
  }
}

pub fn is_joined(reply: JoinReply) -> Bool {
  case reply {
    guild.Joined(..) -> True
    _ -> False
  }
}
