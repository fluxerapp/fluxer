// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{
  type SocketMsg, type SocketRef, Close, DispatchFrame, ReconnectFrame, Send,
  SocketRef,
}
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager/registry as guild_registry
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/session/manager
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import support/guild_fixture as fixture
import support/session_harness as h

pub const channel = 10

pub type Seen {
  Seen(seq: Int, event: String, id: String)
  Moved(seq: Int)
  Reattached
  Reconnect
  Closed
}

pub type Sock {
  Sock(ref: SocketRef, query: Subject(Subject(List(Seen))))
}

type Input {
  Outbound(SocketMsg)
  Query(Subject(List(Seen)))
}

pub fn guild(user: Int) -> fixture.Guild {
  fixture.new(user) |> fixture.with_text(channel)
}

pub fn responder(user: Int, guilds: List(fixture.Guild)) -> h.Responder {
  fn(kind, body) {
    case kind {
      "session" ->
        h.ok_data(h.session_data(
          user,
          False,
          list.map(guilds, fn(found) { found.id }),
        ))
      "guild_collection" -> collection(guilds, body)
      _ -> h.ok_data(json.object([]))
    }
  }
}

fn collection(guilds: List(fixture.Guild), body: Dynamic) -> h.Reply {
  let wanted =
    decode.run(body, decode.at(["guild_id"], decode.string))
    |> result.replace_error(Nil)
    |> result.try(int.parse)
  let name =
    decode.run(body, decode.at(["collection"], decode.string))
    |> result.unwrap("")
  case list.find(guilds, fn(found) { Ok(found.id) == wanted }) {
    Error(Nil) -> unknown()
    Ok(found) ->
      case name {
        "guild" ->
          h.ok_data(json.object([#("guild", fixture.guild_json(found))]))
        "roles" ->
          h.ok_data(json.object([#("roles", fixture.roles_json(found))]))
        "channels" ->
          h.ok_data(
            json.object([
              #("channels", json.preprocessed_array(found.channels)),
              #("thread_only_channels", json.preprocessed_array(found.forums)),
            ]),
          )
        "emojis" ->
          h.ok_data(json.object([#("emojis", json.preprocessed_array([]))]))
        "stickers" ->
          h.ok_data(json.object([#("stickers", json.preprocessed_array([]))]))
        "members" ->
          h.ok_data(
            json.object([
              #(
                "members",
                json.preprocessed_array(
                  list.map(found.members, fn(pair) { pair.1 }),
                ),
              ),
              #("has_more", json.bool(False)),
              #("next_after_user_id", json.null()),
            ]),
          )
        _ -> unknown()
      }
  }
}

fn unknown() -> h.Reply {
  h.Reply(404, "{\"code\":\"UNKNOWN_GUILD\"}")
}

pub fn message(channel_id: Int, number: Int) -> Json {
  json.object([
    #("id", json.string(int.to_string(number))),
    #("channel_id", json.string(int.to_string(channel_id))),
    #("content", json.string("m" <> int.to_string(number))),
    #("author", fixture.user(1)),
  ])
}

pub fn hosts_guild(guild: Int) -> Bool {
  case id.guild_id(guild) {
    Ok(found) -> result.is_ok(guild_registry.lookup(found))
    Error(_) -> False
  }
}

pub fn send_messages(guild: Int, from: Int, count: Int, gap_ms: Int) -> Bool {
  case id.guild_id(guild) |> result.replace_error(Nil) {
    Error(Nil) -> False
    Ok(found) ->
      case guild_registry.lookup(found) {
        Error(Nil) -> False
        Ok(handle) -> {
          span(from, count)
          |> list.each(fn(number) {
            process.send(
              handle.control,
              inbox.Dispatch(
                "MESSAGE_CREATE",
                fixture.to_dynamic(message(channel, number)),
              ),
            )
            process.sleep(gap_ms)
          })
          True
        }
      }
  }
}

pub fn send_notes(user: Int, from: Int, count: Int, gap_ms: Int) -> Bool {
  case id.user_id(user) {
    Error(_) -> False
    Ok(found) -> {
      span(from, count)
      |> list.each(fn(number) {
        let _ =
          presence_client.dispatch_to_user(
            found,
            "USER_NOTE_UPDATE",
            raw_json.from_json(
              json.object([#("id", json.string(int.to_string(number)))]),
            ),
          )
        process.sleep(gap_ms)
      })
      True
    }
  }
}

pub fn socket() -> Sock {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outbox = process.new_subject()
      let query = process.new_subject()
      process.send(ready, #(outbox, query))
      let selector =
        process.new_selector()
        |> process.select_map(outbox, Outbound)
        |> process.select_map(query, Query)
      collect(selector, [])
    })
  let assert Ok(#(outbox, query)) = process.receive(ready, 1000)
  Sock(ref: SocketRef(pid:, outbox:), query:)
}

fn collect(selector: process.Selector(Input), seen: List(Seen)) -> Nil {
  case process.selector_receive_forever(selector) {
    Query(reply) -> {
      process.send(reply, list.reverse(seen))
      collect(selector, seen)
    }
    Outbound(Send(_, frames)) ->
      collect(
        selector,
        list.fold(frames, seen, fn(acc, frame) {
          case frame {
            DispatchFrame(seq, event, payload) -> [
              Seen(seq, event, payload_id(payload)),
              ..acc
            ]
            ReconnectFrame -> [Reconnect, ..acc]
            _ -> acc
          }
        }),
      )
    Outbound(Close(..)) -> collect(selector, [Closed, ..seen])
    Outbound(session.Moved(from:, seq:, ..)) -> {
      session.send(from, session.SocketDown(process.self()))
      collect(selector, [Moved(seq), ..seen])
    }
    Outbound(session.Reattached(..)) -> collect(selector, [Reattached, ..seen])
  }
}

fn payload_id(payload: raw_json.RawJson) -> String {
  id_decode.decode_json(raw_json.to_bits(payload))
  |> result.try(fn(value) {
    decode.run(value, decode.at(["id"], decode.string))
    |> result.replace_error(Nil)
  })
  |> result.unwrap("")
}

pub fn seen(sock: Sock) -> List(Seen) {
  let reply = process.new_subject()
  process.send(sock.query, reply)
  case process.receive(reply, 2000) {
    Ok(found) -> found
    Error(Nil) -> []
  }
}

pub fn start(sock: Sock, token: String) -> String {
  let reply = process.new_subject()
  manager.start(
    h.identify(token),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    sock.ref,
    reply,
  )
  case process.receive(reply, 15_000) {
    Ok(Ok(ref)) -> session.session_id_to_string(ref.id)
    _ -> ""
  }
}

pub fn ready_seq(found: List(Seen)) -> Option(Int) {
  list.find_map(found, fn(item) {
    case item {
      Seen(seq, "READY", _) -> Ok(seq)
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

pub fn named(found: List(Seen), event: String) -> List(String) {
  list.filter_map(found, fn(item) {
    case item {
      Seen(_, name, found_id) if name == event -> Ok(found_id)
      _ -> Error(Nil)
    }
  })
}

pub fn count(found: List(Seen), event: String) -> Int {
  list.length(named(found, event))
}

pub fn numbers(from: Int, count: Int) -> List(String) {
  span(from, count) |> list.map(int.to_string)
}

pub fn seqs_follow(found: List(Seen)) -> Bool {
  let seqs =
    list.filter_map(found, fn(item) {
      case item {
        Seen(seq, _, _) -> Ok(seq)
        _ -> Error(Nil)
      }
    })
  case seqs {
    [] -> True
    [first, ..] -> seqs == span(first, list.length(seqs))
  }
}

pub fn describe(found: List(Seen)) -> String {
  found
  |> list.map(fn(item) {
    case item {
      Seen(seq, event, found_id) ->
        event <> ":" <> int.to_string(seq) <> ":" <> found_id
      Moved(seq) -> "MOVED:" <> int.to_string(seq)
      Reattached -> "REATTACHED"
      Reconnect -> "RECONNECT"
      Closed -> "CLOSED"
    }
  })
  |> string.join(" ")
}

pub fn ids_of(sock: Sock, event: String) -> List(String) {
  named(seen(sock), event)
}

pub fn count_of(sock: Sock, event: String) -> Int {
  count(seen(sock), event)
}

pub fn follows(sock: Sock) -> Bool {
  seqs_follow(seen(sock))
}

pub fn reconnected(sock: Sock) -> Bool {
  list.contains(seen(sock), Reconnect)
}

pub fn ready_of(sock: Sock) -> Int {
  option.unwrap(ready_seq(seen(sock)), -1)
}

pub fn described(sock: Sock) -> String {
  describe(seen(sock))
}

fn span(from: Int, count: Int) -> List(Int) {
  int.range(from: from + count - 1, to: from - 1, with: [], run: list.prepend)
}
