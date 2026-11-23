// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import support/wait

pub type Socket

pub type Sub {
  Sub(conn: Int, subject: String, queue: Option(String), sid: Int)
}

pub type Published {
  Published(
    conn: Int,
    subject: String,
    reply_to: Option(String),
    payload: String,
  )
}

pub opaque type FakeNats {
  FakeNats(hub: Subject(Message), port: Int)
}

pub opaque type Message {
  Accepted(conn: Int, socket: Socket)
  Control(conn: Int, line: String)
  Payload(conn: Int, subject: String, reply_to: Option(String), payload: String)
  Closed(conn: Int)
  Inject(subject: String, reply_to: Option(String), payload: String)
  Drop(conn: Int)
  Mute(Bool)
  Hold(Bool)
  Snapshot(reply: Subject(Hub))
}

pub type Hub {
  Hub(
    sockets: Dict(Int, Socket),
    subs: List(Sub),
    published: List(Published),
    connected_total: Int,
    muted: Bool,
    holding: Bool,
    held: List(Int),
    answered: Dict(Int, Int),
  )
}

type TcpOption {
  Binary
  Packet(PacketMode)
  Active(Bool)
  Reuseaddr(Bool)
  Ip(#(Int, Int, Int, Int))
  Buffer(Int)
  Backlog(Int)
}

type PacketMode {
  Line
  Raw
}

@external(erlang, "gen_tcp", "listen")
fn tcp_listen(port: Int, options: List(TcpOption)) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "accept")
fn tcp_accept(socket: Socket) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "recv")
fn tcp_recv(socket: Socket, length: Int) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "send")
fn tcp_send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "close")
fn tcp_close(socket: Socket) -> Dynamic

@external(erlang, "inet", "port")
fn tcp_port(socket: Socket) -> Result(Int, Dynamic)

@external(erlang, "inet", "setopts")
fn setopts(socket: Socket, options: List(TcpOption)) -> Dynamic

@external(erlang, "gen_tcp", "controlling_process")
fn controlling_process(socket: Socket, pid: process.Pid) -> Dynamic

pub fn start() -> FakeNats {
  let assert Ok(listen) =
    tcp_listen(0, [
      Binary,
      Packet(Line),
      Active(False),
      Reuseaddr(True),
      Ip(#(127, 0, 0, 1)),
      Buffer(1_048_576),
      Backlog(1024),
    ])
  let assert Ok(port) = tcp_port(listen)
  let assert Ok(started) =
    actor.new(Hub(dict.new(), [], [], 0, False, False, [], dict.new()))
    |> actor.on_message(handle)
    |> actor.start
  let hub = started.data
  process.spawn(fn() { accept_loop(listen, hub, 1) })
  FakeNats(hub:, port:)
}

pub fn port(fake: FakeNats) -> Int {
  fake.port
}

pub fn url(fake: FakeNats) -> String {
  "nats://127.0.0.1:" <> int.to_string(fake.port)
}

fn accept_loop(listen: Socket, hub: Subject(Message), next: Int) -> Nil {
  case tcp_accept(listen) {
    Error(_) -> Nil
    Ok(socket) -> {
      let handshake = process.new_subject()
      let reader =
        process.spawn_unlinked(fn() { await_socket(hub, next, handshake) })
      case process.receive(handshake, 5000) {
        Ok(inbox) -> {
          let _ = controlling_process(socket, reader)
          process.send(hub, Accepted(next, socket))
          process.send(inbox, socket)
        }
        Error(Nil) -> Nil
      }
      accept_loop(listen, hub, next + 1)
    }
  }
}

fn await_socket(
  hub: Subject(Message),
  conn: Int,
  handshake: Subject(Subject(Socket)),
) -> Nil {
  let inbox = process.new_subject()
  process.send(handshake, inbox)
  case process.receive(inbox, 5000) {
    Ok(socket) -> read_loop(hub, conn, socket)
    Error(Nil) -> Nil
  }
}

fn read_loop(hub: Subject(Message), conn: Int, socket: Socket) -> Nil {
  case tcp_recv(socket, 0) {
    Error(_) -> process.send(hub, Closed(conn))
    Ok(bytes) -> {
      let line = strip(bytes)
      case string.split(line, " ") {
        ["PUB", subject, size] ->
          read_payload(hub, conn, socket, subject, None, size)
        ["PUB", subject, reply, size] ->
          read_payload(hub, conn, socket, subject, Some(reply), size)
        _ -> {
          process.send(hub, Control(conn, line))
          read_loop(hub, conn, socket)
        }
      }
    }
  }
}

fn read_payload(
  hub: Subject(Message),
  conn: Int,
  socket: Socket,
  subject: String,
  reply_to: Option(String),
  size: String,
) -> Nil {
  let length = result.unwrap(int.parse(size), 0)
  let _ = setopts(socket, [Packet(Raw)])
  case tcp_recv(socket, length + 2) {
    Error(_) -> process.send(hub, Closed(conn))
    Ok(bytes) -> {
      let _ = setopts(socket, [Packet(Line)])
      let payload =
        bit_array.slice(bytes, 0, length)
        |> result.try(bit_array.to_string)
        |> result.unwrap("")
      process.send(hub, Payload(conn, subject, reply_to, payload))
      read_loop(hub, conn, socket)
    }
  }
}

fn strip(bytes: BitArray) -> String {
  bit_array.to_string(bytes)
  |> result.unwrap("")
  |> string.trim_end
}

fn handle(hub: Hub, message: Message) -> actor.Next(Hub, Message) {
  case message {
    Accepted(conn, socket) -> {
      write(
        socket,
        "INFO {\"server_id\":\"fake\",\"max_payload\":1048576,\"headers\":true}\r\n",
      )
      actor.continue(
        Hub(
          ..hub,
          sockets: dict.insert(hub.sockets, conn, socket),
          connected_total: hub.connected_total + 1,
        ),
      )
    }
    Control(conn, line) -> actor.continue(control(hub, conn, line))
    Payload(conn, subject, reply_to, payload) -> {
      let hub =
        Hub(..hub, published: [
          Published(conn, subject, reply_to, payload),
          ..hub.published
        ])
      route(hub, subject, reply_to, payload)
      actor.continue(hub)
    }
    Inject(subject, reply_to, payload) -> {
      route(hub, subject, reply_to, payload)
      actor.continue(hub)
    }
    Closed(conn) -> actor.continue(forget(hub, conn))
    Drop(conn) -> {
      case dict.get(hub.sockets, conn) {
        Ok(socket) -> {
          let _ = tcp_close(socket)
          Nil
        }
        Error(Nil) -> Nil
      }
      actor.continue(forget(hub, conn))
    }
    Mute(muted) -> actor.continue(Hub(..hub, muted:))
    Hold(True) -> actor.continue(Hub(..hub, holding: True))
    Hold(False) -> {
      let hub =
        list.fold(list.reverse(hub.held), hub, fn(hub, conn) {
          case dict.get(hub.sockets, conn) {
            Ok(socket) -> pong(hub, conn, socket)
            Error(Nil) -> hub
          }
        })
      actor.continue(Hub(..hub, holding: False, held: []))
    }
    Snapshot(reply) -> {
      process.send(reply, hub)
      actor.continue(hub)
    }
  }
}

fn pong(hub: Hub, conn: Int, socket: Socket) -> Hub {
  write(socket, "PONG\r\n")
  let count = dict.get(hub.answered, conn) |> result.unwrap(0)
  Hub(..hub, answered: dict.insert(hub.answered, conn, count + 1))
}

fn forget(hub: Hub, conn: Int) -> Hub {
  Hub(
    ..hub,
    sockets: dict.delete(hub.sockets, conn),
    subs: list.filter(hub.subs, fn(sub) { sub.conn != conn }),
  )
}

fn control(hub: Hub, conn: Int, line: String) -> Hub {
  case string.split(line, " ") {
    ["PING"] ->
      case hub.muted, hub.holding, dict.get(hub.sockets, conn) {
        True, _, _ -> hub
        False, True, _ -> Hub(..hub, held: [conn, ..hub.held])
        False, False, Ok(socket) -> pong(hub, conn, socket)
        False, False, Error(Nil) -> hub
      }
    ["SUB", subject, sid] -> add_sub(hub, Sub(conn, subject, None, parse(sid)))
    ["SUB", subject, queue, sid] ->
      add_sub(hub, Sub(conn, subject, Some(queue), parse(sid)))
    ["UNSUB", sid] ->
      Hub(
        ..hub,
        subs: list.filter(hub.subs, fn(sub) {
          !{ sub.conn == conn && sub.sid == parse(sid) }
        }),
      )
    _ -> hub
  }
}

fn add_sub(hub: Hub, sub: Sub) -> Hub {
  Hub(..hub, subs: list.append(hub.subs, [sub]))
}

fn parse(text: String) -> Int {
  result.unwrap(int.parse(text), -1)
}

fn route(
  hub: Hub,
  subject: String,
  reply_to: Option(String),
  payload: String,
) -> Nil {
  let matching =
    list.filter(hub.subs, fn(sub) { matches(sub.subject, subject) })
  let plain = list.filter(matching, fn(sub) { sub.queue == None })
  let groups =
    list.fold(matching, dict.new(), fn(groups, sub) {
      case sub.queue {
        Some(group) ->
          dict.upsert(groups, group, fn(found) {
            option.unwrap(found, [])
            |> list.append([sub])
          })
        None -> groups
      }
    })
  let picked =
    dict.values(groups)
    |> list.filter_map(list.first)
  let targets = list.append(plain, picked)
  case targets, reply_to {
    [], Some(inbox) -> no_responders(hub, inbox)
    _, _ ->
      list.each(targets, fn(sub) {
        deliver(hub, sub, subject, reply_to, payload)
      })
  }
}

fn no_responders(hub: Hub, inbox: String) -> Nil {
  list.each(hub.subs, fn(sub) {
    case matches(sub.subject, inbox), dict.get(hub.sockets, sub.conn) {
      True, Ok(socket) -> {
        let headers = "NATS/1.0 503\r\n\r\n"
        let size = int.to_string(string.byte_size(headers))
        write(
          socket,
          "HMSG "
            <> inbox
            <> " "
            <> int.to_string(sub.sid)
            <> " "
            <> size
            <> " "
            <> size
            <> "\r\n"
            <> headers
            <> "\r\n",
        )
      }
      _, _ -> Nil
    }
  })
}

fn deliver(
  hub: Hub,
  sub: Sub,
  subject: String,
  reply_to: Option(String),
  payload: String,
) -> Nil {
  case dict.get(hub.sockets, sub.conn) {
    Error(Nil) -> Nil
    Ok(socket) -> {
      let reply = case reply_to {
        Some(inbox) -> " " <> inbox
        None -> ""
      }
      write(
        socket,
        "MSG "
          <> subject
          <> " "
          <> int.to_string(sub.sid)
          <> reply
          <> " "
          <> int.to_string(string.byte_size(payload))
          <> "\r\n"
          <> payload
          <> "\r\n",
      )
    }
  }
}

fn write(socket: Socket, text: String) -> Nil {
  let _ = tcp_send(socket, bit_array.from_string(text))
  Nil
}

pub fn matches(pattern: String, subject: String) -> Bool {
  match_tokens(string.split(pattern, "."), string.split(subject, "."))
}

fn match_tokens(pattern: List(String), subject: List(String)) -> Bool {
  case pattern, subject {
    [">"], [_, ..] -> True
    ["*", ..pattern], [_, ..subject] -> match_tokens(pattern, subject)
    [token, ..pattern], [word, ..subject] if token == word ->
      match_tokens(pattern, subject)
    [], [] -> True
    _, _ -> False
  }
}

pub fn snapshot(fake: FakeNats) -> Hub {
  process.call(fake.hub, 5000, Snapshot)
}

pub fn subs(fake: FakeNats) -> List(Sub) {
  snapshot(fake).subs
}

pub fn subs_on(fake: FakeNats, subject: String) -> List(Sub) {
  subs(fake) |> list.filter(fn(sub) { sub.subject == subject })
}

pub fn connections(fake: FakeNats) -> List(Int) {
  snapshot(fake).sockets |> dict.keys |> list.sort(int.compare)
}

pub fn published_on(fake: FakeNats, prefix: String) -> List(Published) {
  snapshot(fake).published
  |> list.filter(fn(entry) { string.starts_with(entry.subject, prefix) })
  |> list.reverse
}

pub fn inject(
  fake: FakeNats,
  subject: String,
  reply_to: Option(String),
  payload: String,
) -> Nil {
  process.send(fake.hub, Inject(subject, reply_to, payload))
}

pub fn drop(fake: FakeNats, conn: Int) -> Nil {
  process.send(fake.hub, Drop(conn))
}

pub fn hold_pongs(fake: FakeNats, holding: Bool) -> Nil {
  process.send(fake.hub, Hold(holding))
}

pub fn answered(fake: FakeNats, conn: Int) -> Int {
  dict.get(snapshot(fake).answered, conn) |> result.unwrap(0)
}

pub fn connected_total(fake: FakeNats) -> Int {
  snapshot(fake).connected_total
}

pub fn mute_pongs(fake: FakeNats, muted: Bool) -> Nil {
  process.send(fake.hub, Mute(muted))
}

pub fn wait_until(timeout_ms: Int, check: fn() -> Bool) -> Bool {
  wait.until_every(timeout_ms, 50, check)
}
