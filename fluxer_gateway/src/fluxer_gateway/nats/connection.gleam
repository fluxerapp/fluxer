// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/nats/address.{type Address}
import fluxer_gateway/nats/metrics as nats_metrics
import fluxer_gateway/nats/protocol.{type ServerOp}
import gleam/bit_array
import gleam/bytes_tree.{type BytesTree}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import logging

fn handshake_slack_ms() -> Int {
  durations.nats().connection_handshake_slack_ms
}

fn ping_interval_ms() -> Int {
  durations.nats().connection_ping_interval_ms
}

fn pong_timeout_ms() -> Int {
  durations.nats().connection_pong_timeout_ms
}

fn send_timeout_ms() -> Int {
  durations.nats().connection_send_timeout_ms
}

const inbox_sid = 1

fn waiter_grace_ms() -> Int {
  durations.nats().connection_waiter_grace_ms
}

pub type Socket

pub type Deliver =
  fn(String, Option(String), BitArray) -> Nil

pub type ReplyFailure {
  NoResponders
  Closed
}

pub type Reply =
  Result(BitArray, ReplyFailure)

pub type PublishFailure {
  TooLarge(size: Int, max: Int)
  WriteFailed(reason: String)
}

pub opaque type Connection {
  Connection(
    socket: Socket,
    subject: Subject(Message),
    max_payload: Int,
    inbox_prefix: String,
  )
}

pub opaque type Message {
  Received(BitArray)
  Replay(List(ServerOp))
  SocketClosed
  SocketFailed(String)
  Subscribe(
    sid: Int,
    subject: String,
    queue: Option(String),
    deliver: Deliver,
    reply: Subject(Nil),
  )
  Unsubscribe(sid: Int)
  AwaitReply(token: String, reply: Subject(Reply), timeout_ms: Int)
  CancelReply(token: String)
  PingTick
  PongDeadline(fence: Int)
  Close
  OwnerExited
}

type State {
  State(
    connection: Connection,
    reader: protocol.Reader,
    subscriptions: Dict(Int, Deliver),
    waiters: Dict(String, Subject(Reply)),
    pings_sent: Int,
    pongs_seen: Int,
    closing: Dict(Int, Closing),
  )
}

type Closing {
  Closing(deliver: Deliver, fence: Int)
}

type TcpOption {
  Binary
  Packet(PacketMode)
  Active(ActiveMode)
  Nodelay(Bool)
  Keepalive(Bool)
  SendTimeout(Int)
  SendTimeoutClose(Bool)
  Inet
  Inet6
}

type PacketMode {
  Raw
}

type ActiveMode {
  Once
}

type SocketError

type Outcome

type IpAddress

@external(erlang, "gen_tcp", "connect")
fn tcp_connect(
  host: Charlist,
  port: Int,
  options: List(TcpOption),
  timeout: Int,
) -> Result(Socket, SocketError)

@external(erlang, "gen_tcp", "send")
fn tcp_send(socket: Socket, data: BytesTree) -> Outcome

@external(erlang, "gen_tcp", "close")
fn tcp_close(socket: Socket) -> Outcome

@external(erlang, "inet", "setopts")
fn setopts(socket: Socket, options: List(TcpOption)) -> Outcome

@external(erlang, "inet", "getaddr")
fn getaddr(host: Charlist, family: TcpOption) -> Result(IpAddress, SocketError)

@external(erlang, "erlang", "is_atom")
fn is_ok_outcome(outcome: Outcome) -> Bool

@external(erlang, "binary", "copy")
fn copy(bytes: BitArray) -> BitArray

@external(erlang, "crypto", "strong_rand_bytes")
fn random_bytes(count: Int) -> BitArray

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

type UniqueOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_positive(options: List(UniqueOption)) -> Int

pub fn start(
  server: Address,
  auth_token: Option(String),
  timeout_ms: Int,
  owner: process.Pid,
) -> Result(Connection, String) {
  let started =
    actor.new_with_initialiser(timeout_ms + handshake_slack_ms(), fn(self) {
      case handshake(self, server, auth_token, timeout_ms) {
        Ok(#(connection, reader, leftover)) -> {
          let owner_monitor = process.monitor(owner)
          process.send_after(self, ping_interval_ms(), PingTick)
          case leftover {
            [] -> Nil
            _ -> process.send(self, Replay(leftover))
          }
          State(
            connection:,
            reader:,
            subscriptions: dict.new(),
            waiters: dict.new(),
            pings_sent: 0,
            pongs_seen: 0,
            closing: dict.new(),
          )
          |> actor.initialised
          |> actor.selecting(
            selector(self)
            |> process.select_specific_monitor(owner_monitor, fn(_) {
              OwnerExited
            }),
          )
          |> actor.returning(connection)
          |> Ok
        }
        Error(reason) -> Error(reason)
      }
    })
    |> actor.on_message(handle)
    |> actor.start
  case started {
    Ok(started) -> Ok(started.data)
    Error(actor.InitFailed(reason)) -> Error(reason)
    Error(actor.InitTimeout) -> Error("connect timed out")
    Error(actor.InitExited(reason)) -> Error(string.inspect(reason))
  }
}

pub fn pid(connection: Connection) -> Result(process.Pid, Nil) {
  process.subject_owner(connection.subject)
}

fn selector(self: Subject(Message)) -> process.Selector(Message) {
  process.new_selector()
  |> process.select(self)
  |> process.select_record(atom.create("tcp"), 2, packet)
  |> process.select_record(atom.create("tcp_closed"), 1, fn(_) { SocketClosed })
  |> process.select_record(atom.create("tcp_error"), 2, fn(raw) {
    SocketFailed(string.inspect(raw))
  })
}

fn packet(raw: Dynamic) -> Message {
  case decode.run(raw, decode.at([2], decode.bit_array)) {
    Ok(data) -> Received(data)
    Error(_) -> SocketFailed("unreadable packet")
  }
}

fn handshake(
  self: Subject(Message),
  server: Address,
  auth_token: Option(String),
  timeout_ms: Int,
) -> Result(#(Connection, protocol.Reader, List(ServerOp)), String) {
  let deadline = clock.monotonic_ms() + timeout_ms
  let host = charlist.from_string(server.host)
  let family = case getaddr(host, Inet) {
    Ok(_) -> Inet
    Error(_) -> Inet6
  }
  let options = [
    Binary,
    Packet(Raw),
    Active(Once),
    Nodelay(True),
    Keepalive(True),
    SendTimeout(send_timeout_ms()),
    SendTimeoutClose(True),
    family,
  ]
  use socket <- result.try(
    tcp_connect(host, server.port, options, timeout_ms)
    |> result.map_error(fn(reason) {
      "connect failed: " <> string.inspect(reason)
    }),
  )
  greet(socket, self, auth_token, deadline)
  |> result.try_recover(fn(reason) {
    let _ = tcp_close(socket)
    Error(reason)
  })
}

fn greet(
  socket: Socket,
  self: Subject(Message),
  auth_token: Option(String),
  deadline: Int,
) -> Result(#(Connection, protocol.Reader, List(ServerOp)), String) {
  let receiving = selector(self)
  use #(info, reader, leftover) <- result.try(await_info(
    socket,
    receiving,
    protocol.reader(),
    deadline,
  ))
  let prefix = "_INBOX." <> bit_array.base16_encode(random_bytes(11))
  let hello =
    protocol.connect(protocol.ConnectOptions(
      auth_token:,
      name: "fluxer_gateway",
      headers: info.headers,
    ))
    |> bytes_tree.append_tree(protocol.ping())
  use Nil <- result.try(checked_write(socket, hello))
  use #(reader, leftover) <- result.try(await_pong(
    socket,
    receiving,
    reader,
    leftover,
    deadline,
  ))
  use Nil <- result.try(checked_write(
    socket,
    protocol.subscribe(prefix <> ".*", None, inbox_sid),
  ))
  Ok(#(
    Connection(
      socket:,
      subject: self,
      max_payload: info.max_payload,
      inbox_prefix: prefix,
    ),
    reader,
    leftover,
  ))
}

fn checked_write(socket: Socket, data: BytesTree) -> Result(Nil, String) {
  case write(socket, data) {
    True -> Ok(Nil)
    False -> Error("write failed")
  }
}

fn receive_ops(
  socket: Socket,
  receiving: process.Selector(Message),
  reader: protocol.Reader,
  deadline: Int,
) -> Result(#(List(ServerOp), protocol.Reader), String) {
  let remaining = int.max(deadline - clock.monotonic_ms(), 0)
  case process.selector_receive(receiving, remaining) {
    Ok(Received(data)) -> {
      let _ = setopts(socket, [Active(Once)])
      protocol.feed(reader, data)
      |> result.map_error(string.inspect)
    }
    Ok(SocketClosed) -> Error("connection closed during handshake")
    Ok(SocketFailed(reason)) -> Error(reason)
    Ok(_) -> receive_ops(socket, receiving, reader, deadline)
    Error(Nil) -> Error("handshake timed out")
  }
}

fn await_info(
  socket: Socket,
  receiving: process.Selector(Message),
  reader: protocol.Reader,
  deadline: Int,
) -> Result(#(protocol.ServerInfo, protocol.Reader, List(ServerOp)), String) {
  use #(ops, reader) <- result.try(receive_ops(
    socket,
    receiving,
    reader,
    deadline,
  ))
  case ops {
    [] -> await_info(socket, receiving, reader, deadline)
    [protocol.Info(info), ..rest] -> Ok(#(info, reader, rest))
    [other, ..] -> Error("expected INFO, got " <> string.inspect(other))
  }
}

fn await_pong(
  socket: Socket,
  receiving: process.Selector(Message),
  reader: protocol.Reader,
  ops: List(ServerOp),
  deadline: Int,
) -> Result(#(protocol.Reader, List(ServerOp)), String) {
  case ops {
    [protocol.Pong, ..rest] -> Ok(#(reader, rest))
    [protocol.Err(message), ..] -> Error("server error: " <> message)
    [protocol.Ping, ..rest] -> {
      let _ = write(socket, protocol.pong())
      await_pong(socket, receiving, reader, rest, deadline)
    }
    [_, ..rest] -> await_pong(socket, receiving, reader, rest, deadline)
    [] -> {
      use #(ops, reader) <- result.try(receive_ops(
        socket,
        receiving,
        reader,
        deadline,
      ))
      await_pong(socket, receiving, reader, ops, deadline)
    }
  }
}

fn write(socket: Socket, data: BytesTree) -> Bool {
  is_ok_outcome(tcp_send(socket, data))
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Received(data) -> {
      let _ = setopts(state.connection.socket, [Active(Once)])
      case protocol.feed(state.reader, data) {
        Error(error) -> stop(state, "protocol error: " <> string.inspect(error))
        Ok(#(ops, reader)) -> {
          nats_metrics.received(
            list.count(ops, is_message),
            bit_array.byte_size(data),
          )
          case list.try_fold(ops, State(..state, reader:), apply) {
            Ok(state) -> actor.continue(state)
            Error(reason) -> stop(state, reason)
          }
        }
      }
    }
    Replay(ops) ->
      case list.try_fold(ops, state, apply) {
        Ok(state) -> actor.continue(state)
        Error(reason) -> stop(state, reason)
      }
    SocketClosed -> stop(state, "connection closed")
    SocketFailed(reason) -> stop(state, reason)
    Subscribe(sid, subject, queue, deliver, reply) -> {
      let _ =
        write(state.connection.socket, protocol.subscribe(subject, queue, sid))
      process.send(reply, Nil)
      actor.continue(
        State(
          ..state,
          subscriptions: dict.insert(state.subscriptions, sid, deliver),
        ),
      )
    }
    Unsubscribe(sid) -> actor.continue(unsubscribe_fenced(state, sid))
    AwaitReply(token, reply, timeout_ms) -> {
      process.send_after(
        state.connection.subject,
        timeout_ms + waiter_grace_ms(),
        CancelReply(token),
      )
      actor.continue(
        State(..state, waiters: dict.insert(state.waiters, token, reply)),
      )
    }
    CancelReply(token) ->
      actor.continue(State(..state, waiters: dict.delete(state.waiters, token)))
    PingTick -> {
      let fence = state.pings_sent + 1
      process.send_after(state.connection.subject, ping_interval_ms(), PingTick)
      process.send_after(
        state.connection.subject,
        pong_timeout_ms(),
        PongDeadline(fence),
      )
      let _ = write(state.connection.socket, protocol.ping())
      actor.continue(State(..state, pings_sent: fence))
    }
    PongDeadline(fence) ->
      case state.pongs_seen >= fence {
        True -> actor.continue(state)
        False -> stop(state, "server stopped answering pings")
      }
    Close -> stop(state, "closed by owner")
    OwnerExited -> stop(state, "owner exited")
  }
}

fn apply(state: State, op: ServerOp) -> Result(State, String) {
  case op {
    protocol.Ping -> {
      let _ = write(state.connection.socket, protocol.pong())
      Ok(state)
    }
    protocol.Pong -> Ok(pong(state))
    protocol.ServerOk | protocol.Info(_) -> Ok(state)
    protocol.Err(message) -> {
      logging.log(logging.Warning, "Gateway NATS server error: " <> message)
      case is_fatal(message) {
        True -> Error("server error: " <> message)
        False -> Ok(state)
      }
    }
    protocol.Msg(subject, sid, reply_to, payload) ->
      Ok(route(state, subject, sid, reply_to, Ok(copy(payload))))
    protocol.HMsg(subject, sid, reply_to, status, payload) ->
      case status {
        Some(503) ->
          Ok(route(state, subject, sid, reply_to, Error(NoResponders)))
        _ -> Ok(route(state, subject, sid, reply_to, Ok(copy(payload))))
      }
  }
}

fn is_message(op: ServerOp) -> Bool {
  case op {
    protocol.Msg(..) | protocol.HMsg(..) -> True
    _ -> False
  }
}

fn is_fatal(message: String) -> Bool {
  let lowered = string.lowercase(message)
  !string.contains(lowered, "permissions violation")
  && !string.contains(lowered, "invalid subject")
}

fn route(
  state: State,
  subject: String,
  sid: Int,
  reply_to: Option(String),
  body: Reply,
) -> State {
  case sid == inbox_sid {
    True -> {
      let token = inbox_token(state.connection.inbox_prefix, subject)
      case dict.get(state.waiters, token) {
        Ok(waiter) -> {
          process.send(waiter, body)
          State(..state, waiters: dict.delete(state.waiters, token))
        }
        Error(Nil) -> state
      }
    }
    False ->
      case subscriber(state, sid), body {
        Ok(deliver), Ok(payload) -> {
          deliver(subject, reply_to, payload)
          state
        }
        _, _ -> state
      }
  }
}

fn subscriber(state: State, sid: Int) -> Result(Deliver, Nil) {
  case dict.get(state.subscriptions, sid) {
    Ok(deliver) -> Ok(deliver)
    Error(Nil) ->
      dict.get(state.closing, sid)
      |> result.map(fn(closing) { closing.deliver })
  }
}

fn unsubscribe_fenced(state: State, sid: Int) -> State {
  case dict.get(state.subscriptions, sid) {
    Error(Nil) -> state
    Ok(deliver) -> {
      let fence = state.pings_sent + 1
      let _ =
        write(
          state.connection.socket,
          protocol.unsubscribe(sid) |> bytes_tree.append_tree(protocol.ping()),
        )
      State(
        ..state,
        subscriptions: dict.delete(state.subscriptions, sid),
        pings_sent: fence,
        closing: dict.insert(state.closing, sid, Closing(deliver:, fence:)),
      )
    }
  }
}

fn pong(state: State) -> State {
  let seen = state.pongs_seen + 1
  State(
    ..state,
    pongs_seen: seen,
    closing: dict.filter(state.closing, fn(_, closing) { closing.fence > seen }),
  )
}

fn inbox_token(prefix: String, subject: String) -> String {
  string.drop_start(subject, string.length(prefix) + 1)
}

fn stop(state: State, reason: String) -> actor.Next(State, Message) {
  logging.log(logging.Warning, "Gateway NATS connection stopped: " <> reason)
  dict.each(state.waiters, fn(_, waiter) { process.send(waiter, Error(Closed)) })
  let _ = tcp_close(state.connection.socket)
  actor.stop()
}

pub fn publish(
  connection: Connection,
  subject: String,
  reply_to: Option(String),
  payload: BitArray,
) -> Result(Nil, PublishFailure) {
  let size = bit_array.byte_size(payload)
  case size > connection.max_payload {
    True -> Error(TooLarge(size:, max: connection.max_payload))
    False ->
      case
        write(connection.socket, protocol.publish(subject, reply_to, payload))
      {
        True -> {
          nats_metrics.sent(size)
          Ok(Nil)
        }
        False -> Error(WriteFailed("socket write failed"))
      }
  }
}

pub fn subscribe(
  connection: Connection,
  subject: String,
  queue: Option(String),
  deliver: Deliver,
  timeout_ms: Int,
) -> Result(Int, Nil) {
  let sid = inbox_sid + unique_positive([Positive])
  let reply = process.new_subject()
  process.send(
    connection.subject,
    Subscribe(sid, subject, queue, deliver, reply),
  )
  case await(connection, reply, timeout_ms) {
    Arrived(Nil) -> Ok(sid)
    Stopped -> Error(Nil)
    TimedOut -> {
      unsubscribe(connection, sid)
      Error(Nil)
    }
  }
}

type Awaited(a) {
  Arrived(a)
  Stopped
  TimedOut
}

fn await(
  connection: Connection,
  reply: Subject(a),
  timeout_ms: Int,
) -> Awaited(a) {
  case pid(connection) {
    Error(Nil) -> Stopped
    Ok(owner) -> {
      let monitor = process.monitor(owner)
      let outcome =
        process.new_selector()
        |> process.select_map(reply, Arrived)
        |> process.select_specific_monitor(monitor, fn(_) { Stopped })
        |> process.selector_receive(timeout_ms)
      process.demonitor_process(monitor)
      case outcome {
        Ok(found) -> found
        Error(Nil) -> TimedOut
      }
    }
  }
}

pub fn unsubscribe(connection: Connection, sid: Int) -> Nil {
  process.send(connection.subject, Unsubscribe(sid))
}

pub type RequestFailure {
  RequestTimeout
  RequestNoResponders
  RequestClosed
  RequestPublishFailed(PublishFailure)
}

pub fn request(
  connection: Connection,
  subject: String,
  payload: BitArray,
  timeout_ms: Int,
) -> Result(BitArray, RequestFailure) {
  let token = int.to_string(unique_integer())
  let reply = process.new_subject()
  process.send(connection.subject, AwaitReply(token, reply, timeout_ms))
  let inbox = connection.inbox_prefix <> "." <> token
  case publish(connection, subject, Some(inbox), payload) {
    Error(failure) -> {
      process.send(connection.subject, CancelReply(token))
      Error(RequestPublishFailed(failure))
    }
    Ok(Nil) ->
      case await(connection, reply, timeout_ms) {
        Arrived(Ok(body)) -> Ok(body)
        Arrived(Error(NoResponders)) -> Error(RequestNoResponders)
        Arrived(Error(Closed)) | Stopped -> Error(RequestClosed)
        TimedOut -> {
          process.send(connection.subject, CancelReply(token))
          Error(RequestTimeout)
        }
      }
  }
}

pub fn close(connection: Connection) -> Nil {
  process.send(connection.subject, Close)
}
