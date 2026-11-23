// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/nats/address
import fluxer_gateway/nats/connection
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option.{type Option, Some}
import gleam/string

type Socket

type TcpOption {
  Binary
  Packet(PacketMode)
  Active(Bool)
  Reuseaddr(Bool)
  Ip(#(Int, Int, Int, Int))
}

type PacketMode {
  Line
}

@external(erlang, "gen_tcp", "listen")
fn tcp_listen(port: Int, options: List(TcpOption)) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "accept")
fn tcp_accept(socket: Socket, timeout: Int) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "recv")
fn tcp_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "send")
fn tcp_send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "close")
fn tcp_close(socket: Socket) -> Dynamic

@external(erlang, "inet", "port")
fn tcp_port(socket: Socket) -> Result(Int, Dynamic)

type Seen {
  Ready(socket: Socket)
  Got(String)
}

fn line(socket: Socket) -> String {
  let assert Ok(bytes) = tcp_recv(socket, 0, 2000)
  let assert Ok(text) = bit_array.to_string(bytes)
  string.trim_end(text)
}

fn send(socket: Socket, text: String) -> Nil {
  let _ = tcp_send(socket, bit_array.from_string(text))
  Nil
}

fn serve(listen: Socket, seen: Subject(Seen)) -> Nil {
  let assert Ok(socket) = tcp_accept(listen, 5000)
  send(
    socket,
    "INFO {\"server_id\":\"test\",\"max_payload\":1048576,\"headers\":true}\r\n",
  )
  let assert "CONNECT " <> _ = line(socket)
  let assert "PING" = line(socket)
  send(socket, "PONG\r\n")
  let assert "SUB _INBOX." <> _ = line(socket)
  process.send(seen, Ready(socket))
  relay(socket, seen)
}

fn relay(socket: Socket, seen: Subject(Seen)) -> Nil {
  case tcp_recv(socket, 0, 3000) {
    Ok(bytes) -> {
      let assert Ok(text) = bit_array.to_string(bytes)
      process.send(seen, Got(string.trim_end(text)))
      relay(socket, seen)
    }
    Error(_) -> Nil
  }
}

fn next_line(seen: Subject(Seen)) -> String {
  let assert Ok(Got(text)) = process.receive(seen, 2000)
  text
}

fn message(sid: Int, reply: String) -> String {
  "MSG rpc.gateway.guild.dispatch "
  <> int.to_string(sid)
  <> " "
  <> reply
  <> " 2\r\nhi\r\n"
}

pub fn message_in_flight_at_unsubscribe_is_still_delivered_test() {
  let assert Ok(listen) =
    tcp_listen(0, [
      Binary,
      Packet(Line),
      Active(False),
      Reuseaddr(True),
      Ip(#(127, 0, 0, 1)),
    ])
  let assert Ok(port) = tcp_port(listen)
  let seen = process.new_subject()
  process.spawn(fn() { serve(listen, seen) })
  let assert Ok(conn) =
    connection.start(
      address.Address("127.0.0.1", port),
      option.None,
      2000,
      process.self(),
    )
  let assert Ok(Ready(socket)) = process.receive(seen, 2000)
  let delivered: Subject(Option(String)) = process.new_subject()
  let assert Ok(sid) =
    connection.subscribe(
      conn,
      "rpc.gateway.guild.>",
      Some("gateway"),
      fn(_, reply_to, _) { process.send(delivered, reply_to) },
      2000,
    )
  let assert "SUB rpc.gateway.guild.> gateway " <> _ = next_line(seen)
  connection.unsubscribe(conn, sid)
  assert next_line(seen) == "UNSUB " <> int.to_string(sid)
  send(socket, message(sid, "_INBOX.before"))
  assert process.receive(delivered, 2000) == Ok(Some("_INBOX.before"))
  assert next_line(seen) == "PING"
  send(socket, "PONG\r\n")
  send(socket, message(sid, "_INBOX.after"))
  assert process.receive(delivered, 300) == Error(Nil)
  connection.close(conn)
  let _ = tcp_close(socket)
  let _ = tcp_close(listen)
  Nil
}
