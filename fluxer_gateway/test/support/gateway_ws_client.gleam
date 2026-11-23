// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Socket

type PacketKind {
  Raw
}

type TcpOption {
  Binary
  Active(Bool)
  Packet(PacketKind)
}

@external(erlang, "gen_tcp", "connect")
fn tcp_connect(
  host: Charlist,
  port: Int,
  options: List(TcpOption),
  timeout: Int,
) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "send")
fn tcp_send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "recv")
fn tcp_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "close")
fn tcp_close(socket: Socket) -> Dynamic

@external(erlang, "crypto", "exor")
fn exor(data: BitArray, mask: BitArray) -> BitArray

@external(erlang, "binary", "copy")
fn copy(subject: BitArray, times: Int) -> BitArray

pub type Client {
  Client(socket: Socket, buffer: BitArray)
}

pub type Event {
  Text(String)
  BinaryFrame(BitArray)
  Close(code: Option(Int), reason: String)
  Ping(BitArray)
  Pong(BitArray)
  Unexpected(opcode: Int)
  Closed
  TimedOut
}

pub type Payload {
  Payload(op: Int, t: Option(String), s: Option(Int), d: Dynamic)
}

pub fn raw_connect(port: Int) -> Socket {
  let assert Ok(socket) =
    tcp_connect(
      charlist.from_string("127.0.0.1"),
      port,
      [Binary, Active(False), Packet(Raw)],
      2000,
    )
  socket
}

pub fn send_raw(socket: Socket, data: BitArray) -> Nil {
  tcp_send(socket, data)
  Nil
}

pub fn recv_raw(socket: Socket, timeout: Int) -> Result(BitArray, Nil) {
  case tcp_recv(socket, 0, timeout) {
    Ok(data) -> Ok(data)
    Error(_) -> Error(Nil)
  }
}

pub fn connect(port: Int, query: String) -> Client {
  let assert Ok(client) = try_connect(port, query)
  client
}

pub fn try_connect(port: Int, query: String) -> Result(Client, String) {
  try_connect_with(port, query, [])
}

pub fn connect_with(
  port: Int,
  query: String,
  headers: List(#(String, String)),
) -> Client {
  let assert Ok(client) = try_connect_with(port, query, headers)
  client
}

pub fn try_connect_with(
  port: Int,
  query: String,
  headers: List(#(String, String)),
) -> Result(Client, String) {
  let socket = raw_connect(port)
  let extra =
    list.map(headers, fn(header) { header.0 <> ": " <> header.1 <> "\r\n" })
    |> string.concat
  send_raw(
    socket,
    bit_array.from_string(
      "GET /"
      <> query
      <> " HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
      <> "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n"
      <> extra
      <> "\r\n",
    ),
  )
  read_upgrade(socket)
}

pub fn read_upgrade(socket: Socket) -> Result(Client, String) {
  read_head(socket, <<>>)
}

pub fn read_all(socket: Socket, buffer: BitArray) -> String {
  case recv_raw(socket, 2000) {
    Ok(data) -> read_all(socket, <<buffer:bits, data:bits>>)
    Error(Nil) -> {
      let assert Ok(text) = bit_array.to_string(buffer)
      text
    }
  }
}

pub fn count(text: String, pattern: String) -> Int {
  list.length(string.split(text, pattern)) - 1
}

fn read_head(socket: Socket, buffer: BitArray) -> Result(Client, String) {
  case split_head(buffer, 0) {
    Some(#(head, rest)) ->
      case string.starts_with(head, "HTTP/1.1 101") {
        True -> Ok(Client(socket:, buffer: rest))
        False -> Error(head)
      }
    None ->
      case recv_raw(socket, 2000) {
        Ok(data) -> read_head(socket, <<buffer:bits, data:bits>>)
        Error(Nil) -> Error("no handshake reply")
      }
  }
}

fn split_head(buffer: BitArray, at: Int) -> Option(#(String, BitArray)) {
  case bit_array.slice(buffer, at, 4) {
    Ok(<<"\r\n\r\n":utf8>>) -> {
      let size = bit_array.byte_size(buffer) - at - 4
      let assert Ok(head) = bit_array.slice(buffer, 0, at)
      let assert Ok(rest) = bit_array.slice(buffer, at + 4, size)
      let assert Ok(head) = bit_array.to_string(head)
      Some(#(head, rest))
    }
    Ok(_) -> split_head(buffer, at + 1)
    Error(Nil) -> None
  }
}

pub fn close_socket(client: Client) -> Nil {
  tcp_close(client.socket)
  Nil
}

pub fn frame(opcode: Int, payload: BitArray, fin: Bool) -> BitArray {
  let key = <<1, 2, 3, 4>>
  let size = bit_array.byte_size(payload)
  let fin_bit = case fin {
    True -> 1
    False -> 0
  }
  let header = case size {
    _ if size < 126 -> <<fin_bit:1, 0:3, opcode:4, 1:1, size:7>>
    _ if size < 65_536 -> <<fin_bit:1, 0:3, opcode:4, 1:1, 126:7, size:16>>
    _ -> <<fin_bit:1, 0:3, opcode:4, 1:1, 127:7, size:64>>
  }
  <<header:bits, key:bits, mask(payload, key):bits>>
}

fn mask(payload: BitArray, key: BitArray) -> BitArray {
  let size = bit_array.byte_size(payload)
  case size, copy(key, size / 4 + 1) {
    0, _ -> payload
    _, <<repeated:bytes-size(size), _:bits>> -> exor(payload, repeated)
    _, _ -> payload
  }
}

pub fn send_frame(client: Client, data: BitArray) -> Nil {
  send_raw(client.socket, data)
}

pub fn send_text(client: Client, text: String) -> Nil {
  send_frame(client, frame(1, bit_array.from_string(text), True))
}

pub fn send_binary(client: Client, data: BitArray) -> Nil {
  send_frame(client, frame(2, data, True))
}

pub fn send_json(client: Client, value: json.Json) -> Nil {
  send_text(client, json.to_string(value))
}

pub fn send_close(client: Client, code: Int) -> Nil {
  send_frame(client, frame(8, <<code:16>>, True))
}

pub fn next(client: Client, timeout: Int) -> #(Client, Event) {
  case parse(client.buffer) {
    Some(#(event, rest)) -> #(Client(..client, buffer: rest), event)
    None ->
      case tcp_recv(client.socket, 0, timeout) {
        Ok(data) ->
          next(
            Client(..client, buffer: <<client.buffer:bits, data:bits>>),
            timeout,
          )
        Error(reason) ->
          case decode.run(reason, atom.decoder()) {
            Ok(tag) ->
              case atom.to_string(tag) {
                "timeout" -> #(client, TimedOut)
                _ -> #(client, Closed)
              }
            Error(_) -> #(client, Closed)
          }
      }
  }
}

fn parse(buffer: BitArray) -> Option(#(Event, BitArray)) {
  case buffer {
    <<_fin:1, _:3, opcode:4, 0:1, 126:7, size:16, rest:bits>> ->
      body(opcode, size, rest)
    <<_fin:1, _:3, opcode:4, 0:1, 127:7, size:64, rest:bits>> ->
      body(opcode, size, rest)
    <<_fin:1, _:3, opcode:4, 0:1, size:7, rest:bits>> if size < 126 ->
      body(opcode, size, rest)
    _ -> None
  }
}

fn body(opcode: Int, size: Int, data: BitArray) -> Option(#(Event, BitArray)) {
  case data {
    <<payload:bytes-size(size), rest:bits>> ->
      Some(#(event(opcode, payload), rest))
    _ -> None
  }
}

fn event(opcode: Int, payload: BitArray) -> Event {
  case opcode, payload {
    1, _ -> {
      let assert Ok(text) = bit_array.to_string(payload)
      Text(text)
    }
    2, _ -> BinaryFrame(payload)
    8, <<code:16, reason:bits>> -> {
      let assert Ok(reason) = bit_array.to_string(reason)
      Close(Some(code), reason)
    }
    8, _ -> Close(None, "")
    9, _ -> Ping(payload)
    10, _ -> Pong(payload)
    _, _ -> Unexpected(opcode)
  }
}

pub fn decode_payload(text: String) -> Payload {
  let decoder = {
    use op <- decode.field("op", decode.int)
    use t <- decode.optional_field("t", None, decode.optional(decode.string))
    use s <- decode.optional_field("s", None, decode.optional(decode.int))
    use d <- decode.optional_field("d", dynamic.nil(), decode.dynamic)
    decode.success(Payload(op:, t:, s:, d:))
  }
  let assert Ok(payload) = json.parse(text, decoder)
  payload
}

pub fn expect_payload(client: Client, timeout: Int) -> #(Client, Payload) {
  let assert #(client, Text(text)) = next(client, timeout)
  #(client, decode_payload(text))
}

pub fn expect_close(client: Client, timeout: Int) -> #(Option(Int), String) {
  case next(client, timeout) {
    #(_, Close(code, reason)) -> #(code, reason)
    #(client, Text(_))
    | #(client, BinaryFrame(_))
    | #(client, Ping(_))
    | #(client, Pong(_)) -> expect_close(client, timeout)
    #(_, Unexpected(_)) | #(_, Closed) | #(_, TimedOut) -> #(
      None,
      "no close frame",
    )
  }
}

pub fn expect_eof(client: Client, timeout: Int) -> Bool {
  case next(client, timeout) {
    #(_, Closed) -> True
    #(_, TimedOut) -> False
    #(client, _) -> expect_eof(client, timeout)
  }
}

pub fn silent(client: Client, timeout: Int) -> #(Client, Bool) {
  case next(client, timeout) {
    #(client, TimedOut) -> #(client, True)
    #(client, _) -> #(client, False)
  }
}

pub fn wait_for(
  client: Client,
  timeout: Int,
  matches: fn(Payload) -> Bool,
) -> #(Client, Payload) {
  let #(client, payload) = expect_payload(client, timeout)
  case matches(payload) {
    True -> #(client, payload)
    False -> wait_for(client, timeout, matches)
  }
}
