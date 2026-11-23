// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/list
import gleam/string
import support/gateway_harness as h
import support/gateway_ws_client as ws

fn no_api(_kind: String, _body: String) -> #(Int, String) {
  #(200, "{\"data\":{}}")
}

const upgrade =
  "GET /?v=1 HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"

pub fn plain_get_on_the_socket_route_is_426_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<
    "GET /?v=1 HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n":utf8,
  >>)
  let reply = ws.read_all(socket, <<>>)
  assert string.starts_with(reply, "HTTP/1.1 426 ")
  assert string.contains(string.lowercase(reply), "upgrade: websocket")
}

pub fn http_1_0_upgrade_is_400_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<
    "GET /?v=1 HTTP/1.0\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: a2V5\r\nSec-WebSocket-Version: 13\r\n\r\n":utf8,
  >>)
  let reply = ws.read_all(socket, <<>>)
  assert string.starts_with(reply, "HTTP/1.1 400 ")
}

pub fn upgrade_after_a_keep_alive_request_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<
    "GET /_health HTTP/1.1\r\nHost: localhost\r\n\r\n":utf8,
  >>)
  let assert Ok(first) = ws.recv_raw(socket, 2000)
  let assert Ok(text) = bit_array.to_string(first)
  assert string.starts_with(text, "HTTP/1.1 200 ")
  assert string.ends_with(text, "\r\n\r\nOK")
  ws.send_raw(socket, <<upgrade:utf8>>)
  let assert Ok(client) = ws.read_upgrade(socket)
  let #(_client, hello) = ws.expect_payload(client, 2000)
  assert hello.op == 10
}

pub fn upgrade_request_split_byte_by_byte_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  string.to_graphemes(upgrade)
  |> list.each(fn(char) { ws.send_raw(socket, bit_array.from_string(char)) })
  let assert Ok(client) = ws.read_upgrade(socket)
  let #(_client, hello) = ws.expect_payload(client, 2000)
  assert hello.op == 10
}

pub fn oversized_header_is_431_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(
    socket,
    bit_array.from_string(
      "GET /_health HTTP/1.1\r\nHost: localhost\r\nX-Big: "
      <> string.repeat("a", 5000)
      <> "\r\n\r\n",
    ),
  )
  let reply = ws.read_all(socket, <<>>)
  assert string.starts_with(reply, "HTTP/1.1 431 ")
}

pub fn unknown_path_is_404_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<
    "GET /gateway HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n":utf8,
  >>)
  let reply = ws.read_all(socket, <<>>)
  assert string.starts_with(reply, "HTTP/1.1 404 ")
}
