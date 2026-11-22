// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/ws/http.{type Version, Http11}
import gleam/bit_array
import gleam/bool
import gleam/bytes_tree.{type BytesTree}
import gleam/crypto
import gleam/http as gleam_http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/list
import gleam/result

const accept_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

pub type Rejection {
  UpgradeRequired
  BadHandshake
}

pub fn accept(
  req: Request(peer),
  version: Version,
) -> Result(String, Rejection) {
  use <- bool.guard(version != Http11, Error(BadHandshake))
  use <- bool.guard(!is_upgrade(req), Error(UpgradeRequired))
  use ws_version <- result.try(
    request.get_header(req, "sec-websocket-version")
    |> result.replace_error(BadHandshake),
  )
  use <- bool.guard(
    !list.contains(["7", "8", "13"], ws_version),
    Error(BadHandshake),
  )
  use key <- result.try(
    request.get_header(req, "sec-websocket-key")
    |> result.replace_error(BadHandshake),
  )
  use <- bool.guard(key == "", Error(BadHandshake))
  Ok(accept_key(key))
}

fn is_upgrade(req: Request(peer)) -> Bool {
  req.method == gleam_http.Get
  && http.header_has_token(req, "connection", "upgrade")
  && http.header_has_token(req, "upgrade", "websocket")
}

pub fn upgrade_required() -> Response(BytesTree) {
  response.new(426)
  |> response.set_header("connection", "upgrade")
  |> response.set_header("upgrade", "websocket")
  |> response.set_body(bytes_tree.new())
}

pub fn accept_key(key: String) -> String {
  crypto.hash(crypto.Sha1, bit_array.from_string(key <> accept_guid))
  |> bit_array.base64_encode(True)
}

pub fn switching(accept: String) -> Response(BytesTree) {
  response.new(101)
  |> response.set_header("upgrade", "websocket")
  |> response.set_header("connection", "Upgrade")
  |> response.set_header("sec-websocket-accept", accept)
  |> response.set_body(bytes_tree.new())
}
