// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{type Settings, Settings}
import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import glisten

pub type Call {
  Call(kind: String, head: String, body: String)
}

pub type Responder =
  fn(String, String) -> #(Int, String)

pub type Stub {
  Stub(port: Int, supervisor: Pid, calls: Subject(Call), previous: Settings)
}

pub fn start(respond: Responder) -> Stub {
  let calls = process.new_subject()
  let name = process.new_name("gateway_api_stub_listener")
  let assert Ok(started) =
    glisten.new(fn(_) { #(<<>>, None) }, fn(buffer, message, connection) {
      case message {
        glisten.Packet(data) ->
          serve(<<buffer:bits, data:bits>>, connection, respond, calls)
        glisten.User(_) -> glisten.continue(buffer)
      }
    })
    |> glisten.bind("127.0.0.1")
    |> glisten.with_listener_name(name)
    |> glisten.start(0)
  process.unlink(started.pid)
  let port = glisten.get_server_info(name, 5000).port
  let previous = settings.get()
  settings.store(
    Settings(
      ..previous,
      api_internal_url: "http://127.0.0.1:" <> int.to_string(port),
      api_rpc_endpoint: None,
    ),
  )
  Stub(port:, supervisor: started.pid, calls:, previous:)
}

pub fn stop(stub: Stub) -> Nil {
  settings.store(stub.previous)
  process.kill(stub.supervisor)
}

pub fn calls(stub: Stub, kind: String) -> List(String) {
  received(stub, kind)
  |> list.map(fn(call) { call.body })
}

pub fn token_calls(stub: Stub, kind: String, token: String) -> List(String) {
  calls(stub, kind)
  |> list.filter(fn(body) {
    string.contains(body, "\"token\":\"" <> token <> "\"")
  })
}

pub fn received(stub: Stub, kind: String) -> List(Call) {
  drain(stub.calls, [])
  |> list.filter(fn(call) { call.kind == kind })
}

fn drain(calls: Subject(Call), acc: List(Call)) -> List(Call) {
  case process.receive(calls, 0) {
    Ok(call) -> drain(calls, [call, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

fn serve(
  buffer: BitArray,
  connection: glisten.Connection(a),
  respond: Responder,
  calls: Subject(Call),
) -> glisten.Next(BitArray, glisten.Message(a)) {
  case request_body(buffer) {
    Error(Nil) -> glisten.continue(buffer)
    Ok(#(head, body)) -> {
      let kind =
        json.parse(body, decode.at(["type"], decode.string))
        |> result.unwrap("")
      process.send(calls, Call(kind:, head:, body:))
      let #(status, reply) = respond(kind, body)
      let head =
        "HTTP/1.1 "
        <> int.to_string(status)
        <> " Stub\r\ncontent-type: application/json\r\ncontent-length: "
        <> int.to_string(string.byte_size(reply))
        <> "\r\n\r\n"
      case glisten.send(connection, bytes_tree.from_string(head <> reply)) {
        Ok(Nil) -> glisten.continue(<<>>)
        Error(_) -> glisten.stop()
      }
    }
  }
}

fn request_body(buffer: BitArray) -> Result(#(String, String), Nil) {
  use text <- result.try(bit_array.to_string(buffer))
  use #(head, body) <- result.try(string.split_once(text, "\r\n\r\n"))
  let length =
    string.split(head, "\r\n")
    |> list.find_map(fn(line) {
      case string.split_once(line, ":") {
        Ok(#(name, value)) ->
          case string.lowercase(name) {
            "content-length" -> int.parse(string.trim(value))
            _ -> Error(Nil)
          }
        Error(Nil) -> Error(Nil)
      }
    })
    |> result.unwrap(0)
  case string.byte_size(body) >= length {
    True -> Ok(#(head, body))
    False -> Error(Nil)
  }
}

pub fn session_reply(user_id: String) -> String {
  json.object([
    #(
      "data",
      json.object([
        #(
          "user",
          json.object([
            #("id", json.string(user_id)),
            #("username", json.string("scenario")),
            #("discriminator", json.string("0001")),
            #("bot", json.bool(False)),
            #("flags", json.int(0)),
          ]),
        ),
        #("guilds", json.preprocessed_array([])),
        #("relationships", json.preprocessed_array([])),
        #("private_channels", json.preprocessed_array([])),
      ]),
    ),
  ])
  |> json.to_string
}
