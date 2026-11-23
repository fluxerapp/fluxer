// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import support/api_breaker

pub type Request {
  Request(kind: String, body: String)
}

pub type Stub {
  Stub(port: Int, pid: Pid, requests: Subject(Request))
}

type Socket

@external(erlang, "gen_tcp", "listen")
fn listen(port: Int, options: List(Dynamic)) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "accept")
fn accept(socket: Socket) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "recv")
fn recv(socket: Socket, length: Int, timeout: Int) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "send")
fn send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "close")
fn close(socket: Socket) -> Dynamic

@external(erlang, "gen_tcp", "controlling_process")
fn controlling_process(socket: Socket, pid: Pid) -> Dynamic

@external(erlang, "inet", "port")
fn socket_port(socket: Socket) -> Result(Int, Dynamic)

fn option(name: String, value: Dynamic) -> Dynamic {
  dynamic.array([atom.to_dynamic(atom.create(name)), value])
}

pub fn start(respond: fn(Request) -> String) -> Stub {
  let requests = process.new_subject()
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let assert Ok(socket) =
        listen(0, [
          atom.to_dynamic(atom.create("binary")),
          option("active", dynamic.bool(False)),
          option("reuseaddr", dynamic.bool(True)),
          option(
            "ip",
            dynamic.array([
              dynamic.int(127),
              dynamic.int(0),
              dynamic.int(0),
              dynamic.int(1),
            ]),
          ),
        ])
      let assert Ok(port) = socket_port(socket)
      process.send(ready, port)
      accept_loop(socket, requests, respond)
    })
  let assert Ok(port) = process.receive(ready, 1000)
  Stub(port:, pid:, requests:)
}

fn accept_loop(
  socket: Socket,
  requests: Subject(Request),
  respond: fn(Request) -> String,
) -> Nil {
  case accept(socket) {
    Ok(connection) -> {
      let handler =
        process.spawn_unlinked(fn() {
          process.sleep(5)
          serve(connection, requests, respond, <<>>)
        })
      let _ = controlling_process(connection, handler)
      accept_loop(socket, requests, respond)
    }
    Error(_) -> Nil
  }
}

fn serve(
  connection: Socket,
  requests: Subject(Request),
  respond: fn(Request) -> String,
  buffer: BitArray,
) -> Nil {
  case parse(buffer) {
    Ok(#(body, rest)) -> {
      let request = Request(kind: kind_of(body), body:)
      process.send(requests, request)
      let reply = respond(request)
      let _ =
        send(
          connection,
          bit_array.from_string(
            "HTTP/1.1 200 OK\r\ncontent-type: application/json\r\ncontent-length: "
            <> int.to_string(string.byte_size(reply))
            <> "\r\n\r\n"
            <> reply,
          ),
        )
      serve(connection, requests, respond, rest)
    }
    Error(Nil) ->
      case recv(connection, 0, 5000) {
        Ok(more) ->
          serve(connection, requests, respond, bit_array.append(buffer, more))
        Error(_) -> {
          let _ = close(connection)
          Nil
        }
      }
  }
}

fn parse(buffer: BitArray) -> Result(#(String, BitArray), Nil) {
  use text <- result.try(bit_array.to_string(buffer))
  use #(head, rest) <- result.try(string.split_once(text, "\r\n\r\n"))
  let length =
    string.split(head, "\r\n")
    |> list.find_map(fn(line) {
      case string.split_once(string.lowercase(line), ":") {
        Ok(#("content-length", value)) -> int.parse(string.trim(value))
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(0)
  case string.byte_size(rest) >= length {
    True -> {
      let body = string.slice(rest, 0, length)
      let left = string.drop_start(rest, string.length(body))
      Ok(#(body, bit_array.from_string(left)))
    }
    False -> Error(Nil)
  }
}

fn kind_of(body: String) -> String {
  case string.split_once(body, "\"type\":\"") {
    Ok(#(_, rest)) ->
      case string.split_once(rest, "\"") {
        Ok(#(kind, _)) -> kind
        Error(Nil) -> ""
      }
    Error(Nil) -> ""
  }
}

pub fn with_api(stub: Stub, run: fn() -> a) -> a {
  let previous = settings.get()
  let _ =
    settings.store(
      settings.Settings(
        ..previous,
        api_rpc_endpoint: Some(
          "http://127.0.0.1:" <> int.to_string(stub.port) <> "/internal/rpc",
        ),
      ),
    )
  let _ = api_breaker.reset()
  let outcome = run()
  let _ = settings.store(previous)
  process.kill(stub.pid)
  outcome
}

pub fn next_request(
  stub: Stub,
  kind: String,
  timeout_ms: Int,
) -> Result(Request, Nil) {
  case process.receive(stub.requests, timeout_ms) {
    Ok(found) if found.kind == kind -> Ok(found)
    Ok(_) -> next_request(stub, kind, timeout_ms)
    Error(Nil) -> Error(Nil)
  }
}
