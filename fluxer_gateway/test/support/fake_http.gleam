// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/table
import gleam/bit_array
import gleam/bool
import gleam/bytes_tree
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import glisten
import support/isolation

pub type Request {
  Request(head: String, body: BitArray)
}

pub type Answer {
  Answer(status: Int, body: String)
  Close
}

pub opaque type Route {
  Route(id: String, url: String)
}

type UniqueOption {
  Positive
  Monotonic
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

@external(erlang, "persistent_term", "put")
fn put_port(key: Atom, port: Int) -> Atom

@external(erlang, "persistent_term", "get")
fn get_port(key: Atom, default: Int) -> Int

@external(erlang, "erlang", "binary_to_atom")
fn owner_name(name: String) -> process.Name(Nil)

const prefix = "/fake/"

fn owner() -> process.Name(Nil) {
  owner_name("fluxer_gateway_fake_http")
}

fn routes() -> table.Table {
  table.named("fluxer_gateway_fake_http_routes")
}

fn served() -> table.Table {
  table.named("fluxer_gateway_fake_http_served")
}

fn port_key() -> Atom {
  atom.create("fluxer_gateway_fake_http_port")
}

pub fn route(handle: fn(Request) -> Answer) -> Route {
  let port = ensure_listener()
  let id = int.to_string(unique_integer([Positive, Monotonic]))
  table.insert(routes(), #(id, handle))
  Route(id:, url: "http://127.0.0.1:" <> int.to_string(port) <> prefix <> id)
}

pub fn url(route: Route) -> String {
  route.url
}

pub fn unroute(route: Route) -> Nil {
  table.delete(routes(), route.id)
  list.each(connections(route), fn(pid) {
    table.delete(served(), #(route.id, pid))
  })
}

pub fn port() -> Int {
  ensure_listener()
}

pub fn connections(route: Route) -> List(Pid) {
  table.to_list(served())
  |> list.filter_map(fn(row: #(#(String, Pid), Nil)) {
    case row.0 {
      #(id, pid) if id == route.id -> Ok(pid)
      _ -> Error(Nil)
    }
  })
}

fn ensure_listener() -> Int {
  case process.named(owner()), get_port(port_key(), 0) {
    Ok(_), port if port > 0 -> port
    _, _ -> start_listener()
  }
}

fn start_listener() -> Int {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    isolation.keep_across_tests()
    case process.register(process.self(), owner()) {
      Ok(Nil) -> {
        let assert True = table.ensure_public(routes())
        let assert True = table.ensure_public(served())
        let listener = process.new_name("fake_http_listener")
        let assert Ok(_) =
          glisten.new(fn(_) { #(<<>>, None) }, serve)
          |> glisten.bind("127.0.0.1")
          |> glisten.with_pool_size(32)
          |> glisten.with_listener_name(listener)
          |> glisten.start(0)
        let _ =
          put_port(port_key(), glisten.get_server_info(listener, 5000).port)
        process.send(ready, Nil)
        process.sleep_forever()
      }
      Error(Nil) -> {
        await_port(5000)
        process.send(ready, Nil)
      }
    }
  })
  let assert Ok(Nil) = process.receive(ready, 10_000)
  get_port(port_key(), 0)
}

fn await_port(left_ms: Int) -> Nil {
  case get_port(port_key(), 0) > 0 || left_ms <= 0 {
    True -> Nil
    False -> {
      process.sleep(5)
      await_port(left_ms - 5)
    }
  }
}

fn serve(
  buffer: BitArray,
  message: glisten.Message(Nil),
  connection: glisten.Connection(Nil),
) -> glisten.Next(BitArray, glisten.Message(Nil)) {
  case message {
    glisten.User(Nil) -> glisten.continue(buffer)
    glisten.Packet(data) ->
      answer_all(bit_array.append(buffer, data), connection)
  }
}

fn answer_all(
  buffer: BitArray,
  connection: glisten.Connection(Nil),
) -> glisten.Next(BitArray, glisten.Message(Nil)) {
  case split_request(buffer) {
    Error(Nil) -> glisten.continue(buffer)
    Ok(#(request, rest)) ->
      case handler_for(request.head) {
        Error(Nil) -> glisten.stop()
        Ok(#(id, handle)) -> {
          table.insert(served(), #(#(id, process.self()), Nil))
          case handle(request) {
            Close -> glisten.stop()
            Answer(status:, body:) -> {
              let head =
                "HTTP/1.1 "
                <> int.to_string(status)
                <> " X\r\ncontent-type: application/json\r\ncontent-length: "
                <> int.to_string(string.byte_size(body))
                <> "\r\n\r\n"
              case
                glisten.send(connection, bytes_tree.from_string(head <> body))
              {
                Ok(Nil) -> answer_all(rest, connection)
                Error(_) -> glisten.stop()
              }
            }
          }
        }
      }
  }
}

fn handler_for(head: String) -> Result(#(String, fn(Request) -> Answer), Nil) {
  use line <- result.try(head |> string.split("\r\n") |> list.first)
  use #(_, path) <- result.try(string.split_once(line, " " <> prefix))
  let id = path |> string.split(" ") |> list.first |> result.unwrap("")
  let id = id |> string.split("/") |> list.first |> result.unwrap("")
  case table.lookup(routes(), id) {
    [#(_, handle), ..] -> Ok(#(id, handle))
    [] -> Error(Nil)
  }
}

fn split_request(buffer: BitArray) -> Result(#(Request, BitArray), Nil) {
  use head_size <- result.try(head_end(buffer, 0))
  use head_bits <- result.try(bit_array.slice(buffer, 0, head_size))
  use head <- result.try(bit_array.to_string(head_bits))
  let length =
    head
    |> string.split("\r\n")
    |> list.find_map(fn(line) {
      case string.split_once(line, ":") {
        Ok(#(name, value)) ->
          case string.lowercase(string.trim(name)) {
            "content-length" -> int.parse(string.trim(value))
            _ -> Error(Nil)
          }
        Error(Nil) -> Error(Nil)
      }
    })
    |> result.unwrap(0)
  let start = head_size + 4
  let total = bit_array.byte_size(buffer)
  use <- bool.guard(total < start + length, Error(Nil))
  use body <- result.try(bit_array.slice(buffer, start, length))
  use rest <- result.try(bit_array.slice(
    buffer,
    start + length,
    total - start - length,
  ))
  Ok(#(Request(head:, body:), rest))
}

fn head_end(buffer: BitArray, at: Int) -> Result(Int, Nil) {
  case bit_array.slice(buffer, at, 4) {
    Error(Nil) -> Error(Nil)
    Ok(<<"\r\n\r\n":utf8>>) -> Ok(at)
    Ok(_) -> head_end(buffer, at + 1)
  }
}
