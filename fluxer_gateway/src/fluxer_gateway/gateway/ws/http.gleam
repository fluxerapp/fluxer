// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import gleam/bit_array
import gleam/bool
import gleam/bytes_tree.{type BytesTree}
import gleam/erlang/atom.{type Atom}
import gleam/http.{type Method}
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri

const max_request_line_length = 8000

const max_header_name_length = 64

const max_header_value_length = 4096

const max_headers = 100

const max_head_size = 428_000

const max_empty_lines = 5

pub type Version {
  Http10
  Http11
}

pub type Parsed(peer) {
  Incomplete
  Parsed(
    request: Request(peer),
    version: Version,
    keep_alive: Bool,
    rest: BitArray,
  )
}

pub type HttpError {
  BadRequest
  UriTooLong
  HeadersTooLarge
  VersionNotSupported
}

@external(erlang, "binary", "split")
fn split(subject: BitArray, pattern: BitArray) -> List(BitArray)

@external(erlang, "binary", "split")
fn split_with(
  subject: BitArray,
  pattern: BitArray,
  options: List(Atom),
) -> List(BitArray)

@external(erlang, "unicode", "characters_to_binary")
fn transcode(data: BitArray, from: Atom, to: Atom) -> String

fn text(data: BitArray) -> String {
  case bit_array.to_string(data) {
    Ok(text) -> text
    Error(Nil) -> transcode(data, atom.create("latin1"), atom.create("utf8"))
  }
}

fn trim(data: BitArray) -> BitArray {
  trim_end(trim_start(data))
}

fn trim_start(data: BitArray) -> BitArray {
  case data {
    <<" ", rest:bits>> | <<"\t", rest:bits>> -> trim_start(rest)
    _ -> data
  }
}

fn trim_end(data: BitArray) -> BitArray {
  let size = bit_array.byte_size(data)
  case bit_array.slice(data, size - 1, 1), bit_array.slice(data, 0, size - 1) {
    Ok(<<" ">>), Ok(init) | Ok(<<"\t">>), Ok(init) -> trim_end(init)
    _, _ -> data
  }
}

pub fn parse(buffer: BitArray, peer: peer) -> Result(Parsed(peer), HttpError) {
  let raw_size = bit_array.byte_size(buffer)
  use stripped <- result.try(skip_empty_lines(buffer, 0))
  case split(stripped, <<"\r\n\r\n">>) {
    [head, rest] -> parse_head(head, rest, peer)
    _ -> incomplete(stripped, raw_size)
  }
}

fn skip_empty_lines(
  buffer: BitArray,
  skipped: Int,
) -> Result(BitArray, HttpError) {
  case buffer {
    <<"\r\n", _:bits>> if skipped >= max_empty_lines -> Error(BadRequest)
    <<"\r\n", rest:bits>> -> skip_empty_lines(rest, skipped + 1)
    _ -> Ok(buffer)
  }
}

fn incomplete(
  buffer: BitArray,
  raw_size: Int,
) -> Result(Parsed(peer), HttpError) {
  let size = bit_array.byte_size(buffer)
  case split(buffer, <<"\r\n">>) {
    [_] if size > max_request_line_length -> Error(UriTooLong)
    _ if raw_size > max_head_size -> Error(HeadersTooLarge)
    _ -> Ok(Incomplete)
  }
}

fn parse_head(
  head: BitArray,
  rest: BitArray,
  peer: peer,
) -> Result(Parsed(peer), HttpError) {
  case split_with(head, <<"\r\n">>, [atom.create("global")]) {
    [line, ..lines] -> {
      use <- bool.guard(
        bit_array.byte_size(line) > max_request_line_length,
        Error(UriTooLong),
      )
      use <- bool.guard(
        list.length(lines) > max_headers,
        Error(HeadersTooLarge),
      )
      use #(method, target, version) <- result.try(request_line(text(line)))
      use headers <- result.try(parse_headers(lines, []))
      use request <- result.try(build(method, target, version, headers, peer))
      Ok(Parsed(
        request:,
        version:,
        keep_alive: keep_alive(version, headers),
        rest:,
      ))
    }
    [] -> Error(BadRequest)
  }
}

fn request_line(line: String) -> Result(#(Method, String, Version), HttpError) {
  case string.split(line, " ") {
    [method, target, version] -> {
      use method <- result.try(
        http.parse_method(method) |> result.replace_error(BadRequest),
      )
      use version <- result.try(parse_version(version))
      Ok(#(method, target, version))
    }
    _ -> Error(BadRequest)
  }
}

fn parse_version(version: String) -> Result(Version, HttpError) {
  case version {
    "HTTP/1.1" -> Ok(Http11)
    "HTTP/1.0" -> Ok(Http10)
    "HTTP/" <> _ -> Error(VersionNotSupported)
    _ -> Error(BadRequest)
  }
}

fn parse_headers(
  lines: List(BitArray),
  headers: List(#(String, String)),
) -> Result(List(#(String, String)), HttpError) {
  case lines {
    [] -> Ok(list.reverse(headers))
    [<<" ", _:bits>>, ..] | [<<"\t", _:bits>>, ..] -> Error(BadRequest)
    [line, ..rest] ->
      case split(line, <<":">>) {
        [raw_name, raw_value] -> {
          let raw_value = trim(raw_value)
          let name = text(raw_name)
          use <- bool.guard(
            name == ""
              || string.contains(name, " ")
              || string.contains(name, "\t"),
            Error(BadRequest),
          )
          use <- bool.guard(
            bit_array.byte_size(raw_name) > max_header_name_length
              || bit_array.byte_size(raw_value) > max_header_value_length,
            Error(HeadersTooLarge),
          )
          parse_headers(
            rest,
            add_header(headers, string.lowercase(name), text(raw_value)),
          )
        }
        _ -> Error(BadRequest)
      }
  }
}

fn add_header(
  headers: List(#(String, String)),
  name: String,
  value: String,
) -> List(#(String, String)) {
  case list.key_pop(headers, name) {
    Ok(#(existing, others)) -> [
      #(name, existing <> separator(name) <> value),
      ..others
    ]
    Error(Nil) -> [#(name, value), ..headers]
  }
}

fn separator(name: String) -> String {
  case name {
    "cookie" -> "; "
    _ -> ", "
  }
}

fn build(
  method: Method,
  target: String,
  version: Version,
  headers: List(#(String, String)),
  peer: peer,
) -> Result(Request(peer), HttpError) {
  use #(path, query, target_host) <- result.try(parse_target(target))
  let host_header = list.key_find(headers, "host")
  use #(host, port) <- result.try(case target_host, host_header, version {
    Some(host), _, _ -> Ok(#(host, None))
    None, Ok(value), _ -> Ok(split_host(value))
    None, Error(Nil), Http10 -> Ok(#("", None))
    None, Error(Nil), Http11 -> Error(BadRequest)
  })
  Ok(Request(
    method:,
    headers:,
    body: peer,
    scheme: http.Http,
    host:,
    port:,
    path:,
    query:,
  ))
}

fn parse_target(
  target: String,
) -> Result(#(String, Option(String), Option(String)), HttpError) {
  case target {
    "/" <> _ ->
      case string.split_once(target, "?") {
        Ok(#(path, query)) -> Ok(#(path, Some(query), None))
        Error(Nil) -> Ok(#(target, None, None))
      }
    "http://" <> _ | "https://" <> _ ->
      case uri.parse(target) {
        Ok(parsed) -> {
          let path = case parsed.path {
            "" -> "/"
            path -> path
          }
          Ok(#(path, parsed.query, parsed.host))
        }
        Error(Nil) -> Error(BadRequest)
      }
    _ -> Error(BadRequest)
  }
}

fn split_host(value: String) -> #(String, Option(Int)) {
  case string.split_once(value, ":") {
    Ok(#(host, port)) ->
      case id.parse_decimal(port) {
        Ok(port) -> #(host, Some(port))
        Error(Nil) -> #(value, None)
      }
    Error(Nil) -> #(value, None)
  }
}

fn keep_alive(version: Version, headers: List(#(String, String))) -> Bool {
  let connection =
    list.key_find(headers, "connection")
    |> result.map(tokens)
    |> result.unwrap([])
  let chunked = result.is_ok(list.key_find(headers, "transfer-encoding"))
  let has_length = case list.key_find(headers, "content-length") {
    Ok("0") | Error(Nil) -> False
    Ok(_) -> True
  }
  let has_body = chunked || has_length
  use <- bool.guard(has_body, False)
  case version {
    Http11 -> !list.contains(connection, "close")
    Http10 -> list.contains(connection, "keep-alive")
  }
}

pub fn tokens(value: String) -> List(String) {
  string.split(value, ",")
  |> list.map(fn(token) { string.lowercase(string.trim(token)) })
  |> list.filter(fn(token) { token != "" })
}

pub fn header_has_token(
  req: Request(peer),
  name: String,
  token: String,
) -> Bool {
  case request.get_header(req, name) {
    Ok(value) -> list.contains(tokens(value), token)
    Error(Nil) -> False
  }
}

pub fn error_status(error: HttpError) -> Int {
  case error {
    BadRequest -> 400
    UriTooLong -> 414
    HeadersTooLarge -> 431
    VersionNotSupported -> 505
  }
}

pub fn encode_reply(
  response: Response(BytesTree),
  method: Method,
  close: Bool,
) -> BytesTree {
  case method {
    http.Head -> encode_head(response, close)
    _ -> encode(response, close)
  }
}

pub fn encode(response: Response(BytesTree), close: Bool) -> BytesTree {
  bytes_tree.append_tree(encode_head(response, close), response.body)
}

fn encode_head(response: Response(BytesTree), close: Bool) -> BytesTree {
  let length = case response.status {
    101 -> []
    _ -> [
      #("content-length", int.to_string(bytes_tree.byte_size(response.body))),
    ]
  }
  let connection = case close {
    True -> [#("connection", "close")]
    False -> []
  }
  list.flatten([response.headers, length, connection])
  |> list.fold(
    bytes_tree.from_string(
      "HTTP/1.1 "
      <> int.to_string(response.status)
      <> " "
      <> reason(response.status)
      <> "\r\n",
    ),
    fn(tree, header) {
      bytes_tree.append_string(tree, header.0 <> ": " <> header.1 <> "\r\n")
    },
  )
  |> bytes_tree.append_string("\r\n")
}

fn reason(status: Int) -> String {
  case status {
    101 -> "Switching Protocols"
    200 -> "OK"
    400 -> "Bad Request"
    403 -> "Forbidden"
    404 -> "Not Found"
    409 -> "Conflict"
    414 -> "URI Too Long"
    426 -> "Upgrade Required"
    431 -> "Request Header Fields Too Large"
    503 -> "Service Unavailable"
    505 -> "HTTP Version Not Supported"
    _ -> "Unknown"
  }
}
