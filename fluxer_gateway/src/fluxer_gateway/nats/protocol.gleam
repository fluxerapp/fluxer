// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import gleam/bit_array
import gleam/bytes_tree.{type BytesTree}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type ServerOp {
  Info(ServerInfo)
  Msg(subject: String, sid: Int, reply_to: Option(String), payload: BitArray)
  HMsg(
    subject: String,
    sid: Int,
    reply_to: Option(String),
    status: Option(Int),
    payload: BitArray,
  )
  Ping
  Pong
  ServerOk
  Err(message: String)
}

pub type ServerInfo {
  ServerInfo(max_payload: Int, headers: Bool)
}

pub type ParseError {
  UnknownOperation(line: String)
  BadArguments(line: String)
  BadInfo
  PayloadTooLarge(size: Int)
}

pub const default_max_payload = 1_048_576

const max_line = 4096

const max_parse_payload = 67_108_864

pub fn parse(
  buffer: BitArray,
) -> Result(#(List(ServerOp), BitArray), ParseError) {
  parse_loop(buffer, [])
}

pub opaque type Reader {
  Reader(pending: List(BitArray), size: Int, wanted: Int)
}

pub fn reader() -> Reader {
  Reader(pending: [], size: 0, wanted: 0)
}

pub fn feed(
  reader: Reader,
  data: BitArray,
) -> Result(#(List(ServerOp), Reader), ParseError) {
  let pending = [data, ..reader.pending]
  let size = reader.size + bit_array.byte_size(data)
  case size < reader.wanted {
    True -> Ok(#([], Reader(pending:, size:, wanted: reader.wanted)))
    False -> {
      use #(ops, rest) <- result.map(parse(join(list.reverse(pending))))
      let left = bit_array.byte_size(rest)
      #(ops, Reader(pending: [rest], size: left, wanted: wanted(rest, left)))
    }
  }
}

@external(erlang, "erlang", "iolist_to_binary")
fn join(chunks: List(BitArray)) -> BitArray

fn wanted(rest: BitArray, size: Int) -> Int {
  case size, split_line(rest, 0) {
    0, _ -> 1
    _, None -> size + 1
    _, Some(#(line, _)) ->
      case string.split_once(line, " ") {
        Ok(#(verb, arguments)) ->
          case message_header(string.uppercase(verb), line, arguments) {
            Ok(parsed) -> string.byte_size(line) + parsed.size + 4
            Error(_) -> 0
          }
        Error(Nil) -> 0
      }
  }
}

fn parse_loop(
  buffer: BitArray,
  acc: List(ServerOp),
) -> Result(#(List(ServerOp), BitArray), ParseError) {
  case parse_one(buffer) {
    Error(error) -> Error(error)
    Ok(None) -> Ok(#(list.reverse(acc), buffer))
    Ok(Some(#(op, rest))) -> parse_loop(rest, [op, ..acc])
  }
}

pub fn parse_first(
  buffer: BitArray,
) -> Result(Option(#(ServerOp, BitArray)), ParseError) {
  parse_one(buffer)
}

fn parse_one(
  buffer: BitArray,
) -> Result(Option(#(ServerOp, BitArray)), ParseError) {
  case split_line(buffer, 0) {
    None ->
      case bit_array.byte_size(buffer) > max_line {
        True -> Error(UnknownOperation("line too long"))
        False -> Ok(None)
      }
    Some(#(line, rest)) -> operation(line, rest)
  }
}

fn split_line(buffer: BitArray, at: Int) -> Option(#(String, BitArray)) {
  case bit_array.slice(buffer, at, 2) {
    Ok(<<"\r\n":utf8>>) ->
      case bit_array.slice(buffer, 0, at) {
        Ok(line_bytes) ->
          case bit_array.to_string(line_bytes) {
            Ok(line) -> {
              let size = bit_array.byte_size(buffer)
              case bit_array.slice(buffer, at + 2, size - at - 2) {
                Ok(rest) -> Some(#(line, rest))
                Error(Nil) -> None
              }
            }
            Error(Nil) -> Some(#("", <<>>))
          }
        Error(Nil) -> None
      }
    Ok(_) ->
      case at > max_line {
        True -> None
        False -> split_line(buffer, at + 1)
      }
    Error(Nil) -> None
  }
}

fn operation(
  line: String,
  rest: BitArray,
) -> Result(Option(#(ServerOp, BitArray)), ParseError) {
  case string.split_once(line, " ") {
    Ok(#(verb, arguments)) ->
      case string.uppercase(verb) {
        "MSG" as verb -> msg(verb, line, arguments, rest)
        "HMSG" as verb -> hmsg(verb, line, arguments, rest)
        "INFO" -> info(arguments, rest)
        "-ERR" -> Ok(Some(#(Err(unquote(arguments)), rest)))
        _ -> Error(UnknownOperation(line))
      }
    Error(Nil) ->
      case string.uppercase(line) {
        "PING" -> Ok(Some(#(Ping, rest)))
        "PONG" -> Ok(Some(#(Pong, rest)))
        "+OK" -> Ok(Some(#(ServerOk, rest)))
        "-ERR" -> Ok(Some(#(Err(""), rest)))
        _ -> Error(UnknownOperation(line))
      }
  }
}

fn unquote(text: String) -> String {
  let trimmed = string.trim(text)
  case string.starts_with(trimmed, "'") && string.ends_with(trimmed, "'") {
    True -> trimmed |> string.drop_start(1) |> string.drop_end(1)
    False -> trimmed
  }
}

fn fields(arguments: String) -> List(String) {
  arguments
  |> string.split(" ")
  |> list.filter(fn(field) { field != "" })
}

type MsgArguments {
  MsgArguments(
    subject: String,
    sid: Int,
    reply_to: Option(String),
    header_size: Int,
    size: Int,
  )
}

fn message_header(
  verb: String,
  line: String,
  arguments: String,
) -> Result(MsgArguments, ParseError) {
  case verb, fields(arguments) {
    "MSG", [subject, sid, size] -> msg_arguments(subject, sid, None, "0", size)
    "MSG", [subject, sid, reply_to, size] ->
      msg_arguments(subject, sid, Some(reply_to), "0", size)
    "HMSG", [subject, sid, header_size, size] ->
      msg_arguments(subject, sid, None, header_size, size)
    "HMSG", [subject, sid, reply_to, header_size, size] ->
      msg_arguments(subject, sid, Some(reply_to), header_size, size)
    _, _ -> Error(Nil)
  }
  |> result.replace_error(BadArguments(line))
}

fn msg(
  verb: String,
  line: String,
  arguments: String,
  rest: BitArray,
) -> Result(Option(#(ServerOp, BitArray)), ParseError) {
  use parsed <- result.try(message_header(verb, line, arguments))
  use taken <- result.map(take_payload(rest, parsed.size))
  use #(payload, after) <- option.map(taken)
  #(
    Msg(
      subject: parsed.subject,
      sid: parsed.sid,
      reply_to: parsed.reply_to,
      payload:,
    ),
    after,
  )
}

fn hmsg(
  verb: String,
  line: String,
  arguments: String,
  rest: BitArray,
) -> Result(Option(#(ServerOp, BitArray)), ParseError) {
  use parsed <- result.try(message_header(verb, line, arguments))
  use taken <- result.try(take_payload(rest, parsed.size))
  case taken {
    None -> Ok(None)
    Some(#(body, after)) ->
      case
        bit_array.slice(body, 0, parsed.header_size),
        bit_array.slice(
          body,
          parsed.header_size,
          parsed.size - parsed.header_size,
        )
      {
        Ok(headers), Ok(payload) ->
          Ok(
            Some(#(
              HMsg(
                subject: parsed.subject,
                sid: parsed.sid,
                reply_to: parsed.reply_to,
                status: status(headers),
                payload:,
              ),
              after,
            )),
          )
        _, _ -> Error(BadArguments(line))
      }
  }
}

fn msg_arguments(
  subject: String,
  sid: String,
  reply_to: Option(String),
  header_size: String,
  size: String,
) -> Result(MsgArguments, Nil) {
  case
    id.parse_decimal(sid),
    id.parse_decimal(header_size),
    id.parse_decimal(size)
  {
    Ok(sid), Ok(header_size), Ok(size)
      if header_size >= 0 && size >= header_size
    -> Ok(MsgArguments(subject:, sid:, reply_to:, header_size:, size:))
    _, _, _ -> Error(Nil)
  }
}

fn take_payload(
  rest: BitArray,
  size: Int,
) -> Result(Option(#(BitArray, BitArray)), ParseError) {
  case size > max_parse_payload {
    True -> Error(PayloadTooLarge(size))
    False -> {
      let available = bit_array.byte_size(rest)
      case available >= size + 2 {
        False -> Ok(None)
        True ->
          case
            bit_array.slice(rest, 0, size),
            bit_array.slice(rest, size + 2, available - size - 2)
          {
            Ok(payload), Ok(after) -> Ok(Some(#(payload, after)))
            _, _ -> Ok(None)
          }
      }
    }
  }
}

fn status(headers: BitArray) -> Option(Int) {
  case bit_array.to_string(headers) {
    Ok(text) ->
      case string.split_once(text, "\r\n") {
        Ok(#(first, _)) -> status_line(first)
        Error(Nil) -> status_line(text)
      }
    Error(Nil) -> None
  }
}

fn status_line(line: String) -> Option(Int) {
  case fields(line) {
    ["NATS/1.0", code, ..] -> id.parse_decimal(code) |> option.from_result
    _ -> None
  }
}

fn info(
  arguments: String,
  rest: BitArray,
) -> Result(Option(#(ServerOp, BitArray)), ParseError) {
  let decoder = {
    use max_payload <- decode.optional_field(
      "max_payload",
      default_max_payload,
      decode.int,
    )
    use headers <- decode.optional_field("headers", False, decode.bool)
    decode.success(ServerInfo(max_payload:, headers:))
  }
  case json.parse(arguments, decoder) {
    Ok(found) -> Ok(Some(#(Info(found), rest)))
    Error(_) -> Error(BadInfo)
  }
}

pub type ConnectOptions {
  ConnectOptions(auth_token: Option(String), name: String, headers: Bool)
}

pub fn connect(options: ConnectOptions) -> BytesTree {
  let auth = case options.auth_token {
    Some(token) -> [#("auth_token", json.string(token))]
    None -> []
  }
  let body =
    json.object([
      #("verbose", json.bool(False)),
      #("pedantic", json.bool(False)),
      #("tls_required", json.bool(False)),
      #("name", json.string(options.name)),
      #("lang", json.string("gleam")),
      #("version", json.string("1.0.0")),
      #("protocol", json.int(1)),
      #("headers", json.bool(options.headers)),
      #("no_responders", json.bool(options.headers)),
      ..auth
    ])
  bytes_tree.from_string("CONNECT ")
  |> bytes_tree.append_tree(
    bytes_tree.from_string_tree(json.to_string_tree(body)),
  )
  |> bytes_tree.append_string("\r\n")
}

pub fn publish(
  subject: String,
  reply_to: Option(String),
  payload: BitArray,
) -> BytesTree {
  let size = int.to_string(bit_array.byte_size(payload))
  let line = case reply_to {
    Some(inbox) -> "PUB " <> subject <> " " <> inbox <> " " <> size <> "\r\n"
    None -> "PUB " <> subject <> " " <> size <> "\r\n"
  }
  bytes_tree.from_string(line)
  |> bytes_tree.append(payload)
  |> bytes_tree.append_string("\r\n")
}

pub fn subscribe(
  subject: String,
  queue: Option(String),
  sid: Int,
) -> BytesTree {
  let line = case queue {
    Some(group) ->
      "SUB " <> subject <> " " <> group <> " " <> int.to_string(sid) <> "\r\n"
    None -> "SUB " <> subject <> " " <> int.to_string(sid) <> "\r\n"
  }
  bytes_tree.from_string(line)
}

pub fn unsubscribe(sid: Int) -> BytesTree {
  bytes_tree.from_string("UNSUB " <> int.to_string(sid) <> "\r\n")
}

pub fn ping() -> BytesTree {
  bytes_tree.from_string("PING\r\n")
}

pub fn pong() -> BytesTree {
  bytes_tree.from_string("PONG\r\n")
}

pub fn valid_token(text: String) -> Bool {
  text != ""
  && !string.contains(text, " ")
  && !string.contains(text, "\t")
  && !string.contains(text, "\r")
  && !string.contains(text, "\n")
}
