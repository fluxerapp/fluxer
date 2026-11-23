// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type CloseCode}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type ClientOp, type Identify}
import fluxer_gateway/gateway/identify.{type IdentifyError}
import fluxer_gateway/gateway/opcode.{type Opcode}
import fluxer_gateway/gateway/resume.{type ResumeDecodeError, type ResumeRequest}
import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Frame {
  Frame(op: Opcode, body: Body)
}

pub type Body {
  HeartbeatBody(Option(Result(Option(Int), Nil)))
  IdentifyBody(Option(Result(Identify, IdentifyError)))
  ResumeBody(Option(Result(ResumeRequest, ResumeDecodeError)))
  RouteBody(Option(Route))
}

pub type Route {
  Forward(ClientOp)
  QueueVoice(VoiceUpdate)
  Reject(code: CloseCode, reason: String)
  Ignore
}

pub type VoiceUpdate {
  VoiceUpdate(op: ClientOp, guild: Field, channel: Field, connection: Field)
}

pub opaque type Field {
  Field(Option(Dynamic))
}

pub type DecodeError {
  DecodeFailed
  MissingOp
}

pub fn decode(bytes: BitArray) -> Result(Frame, DecodeError) {
  use fields <- result.try(
    json.parse_bits(bytes, decode.dict(decode.string, decode.dynamic))
    |> result.replace_error(DecodeFailed),
  )
  case dict.get(fields, "op") {
    Error(Nil) -> Error(MissingOp)
    Ok(op) -> {
      let op =
        decode.run(op, decode.int)
        |> result.map(opcode.from_int)
        |> result.unwrap(opcode.Unknown)
      let data = dict.get(fields, "d") |> option.from_result
      let body = case op {
        opcode.VoiceSignal ->
          RouteBody(option.map(data, fn(_) { signal_route(bytes) }))
        _ -> body(op, data)
      }
      Ok(Frame(op:, body:))
    }
  }
}

fn body(op: Opcode, data: Option(Dynamic)) -> Body {
  case op {
    opcode.Heartbeat ->
      HeartbeatBody(
        option.map(data, fn(seq) {
          decode.run(seq, decode.optional(decode.int))
          |> result.replace_error(Nil)
        }),
      )
    opcode.Identify ->
      IdentifyBody(
        option.map(data, run(_, identify.decoder(), identify.InvalidPayload)),
      )
    opcode.Resume ->
      ResumeBody(
        option.map(data, run(_, resume.decoder(), resume.InvalidResumePayload)),
      )
    _ -> RouteBody(option.map(data, route(op, _)))
  }
}

fn run(
  data: Dynamic,
  decoder: Decoder(Result(a, e)),
  invalid: e,
) -> Result(a, e) {
  decode.run(data, decoder) |> result.unwrap(Error(invalid))
}

pub fn route(op: Opcode, data: Dynamic) -> Route {
  case op {
    opcode.PresenceUpdate ->
      case presence(data) {
        Ok(normalized) -> Forward(session.PresenceUpdate(normalized))
        Error(Nil) -> Reject(error.DecodeError, "Invalid presence payload")
      }
    opcode.VoiceStateUpdate ->
      case is_object(data) {
        True -> QueueVoice(voice_update(data))
        False -> Reject(error.DecodeError, "Invalid voice payload")
      }
    opcode.RequestGuildMembers -> Forward(session.RequestGuildMembers(data))
    opcode.LazyRequest -> Forward(session.LazyRequest(data))
    opcode.RequestGuildCounts -> Forward(session.RequestGuildCounts(data))
    opcode.RequestChannelMemberCounts ->
      Forward(session.RequestChannelMemberCounts(data))
    opcode.RequestForumUnreads ->
      case is_object(data) {
        True -> Forward(session.RequestForumUnreads(data))
        False -> unknown_opcode()
      }
    _ -> unknown_opcode()
  }
}

pub fn signal_route(frame: BitArray) -> Route {
  let data = {
    use data <- result.try(raw_json.field(raw_json.wrap(frame), "d"))
    use _ <- result.map(raw_json.fields(data))
    data
  }
  case data {
    Ok(data) -> Forward(session.VoiceSignal(data))
    Error(Nil) -> Ignore
  }
}

fn unknown_opcode() -> Route {
  Reject(error.UnknownOpcode, "Unknown opcode")
}

pub fn voice_update(data: Dynamic) -> VoiceUpdate {
  VoiceUpdate(
    op: session.VoiceStateUpdate(data),
    guild: Field(field(data, "guild_id")),
    channel: Field(field(data, "channel_id")),
    connection: Field(field(data, "connection_id")),
  )
}

pub fn is_absent(field: Field) -> Bool {
  field == Field(None)
}

pub fn describe(field: Field) -> String {
  case field {
    Field(Some(value)) ->
      case decode.run(value, decode.string), decode.run(value, decode.int) {
        Ok(text), _ -> text
        _, Ok(number) -> int.to_string(number)
        _, _ -> "null"
      }
    Field(None) -> "undefined"
  }
}

pub fn presence(data: Dynamic) -> Result(Dynamic, Nil) {
  use raw_status <- result.try(field(data, "status") |> option.to_result(Nil))
  use status <- result.map(status(raw_status))
  let base = [
    #(dynamic.string("status"), dynamic.string(status)),
    #(dynamic.string("afk"), dynamic.bool(is_true(data, "afk"))),
    #(dynamic.string("mobile"), dynamic.bool(is_true(data, "mobile"))),
  ]
  case field(data, "custom_status") {
    Some(custom_status) -> [
      #(dynamic.string("custom_status"), custom_status),
      ..base
    ]
    None -> base
  }
  |> dynamic.properties
}

fn status(raw: Dynamic) -> Result(String, Nil) {
  case decode.run(raw, decode.string) {
    Ok("") | Ok("online") -> Ok("online")
    Ok("dnd") -> Ok("dnd")
    Ok("idle") -> Ok("idle")
    Ok("invisible") | Ok("offline") -> Ok("invisible")
    Ok(_) -> Error(Nil)
    Error(_) -> Ok("online")
  }
}

fn is_true(data: Dynamic, name: String) -> Bool {
  case field(data, name) {
    Some(value) -> decode.run(value, decode.bool) == Ok(True)
    None -> False
  }
}

fn field(data: Dynamic, name: String) -> Option(Dynamic) {
  decode.run(data, decode.at([name], decode.dynamic))
  |> option.from_result
}

fn is_object(data: Dynamic) -> Bool {
  decode.run(data, decode.dict(decode.string, decode.dynamic))
  |> result.is_ok
}

pub fn valid_text(payload: BitArray) -> Bool {
  bit_array.is_utf8(payload)
}
