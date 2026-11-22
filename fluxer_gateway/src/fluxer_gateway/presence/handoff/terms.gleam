// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/presence/handoff/wire_v1.{type SubjectV1, SubjectV1}
import fluxer_gateway/presence/remote
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import gleam/result

type Encoding {
  Deterministic
}

type Decoding {
  Safe
}

@external(erlang, "erlang", "term_to_binary")
fn term_to_binary(term: a, options: List(Encoding)) -> BitArray

@external(erlang, "rpc", "call")
fn local_call(
  node: Atom,
  module: Atom,
  function: Atom,
  arguments: List(Dynamic),
) -> Dynamic

@external(erlang, "erlang", "node")
fn self_node() -> Atom

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_subject(value: Dynamic) -> Subject(message)

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "erlang", "is_tuple")
fn is_tuple(value: Dynamic) -> Bool

@external(erlang, "erlang", "tuple_size")
fn tuple_size(value: Dynamic) -> Int

pub fn encode(term: a) -> BitArray {
  term_to_binary(term, [Deterministic])
}

pub fn decode(bytes: BitArray, decoder: Decoder(a)) -> Result(a, String) {
  let decoded =
    local_call(
      self_node(),
      atom.create("erlang"),
      atom.create("binary_to_term"),
      [to_dynamic(bytes), to_dynamic([Safe])],
    )
  case decode.run(decoded, failed_call()) {
    Ok(Nil) -> Error("the body is not a valid term")
    Error(_) ->
      decode.run(decoded, decoder)
      |> result.replace_error("the body does not match its wire record")
  }
}

fn failed_call() -> Decoder(Nil) {
  use _ <- decode.field(0, tag("badrpc"))
  decode.success(Nil)
}

pub fn subject(target: Subject(message)) -> SubjectV1 {
  SubjectV1(node: atom.to_string(remote.node_of(target)), term: encode(target))
}

pub fn to_subject(wire: SubjectV1) -> Result(Subject(message), String) {
  let _ = atom.create(wire.node)
  use found <- result.try(decode(wire.term, decode.dynamic))
  case subject_shape(found) {
    True -> Ok(unsafe_subject(found))
    False -> Error("the subject is not a process subject")
  }
}

fn subject_shape(value: Dynamic) -> Bool {
  case is_tuple(value) {
    False -> False
    True ->
      case
        tuple_size(value),
        decode.run(value, decode.at([0], atom.decoder()))
      {
        3, Ok(tag) ->
          atom.to_string(tag) == "subject"
          && result.map(element(value, 1), is_pid) == Ok(True)
        2, Ok(tag) ->
          atom.to_string(tag) == "named_subject"
          && result.is_ok(decode.run(value, decode.at([1], atom.decoder())))
        _, _ -> False
      }
  }
}

fn element(value: Dynamic, index: Int) -> Result(Dynamic, Nil) {
  decode.run(value, decode.at([index], decode.dynamic))
  |> result.replace_error(Nil)
}

pub fn tag(name: String) -> Decoder(Nil) {
  decode.new_primitive_decoder(name, fn(value) {
    case decode.run(value, atom.decoder()) {
      Ok(found) ->
        case atom.to_string(found) == name {
          True -> Ok(Nil)
          False -> Error(Nil)
        }
      Error(_) -> Error(Nil)
    }
  })
}

pub fn record(name: String, next: fn() -> Decoder(a)) -> Decoder(a) {
  use _ <- decode.field(0, tag(name))
  next()
}

pub fn option(inner: Decoder(a)) -> Decoder(Option(a)) {
  decode.one_of(decode.map(tag("none"), fn(_) { None }), [
    {
      use _ <- decode.field(0, tag("some"))
      use value <- decode.field(1, inner)
      decode.success(Some(value))
    },
  ])
}

pub fn pair(first: Decoder(a), second: Decoder(b)) -> Decoder(#(a, b)) {
  use a <- decode.field(0, first)
  use b <- decode.field(1, second)
  decode.success(#(a, b))
}

pub fn subject_wire() -> Decoder(SubjectV1) {
  use <- record("subject_v1")
  use node <- decode.field(1, decode.string)
  use term <- decode.field(2, decode.bit_array)
  decode.success(SubjectV1(node:, term:))
}
