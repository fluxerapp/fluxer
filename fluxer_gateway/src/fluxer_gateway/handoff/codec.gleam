// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob.{type Header, type HeaderError, type Part}
import bouncer_proto/codec_range.{type CodecRange}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/result

pub type CodecError {
  Envelope(error: HeaderError)
  WrongKind(expected: Int, found: Int)
  WrongPart(expected: Part, found: Part)
  UnreadableVersion(version: Int)
  UnwritableVersion(version: Int)
  Malformed(reason: String)
}

pub type Codec(value) {
  Codec(
    ns: Int,
    part: Part,
    range: CodecRange,
    encode_body: fn(value, Int) -> Result(BitArray, CodecError),
    decode_body: fn(Int, BitArray) -> Result(value, CodecError),
  )
}

pub fn encode(
  codec: Codec(value),
  value: value,
  version: Int,
  epoch: Int,
) -> Result(BitArray, CodecError) {
  case
    version >= codec.range.max_write - 1 && version <= codec.range.max_write
  {
    False -> Error(UnwritableVersion(version))
    True -> {
      use body <- result.try(codec.encode_body(value, version))
      blob.wrap(
        ns: codec.ns,
        codec_version: version,
        part: codec.part,
        flags: 0,
        epoch: epoch,
        body: body,
      )
      |> result.map_error(Envelope)
    }
  }
}

pub fn decode(
  codec: Codec(value),
  bytes: BitArray,
) -> Result(#(Header, value), CodecError) {
  use #(header, body) <- result.try(
    blob.unwrap(bytes) |> result.map_error(Envelope),
  )
  case
    header.ns == codec.ns,
    header.part == codec.part,
    header.codec_version >= codec.range.oldest_read
    && header.codec_version <= codec.range.max_read
  {
    False, _, _ -> Error(WrongKind(expected: codec.ns, found: header.ns))
    _, False, _ -> Error(WrongPart(expected: codec.part, found: header.part))
    _, _, False -> Error(UnreadableVersion(header.codec_version))
    True, True, True -> {
      use value <- result.map(codec.decode_body(header.codec_version, body))
      #(header, value)
    }
  }
}

type TermOption {
  Deterministic
  Safe
}

@external(erlang, "erlang", "term_to_binary")
fn term_to_binary(value: a, options: List(TermOption)) -> BitArray

@external(erlang, "erpc", "multicall")
fn erpc_multicall(
  nodes: List(Atom),
  module: Atom,
  function: Atom,
  arguments: List(Dynamic),
  timeout: Int,
) -> List(Dynamic)

@external(erlang, "erlang", "node")
fn own_node() -> Atom

@external(erlang, "erlang", "is_tuple")
fn is_tuple(value: Dynamic) -> Bool

@external(erlang, "erlang", "tuple_size")
fn tuple_size(value: Dynamic) -> Int

@external(erlang, "erlang", "element")
fn element(index: Int, value: Dynamic) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn trusted(value: Dynamic) -> a

@external(erlang, "gleam_stdlib", "identity")
fn options_dynamic(options: List(TermOption)) -> Dynamic

const decode_timeout_ms = 30_000

pub fn term_body(value: a) -> BitArray {
  term_to_binary(value, [Deterministic])
}

pub fn term_value(body: BitArray) -> Result(Dynamic, CodecError) {
  let replies =
    erpc_multicall(
      [own_node()],
      atom.create("erlang"),
      atom.create("binary_to_term"),
      [dynamic.bit_array(body), options_dynamic([Safe])],
      decode_timeout_ms,
    )
  case replies {
    [reply] ->
      case is_tuple(reply) && tuple_size(reply) == 2 {
        True ->
          case decode.run(element(1, reply), atom.decoder()) {
            Ok(tag) ->
              case atom.to_string(tag) {
                "ok" -> Ok(element(2, reply))
                _ -> Error(Malformed("unsafe or invalid term"))
              }
            Error(_) -> Error(Malformed("unexpected decode reply"))
          }
        False -> Error(Malformed("unexpected decode reply"))
      }
    _ -> Error(Malformed("unexpected decode reply"))
  }
}

pub fn term_record(
  body: BitArray,
  tag: String,
  arity: Int,
) -> Result(a, CodecError) {
  use value <- result.try(term_value(body))
  case is_tuple(value) && tuple_size(value) == arity + 1 {
    False -> Error(Malformed("expected " <> tag))
    True ->
      case decode.run(element(1, value), atom.decoder()) {
        Ok(found) ->
          case atom.to_string(found) == tag {
            True -> Ok(trusted(value))
            False -> Error(Malformed("expected " <> tag))
          }
        Error(_) -> Error(Malformed("expected " <> tag))
      }
  }
}

pub fn term_of(body: BitArray) -> Result(a, CodecError) {
  term_value(body) |> result.map(trusted)
}
