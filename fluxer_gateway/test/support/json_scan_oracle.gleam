// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result

pub fn object(bytes: BitArray) -> Result(List(#(String, BitArray)), Nil) {
  case ws(bytes) {
    <<"{", rest:bits>> ->
      case ws(rest) {
        <<"}", tail:bits>> -> finish(tail, [])
        body -> members(bytes, body, [])
      }
    _ -> Error(Nil)
  }
}

pub fn array(bytes: BitArray) -> Result(List(BitArray), Nil) {
  case ws(bytes) {
    <<"[", rest:bits>> ->
      case ws(rest) {
        <<"]", tail:bits>> -> finish(tail, [])
        body -> elements(bytes, body, [])
      }
    _ -> Error(Nil)
  }
}

fn members(
  whole: BitArray,
  rest: BitArray,
  acc: List(#(String, BitArray)),
) -> Result(List(#(String, BitArray)), Nil) {
  use #(key_bytes, after_key) <- result.try(slice_value(whole, rest))
  use key <- result.try(
    json.parse_bits(key_bytes, decode.string) |> result.replace_error(Nil),
  )
  use after_colon <- result.try(case ws(after_key) {
    <<":", tail:bits>> -> Ok(ws(tail))
    _ -> Error(Nil)
  })
  use #(value, after_value) <- result.try(slice_value(whole, after_colon))
  let acc = [#(key, value), ..acc]
  case ws(after_value) {
    <<",", tail:bits>> -> members(whole, ws(tail), acc)
    <<"}", tail:bits>> -> finish(tail, acc)
    _ -> Error(Nil)
  }
}

fn elements(
  whole: BitArray,
  rest: BitArray,
  acc: List(BitArray),
) -> Result(List(BitArray), Nil) {
  use #(value, after_value) <- result.try(slice_value(whole, rest))
  let acc = [value, ..acc]
  case ws(after_value) {
    <<",", tail:bits>> -> elements(whole, ws(tail), acc)
    <<"]", tail:bits>> -> finish(tail, acc)
    _ -> Error(Nil)
  }
}

fn finish(tail: BitArray, acc: List(a)) -> Result(List(a), Nil) {
  case ws(tail) {
    <<>> -> Ok(list.reverse(acc))
    _ -> Error(Nil)
  }
}

fn slice_value(
  whole: BitArray,
  rest: BitArray,
) -> Result(#(BitArray, BitArray), Nil) {
  use after <- result.try(skip_value(rest))
  let size = bit_array.byte_size(whole)
  let start = size - bit_array.byte_size(rest)
  let length = bit_array.byte_size(rest) - bit_array.byte_size(after)
  bit_array.slice(whole, start, length)
  |> result.map(fn(value) { #(value, after) })
}

fn ws(bytes: BitArray) -> BitArray {
  case bytes {
    <<" ", rest:bits>>
    | <<"\n", rest:bits>>
    | <<"\r", rest:bits>>
    | <<"\t", rest:bits>> -> ws(rest)
    _ -> bytes
  }
}

fn skip_value(bytes: BitArray) -> Result(BitArray, Nil) {
  case bytes {
    <<"\"", rest:bits>> -> skip_string(rest)
    <<"{", rest:bits>> | <<"[", rest:bits>> -> skip_nested(rest, 1)
    <<"true", rest:bits>> | <<"null", rest:bits>> -> Ok(rest)
    <<"false", rest:bits>> -> Ok(rest)
    <<byte, _:bits>> if byte == 45 || byte >= 48 && byte <= 57 ->
      Ok(skip_number(bytes))
    _ -> Error(Nil)
  }
}

fn skip_string(bytes: BitArray) -> Result(BitArray, Nil) {
  case bytes {
    <<"\\", _, rest:bits>> -> skip_string(rest)
    <<"\"", rest:bits>> -> Ok(rest)
    <<_, rest:bits>> -> skip_string(rest)
    _ -> Error(Nil)
  }
}

fn skip_nested(bytes: BitArray, depth: Int) -> Result(BitArray, Nil) {
  case depth, bytes {
    0, _ -> Ok(bytes)
    _, <<"\"", rest:bits>> ->
      skip_string(rest) |> result.try(skip_nested(_, depth))
    _, <<"{", rest:bits>> | _, <<"[", rest:bits>> ->
      skip_nested(rest, depth + 1)
    _, <<"}", rest:bits>> | _, <<"]", rest:bits>> ->
      skip_nested(rest, depth - 1)
    _, <<_, rest:bits>> -> skip_nested(rest, depth)
    _, _ -> Error(Nil)
  }
}

fn skip_number(bytes: BitArray) -> BitArray {
  case bytes {
    <<byte, rest:bits>>
      if byte >= 48
      && byte <= 57
      || byte == 45
      || byte == 43
      || byte == 46
      || byte == 101
      || byte == 69
    -> skip_number(rest)
    _ -> bytes
  }
}
