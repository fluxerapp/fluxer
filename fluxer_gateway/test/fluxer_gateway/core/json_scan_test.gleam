// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_scan.{Absent, Found}
import gleam/bit_array
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import support/json_scan_oracle as oracle

fn corpus() -> List(BitArray) {
  [
    <<"{}">>,
    <<" { } ">>,
    <<"{\"a\":1}">>,
    <<
      "{ \"b\" : 1 , \"a\":{\"x\":[1,{\"y\":\"}\"}]}, \"c\":\"q\\\"uote\", \"d\":null }",
    >>,
    <<
      "{\"t\":true,\"f\":false,\"n\":-5.5e3,\"s\":\"\\u00e9\",\"e\":[],\"o\":{}}",
    >>,
    <<"{\"esc\\\"aped\":1,\"\\u00e9\":2}">>,
    message(3),
    message(40),
  ]
}

fn message(n: Int) -> BitArray {
  json.object([
    #("id", json.string("1310000000000000001")),
    #("content", json.string(string.repeat("hi \"there\" ", n))),
    #(
      "mentions",
      json.array(int.range(from: n, to: 0, with: [], run: list.prepend), fn(i) {
        json.object([
          #("id", json.string(int.to_string(i))),
          #("roles", json.array([i, i + 1], json.int)),
        ])
      }),
    ),
    #("flags", json.int(n)),
  ])
  |> json.to_string
  |> bit_array.from_string
}

pub fn object_matches_the_gleam_oracle_test() {
  list.each(corpus(), fn(bytes) {
    assert json_scan.object(bytes) == oracle.object(bytes)
  })
}

pub fn object_keeps_order_and_slices_test() {
  let assert Ok(fields) =
    json_scan.object(<<"{\"b\":1, \"a\": [2, \"x\"] ,\"esc\\\"\":null}">>)
  assert fields
    == [#("b", <<"1">>), #("a", <<"[2, \"x\"]">>), #("esc\"", <<"null">>)]
}

pub fn array_matches_the_gleam_oracle_test() {
  let bytes = <<
    "[1, \"two\", {\"three\":[3]}, [4], true, false, null, -5.5e3]",
  >>
  assert json_scan.array(bytes) == oracle.array(bytes)
  let assert Ok(items) = json_scan.array(bytes)
  assert list.length(items) == 8
  assert json_scan.array(<<"[]">>) == Ok([])
}

pub fn malformed_input_is_an_error_test() {
  list.each(
    [
      <<>>,
      <<"{">>,
      <<"{\"a\"}">>,
      <<"{\"a\":1,}">>,
      <<"{\"a\":[1}">>,
      <<"{} x">>,
      <<"[1]">>,
    ],
    fn(bytes) {
      assert json_scan.object(bytes) == Error(Nil)
    },
  )
  assert json_scan.array(<<"{}">>) == Error(Nil)
}

pub fn large_input_takes_the_dirty_path_test() {
  let bytes = message(20_000)
  assert bit_array.byte_size(bytes) > 262_144
  assert json_scan.object(bytes) == oracle.object(bytes)
  let assert Ok(Found(..)) = json_scan.locate(bytes, "flags")
  assert json_scan.field(bytes, "flags") == Ok(<<"20000">>)
}

pub fn locate_gives_value_and_cut_offsets_test() {
  let bytes = <<"{\"a\":1, \"b\":[2], \"c\":3}">>
  let assert Ok(Found(value_start:, value_end:, cut_start:, cut_end:)) =
    json_scan.locate(bytes, "b")
  assert bit_array.slice(bytes, value_start, value_end - value_start)
    == Ok(<<"[2]">>)
  let assert Ok(head) = bit_array.slice(bytes, 0, cut_start)
  let assert Ok(tail) =
    bit_array.slice(bytes, cut_end, bit_array.byte_size(bytes) - cut_end)
  assert bit_array.append(head, tail) == <<"{\"a\":1, \"c\":3}">>
  assert json_scan.locate(bytes, "z") == Ok(Absent(close: 22, empty: False))
  assert json_scan.locate(<<"{}">>, "z") == Ok(Absent(close: 1, empty: True))
  assert json_scan.field(bytes, "z") == Error(Nil)
}
