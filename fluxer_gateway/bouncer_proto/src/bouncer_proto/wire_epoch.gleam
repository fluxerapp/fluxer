// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/list
import gleam/order
import gleam/string

pub type Shape {
  Shape(type_name: String, constructors: List(#(String, Int)))
}

@external(erlang, "erlang", "crc32")
fn crc32(data: BitArray) -> Int

fn constructor_order(a: #(String, Int), b: #(String, Int)) -> order.Order {
  case string.compare(a.0, b.0) {
    order.Eq -> int.compare(a.1, b.1)
    other -> other
  }
}

pub fn canonical(shapes: List(Shape)) -> String {
  shapes
  |> list.map(fn(shape) {
    let constructors =
      shape.constructors
      |> list.sort(constructor_order)
      |> list.map(fn(pair) { pair.0 <> "/" <> int.to_string(pair.1) })
      |> string.join(",")
    shape.type_name <> "{" <> constructors <> "}"
  })
  |> list.sort(string.compare)
  |> string.join("\n")
}

pub fn fingerprint(shapes: List(Shape)) -> Int {
  crc32(<<canonical(shapes):utf8>>)
}

pub const first_bridged = 5

pub fn compatible(a: Int, b: Int) -> Bool {
  a == b || { int.absolute_value(a - b) == 1 && int.max(a, b) >= first_bridged }
}

pub fn bridged(epoch: Int) -> Bool {
  epoch >= first_bridged
}

pub fn floor(epoch: Int) -> Int {
  case bridged(epoch) {
    True -> epoch - 1
    False -> epoch
  }
}
