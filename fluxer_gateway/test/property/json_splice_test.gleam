// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import property/gen.{type Rng}

type Model {
  MNull
  MBool(Bool)
  MInt(Int)
  MStr(String)
  MArr(List(Model))
  MObj(List(#(String, Model)))
}

const alphabet = [
  "a", "b", "c", "id", "\"", "\\", "/", "\n", "\t", "é", "😀", " ", "{", "}", "[",
  "]", ",", ":", "\u{1}", "\u{7f}", "0", "9",
]

const key_pool = [
  "a", "b", "id", "guild_id", "a\"b", "\\", "é", "😀", "", "{", "x y", "\n",
]

fn text(rng: Rng, pool: List(String)) -> #(String, Rng) {
  let #(length, rng) = gen.int(rng, 5)
  let #(parts, rng) = gen.list(rng, length, gen.pick(_, pool, "a"))
  #(string.concat(parts), rng)
}

fn model(rng: Rng, depth: Int) -> #(Model, Rng) {
  let #(kind, rng) = case depth <= 0 {
    True -> gen.int(rng, 4)
    False -> gen.int(rng, 6)
  }
  case kind {
    0 -> #(MNull, rng)
    1 -> {
      let #(flag, rng) = gen.bool(rng)
      #(MBool(flag), rng)
    }
    2 -> {
      let #(value, rng) =
        gen.pick(
          rng,
          [0, 1, -1, 42, 1_000_000_000_000_000_000, 9_223_372_036_854_775_807],
          0,
        )
      #(MInt(value), rng)
    }
    3 -> {
      let #(value, rng) = text(rng, alphabet)
      #(MStr(value), rng)
    }
    4 -> {
      let #(count, rng) = gen.int(rng, 4)
      let #(items, rng) = gen.list(rng, count, model(_, depth - 1))
      #(MArr(items), rng)
    }
    _ -> object(rng, depth - 1)
  }
}

fn object(rng: Rng, depth: Int) -> #(Model, Rng) {
  let #(count, rng) = gen.int(rng, 6)
  let #(keys, rng) = gen.list(rng, count, text(_, key_pool))
  let keys = list.unique(keys)
  let #(values, rng) =
    gen.list(rng, list.length(keys), model(_, int.max(depth, 0)))
  #(MObj(list.zip(keys, values)), rng)
}

fn space(rng: Rng) -> #(String, Rng) {
  let #(count, rng) = gen.int(rng, 3)
  let #(parts, rng) =
    gen.list(rng, count, gen.pick(_, [" ", "\n", "\t", "\r"], " "))
  #(string.concat(parts), rng)
}

fn escape_char(rng: Rng, char: String) -> #(String, Rng) {
  let assert [code] = string.to_utf_codepoints(char)
  let value = string.utf_codepoint_to_int(code)
  let #(fancy, rng) = gen.chance(rng, 30)
  let encoded = case char {
    "\"" -> "\\\""
    "\\" -> "\\\\"
    "\n" -> "\\n"
    "\t" -> "\\t"
    "/" if fancy -> "\\/"
    _ if value < 32 -> unicode(value)
    _ if fancy && value < 65_536 -> unicode(value)
    _ if fancy -> {
      let offset = value - 65_536
      unicode(55_296 + int.bitwise_shift_right(offset, 10))
      <> unicode(56_320 + int.bitwise_and(offset, 1023))
    }
    _ -> char
  }
  #(encoded, rng)
}

fn unicode(value: Int) -> String {
  "\\u" <> string.pad_start(string.lowercase(int.to_base16(value)), 4, "0")
}

fn encode_string(rng: Rng, value: String) -> #(String, Rng) {
  let #(parts, rng) =
    list.fold(string.to_utf_codepoints(value), #([], rng), fn(state, code) {
      let #(acc, rng) = state
      let #(piece, rng) = escape_char(rng, string.from_utf_codepoints([code]))
      #([piece, ..acc], rng)
    })
  #("\"" <> string.concat(list.reverse(parts)) <> "\"", rng)
}

fn encode(rng: Rng, value: Model) -> #(String, Rng) {
  case value {
    MNull -> #("null", rng)
    MBool(True) -> #("true", rng)
    MBool(False) -> #("false", rng)
    MInt(number) -> #(int.to_string(number), rng)
    MStr(text) -> encode_string(rng, text)
    MArr(items) -> {
      let #(parts, rng) = encode_all(rng, items)
      let #(open, rng) = space(rng)
      #("[" <> open <> string.join(parts, ",") <> "]", rng)
    }
    MObj(fields) -> {
      let #(parts, rng) =
        list.fold(fields, #([], rng), fn(state, pair) {
          let #(acc, rng) = state
          let #(key, rng) = encode_string(rng, pair.0)
          let #(value, rng) = encode(rng, pair.1)
          let #(a, rng) = space(rng)
          let #(b, rng) = space(rng)
          let #(c, rng) = space(rng)
          #([a <> key <> b <> ":" <> c <> value, ..acc], rng)
        })
      let #(close, rng) = space(rng)
      #("{" <> string.join(list.reverse(parts), ",") <> close <> "}", rng)
    }
  }
}

fn encode_all(rng: Rng, items: List(Model)) -> #(List(String), Rng) {
  let #(parts, rng) =
    list.fold(items, #([], rng), fn(state, item) {
      let #(acc, rng) = state
      let #(text, rng) = encode(rng, item)
      let #(a, rng) = space(rng)
      #([a <> text <> a, ..acc], rng)
    })
  #(list.reverse(parts), rng)
}

fn decoder() -> decode.Decoder(Model) {
  use <- decode.recursive
  decode.one_of(decode.map(decode.int, MInt), [
    decode.map(decode.string, MStr),
    decode.map(decode.bool, MBool),
    decode.map(decode.list(decoder()), MArr),
    decode.map(decode.dict(decode.string, decoder()), fn(found) {
      MObj(normal_fields(dict_list(found)))
    }),
    decode.success(MNull),
  ])
}

@external(erlang, "maps", "to_list")
fn dict_list(map: a) -> List(#(String, Model))

fn normal_fields(fields: List(#(String, Model))) -> List(#(String, Model)) {
  list.sort(fields, fn(a, b) { string.compare(a.0, b.0) })
}

fn normal(value: Model) -> Model {
  case value {
    MArr(items) -> MArr(list.map(items, normal))
    MObj(fields) ->
      MObj(
        normal_fields(list.map(fields, fn(pair) { #(pair.0, normal(pair.1)) })),
      )
    other -> other
  }
}

fn parsed(raw: RawJson) -> Result(Model, String) {
  json.parse_bits(raw_json.to_bits(raw), decoder())
  |> result.map_error(fn(_) {
    "not valid JSON: " <> string.inspect(raw_json.to_bits(raw))
  })
}

fn same(raw: RawJson, expected: Model, what: String) -> Result(Nil, String) {
  use found <- result.try(parsed(raw))
  gen.expect(found == normal(expected), fn() {
    what
    <> ": got "
    <> string.inspect(found)
    <> " want "
    <> string.inspect(normal(expected))
    <> " bytes "
    <> string.inspect(raw_json.to_bits(raw))
  })
}

fn set_field(fields: List(#(String, Model)), key: String, value: Model) {
  case list.key_find(fields, key) {
    Ok(_) ->
      list.map(fields, fn(pair) {
        case pair.0 == key {
          True -> #(key, value)
          False -> pair
        }
      })
    Error(Nil) -> list.append(fields, [#(key, value)])
  }
}

fn object_case(rng: Rng) -> Result(Nil, String) {
  let #(top, rng) = object(rng, 2)
  let assert MObj(fields) = top
  let #(bytes, rng) = encode(rng, top)
  let #(lead, rng) = space(rng)
  let #(tail, rng) = space(rng)
  let raw = raw_json.wrap_string(lead <> bytes <> tail)
  let #(key, rng) = case fields {
    [] -> text(rng, key_pool)
    _ -> {
      let #(existing, rng) = gen.chance(rng, 60)
      case existing {
        True -> {
          let #(pair, rng) = gen.pick(rng, fields, #("a", MNull))
          #(pair.0, rng)
        }
        False -> text(rng, key_pool)
      }
    }
  }
  let #(value, rng) = model(rng, 2)
  let #(value_text, _) = encode(rng, value)
  let value_raw = raw_json.wrap_string(value_text)
  let present = list.key_find(fields, key)
  use _ <- result.try(same(raw, top, "round trip"))
  use listed <- result.try(
    raw_json.fields(raw)
    |> result.replace_error("fields failed on " <> lead <> bytes <> tail),
  )
  use _ <- result.try(
    gen.expect(
      list.map(listed, fn(pair) { pair.0 })
        == list.map(fields, fn(pair) { pair.0 }),
      fn() { "fields keys differ for " <> bytes },
    ),
  )
  use _ <- result.try(
    list.try_each(list.zip(listed, fields), fn(pair) {
      same(pair.0.1, pair.1.1, "fields value of " <> pair.1.0)
    }),
  )
  use _ <- result.try(case present, raw_json.field(raw, key) {
    Ok(expected), Ok(found) -> same(found, expected, "field " <> key)
    Error(Nil), Error(Nil) -> Ok(Nil)
    _, _ ->
      Error(
        "field presence differs for " <> string.inspect(key) <> " in " <> bytes,
      )
  })
  use _ <- result.try(case raw_json.with_field(raw, key, value_raw) {
    Ok(written) ->
      same(written, MObj(set_field(fields, key, value)), "with_field " <> key)
    Error(Nil) -> Error("with_field failed on " <> bytes)
  })
  use _ <- result.try(
    case present, raw_json.replace_field(raw, key, value_raw) {
      Ok(_), Ok(written) ->
        same(written, MObj(set_field(fields, key, value)), "replace_field")
      Error(Nil), Error(Nil) -> Ok(Nil)
      _, _ -> Error("replace_field presence differs for " <> key)
    },
  )
  use _ <- result.try(case raw_json.without_field(raw, key) {
    Ok(written) ->
      same(
        written,
        MObj(list.filter(fields, fn(pair) { pair.0 != key })),
        "without_field " <> key,
      )
    Error(Nil) -> Error("without_field failed on " <> bytes)
  })
  let tree = json_tree.from_raw(raw)
  use _ <- result.try(
    case json_tree.set(tree, key, json_tree.from_raw(value_raw)) {
      Ok(written) ->
        same(
          json_tree.to_raw(written),
          MObj(set_field(fields, key, value)),
          "json_tree.set",
        )
      Error(Nil) -> Error("json_tree.set failed")
    },
  )
  use _ <- result.try(case json_tree.remove(tree, key) {
    Ok(written) ->
      same(
        json_tree.to_raw(written),
        MObj(list.filter(fields, fn(pair) { pair.0 != key })),
        "json_tree.remove",
      )
    Error(Nil) -> Error("json_tree.remove failed")
  })
  use _ <- result.try(tree_case(tree, top))
  let #(other_bytes, _) = encode(new_rng(rng), top)
  use left <- result.try(
    json_tree.canonical(tree) |> result.replace_error("canonical failed"),
  )
  use right <- result.try(
    json_tree.canonical(json_tree.from_raw(raw_json.wrap_string(other_bytes)))
    |> result.replace_error("canonical failed"),
  )
  use expanded <- result.try(
    json_tree.canonical(deep(tree)) |> result.replace_error("canonical failed"),
  )
  use _ <- result.try(
    gen.expect(left == right && left == expanded, fn() {
      "canonical differs: "
      <> string.inspect(left)
      <> " vs "
      <> string.inspect(right)
      <> " vs "
      <> string.inspect(expanded)
    }),
  )
  same(raw_json.wrap(left), top, "canonical content")
}

fn new_rng(rng: Rng) -> Rng {
  let #(seed, _) = gen.int(rng, 1_000_000_000)
  gen.new(seed)
}

fn deep(tree: json_tree.JsonTree) -> json_tree.JsonTree {
  case json_tree.expand(tree) {
    Ok(json_tree.Array(items)) -> json_tree.Array(list.map(items, deep))
    Ok(json_tree.Object(fields)) ->
      json_tree.Object(list.map(fields, fn(pair) { #(pair.0, deep(pair.1)) }))
    Ok(other) -> other
    Error(Nil) -> tree
  }
}

fn tree_case(tree: json_tree.JsonTree, value: Model) -> Result(Nil, String) {
  use _ <- result.try(same(
    json_tree.to_raw(deep(tree)),
    value,
    "deep expand re-encode",
  ))
  use _ <- result.try(same(
    raw_json.from_json(json_tree.to_json(deep(tree))),
    value,
    "deep expand to_json",
  ))
  let from_term =
    json.parse_bits(raw_json.to_bits(json_tree.to_raw(tree)), decode.dynamic)
    |> result.map(json_tree.from_term)
  use _ <- result.try(case from_term {
    Ok(term) -> same(json_tree.to_raw(term), value, "from_term")
    Error(_) -> Error("from_term parse failed")
  })
  scalars(tree, value)
}

fn scalars(tree: json_tree.JsonTree, value: Model) -> Result(Nil, String) {
  case value {
    MStr(text) -> {
      use _ <- result.try(
        gen.expect(json_tree.string(tree) == Ok(text), fn() {
          "json_tree.string "
          <> string.inspect(json_tree.string(tree))
          <> " want "
          <> string.inspect(text)
        }),
      )
      let raw = json_tree.to_raw(tree)
      use _ <- result.try(
        gen.expect(raw_json.string(raw) == Ok(text), fn() {
          "raw_json.string "
          <> string.inspect(raw_json.string(raw))
          <> " want "
          <> string.inspect(text)
        }),
      )
      let expected = case int.parse(text) {
        Ok(number) if number > 0 && number <= 9_223_372_036_854_775_807 ->
          case string.starts_with(text, "0") || string.starts_with(text, "+") {
            True -> Error(Nil)
            False -> Ok(number)
          }
        _ -> Error(Nil)
      }
      gen.expect(raw_json.snowflake(raw) == expected, fn() {
        "raw_json.snowflake of "
        <> string.inspect(text)
        <> " gave "
        <> string.inspect(raw_json.snowflake(raw))
      })
    }
    MInt(number) ->
      gen.expect(json_tree.int(tree) == Ok(number), fn() { "json_tree.int" })
    MBool(flag) ->
      gen.expect(json_tree.bool(tree) == Ok(flag), fn() { "json_tree.bool" })
    MArr(items) ->
      case json_tree.items(tree) {
        Ok(found) ->
          case list.length(found) == list.length(items) {
            True ->
              list.try_each(list.zip(found, items), fn(pair) {
                scalars(pair.0, pair.1)
              })
            False -> Error("json_tree.items length")
          }
        Error(Nil) -> Error("json_tree.items failed")
      }
    MObj(fields) ->
      list.try_each(fields, fn(pair) {
        case json_tree.field(tree, pair.0) {
          Ok(found) -> scalars(found, pair.1)
          Error(Nil) ->
            Error("json_tree.field missing " <> string.inspect(pair.0))
        }
      })
    MNull -> Ok(Nil)
  }
}

fn string_case(rng: Rng) -> Result(Nil, String) {
  let #(digits, rng) = gen.list(rng, 19, gen.pick(_, ["0", "1", "5", "9"], "1"))
  let #(length, rng) = gen.range(rng, 1, 20)
  let #(lead, rng) = gen.pick(rng, ["", "", "-", "+", "0", " "], "")
  let candidate = lead <> string.concat(list.take(digits, length))
  let #(as_number, rng) = gen.bool(rng)
  let #(bytes, _) = case as_number {
    True -> #(candidate, rng)
    False -> encode_string(rng, candidate)
  }
  let raw = raw_json.wrap_string(bytes)
  let numeric = case int.parse(candidate), lead {
    Ok(n), "" if n > 0 && n <= 9_223_372_036_854_775_807 ->
      case string.starts_with(candidate, "0") {
        True -> Error(Nil)
        False -> Ok(n)
      }
    _, _ -> Error(Nil)
  }
  let expected = case as_number {
    True ->
      case json.parse(bytes, decode.int) {
        Ok(n) if n > 0 && n <= 9_223_372_036_854_775_807 -> Ok(n)
        _ -> Error(Nil)
      }
    False -> numeric
  }
  let tree_found = json_tree.snowflake(json_tree.from_raw(raw))
  use _ <- result.try(
    gen.expect(tree_found == expected, fn() {
      "json_tree.snowflake of "
      <> bytes
      <> " gave "
      <> string.inspect(tree_found)
      <> " want "
      <> string.inspect(expected)
    }),
  )
  let expected = case as_number && lead == " " {
    True -> Error(Nil)
    False -> expected
  }
  let found = raw_json.snowflake(raw)
  gen.expect(found == expected, fn() {
    "raw_json.snowflake of "
    <> bytes
    <> " gave "
    <> string.inspect(found)
    <> " want "
    <> string.inspect(expected)
  })
}

pub fn raw_json_splices_agree_with_a_full_decode_test() {
  gen.check("raw_json splices", 1500, object_case)
}

pub fn snowflake_readers_agree_with_a_full_decode_test() {
  gen.check("snowflake readers", 3000, string_case)
}
