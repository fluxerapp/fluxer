// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import support/isolation

type ExportItem {
  Exports
}

type Encoding {
  Utf8
}

type Option {
  Verbose
  NoTty
  Report(#(Atom, List(#(Atom, Bool))))
  ScaleTimeouts(Int)
}

@external(erlang, "gleeunit_ffi", "find_files")
fn find_files(matching matching: String, in in: String) -> List(String)

@external(erlang, "gleeunit_ffi", "run_eunit")
fn run_eunit(tests: Dynamic, options: List(Option)) -> Result(Nil, Dynamic)

@external(erlang, "erlang", "get_module_info")
fn module_info_of(module: Atom, item: ExportItem) -> List(#(Atom, Int))

@external(erlang, "code", "ensure_loaded")
fn ensure_loaded(module: Atom) -> Dynamic

@external(erlang, "erlang", "binary_to_atom")
fn to_atom(name: String, encoding: Encoding) -> Atom

@external(erlang, "erlang", "halt")
fn halt(status: Int) -> Nil

@external(erlang, "rand", "seed")
fn rand_seed(algorithm: Atom, seed: Int) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

pub fn main() -> Nil {
  isolation.snapshot()
  let shuffled = case envoy.get("FLUXER_TEST_SEED") |> result.try(int.parse) {
    Ok(seed) -> {
      let _ = rand_seed(atom.create("exsss"), seed)
      True
    }
    Error(Nil) -> False
  }
  let tests =
    find_files(matching: "**/*.{erl,gleam}", in: "test")
    |> list.filter(selected)
    |> list.map(module_name)
    |> arrange(shuffled)
    |> list.flat_map(fn(module) { arrange(module_tests(module), shuffled) })
  let setup = fn() { isolation.reset() }
  let cleanup = fn(_: Nil) { Nil }
  let options = [
    Verbose,
    NoTty,
    Report(
      #(atom.create("gleeunit_progress"), [#(atom.create("colored"), True)]),
    ),
    ScaleTimeouts(10),
  ]
  let suite = to_dynamic(#(atom.create("foreach"), setup, cleanup, tests))
  case run_eunit(suite, options) {
    Ok(Nil) -> halt(0)
    Error(_) -> halt(1)
  }
}

fn selected(path: String) -> Bool {
  case envoy.get("FLUXER_TEST_ONLY") {
    Ok(filter) ->
      string.split(filter, ",")
      |> list.any(fn(part) { part != "" && string.contains(path, part) })
    Error(Nil) -> True
  }
}

fn arrange(items: List(a), shuffled: Bool) -> List(a) {
  case shuffled {
    True -> list.shuffle(items)
    False -> items
  }
}

fn module_name(path: String) -> Atom {
  path
  |> string.replace(".gleam", "")
  |> string.replace("/", "@")
  |> to_atom(Utf8)
}

fn module_tests(module: Atom) -> List(Dynamic) {
  let _ = ensure_loaded(module)
  module_info_of(module, Exports)
  |> list.filter_map(fn(export) {
    let #(function, arity) = export
    let name = atom.to_string(function)
    case
      arity,
      string.ends_with(name, "_test"),
      string.ends_with(name, "_test_")
    {
      0, True, _ -> Ok(to_dynamic(#(module, function)))
      0, _, True ->
        Ok(to_dynamic(#(atom.create("generator"), module, function)))
      _, _, _ -> Error(Nil)
    }
  })
}
