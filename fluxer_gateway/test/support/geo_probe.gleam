// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub const latitude = "59.3293"

pub const longitude = "18.0686"

pub fn fields() -> List(#(String, Json)) {
  [#("latitude", json.float(59.3293)), #("longitude", json.string(longitude))]
}

pub fn leaks(text: String) -> Bool {
  list.any(["latitude", "longitude", latitude, longitude], string.contains(
    text,
    _,
  ))
}

pub fn carries(text: String) -> Bool {
  string.contains(text, "\"latitude\":\"" <> latitude <> "\"")
  && string.contains(text, "\"longitude\":\"" <> longitude <> "\"")
}

pub fn leaking(texts: List(String)) -> List(String) {
  list.filter(texts, leaks)
}

pub opaque type Capture {
  Capture(lines: Subject(String), primary: Dynamic, console: Option(Dynamic))
}

@external(erlang, "persistent_term", "put")
fn put_term(key: Atom, value: Option(Subject(String))) -> Atom

@external(erlang, "persistent_term", "get")
fn get_term(
  key: Atom,
  default: Option(Subject(String)),
) -> Option(Subject(String))

@external(erlang, "logger", "add_handler")
fn add_handler(id: Atom, module: Atom, config: Dynamic) -> Dynamic

@external(erlang, "logger", "remove_handler")
fn remove_handler(id: Atom) -> Dynamic

@external(erlang, "logger", "get_primary_config")
fn get_primary_config() -> Dynamic

@external(erlang, "logger", "set_primary_config")
fn restore_primary_config(config: Dynamic) -> Dynamic

@external(erlang, "logger", "set_primary_config")
fn set_primary_config(key: Atom, value: Atom) -> Dynamic

@external(erlang, "logger", "get_handler_config")
fn get_handler_config(id: Atom) -> Result(Dynamic, Dynamic)

@external(erlang, "logger", "set_handler_config")
fn set_handler_config(id: Atom, key: Atom, value: Dynamic) -> Dynamic

fn console_level() -> Option(Dynamic) {
  let level = atom.create("level")
  get_handler_config(atom.create("default"))
  |> result.replace_error(Nil)
  |> result.try(fn(config) {
    decode.run(config, decode.at([level], decode.dynamic))
    |> result.replace_error(Nil)
  })
  |> option.from_result
}

fn set_console_level(level: Dynamic) -> Nil {
  let _ =
    set_handler_config(atom.create("default"), atom.create("level"), level)
  Nil
}

fn key() -> Atom {
  atom.create("fluxer_gateway_geo_probe_lines")
}

fn handler() -> Atom {
  atom.create("fluxer_gateway_geo_probe")
}

pub fn capture_logs() -> Capture {
  let lines = process.new_subject()
  put_term(key(), Some(lines))
  let primary = get_primary_config()
  let console = console_level()
  case console {
    Some(_) -> set_console_level(atom.to_dynamic(atom.create("notice")))
    None -> Nil
  }
  let _ = set_primary_config(atom.create("level"), atom.create("all"))
  let _ = remove_handler(handler())
  let _ =
    add_handler(
      handler(),
      atom.create("support@geo_probe"),
      dynamic.properties([
        #(
          atom.to_dynamic(atom.create("level")),
          atom.to_dynamic(atom.create("all")),
        ),
      ]),
    )
  Capture(lines:, primary:, console:)
}

pub fn log(event: Dynamic, _config: Dynamic) -> Nil {
  case get_term(key(), None) {
    Some(lines) -> process.send(lines, string.inspect(event))
    None -> Nil
  }
}

pub fn finish(capture: Capture) -> List(String) {
  let _ = remove_handler(handler())
  let _ = restore_primary_config(capture.primary)
  let _ = option.map(capture.console, set_console_level)
  put_term(key(), None)
  drain(capture.lines, [])
}

fn drain(lines: Subject(String), acc: List(String)) -> List(String) {
  case process.receive(lines, 0) {
    Ok(line) -> drain(lines, [line, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}
