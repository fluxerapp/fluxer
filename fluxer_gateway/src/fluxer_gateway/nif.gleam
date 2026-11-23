// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/application
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type LoadError {
  NoPrivDirectory
  LoadFailed(reason: String)
}

type Key {
  FluxerGatewayNifLoaded
}

type Stored

@external(erlang, "erlang", "apply")
fn apply(
  module: atom.Atom,
  function: atom.Atom,
  arguments: List(Dynamic),
) -> Dynamic

@external(erlang, "erlang", "nif_error")
fn nif_error(reason: String) -> a

@external(erlang, "gleam_stdlib", "identity")
fn unlisted(nothing: List(Dynamic)) -> Dynamic

@external(erlang, "persistent_term", "get")
fn get_loaded(key: Key, default: Bool) -> Bool

@external(erlang, "persistent_term", "put")
fn put_loaded(key: Key, value: Bool) -> Stored

pub fn load() -> Result(Nil, LoadError) {
  case is_loaded(), application.priv_directory("fluxer_gateway") {
    True, _ -> Ok(Nil)
    False, Error(Nil) -> Error(NoPrivDirectory)
    False, Ok(directory) -> {
      let outcome = load_nif(directory <> "/gateway_nif")
      case decode.run(outcome, atom.decoder()), error_kind(outcome) {
        Ok(_), _ | _, Ok("reload") -> verify_exports()
        _, _ -> Error(LoadFailed(string.inspect(outcome)))
      }
    }
  }
}

fn verify_exports() -> Result(Nil, LoadError) {
  let provided =
    apply(this_module(), atom.create("gateway_nif_exports"), [])
    |> decode.run(decode.list(provided_decoder()))
    |> result.unwrap([])
  let missing =
    list.filter(declared_nifs(), fn(nif) { !list.contains(provided, nif) })
  case provided, missing {
    [], _ ->
      Error(LoadFailed(
        "gateway_nif.so does not list its NIFs, rebuild it with scripts/build.sh",
      ))
    _, [] -> {
      put_loaded(FluxerGatewayNifLoaded, True)
      Ok(Nil)
    }
    _, missing ->
      Error(LoadFailed(
        "gateway_nif.so lacks "
        <> list.map(missing, describe) |> string.join(", ")
        <> ", rebuild it with scripts/build.sh",
      ))
  }
}

fn provided_decoder() -> decode.Decoder(#(String, Int)) {
  use name <- decode.field(0, decode.string)
  use arity <- decode.field(1, decode.int)
  decode.success(#(name, arity))
}

fn declared_nifs() -> List(#(String, Int)) {
  let exported =
    apply(this_module(), atom.create("module_info"), [
      atom.to_dynamic(atom.create("exports")),
    ])
  let decoder = {
    use name <- decode.field(0, atom.decoder())
    use arity <- decode.field(1, decode.int)
    decode.success(#(atom.to_string(name), arity))
  }
  decode.run(exported, decode.list(decoder))
  |> result.unwrap([])
  |> list.filter(fn(function) { !list.contains(not_nifs, function) })
}

fn this_module() -> atom.Atom {
  atom.create("fluxer_gateway@nif")
}

const not_nifs = [
  #("load", 0),
  #("is_loaded", 0),
  #("module_info", 0),
  #("module_info", 1),
]

fn describe(nif: #(String, Int)) -> String {
  nif.0 <> "/" <> int.to_string(nif.1)
}

fn load_nif(path: String) -> Dynamic {
  let characters =
    string.to_utf_codepoints(path)
    |> list.map(fn(codepoint) {
      dynamic.int(string.utf_codepoint_to_int(codepoint))
    })
  apply(atom.create("erlang"), atom.create("load_nif"), [
    dynamic.list(characters),
    dynamic.int(0),
  ])
}

pub fn is_loaded() -> Bool {
  get_loaded(FluxerGatewayNifLoaded, False)
}

fn error_kind(outcome: Dynamic) -> Result(String, Nil) {
  decode.run(outcome, decode.at([1, 0], atom.decoder()))
  |> result.map(atom.to_string)
  |> result.replace_error(Nil)
}

pub fn perm_new(_guild: a0, _config: a1, _owner: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_load(_perms: a0, _snapshot: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_load_dirty(_perms: a0, _snapshot: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_apply(_perms: a0, _events: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_apply_dirty(_perms: a0, _events: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_compact(_perms: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_compact_dirty(_perms: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_model(_reader: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_permissions(_reader: a0, _user: a1, _scope: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_check(_reader: a0, _user: a1, _scope: a2, _bits: a3) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_visible(_reader: a0, _user: a1, _channel: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_has_virtual_view(_reader: a0, _user: a1, _channel: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_viewable(_reader: a0, _user: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_shares_view_many(_reader: a0, _user: a1, _candidates: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_audience(_reader: a0, _channel: a1, _need: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_viewers(_reader: a0, _channel: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_viewers_dirty(_reader: a0, _channel: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_filter_viewers(_reader: a0, _channel: a1, _users: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_thread_permissions(
  _reader: a0,
  _user: a1,
  _parent: a2,
  _private: a3,
  _member: a4,
  _timed_out: a5,
) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_query(_reader: a0, _query: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_query_dirty(_reader: a0, _query: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_reply(_reader: a0, _query: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_epoch(_reader: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_revision(_reader: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_status(_perms: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn perm_release(_perms: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_new(_guild: a0, _config: a1, _owner: a2, _perms: a3) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_load(_list: a0, _snapshot: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_load_dirty(_list: a0, _snapshot: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_apply(_list: a0, _events: a1, _delta: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_apply_dirty(_list: a0, _events: a1, _delta: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_fill(_list: a0, _items: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_resume(_list: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_query(_list: a0, _query: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_query_dirty(_list: a0, _query: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_counts(_counts: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn ml_release(_list: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn push_masks_set(_list: a0, _updates: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn push_masks_query(_list: a0, _query: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn push_job_body(_head: a0, _users: a1, _tail: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn push_job_body_dirty(_head: a0, _users: a1, _tail: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn zstd_compressor_new(
  _level: a0,
  _window_log: a1,
  _max_block_size: a2,
) -> r {
  nif_error("nif_not_loaded")
}

pub fn zstd_compress(_compressor: a0, _data: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn zstd_compress_dirty(_compressor: a0, _data: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn zstd_decompressor_new(_window_log_max: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn zstd_decompress(_decompressor: a0, _data: a1, _max_size: a2) -> r {
  nif_error("nif_not_loaded")
}

pub fn md_render_push_preview(_content: a0, _context: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn json_scan_object(_data: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn json_scan_object_dirty(_data: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn json_scan_array(_data: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn json_scan_array_dirty(_data: a0) -> r {
  nif_error("nif_not_loaded")
}

pub fn json_scan_locate(_data: a0, _key: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn json_scan_locate_dirty(_data: a0, _key: a1) -> r {
  nif_error("nif_not_loaded")
}

pub fn gateway_nif_metrics() -> r {
  nif_error("nif_not_loaded")
}

pub fn gateway_nif_exports() -> Dynamic {
  unlisted([])
}
