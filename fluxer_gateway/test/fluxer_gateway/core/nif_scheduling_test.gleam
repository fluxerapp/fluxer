// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_scan.{Found}
import fluxer_gateway/gateway/zstd
import fluxer_gateway/member_list/config as ml_config
import fluxer_gateway/member_list/native as ml_native
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/native as perms_native
import fluxer_gateway/perms/snapshot as perm_snapshot
import gleam/bit_array
import gleam/bytes_tree.{type BytesTree}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import support/nif_probe
import support/perms_ml_fixture as f

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn as_tree(value: List(Dynamic)) -> BytesTree

fn numbers(count: Int) -> List(String) {
  int.range(from: 0, to: count, with: [], run: fn(acc, i) {
    [int.to_string(i), ..acc]
  })
  |> list.reverse
}

pub fn an_array_with_many_items_reschedules_and_keeps_every_item_test() {
  let items = numbers(5000)
  let bytes = bit_array.from_string("[" <> string.join(items, ",") <> "]")
  assert bit_array.byte_size(bytes) < 262_144
  let assert Ok(found) = json_scan.array(bytes)
  assert list.map(found, bit_array.to_string) == list.map(items, Ok)
}

pub fn an_object_with_many_members_reschedules_and_keeps_order_test() {
  let keys = numbers(5000)
  let body =
    keys
    |> list.map(fn(key) { "\"k" <> key <> "\":" <> key })
    |> string.join(",")
  let bytes = bit_array.from_string("{" <> body <> "}")
  let assert Ok(found) = json_scan.object(bytes)
  assert list.map(found, fn(pair) { pair.0 })
    == list.map(keys, fn(key) { "k" <> key })
  assert list.map(found, fn(pair) { bit_array.to_string(pair.1) })
    == list.map(keys, Ok)
  let assert Ok(Found(value_start:, value_end:, ..)) =
    json_scan.locate(bytes, "k4999")
  assert bit_array.slice(bytes, value_start, value_end - value_start)
    == Ok(<<"4999">>)
}

pub fn a_large_decompress_reschedules_and_round_trips_test() {
  let text = string.repeat("member list payload ", 20_000)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(packed) =
    zstd.compress_dirty(compressor, bytes_tree.from_string(text))
  let assert Ok(unpacked) = zstd.decompress(decompressor, packed, 1_000_000)
  assert unpacked == bit_array.from_string(text)
  let assert Ok(small) =
    zstd.compress(compressor, bytes_tree.from_string("{\"op\":1}"))
  let assert Ok(back) = zstd.decompress(decompressor, small, 16_384)
  assert back == <<"{\"op\":1}">>
}

fn json_tree(count: Int) -> BytesTree {
  numbers(count)
  |> json.array(json.string)
  |> json.to_string_tree
  |> bytes_tree.from_string_tree
}

fn round_trip(packed: BitArray, limit: Int) -> BitArray {
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(unpacked) = zstd.decompress(decompressor, packed, limit)
  unpacked
}

pub fn json_iodata_with_byte_integers_compresses_on_the_normal_scheduler_test() {
  let tree = json_tree(40)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let assert Ok(packed) = zstd.compress(compressor, tree)
  assert round_trip(packed, 16_384) == bytes_tree.to_bit_array(tree)
}

pub fn a_large_json_iodata_goes_dirty_before_its_tail_is_read_test() {
  let tree = json_tree(20_000)
  assert bytes_tree.byte_size(tree) > 65_536
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let spoiled = as_tree([to_dynamic(tree), to_dynamic(atom.create("tail"))])
  assert zstd.compress(compressor, spoiled) == Error(zstd.NeedsDirty)
  assert zstd.compress_dirty(compressor, spoiled)
    == Error(zstd.BadInput("data"))
  assert zstd.compress(compressor, tree) == Error(zstd.NeedsDirty)
  let assert Ok(packed) = zstd.compress_dirty(compressor, tree)
  assert round_trip(packed, 1_000_000) == bytes_tree.to_bit_array(tree)
}

pub fn iodata_with_a_wide_integer_is_bad_input_test() {
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let wide = as_tree([to_dynamic(<<"a">>), to_dynamic(256)])
  assert zstd.compress(compressor, wide) == Error(zstd.BadInput("data"))
  let plain = as_tree([to_dynamic(<<"a">>)])
  let assert Ok(packed) = zstd.compress(compressor, plain)
  assert round_trip(packed, 16) == <<"a">>
}

fn members(count: Int) -> List(#(Int, BitArray)) {
  int.range(from: count, to: 0, with: [], run: fn(acc, user) {
    [#(user, <<"member">>), ..acc]
  })
}

pub fn a_large_member_list_snapshot_goes_dirty_before_it_is_decoded_test() {
  let assert Ok(Nil) = nif.load()
  let #(handle, _) = perms.new(f.guild(100), process.self())
  let #(list, _) =
    ml_native.ml_new(
      100,
      ml_config.to_native(ml_config.default()),
      process.self(),
      perms.native_handle(handle),
    )
  let spoiled =
    ml_native.Snapshot(
      members: list.append(members(20_000), [#(0, <<>>)]),
      statuses: [],
      connected: [],
      updates: ml_native.UpdatesOn,
    )
  assert ml_native.ml_load(list, spoiled) == Error(perms_native.NeedsDirty)
  let assert Error(perms_native.BadInput(_)) =
    ml_native.ml_load_dirty(list, spoiled)
  let small =
    ml_native.Snapshot(..spoiled, members: [#(1, <<"one">>), #(0, <<>>)])
  let assert Error(perms_native.BadInput(_)) = ml_native.ml_load(list, small)
  ml_native.ml_release(list)
  perms.release(handle)
}

pub fn a_normal_permission_load_charges_its_timeslice_test() {
  let assert Ok(Nil) = nif.load()
  let source =
    int.range(
      from: 1,
      to: 2001,
      with: f.base([f.role(100, f.view, 0)], [f.text(11, [])]),
      run: fn(acc, user) { perm_snapshot.add_member(acc, f.member(user, [])) },
    )
  let native = perm_snapshot.to_native(source)
  let #(handle, _) = perms.new(f.guild(100), process.self())
  let #(loaded, spent) =
    nif_probe.charged(fn() {
      perms_native.perm_load(perms.native_handle(handle), native)
    })
  assert loaded == Ok(1)
  assert spent >= 1000
  perms.release(handle)
}
