// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree
import fluxer_gateway/core/presence.{Dnd, Idle, Invisible, Offline, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/custom_status
import fluxer_gateway/presence/digest
import fluxer_gateway/presence/display.{SessionState}
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/payload
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/route
import fluxer_gateway/presence/status
import fluxer_gateway/presence/version
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

pub fn status_parse_test() {
  status.settings_invisible(Some("offline")) |> should.equal(Some(True))
  status.settings_invisible(Some("idle")) |> should.equal(Some(False))
  status.settings_invisible(Some("x")) |> should.equal(None)
  status.to_wire(Invisible) |> should.equal("offline")
}

pub fn precedence_test() {
  status.precedence([Idle, Online, Invisible]) |> should.equal(Online)
  status.precedence([Idle, Dnd]) |> should.equal(Dnd)
  status.precedence([]) |> should.equal(Offline)
}

pub fn display_round_trip_test() {
  list.each([Online, Idle, Dnd, Invisible, Offline], fn(s) {
    let shown = display.new(status: s, mobile: True, afk: False)
    display.status(shown) |> should.equal(s)
    display.mobile(shown) |> should.be_true
    display.afk(shown) |> should.be_false
    display.from_int(display.to_int(shown)) |> should.equal(Ok(shown))
  })
}

pub fn flatten_rules_test() {
  let mobile_online = SessionState(status: Online, afk: True, mobile: True)
  let idle = SessionState(status: Idle, afk: True, mobile: False)
  let shown = display.flatten([mobile_online, idle], invisible: False)
  display.status(shown) |> should.equal(Online)
  display.mobile(shown) |> should.be_true
  display.afk(shown) |> should.be_false
  let hidden = display.flatten([mobile_online, idle], invisible: True)
  display.status(hidden) |> should.equal(Invisible)
  display.afk(hidden) |> should.be_true
  display.flatten([], invisible: False) |> should.equal(display.offline())
}

pub fn external_display_test() {
  display.new(status: Invisible, mobile: True, afk: True)
  |> display.external
  |> display.status
  |> should.equal(Offline)
}

pub fn id_set_test() {
  let a = id_set.from_list([5, 1, 3, 3, -1])
  id_set.to_list(a) |> should.equal([1, 3, 5])
  id_set.contains(a, 3) |> should.be_true
  id_set.contains(a, 4) |> should.be_false
  let b = id_set.from_list([3, 4])
  id_set.to_list(id_set.union(a, b)) |> should.equal([1, 3, 4, 5])
  id_set.to_list(id_set.subtract(a, b)) |> should.equal([1, 5])
  let #(added, removed) = id_set.diff(old: a, new: b)
  id_set.to_list(added) |> should.equal([4])
  id_set.to_list(removed) |> should.equal([1, 5])
  id_set.size(id_set.from_packed(id_set.to_packed(a))) |> should.equal(3)
}

pub fn id_set_remove_test() {
  let a = id_set.from_list([1, 3, 5, 1_310_000_000_000_000_007])
  id_set.to_list(id_set.remove(a, 3))
  |> should.equal([1, 5, 1_310_000_000_000_000_007])
  id_set.to_list(id_set.remove(a, 1))
  |> should.equal([3, 5, 1_310_000_000_000_000_007])
  id_set.to_list(id_set.remove(a, 1_310_000_000_000_000_007))
  |> should.equal([1, 3, 5])
  id_set.remove(a, 4) |> should.equal(a)
  id_set.remove(id_set.empty(), 4) |> should.equal(id_set.empty())
}

pub fn id_set_group_low_partitions_by_shard_test() {
  let users =
    int.range(1, 401, [], list.prepend)
    |> list.map(fn(n) { 1_310_000_000_000_000_000 + n * 7919 })
  let set = id_set.from_list([5, ..users])
  let picked =
    id_set.group_low(set, route.shard_of_low)
    |> dict.to_list
    |> list.flat_map(fn(group) {
      list.map(group.1, fn(user) {
        route.shard(user) |> should.equal(group.0)
        user
      })
    })
  list.sort(picked, int.compare) |> should.equal(id_set.to_list(set))
}

pub fn version_seen_test() {
  let first = version.first(version.new_epoch())
  let second = version.next(first)
  version.compare(second, first) |> should.equal(version.Newer)
  version.compare(first, second) |> should.equal(version.Stale)
  version.compare(version.first(version.new_epoch()), second)
  |> should.equal(version.OtherEpoch)
  let #(seen, fresh) = version.observe(version.empty_seen(), 1, second)
  fresh |> should.be_true
  let #(seen, again) = version.observe(seen, 1, first)
  again |> should.be_false
  version.seen_size(version.forget(seen, [1])) |> should.equal(0)
}

pub fn digest_test() {
  let full = digest.of([1, 2, 3])
  digest.remove(full, 3) |> should.equal(digest.of([1, 2]))
  digest.add(digest.of([2, 1]), 3) |> should.equal(full)
  { digest.of([1, 2]) == digest.of([3]) } |> should.be_false
  digest.remove(digest.empty(), 9)
  |> digest.add(9)
  |> should.equal(digest.empty())
}

pub fn shard_range_test() {
  int.range(1, 501, [], list.prepend)
  |> list.each(fn(user) {
    let shard = route.shard(user)
    { shard >= 0 && shard < route.shard_count } |> should.be_true
  })
  route.shard(123_456_789) |> should.equal(route.shard(123_456_789))
  let used =
    int.range(0, 4000, [], list.prepend)
    |> list.map(fn(n) { route.shard(1_310_000_000_000_000_000 + n) })
    |> list.unique
  list.length(used) |> should.equal(route.shard_count)
}

fn tree(bits: BitArray) -> json_tree.JsonTree {
  json_tree.from_raw(raw_json.wrap(bits))
}

fn profile_of(bits: BitArray) -> profile.Profile {
  tree(bits) |> json_tree.fields |> should.be_ok |> profile.from_fields
}

pub fn canonical_tree_test() {
  tree(<<"{\"b\":1,\"a\":[true,null,\"x\"]}">>)
  |> json_tree.canonical
  |> should.equal(Ok(<<"{\"a\":[true,null,\"x\"],\"b\":1}">>))
  tree(<<"{\"b\":{\"d\":\"\\u0041\",\"c\":1.5}, \"a\" : \"x/y\"}">>)
  |> json_tree.canonical
  |> should.equal(Ok(<<"{\"a\":\"x/y\",\"b\":{\"c\":1.5,\"d\":\"A\"}}">>))
  json_tree.snowflake(json_tree.String("0123")) |> should.equal(Error(Nil))
  json_tree.snowflake(json_tree.String("123")) |> should.equal(Ok(123))
  profile.wire(profile_of(<<"{\"avatar_color\":\"-5\",\"flags\":\"\"}">>))
  |> should.equal(<<"{\"avatar_color\":-5}">>)
  profile.wire(
    profile_of(<<"{\"id\":42,\"avatar_color\":\"+7\",\"flags\":\"07\"}">>),
  )
  |> should.equal(<<"{\"avatar_color\":7,\"flags\":7,\"id\":\"42\"}">>)
}

pub fn profile_wire_test() {
  let made =
    profile_of(<<
      "{\"id\":\"42\",\"username\":\"ann\",\"avatar\":\"h\",\"flags\":\"3\",\"email\":\"x\",\"profile_restricted\":true}",
    >>)
  profile.restricted(made) |> should.be_true
  profile.wire(made)
  |> should.equal(<<
    "{\"avatar\":null,\"flags\":3,\"global_name\":null,\"id\":\"42\",\"username\":\"ann\"}",
  >>)
  let same = profile.apply_update(made, made)
  same.differs |> should.be_false
  same.restriction_changed |> should.be_false
}

fn status_value(text: String) -> custom_status.CustomStatus {
  tree(bit_array.from_string(text))
  |> custom_status.from_tree
  |> should.be_ok
}

pub fn custom_status_classify_test() {
  let current = status_value("{\"text\":\"hi\",\"extra\":1}")
  let same = status_value("{\"text\":\"hi\"}")
  let other = status_value("{\"text\":\"yo\"}")
  custom_status.classify(Some(current), custom_status.OpMap(same))
  |> should.equal(custom_status.Keep)
  custom_status.classify(Some(current), custom_status.OpMap(other))
  |> should.equal(custom_status.Validate(other))
  custom_status.classify(Some(current), custom_status.OpNull)
  |> should.equal(custom_status.Clear)
  custom_status.classify(Some(current), custom_status.OpAbsent)
  |> should.equal(custom_status.Keep)
}

pub fn custom_status_visible_test() {
  let value = status_value("{\"text\":\"hi\"}")
  let online = display.new(status: Online, mobile: False, afk: False)
  let invisible = display.new(status: Invisible, mobile: False, afk: False)
  custom_status.visible_wire(
    Some(value),
    now_ms: 0,
    restricted: False,
    display: online,
  )
  |> should.equal(<<"{\"text\":\"hi\"}">>)
  custom_status.visible_wire(
    Some(value),
    now_ms: 0,
    restricted: True,
    display: online,
  )
  |> should.equal(<<"null">>)
  custom_status.visible_wire(
    Some(value),
    now_ms: 0,
    restricted: False,
    display: invisible,
  )
  |> should.equal(<<"null">>)
}

pub fn payload_wire_test() {
  let shown = display.new(status: Idle, mobile: False, afk: True)
  payload.status_wire(shown, <<"null">>, <<"{\"id\":1}">>)
  |> should.equal(<<
    "{\"afk\":true,\"custom_status\":null,\"mobile\":false,\"status\":\"idle\",\"user\":{\"id\":1}}",
  >>)
  payload.forced_offline_wire(9)
  |> should.equal(<<
    "{\"afk\":false,\"custom_status\":null,\"mobile\":false,\"status\":\"offline\",\"user\":{\"id\":\"9\"}}",
  >>)
}
