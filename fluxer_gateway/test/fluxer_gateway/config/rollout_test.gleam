// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/cgroup
import fluxer_gateway/config/node_name
import fluxer_gateway/config/rollout
import gleam/dict
import gleam/dynamic
import gleam/option.{None, Some}

pub fn empty_config_is_default_test() {
  assert rollout.validate(dict.new()) == Ok(rollout.default_config())
}

pub fn invalid_field_rejects_test() {
  let fields = dict.from_list([#("rpc_request_timeout_ms", dynamic.int(500))])
  let assert Error(_) = rollout.validate(fields)
  let fields = dict.from_list([#("session_rollout_mode", dynamic.string("x"))])
  let assert Error(_) = rollout.validate(fields)
}

pub fn unknown_fields_are_ignored_test() {
  let fields = dict.from_list([#("something_else", dynamic.string("x"))])
  assert rollout.validate(fields) == Ok(rollout.default_config())
}

pub fn fractional_percentage_is_accepted_test() {
  let fields =
    dict.from_list([#("guild_rollout_percentage", dynamic.float(12.5))])
  let assert Ok(_) = rollout.validate(fields)
  let fields =
    dict.from_list([#("guild_rollout_percentage", dynamic.float(100.5))])
  let assert Error(_) = rollout.validate(fields)
}

pub fn eligibility_bounds_test() {
  assert rollout.eligible("1", rollout.WholePercentage(100), rollout.Modulo)
  assert !rollout.eligible("1", rollout.WholePercentage(0), rollout.Random)
  assert !rollout.eligible(
    "1",
    rollout.FractionalPercentage(0.0),
    rollout.Modulo,
  )
}

pub fn payload_config_test() {
  let assert Ok(fields) =
    rollout.payload_config(<<
      "{\"type\":\"gateway_rollout_config\",\"config\":{\"max_concurrent_guild_starts\":5}}":utf8,
    >>)
  assert dict.has_key(fields, "max_concurrent_guild_starts")
  let assert Ok(fields) =
    rollout.payload_config(<<"{\"max_concurrent_guild_starts\":5}":utf8>>)
  assert dict.has_key(fields, "max_concurrent_guild_starts")
  let assert Error(Nil) = rollout.payload_config(<<"[1]":utf8>>)
}

pub fn cgroup_directories_test() {
  assert cgroup.directories("/kubepods/pod1/abc")
    == [
      "/sys/fs/cgroup",
      "/sys/fs/cgroup/kubepods",
      "/sys/fs/cgroup/kubepods/pod1",
      "/sys/fs/cgroup/kubepods/pod1/abc",
    ]
}

pub fn cgroup_lowest_limit_test() {
  let read = fn(path) {
    case path {
      "/sys/fs/cgroup/memory.max" -> Some("max\n")
      "/sys/fs/cgroup/a/memory.max" -> Some("2147483648\n")
      "/sys/fs/cgroup/a/b/memory.max" -> Some("4294967296\n")
      _ -> None
    }
  }
  assert cgroup.own_memory_limit("0::/a/b\n", read) == Some(2_147_483_648)
  assert cgroup.own_memory_limit("1:cpu:/a\n", read) == None
  assert cgroup.session_start_ceiling(2_147_483_648) == 341
  assert cgroup.session_start_ceiling(1024) == 8
}

pub fn node_name_test() {
  assert node_name.parse(" gw@10.0.0.1 ") == Ok("gw@10.0.0.1")
  assert node_name.parse("gw") == Error(Nil)
  assert node_name.parse("gw@a@b") == Error(Nil)
  assert node_name.parse("g w@host") == Error(Nil)
  assert node_name.from_parts(base: "gw", host: "10.0.0.2") == Ok("gw@10.0.0.2")
}
