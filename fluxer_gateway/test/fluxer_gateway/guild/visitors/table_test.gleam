// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{SessionRef}
import fluxer_gateway/guild/visitors/table
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/set

fn visitor(user: Int, tier: table.Tier, since: Int, ip: option.Option(String)) {
  let assert Ok(user_id) = id.user_id(user)
  let ref =
    SessionRef(
      id: session.new_session_id(),
      user: user_id,
      inbox: process.new_subject(),
    )
  table.Visitor(
    ref:,
    pid: process.self(),
    monitor: process.monitor(process.self()),
    adult: False,
    thread_viewer: False,
    operator: False,
    tier:,
    ip_key: ip,
    since_ms: since,
    focus: None,
    view: set.from_list([1, 2]),
  )
}

pub fn counts_follow_put_and_remove_test() {
  let first = visitor(5, table.Unclaimed, 10, Some("ip"))
  let second = visitor(5, table.Verified, 20, Some("ip"))
  let current =
    table.new()
    |> table.put(first)
    |> table.put(second)
  assert table.count(current) == 2
  assert table.user_count(current, 5) == 2
  assert table.ip_count(current, Some("ip")) == 2
  let #(current, removed) = table.remove(current, table.key(first))
  assert removed == Some(first)
  assert table.user_count(current, 5) == 1
  assert table.ip_count(current, None) == 0
}

pub fn oldest_unverified_test() {
  let old = visitor(1, table.Unverified, 5, None)
  let young = visitor(2, table.Unclaimed, 50, None)
  let verified = visitor(3, table.Verified, 1, None)
  let current =
    table.new()
    |> table.put(young)
    |> table.put(verified)
    |> table.put(old)
  assert table.oldest_unverified(current) == Some(old)
}

pub fn focus_index_test() {
  let found = visitor(1, table.Unclaimed, 5, None)
  let current = table.new() |> table.put(found)
  let focused = table.Visitor(..found, focus: Some(2))
  let current = table.replace(current, focused)
  assert table.focused(current, 2) == [focused]
  let current = table.replace(current, table.Visitor(..focused, focus: None))
  assert table.focused(current, 2) == []
}

pub fn tier_and_ip_parse_test() {
  assert table.tier_from_wire(Some("verified")) == table.Verified
  assert table.tier_from_wire(Some("other")) == table.Unclaimed
  assert table.ip_key(Some("")) == None
}

pub fn monitor_index_follows_put_replace_and_remove_test() {
  let found = visitor(1, table.Unclaimed, 5, None)
  let other = visitor(2, table.Unclaimed, 6, None)
  let current =
    table.new()
    |> table.put(found)
    |> table.put(other)
  assert table.find_by_monitor(current, found.monitor) == Some(found)
  assert table.find_by_monitor(current, other.monitor) == Some(other)
  let moved = table.Visitor(..found, monitor: process.monitor(process.self()))
  let current = table.replace(current, moved)
  assert table.find_by_monitor(current, found.monitor) == None
  assert table.find_by_monitor(current, moved.monitor) == Some(moved)
  let #(current, _) = table.remove(current, table.key(moved))
  assert table.find_by_monitor(current, moved.monitor) == None
  assert table.find_by_monitor(current, other.monitor) == Some(other)
}

pub fn put_over_existing_key_keeps_counts_and_index_test() {
  let found = visitor(1, table.Unclaimed, 5, Some("ip"))
  let again = table.Visitor(..found, monitor: process.monitor(process.self()))
  let current =
    table.new()
    |> table.put(found)
    |> table.put(again)
  assert table.count(current) == 1
  assert table.user_count(current, 1) == 1
  assert table.ip_count(current, Some("ip")) == 1
  assert table.find_by_monitor(current, found.monitor) == None
  assert table.find_by_monitor(current, again.monitor) == Some(again)
}
