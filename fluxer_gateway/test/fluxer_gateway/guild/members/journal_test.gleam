// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/member
import fluxer_gateway/guild/members.{Changes}
import gleam/int
import gleam/list
import gleam/option.{None}
import support/guild_handoff_fixtures as fixtures

fn rows(users: List(Int)) -> List(member.Member) {
  fixtures.loaded(users).members
}

fn user(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn ids(found: List(member.Member)) -> List(Int) {
  found
  |> list.map(fn(row) { id.user_id_to_int(member.user_id(row)) })
  |> list.sort(int.compare)
}

pub fn a_tracked_table_reports_each_write_once_test() {
  let table = members.new(None)
  members.put_many(table, rows([1, 2, 3]))
  let tracked = members.track(table)
  assert members.take_changes(tracked) == Changes(upserted: [], removed: [])
  members.put_many(tracked, rows([4]))
  members.remove(tracked, user(2))
  members.put_many(tracked, rows([1]))
  members.put_many(tracked, rows([1]))
  let first = members.take_changes(tracked)
  assert ids(first.upserted) == [1, 4]
  assert first.removed == [2]
  assert members.take_changes(tracked) == Changes(upserted: [], removed: [])
  members.replace_all(tracked, rows([1, 5]))
  let second = members.take_changes(tracked)
  assert ids(second.upserted) == [1, 5]
  assert list.sort(second.removed, int.compare) == [3, 4]
  let plain = members.untrack(tracked)
  members.put_many(plain, rows([6]))
  assert members.take_changes(plain) == Changes(upserted: [], removed: [])
  assert members.count(plain) == 3
  members.drop(plain)
}

pub fn removing_a_member_added_during_tracking_reports_a_removal_test() {
  let tracked = members.track(members.new(None))
  members.put_many(tracked, rows([7]))
  members.remove(tracked, user(7))
  assert members.take_changes(tracked) == Changes(upserted: [], removed: [7])
  members.drop(tracked)
}
