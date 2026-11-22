// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/session/manager/loads
import fluxer_gateway/session/manager/registry
import gleam/dict
import gleam/int
import gleam/list
import gleam/option

pub fn balance_fills_the_lightest_target_first_test() {
  assert loads.balance(["a", "b", "c", "d", "e"], [#("x", 3), #("y", 0)])
    == [#("a", "y"), #("b", "y"), #("c", "y"), #("d", "x"), #("e", "y")]
}

pub fn balance_without_targets_places_nothing_test() {
  assert loads.balance(["a", "b"], []) == []
  assert loads.balance([], [#("x", 0)]) == []
}

fn roll(pods: Int, per_pod: Int) -> dict.Dict(Int, Int) {
  list.repeat(Nil, pods)
  |> list.index_map(fn(_, step) { step })
  |> list.fold(dict.new(), fn(fresh, step) {
    let fresh = dict.insert(fresh, step, 0)
    let placed =
      loads.balance(list.repeat(Nil, per_pod), dict.to_list(fresh))
      |> list.map(fn(pair) { pair.1 })
    list.fold(placed, fresh, fn(acc, target) {
      dict.upsert(acc, target, fn(found) { option.unwrap(found, 0) + 1 })
    })
  })
}

pub fn a_rolling_drain_leaves_every_new_pod_with_one_old_pods_share_test() {
  let counts = dict.values(roll(12, 300))
  assert int.sum(counts) == 12 * 300
  assert list.all(counts, fn(count) { count == 300 })
}

pub fn of_reads_the_session_count_of_reachable_targets_test() {
  assert loads.of([node.self()], 2000) == [#(node.self(), registry.count())]
  assert loads.of([], 2000) == []
}
