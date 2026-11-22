// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom
import gleam/list
import gleam/string
import support/cluster_peers as peers
import support/cluster_trio as trio

const users = 3000

pub fn presence_and_push_reroute_at_once_when_a_node_crashes_test_() {
  #(atom.create("timeout"), 120, fn() {
    let b = peers.unique_name("scn3x_b")
    let c = peers.unique_name("scn3x_c")
    let d = peers.unique_name("scn3x_d")
    let names = [b, c, d]
    let sorted = list.sort(names, string.compare)
    let peer_b = trio.boot(b, names)
    let peer_c = trio.boot(c, names)
    let peer_d = trio.boot(d, names)
    let survivors = [peer_b, peer_c]
    list.each([peer_b, peer_c, peer_d], trio.quicken(_, 60_000))
    assert peers.wait_until(40_000, fn() {
      list.all([peer_b, peer_c, peer_d], fn(peer) {
        trio.serving(peer) == sorted
        && trio.probe_names(peer, "presence_nodes") == sorted
        && trio.probe_int(peer, "presence_owned_by", [
          peers.to_dynamic(d),
          peers.to_dynamic(users),
        ])
        > users / 5
      })
    })
    assert trio.probe_int(peer_b, "push_owned_by", [
        peers.to_dynamic(d),
        peers.to_dynamic(users),
      ])
      > users / 5

    trio.crash(peer_d)
    assert peers.wait_until(5000, fn() {
      list.all(survivors, fn(peer) { !trio.is_up(peer, d) })
    })
    assert peers.wait_until(3000, fn() {
      list.all(survivors, fn(peer) {
        trio.probe_int(peer, "presence_unavailable", [peers.to_dynamic(users)])
        == 0
        && trio.probe_int(peer, "push_unavailable", [peers.to_dynamic(users)])
        == 0
      })
    })
    list.each(survivors, fn(peer) {
      assert list.contains(trio.probe_names(peer, "members"), d)
      assert trio.probe_int(peer, "presence_owned_by", [
          peers.to_dynamic(d),
          peers.to_dynamic(users),
        ])
        == 0
      assert trio.probe_names(peer, "presence_nodes")
        == list.filter(sorted, fn(name) { name != d })
    })
    let b_view =
      trio.probe_int(peer_b, "presence_owned_by", [
        peers.to_dynamic(c),
        peers.to_dynamic(users),
      ])
    let c_view =
      trio.probe_int(peer_c, "presence_owned_by", [
        peers.to_dynamic(c),
        peers.to_dynamic(users),
      ])
    assert b_view == c_view
    list.each(survivors, peers.stop)
  })
}
