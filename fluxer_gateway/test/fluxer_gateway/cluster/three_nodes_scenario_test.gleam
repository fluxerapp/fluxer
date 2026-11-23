// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/cluster_relay_probe as probe
import support/cluster_trio as trio

fn collector(peer: Peer) -> Dynamic {
  peers.call(peer, "support@cluster_relay_probe", "start_collector", [])
}

fn collected(peer: Peer, found: Dynamic) -> List(String) {
  let events =
    peers.call(peer, "support@cluster_relay_probe", "collected", [found])
  let assert Ok(events) = decode.run(events, decode.list(decode.string))
  events
}

fn send_events(from: Peer, targets: List(Dynamic), count: Int) -> Nil {
  peers.call(from, "support@cluster_relay_probe", "send_events", [
    peers.to_dynamic(targets),
    peers.to_dynamic(count),
  ])
  Nil
}

pub fn partition_relay_drain_and_heal_on_three_nodes_test() {
  let b = peers.unique_name("scn3_b")
  let c = peers.unique_name("scn3_c")
  let d = peers.unique_name("scn3_d")
  let names = [b, c, d]
  let sorted = list.sort(names, string.compare)
  let peer_b = trio.boot(b, names)
  let peer_c = trio.boot(c, names)
  let peer_d = trio.boot(d, names)
  let all = [peer_b, peer_c, peer_d]
  list.each(all, trio.quicken(_, 2000))
  assert peers.wait_until(30_000, fn() {
    list.all(all, fn(peer) { trio.serving(peer) == sorted })
  })

  trio.cut(peer_c, c, peer_d, d)
  assert peers.wait_until(8000, fn() {
    !trio.is_up(peer_c, d) && !trio.is_up(peer_d, c)
  })
  assert trio.is_up(peer_b, c) && trio.is_up(peer_b, d)
  assert list.all(all, fn(peer) { trio.serving(peer) == sorted })

  let near = collector(peer_c)
  let middle = collector(peer_b)
  let far = collector(peer_d)
  send_events(peer_c, [near, far, middle], 300)
  let expected = probe.events(300)
  assert peers.wait_until(5000, fn() {
    collected(peer_c, near) == expected && collected(peer_b, middle) == expected
  })
  assert collected(peer_d, far) == []

  process.sleep(8000)
  assert list.all(all, fn(peer) { trio.serving(peer) == sorted })

  trio.mend(peer_c, c, peer_d, d)
  assert peers.wait_until(15_000, fn() {
    trio.is_up(peer_c, d) && trio.is_up(peer_d, c)
  })
  let late = collector(peer_d)
  send_events(peer_c, [late], 50)
  assert peers.wait_until(5000, fn() {
    collected(peer_d, late) == probe.events(50)
  })

  peers.call(peer_d, "fluxer_gateway@app@drain", "start", [])
  let without_d = list.filter(sorted, fn(name) { name != d })
  assert peers.wait_until(5000, fn() {
    list.all(all, fn(peer) { trio.serving(peer) == without_d })
  })
  peers.stop(peer_d)
  assert peers.wait_until(8000, fn() {
    !trio.is_up(peer_b, d) && !trio.is_up(peer_c, d)
  })
  assert trio.serving(peer_b) == without_d
  assert trio.serving(peer_c) == without_d
  send_events(peer_b, [middle, far], 20)
  assert peers.wait_until(5000, fn() {
    collected(peer_b, middle) == list.append(expected, probe.events(20))
  })
  peers.stop(peer_c)
  peers.stop(peer_b)
}

pub fn isolated_node_is_dropped_by_the_majority_and_rejoins_after_heal_test() {
  let b = peers.unique_name("scn3i_b")
  let c = peers.unique_name("scn3i_c")
  let d = peers.unique_name("scn3i_d")
  let names = [b, c, d]
  let sorted = list.sort(names, string.compare)
  let peer_b = trio.boot(b, names)
  let peer_c = trio.boot(c, names)
  let peer_d = trio.boot(d, names)
  let all = [peer_b, peer_c, peer_d]
  list.each(all, trio.quicken(_, 2000))
  assert peers.wait_until(30_000, fn() {
    list.all(all, fn(peer) { trio.serving(peer) == sorted })
  })

  trio.cut(peer_b, b, peer_d, d)
  trio.cut(peer_c, c, peer_d, d)
  let without_d = list.filter(sorted, fn(name) { name != d })
  assert peers.wait_until(20_000, fn() {
    trio.serving(peer_b) == without_d && trio.serving(peer_c) == without_d
  })
  assert peers.wait_until(20_000, fn() { trio.serving(peer_d) == [d] })
  assert trio.is_up(peer_b, c) && trio.is_up(peer_c, b)

  trio.mend(peer_b, b, peer_d, d)
  trio.mend(peer_c, c, peer_d, d)
  assert peers.wait_until(30_000, fn() {
    list.all(all, fn(peer) { trio.serving(peer) == sorted })
  })
  assert trio.is_up(peer_b, d) && trio.is_up(peer_c, d) && trio.is_up(peer_d, b)
  list.each(all, peers.stop)
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: atom.Atom) -> Int

fn now_ms() -> Int {
  monotonic(atom.create("millisecond"))
}

pub fn relay_keeps_order_through_a_drain_and_skips_a_crashed_node_test() {
  let b = peers.unique_name("scn3r_b")
  let c = peers.unique_name("scn3r_c")
  let d = peers.unique_name("scn3r_d")
  let names = [b, c, d]
  let sorted = list.sort(names, string.compare)
  let peer_b = trio.boot(b, names)
  let peer_c = trio.boot(c, names)
  let peer_d = trio.boot(d, names)
  let all = [peer_b, peer_c, peer_d]
  list.each(all, trio.quicken(_, 60_000))
  assert peers.wait_until(30_000, fn() {
    list.all(all, fn(peer) { trio.serving(peer) == sorted })
  })

  let from_b = list.map(all, collector)
  let from_d = [collector(peer_b), collector(peer_c)]
  let done = process.new_subject()
  process.spawn(fn() {
    send_events(peer_d, from_d, 400)
    process.send(done, Nil)
  })
  peers.cast(peer_d, "fluxer_gateway@app@drain", "start", [])
  send_events(peer_b, from_b, 400)
  let assert Ok(Nil) = process.receive(done, 20_000)
  let expected = probe.events(400)
  assert peers.wait_until(8000, fn() {
    list.zip(all, from_b)
    |> list.all(fn(pair) { collected(pair.0, pair.1) == expected })
    && list.zip([peer_b, peer_c], from_d)
    |> list.all(fn(pair) { collected(pair.0, pair.1) == expected })
  })
  let without_d = list.filter(sorted, fn(name) { name != d })
  assert peers.wait_until(8000, fn() {
    list.all(all, fn(peer) { trio.serving(peer) == without_d })
  })

  let near = collector(peer_b)
  let lost = collector(peer_d)
  trio.crash(peer_d)
  assert peers.wait_until(5000, fn() {
    !trio.is_up(peer_b, d) && !trio.is_up(peer_c, d)
  })
  let started = now_ms()
  send_events(peer_c, [lost, near], 300)
  assert now_ms() - started < 3000
  assert peers.wait_until(5000, fn() {
    collected(peer_b, near) == probe.events(300)
  })
  peers.stop(peer_c)
  peers.stop(peer_b)
}
