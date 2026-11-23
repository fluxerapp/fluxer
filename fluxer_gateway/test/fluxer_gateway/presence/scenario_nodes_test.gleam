// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers

const nodes_module = "support@presence_nodes"

fn env(names: List(String)) -> List(#(String, String)) {
  [
    #("FLUXER_GATEWAY_PORT", "0"),
    #("FLUXER_GATEWAY_CLUSTER_ENABLED", "true"),
    #("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", string.join(names, ",")),
    #("FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS", "200"),
    #("FLUXER_NATS_URL", "nats://127.0.0.1:1"),
    #("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:1"),
    #("FLUXER_GATEWAY_LOGGER_LEVEL", "critical"),
  ]
}

fn boot(name: String, names: List(String)) -> Peer {
  let peer = peers.start(name, env(names))
  let started =
    peers.call(peer, "application", "ensure_all_started", [
      atom.to_dynamic(atom.create("fluxer_gateway")),
    ])
  let assert Ok("ok") =
    decode.run(
      started,
      decode.at([0], atom.decoder()) |> decode.map(atom.to_string),
    )
    as { "gateway failed to start on peer: " <> string.inspect(started) }
  let _ = peers.call(peer, nodes_module, "tune", [])
  peer
}

fn on(peer: Peer, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(peer, nodes_module, function, arguments)
}

fn ints(found: Dynamic) -> List(Int) {
  let assert Ok(value) = decode.run(found, decode.list(decode.int))
    as string.inspect(found)
  value
}

fn strings(found: Dynamic) -> List(String) {
  let assert Ok(value) = decode.run(found, decode.list(decode.string))
    as string.inspect(found)
  value
}

fn int_of(found: Dynamic) -> Int {
  let assert Ok(value) = decode.run(found, decode.int) as string.inspect(found)
  value
}

fn bool_of(found: Dynamic) -> Bool {
  let assert Ok(value) = decode.run(found, decode.bool) as string.inspect(found)
  value
}

fn users_from(base: Int, count: Int) -> List(Int) {
  int.range(1, count + 1, [], list.prepend)
  |> list.reverse
  |> list.map(fn(n) { base + n })
}

fn arg(value: a) -> Dynamic {
  peers.to_dynamic(value)
}

fn hosted(peer: Peer, users: List(Int)) -> List(Int) {
  ints(on(peer, "hosted", [arg(users)]))
}

fn owners(peer: Peer, users: List(Int)) -> List(String) {
  strings(on(peer, "owners", [arg(users)]))
}

fn session_nodes(peer: Peer, users: List(Int), tag: Int) -> List(String) {
  strings(on(peer, "session_nodes", [arg(users), arg(tag)]))
}

fn cached(peer: Peer, users: List(Int)) -> List(Int) {
  ints(on(peer, "cached", [arg(users)]))
}

fn report(peer: Peer, user: Int, tag: Int) -> #(String, Bool) {
  let found = on(peer, "report", [arg(user), arg(tag)])
  let decoder = {
    use node <- decode.field(1, decode.string)
    use terminated <- decode.field(2, decode.bool)
    decode.success(#(node, terminated))
  }
  let assert Ok(value) = decode.run(found, decoder) as string.inspect(found)
  value
}

pub fn node_down_mid_handoff_leaves_one_presence_per_user_test() {
  let a = peers.unique_name("scn_pa")
  let b = peers.unique_name("scn_pb")
  let peer_a = boot(a, [a, b])
  let users = users_from(7_900_000_000_000_000, 60)
  assert int_of(on(peer_a, "start_sessions", [arg(users), arg(1)])) == 60
  assert hosted(peer_a, users) == users
  let peer_b = boot(b, [a, b])
  assert peers.wait_until(20_000, fn() {
    list.contains(owners(peer_a, users), b)
  })
  let moving = list.filter(users, fn(user) { owners(peer_a, [user]) == [b] })
  assert peers.wait_until(10_000, fn() { hosted(peer_b, moving) != [] })
  peers.stop(peer_b)
  assert peers.wait_until(45_000, fn() {
    list.all(owners(peer_a, users), fn(owner) { owner == a })
  })
  let settled =
    peers.wait_until(20_000, fn() {
      hosted(peer_a, users) == users
      && list.all(session_nodes(peer_a, users, 1), fn(node) { node == a })
      && cached(peer_a, users) == users
    })
  assert settled
    as string.inspect(#(
      list.filter(users, fn(u) { !list.contains(hosted(peer_a, users), u) }),
      session_nodes(peer_a, users, 1),
      list.length(cached(peer_a, users)),
    ))
  peers.stop(peer_a)
}

pub fn handoff_carries_pending_termination_of_lost_session_test() {
  let a = peers.unique_name("scn_da")
  let b = peers.unique_name("scn_db")
  let peer_a = boot(a, [a, b])
  let candidates = users_from(7_900_000_000_001_000, 40)
  assert int_of(on(peer_a, "start_sessions", [arg(candidates), arg(1)])) == 40
  assert int_of(on(peer_a, "start_sessions", [arg(candidates), arg(2)])) == 40
  list.each(candidates, fn(user) {
    let _ =
      on(peer_a, "exit_session", [
        arg(user),
        arg(2),
        on(peer_a, "handoff_exit", []),
      ])
    assert bool_of(on(peer_a, "terminate", [arg(user), arg(["hash"])]))
  })
  assert list.all(candidates, fn(user) { report(peer_a, user, 1).1 })
  let peer_b = boot(b, [a, b])
  assert peers.wait_until(20_000, fn() {
    list.contains(owners(peer_a, candidates), b)
  })
  let moved =
    list.filter(candidates, fn(user) { owners(peer_a, [user]) == [b] })
  assert peers.wait_until(15_000, fn() { hosted(peer_b, moved) == moved })
  assert int_of(on(peer_a, "start_sessions", [arg(moved), arg(2)]))
    == list.length(moved)
  let doomed =
    peers.wait_until(5000, fn() {
      list.all(moved, fn(user) { report(peer_a, user, 2).1 })
    })
  assert doomed
    as string.inspect(list.map(moved, fn(user) { report(peer_a, user, 2) }))
  peers.stop(peer_b)
  peers.stop(peer_a)
}

fn statuses(peer: Peer, guild: Int, users: List(Int)) -> List(String) {
  strings(on(peer, "observer_statuses", [arg(guild), arg(users)]))
}

fn interested(peer: Peer, users: List(Int)) -> List(List(String)) {
  let found = on(peer, "interested", [arg(users)])
  let assert Ok(value) =
    decode.run(found, decode.list(decode.list(decode.string)))
    as string.inspect(found)
  value
}

pub fn interest_churn_across_nodes_and_owner_loss_test() {
  let a = peers.unique_name("scn_ia")
  let b = peers.unique_name("scn_ib")
  let peer_a = boot(a, [a, b])
  let peer_b = boot(b, [a, b])
  let users = users_from(7_900_000_000_002_000, 40)
  let guild = 7_900_000_000_002_900
  assert peers.wait_until(20_000, fn() {
    let found = owners(peer_a, users)
    list.contains(found, a) && list.contains(found, b)
  })
  assert peers.wait_until(5000, fn() {
    owners(peer_b, users) == owners(peer_a, users)
  })
  let remote_owned =
    list.filter(users, fn(user) { owners(peer_a, [user]) == [b] })
  assert bool_of(on(peer_a, "start_observer", [arg(guild), arg(users)]))
  assert peers.wait_until(5000, fn() {
    list.all(interested(peer_b, remote_owned), list.contains(_, a))
  })
  assert int_of(on(peer_b, "start_sessions", [arg(users), arg(1)])) == 40
  let all_online = list.map(users, fn(_) { "online" })
  assert peers.wait_until(5000, fn() {
    statuses(peer_a, guild, users) == all_online
  })
  assert peers.wait_until(5000, fn() {
    list.all(interested(peer_b, remote_owned), list.contains(_, a))
  })
  let kept = list.take(users, 15)
  int.range(0, 6, Nil, fn(_, round) {
    let _ =
      on(peer_a, "observer_unregister", [
        arg(guild),
        arg(list.drop(users, round)),
      ])
    let _ = on(peer_a, "observer_register", [arg(guild), arg(users)])
    let _ = on(peer_a, "observer_drop", [arg(guild)])
    let _ =
      on(peer_a, "observer_register", [arg(guild), arg(list.drop(users, 20))])
    let _ = on(peer_a, "observer_register", [arg(guild), arg(kept)])
    let _ =
      on(peer_a, "observer_unregister", [arg(guild), arg(list.drop(users, 20))])
    Nil
  })
  let wanted =
    list.map(remote_owned, fn(user) {
      case list.contains(kept, user) {
        True -> [a]
        False -> []
      }
    })
  let released =
    peers.wait_until(5000, fn() {
      list.map(
        interested(peer_b, remote_owned),
        list.filter(_, fn(n) { n == a }),
      )
      == wanted
    })
  assert released as string.inspect(#(wanted, interested(peer_b, remote_owned)))
  assert ints(on(peer_a, "subscribed", [arg(users)])) == kept
  peers.stop(peer_b)
  let expected =
    list.map(users, fn(user) {
      case list.contains(kept, user) && list.contains(remote_owned, user) {
        True -> "offline"
        False -> "?"
      }
    })
  let went_offline =
    peers.wait_until(45_000, fn() {
      list.map2(statuses(peer_a, guild, users), expected, fn(seen, want) {
        case want {
          "?" -> "?"
          _ -> seen
        }
      })
      == expected
    })
  assert went_offline
    as string.inspect(#(expected, statuses(peer_a, guild, users)))
  peers.stop(peer_a)
}
