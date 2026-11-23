// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/session_harness as h

const boot_ms = 45_000

fn env(names: List(String), api_port: Int) -> List(#(String, String)) {
  [
    #("FLUXER_GATEWAY_PORT", "0"),
    #("FLUXER_GATEWAY_CLUSTER_ENABLED", "true"),
    #("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", string.join(names, ",")),
    #("FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS", "500"),
    #("FLUXER_NATS_URL", "nats://127.0.0.1:1"),
    #("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:1"),
    #(
      "FLUXER_GATEWAY_API_RPC_ENDPOINT",
      "http://127.0.0.1:" <> int.to_string(api_port) <> "/internal/rpc",
    ),
    #("FLUXER_GATEWAY_LOGGER_LEVEL", "critical"),
  ]
}

fn boot(name: String, names: List(String), api_port: Int) -> Peer {
  let peer = peers.start_within(name, env(names, api_port), boot_ms)
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
  peer
}

fn serving(peer: Peer) -> List(String) {
  let found = peers.call(peer, "fluxer_gateway@cluster@router", "serving", [])
  let assert Ok(names) =
    decode.run(
      found,
      decode.list(decode.at([1], atom.decoder() |> decode.map(atom.to_string))),
    )
  list.sort(names, string.compare)
}

fn on(peer: Peer, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(peer, "support@session_peer", function, arguments)
}

fn settled(peer: Peer) -> Bool {
  decode.run(on(peer, "rebalance_settled", []), decode.bool) == Ok(True)
}

fn text(value: String) -> Dynamic {
  dynamic.string(value)
}

fn hosts(peer: Peer, key: String) -> Bool {
  decode.run(on(peer, "hosts", [text(key)]), decode.bool) == Ok(True)
}

fn events(peer: Peer, sock: Dynamic) -> List(String) {
  let assert Ok(seen) =
    decode.run(on(peer, "events", [sock]), decode.list(decode.string))
  seen
}

fn resume(
  peer: Peer,
  sock: Dynamic,
  key: String,
  token: String,
  seq: Int,
) -> String {
  let assert Ok(outcome) =
    decode.run(
      on(peer, "resume", [sock, text(key), text(token), dynamic.int(seq)]),
      decode.string,
    )
  outcome
}

fn event_seq(seen: List(String), name: String) -> Result(Int, Nil) {
  list.find_map(list.reverse(seen), fn(event) {
    case string.split_once(event, ":") {
      Ok(#(found, seq)) if found == name -> int.parse(seq)
      _ -> Error(Nil)
    }
  })
}

type Node {
  Node(peer: Peer, name: String)
}

pub fn session_hops_between_nodes_and_resumes_through_either_test() {
  let user = h.unique_user()
  use <- h.with_api(h.session_api(h.session_data(user, False, [])))
  let api_port = h.stub_port()
  let b = peers.unique_name("scn_sb")
  let c = peers.unique_name("scn_sc")
  let sorted = list.sort([b, c], string.compare)
  let assert [node_b, node_c] =
    peers.in_parallel(
      [b, c],
      fn(name) { Node(boot(name, [b, c], api_port), name) },
      boot_ms,
    )
  assert peers.wait_until(25_000, fn() {
    serving(node_b.peer) == sorted && serving(node_c.peer) == sorted
  })
  assert peers.wait_until(25_000, fn() {
    settled(node_b.peer) && settled(node_c.peer)
  })

  let sock = on(node_b.peer, "socket", [])
  let assert Ok(#(key, ready_seq)) =
    decode.run(on(node_b.peer, "start", [sock, text("tok-x1")]), {
      use key <- decode.field(0, decode.string)
      use seq <- decode.field(1, decode.int)
      decode.success(#(key, seq))
    })
  assert key != ""
  assert ready_seq > 0
  let #(host, other) = case hosts(node_b.peer, key) {
    True -> #(node_b, node_c)
    False -> #(node_c, node_b)
  }
  assert hosts(host.peer, key)

  assert on(host.peer, "dispatch", [text(key), text("one")])
    == dynamic.bool(True)
  assert peers.wait_until(3000, fn() {
    event_seq(events(node_b.peer, sock), "MESSAGE_CREATE") |> is_ok
  })
  let assert Ok(one) = event_seq(events(node_b.peer, sock), "MESSAGE_CREATE")

  let final_seq =
    list.fold(
      [#(host, other), #(other, host), #(host, other)],
      one,
      fn(seq, hop) {
        let #(from, to) = hop
        assert on(from.peer, "hand_off", [text(key), text(to.name)])
          == dynamic.bool(True)
        let fresh = on(from.peer, "socket", [])
        assert resume(from.peer, fresh, key, "tok-x1", ready_seq) == "ok"
        assert hosts(to.peer, key)
        assert peers.wait_until(3000, fn() { !hosts(from.peer, key) })
        assert peers.wait_until(3000, fn() {
          event_seq(events(from.peer, fresh), "RESUMED") |> is_ok
        })
        let seen = events(from.peer, fresh)
        assert event_seq(seen, "MESSAGE_CREATE") == Ok(one)
        let assert Ok(resumed) = event_seq(seen, "RESUMED")
        assert resumed >= seq
        on(from.peer, "drop", [fresh])
        resumed
      },
    )

  let raced =
    list.fold(
      [#(other, host), #(host, other), #(other, host)],
      final_seq,
      fn(seq, hop) {
        let #(from, to) = hop
        let fresh = on(from.peer, "socket", [])
        let assert Ok(#(moved, resumed)) =
          decode.run(
            on(from.peer, "hand_off_racing_resume", [
              text(key),
              text(to.name),
              fresh,
              text("tok-x1"),
              dynamic.int(ready_seq),
            ]),
            {
              use moved <- decode.field(0, decode.bool)
              use resumed <- decode.field(1, decode.string)
              decode.success(#(moved, resumed))
            },
          )
        assert moved
        assert resumed == "ok"
        assert peers.wait_until(3000, fn() {
          hosts(to.peer, key) && !hosts(from.peer, key)
        })
        assert peers.wait_until(3000, fn() {
          event_seq(events(from.peer, fresh), "RESUMED") |> is_ok
        })
        let assert Ok(resumed_seq) =
          event_seq(events(from.peer, fresh), "RESUMED")
        on(from.peer, "drop", [fresh])
        int.max(seq, resumed_seq)
      },
    )
  let final_seq =
    list.fold([#(host, other), #(other, host)], raced, fn(seq, hop) {
      let #(from, to) = hop
      let handed = process.new_subject()
      process.spawn_unlinked(fn() {
        process.send(
          handed,
          on(from.peer, "hand_off", [text(key), text(to.name)]),
        )
      })
      let fresh = on(to.peer, "socket", [])
      assert resume(to.peer, fresh, key, "tok-x1", ready_seq) == "ok"
      assert process.receive(handed, 15_000) == Ok(dynamic.bool(True))
      assert peers.wait_until(3000, fn() {
        hosts(to.peer, key) && !hosts(from.peer, key)
      })
      assert peers.wait_until(3000, fn() {
        event_seq(events(to.peer, fresh), "RESUMED") |> is_ok
      })
      let assert Ok(resumed_seq) = event_seq(events(to.peer, fresh), "RESUMED")
      on(to.peer, "drop", [fresh])
      int.max(seq, resumed_seq)
    })
  let last = on(host.peer, "socket", [])
  assert resume(host.peer, last, key, "tok-x1", final_seq) == "ok"
  assert peers.wait_until(3000, fn() {
    event_seq(events(host.peer, last), "RESUMED") == Ok(final_seq)
  })
  assert list.contains(events(node_b.peer, sock), "RECONNECT")

  peers.stop(node_c.peer)
  peers.stop(node_b.peer)
}

fn is_ok(result: Result(a, b)) -> Bool {
  case result {
    Ok(_) -> True
    Error(_) -> False
  }
}
