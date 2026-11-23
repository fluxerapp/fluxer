// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers

const user = 1_539_791_936_896_897_201

const channel = 1_539_791_936_896_897_300

fn env() -> List(#(String, String)) {
  [
    #("FLUXER_GATEWAY_LOGGER_LEVEL", "critical"),
    #("FLUXER_NATS_URL", "nats://127.0.0.1:1"),
    #("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:1"),
  ]
}

fn with_pair(prefix: String, run: fn(Peer, Peer) -> Nil) -> Nil {
  let names = [
    peers.unique_name(prefix <> "_five"),
    peers.unique_name(prefix <> "_four"),
  ]
  let assert [five, four] =
    peers.in_parallel(names, peers.start_within(_, env(), 45_000), 45_000)
  run(five, four)
  peers.stop(five)
  peers.stop(four)
}

fn on(peer: Peer, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(peer, "support@epoch_voice_peer", function, arguments)
}

fn truthy(found: Dynamic) -> Bool {
  decode.run(found, decode.bool) == Ok(True)
}

fn seen(four: Peer, dm: Dynamic) -> List(String) {
  let assert Ok(found) =
    decode.run(on(four, "legacy_dm_seen", [dm]), decode.list(decode.string))
  list.sort(found, string.compare)
}

fn dm_connection(five: Peer, session: Dynamic, conn: String) -> String {
  let assert Ok(found) =
    decode.run(
      on(five, "dm_connection", [session, dynamic.string(conn)]),
      decode.string,
    )
  found
}

pub fn an_epoch_five_call_reaches_an_epoch_four_dm_by_its_registered_subject_test() {
  use five, four <- with_pair("ev_dm4")
  let session_id = "0123456789abcdef0123456789abcdef"
  let assert Ok(#(dm, member)) =
    decode.run(
      on(four, "legacy_dm", [dynamic.string(session_id), dynamic.int(user)]),
      {
        use dm <- decode.field(0, decode.dynamic)
        use member <- decode.field(1, decode.dynamic)
        decode.success(#(dm, member))
      },
    )
  assert truthy(
    on(five, "treat_as_epoch_four", [dynamic.string(peers.node_name(four))]),
  )
  assert truthy(
    on(five, "serve_legacy_member", [
      member,
      dynamic.int(user),
      dynamic.int(channel),
      dynamic.string(session_id),
      dynamic.string("c4"),
    ]),
  )
  assert peers.wait_until(5000, fn() { list.length(seen(four, dm)) >= 2 })
  assert seen(four, dm) == ["converted c4", "force c4"]
}

pub fn an_epoch_five_dm_follows_an_epoch_four_call_that_cannot_note_it_test() {
  use five, four <- with_pair("ev_call4")
  assert truthy(
    on(five, "route_calls_to", [dynamic.string(peers.node_name(four))]),
  )
  let assert Ok(#(session, session_id)) =
    decode.run(on(five, "dm_session", [dynamic.int(user)]), {
      use session <- decode.field(0, decode.dynamic)
      use session_id <- decode.field(1, decode.string)
      decode.success(#(session, session_id))
    })
  let controls =
    on(four, "legacy_call", [
      dynamic.int(channel),
      dynamic.int(user),
      dynamic.string(session_id),
      dynamic.string("c5"),
    ])
  on(five, "adopt", [session, dynamic.int(channel)])
  assert peers.wait_until(5000, fn() {
    dm_connection(five, session, "c5") == "p2p"
  })
  assert peers.wait_until(5000, fn() {
    truthy(on(four, "legacy_call_rebound", [controls]))
  })
  assert truthy(
    on(four, "legacy_call_convert", [controls, dynamic.string("c5")]),
  )
  assert peers.wait_until(5000, fn() {
    dm_connection(five, session, "c5") == "sfu"
  })
  assert truthy(on(four, "legacy_call_drop", [controls, dynamic.string("c5")]))
  assert peers.wait_until(5000, fn() {
    dm_connection(five, session, "c5") == "missing"
  })
}

const other_channel = 1_539_791_936_896_897_301

pub fn a_stalled_epoch_four_call_does_not_hold_up_other_calls_test() {
  use five, four <- with_pair("ev_stall4")
  assert truthy(
    on(five, "route_calls_to", [dynamic.string(peers.node_name(four))]),
  )
  let assert Ok(#(session, session_id)) =
    decode.run(on(five, "dm_session", [dynamic.int(user)]), {
      use session <- decode.field(0, decode.dynamic)
      use session_id <- decode.field(1, decode.string)
      decode.success(#(session, session_id))
    })
  let stalled =
    on(four, "legacy_call", [
      dynamic.int(channel),
      dynamic.int(user),
      dynamic.string(session_id),
      dynamic.string("c7"),
    ])
  let healthy =
    on(four, "legacy_call", [
      dynamic.int(other_channel),
      dynamic.int(user),
      dynamic.string(session_id),
      dynamic.string("c8"),
    ])
  on(five, "adopt", [session, dynamic.int(channel)])
  assert peers.wait_until(5000, fn() {
    truthy(on(four, "legacy_call_rebound", [stalled]))
  })
  on(four, "legacy_call_stall", [stalled])
  process.sleep(1000)
  on(five, "adopt", [session, dynamic.int(other_channel)])
  assert peers.wait_until(2000, fn() {
    truthy(on(four, "legacy_call_rebound", [healthy]))
  })
}
