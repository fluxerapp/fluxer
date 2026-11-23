// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/atom
import support/cluster_peers as peers
import support/remote_owner_probe as probe

fn pair(prefix: String) -> #(peers.Peer, peers.Peer) {
  let asker = peers.start(peers.unique_name(prefix <> "_asker"), [])
  let owner = peers.start(peers.unique_name(prefix <> "_owner"), [])
  let connected =
    peers.call(asker, "net_kernel", "connect_node", [
      atom.to_dynamic(owner.name),
    ])
  assert decode.run(connected, decode.bool) == Ok(True)
  #(asker, owner)
}

fn answered(found: dynamic.Dynamic) -> #(Bool, Int) {
  let decoder = {
    use refused <- decode.field(0, decode.bool)
    use elapsed <- decode.field(1, decode.int)
    decode.success(#(refused, elapsed))
  }
  let assert Ok(answer) = decode.run(found, decoder)
  answer
}

pub fn a_guild_ask_to_a_node_without_the_shard_fails_at_once_test() {
  let #(asker, owner) = pair("guild_ask")
  let #(refused, elapsed) =
    answered(
      peers.call(asker, "support@remote_owner_probe", "guild_ask_ms", [
        dynamic.string(peers.node_name(owner)),
      ]),
    )
  peers.stop(asker)
  peers.stop(owner)
  assert refused
  assert elapsed < probe.wait_ms / 4
}

pub fn a_presence_call_to_a_node_without_the_manager_fails_at_once_test() {
  let #(asker, owner) = pair("presence_call")
  let #(refused, elapsed) =
    answered(
      peers.call(asker, "support@remote_owner_probe", "presence_call_ms", [
        dynamic.string(peers.node_name(owner)),
      ]),
    )
  peers.stop(asker)
  peers.stop(owner)
  assert refused
  assert elapsed < probe.wait_ms / 4
}

pub fn a_guild_ask_whose_shard_dies_in_flight_fails_at_once_test() {
  let #(asker, owner) = pair("guild_dies")
  let armed =
    peers.call(
      owner,
      "support@remote_owner_probe",
      "shard_that_dies_on_ask",
      [],
    )
  let #(refused, elapsed) =
    answered(
      peers.call(asker, "support@remote_owner_probe", "guild_ask_ms", [
        dynamic.string(peers.node_name(owner)),
      ]),
    )
  peers.stop(asker)
  peers.stop(owner)
  assert decode.run(armed, decode.bool) == Ok(True)
  assert refused
  assert elapsed < probe.wait_ms / 4
}

pub fn a_presence_call_whose_manager_dies_in_flight_fails_at_once_test() {
  let #(asker, owner) = pair("presence_dies")
  let armed =
    peers.call(
      owner,
      "support@remote_owner_probe",
      "manager_that_dies_on_call",
      [],
    )
  let #(refused, elapsed) =
    answered(
      peers.call(asker, "support@remote_owner_probe", "presence_call_ms", [
        dynamic.string(peers.node_name(owner)),
      ]),
    )
  peers.stop(asker)
  peers.stop(owner)
  assert decode.run(armed, decode.bool) == Ok(True)
  assert refused
  assert elapsed < probe.wait_ms / 4
}
