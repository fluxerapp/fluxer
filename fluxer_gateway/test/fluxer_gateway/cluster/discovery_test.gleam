// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/discovery
import fluxer_gateway/core/node
import gleam/list

pub fn peers_from_addresses_test() {
  let peers =
    discovery.peers_from_addresses(
      ["10.0.0.2", "10.0.0.1", "10.0.0.2", "bad host"],
      "fluxer_gateway",
    )
  assert list.map(peers, node.to_string)
    == ["fluxer_gateway@10.0.0.1", "fluxer_gateway@10.0.0.2"]
}

pub fn static_peers_test() {
  let peers =
    discovery.static_peers([
      "fluxer_gateway@b.local",
      "nonsense",
      "fluxer_gateway@a.local",
    ])
  assert list.map(peers, node.to_string)
    == ["fluxer_gateway@a.local", "fluxer_gateway@b.local"]
}

pub fn self_is_never_a_peer_test() {
  let self = node.to_string(node.self())
  assert discovery.static_peers([self]) == []
}
