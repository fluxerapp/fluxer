// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/node_name
import fluxer_gateway/core/node.{type Node}
import gleam/dict.{type Dict}

type Key {
  FluxerGatewayClusterPodNames
}

type Stored

@external(erlang, "persistent_term", "get")
fn get_names(key: Key, default: Dict(Node, String)) -> Dict(Node, String)

@external(erlang, "persistent_term", "put")
fn put_names(key: Key, value: Dict(Node, String)) -> Stored

pub fn local_pod_name() -> String {
  node_name.pod_name()
}

fn node_pod_names() -> Dict(Node, String) {
  get_names(FluxerGatewayClusterPodNames, dict.new())
}

pub fn cache_node_pod_name(peer: Node, pod_name: String) -> Nil {
  let names = node_pod_names()
  case pod_name == "" || dict.get(names, peer) == Ok(pod_name) {
    True -> Nil
    False -> {
      put_names(
        FluxerGatewayClusterPodNames,
        dict.insert(names, peer, pod_name),
      )
      Nil
    }
  }
}
