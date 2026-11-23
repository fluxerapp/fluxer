// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/worker.{Draining, Entry, Gone, Profile, Ready, WorkerId}
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/links
import fluxer_gateway/core/node
import fluxer_gateway/handoff/service
import gleam/erlang/atom
import gleam/list
import gleam/string

fn profile(role: String, template_hash: String) {
  Profile(role, "default", "dev", template_hash, 1, [])
}

fn peer(name: String, role: String, template_hash: String, status) {
  Entry(WorkerId(atom.create(name), 1), profile(role, template_hash), status, 1)
}

fn me(template_hash: String) {
  Entry(
    WorkerId(node.to_atom(node.self()), lease.incarnation()),
    profile("websocket+sessions", template_hash),
    Draining,
    1,
  )
}

fn names(nodes: List(node.Node)) -> List(String) {
  nodes
  |> list.map(node.to_string)
  |> list.sort(string.compare)
}

pub fn session_targets_prefer_ready_peers_of_the_new_template_test() {
  let _ = links.replace([])
  assert links.replace([
    me("old"),
    peer("a@h", "websocket+sessions", "old", Ready),
    peer("b@h", "websocket+sessions", "new", Ready),
    peer("c@h", "websocket+sessions", "new", Draining),
    peer("d@h", "websocket+sessions", "new", Gone),
    peer("e@h", "guilds", "new", Ready),
  ])
  assert names(service.session_targets()) == ["b@h"]
  let _ = links.replace([])
  Nil
}

pub fn session_targets_fall_back_to_ready_peers_of_the_same_template_test() {
  let _ = links.replace([])
  assert links.replace([
    me("old"),
    peer("a@h", "websocket+sessions", "old", Ready),
    peer("f@h", "websocket+sessions", "old", Ready),
    peer("c@h", "websocket+sessions", "new", Draining),
  ])
  assert names(service.session_targets()) == ["a@h", "f@h"]
  let _ = links.replace([])
  Nil
}

pub fn session_targets_are_empty_without_an_own_entry_test() {
  let _ = links.replace([])
  assert links.replace([peer("a@h", "websocket+sessions", "new", Ready)])
  assert service.session_targets() == []
  let _ = links.replace([])
  Nil
}
