// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/core/node
import fluxer_gateway/presence/route
import gleam/erlang/atom
import gleam/int
import gleam/list

fn users(count: Int) -> List(Int) {
  int.range(from: 1, to: count + 1, with: [], run: list.prepend)
}

pub fn presence_unavailable(count: Int) -> Int {
  users(count)
  |> list.count(fn(user) {
    case route.owner(user) {
      Ok(_) -> False
      Error(_) -> True
    }
  })
}

pub fn presence_owned_by(name: String, count: Int) -> Int {
  let wanted = atom.create(name)
  users(count)
  |> list.count(fn(user) { route.owner(user) == Ok(wanted) })
}

pub fn push_unavailable(count: Int) -> Int {
  users(count)
  |> list.count(fn(key) {
    case router.owner(router.PushKey(key), node.Push) {
      router.OwnerUnavailable(_) -> True
      _ -> False
    }
  })
}

pub fn push_owned_by(name: String, count: Int) -> Int {
  let wanted = node.from_atom(atom.create(name))
  users(count)
  |> list.count(fn(key) {
    router.owner(router.PushKey(key), node.Push) == router.Remote(wanted)
  })
}

pub fn presence_nodes() -> List(String) {
  route.presence_nodes() |> list.map(atom.to_string)
}

pub fn members() -> List(String) {
  router.members() |> list.map(node.to_string)
}
