// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/list

type Kind {
  This
  Known
}

@external(erlang, "erlang", "nodes")
fn nodes_of(kinds: List(Kind)) -> List(Atom)

pub fn known() -> List(String) {
  nodes_of([This, Known])
  |> list.map(atom.to_string)
  |> list.unique
}

pub fn ensure(names: List(String)) -> Nil {
  list.each(names, fn(name) {
    let _ = atom.create(name)
    Nil
  })
}
