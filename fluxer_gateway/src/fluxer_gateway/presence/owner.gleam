// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/presence/table
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

fn init_timeout_ms() -> Int {
  durations.presence().owner_init_timeout_ms
}

pub fn tables(names: List(Atom)) -> ChildSpecification(Nil) {
  tables_with(names, [], [])
}

pub fn tables_with(
  names: List(Atom),
  bags: List(Atom),
  ordered: List(Atom),
) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(_subject) {
      list.each(names, table.create)
      list.each(bags, table.create_duplicate_bag)
      list.each(ordered, table.create_ordered)
      Ok(actor.initialised(Nil))
    })
    |> actor.on_message(fn(state, _message: Nil) { actor.continue(state) })
    |> actor.start
    |> started_nil
  })
}

pub fn started_nil(
  result: Result(actor.Started(a), actor.StartError),
) -> Result(actor.Started(Nil), actor.StartError) {
  case result {
    Ok(actor.Started(pid:, ..)) -> Ok(actor.Started(pid:, data: Nil))
    Error(reason) -> Error(reason)
  }
}
