// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/table
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process.{type Pid}
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

fn init_timeout_ms() -> Int {
  durations.guild().ets_owner_init_timeout_ms
}

pub opaque type Message {
  Inherited(transfer: Dynamic)
}

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: Dynamic) -> Dynamic

@external(erlang, "ets", "delete")
fn delete_table(table: Dynamic) -> Bool

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
  |> supervision.map_data(fn(_) { Nil })
}

fn start() -> actor.StartResult(process.Subject(Message)) {
  actor.new_with_initialiser(init_timeout_ms(), fn(subject) {
    let options = [table.Public, table.Set, table.ReadConcurrency(True)]
    table.ensure_named(names.registry_table, options)
    table.ensure_named(names.read_model_table, options)
    table.ensure_named(names.unavailability_table, options)
    table.ensure_named(names.health_table, [
      table.WriteConcurrency(True),
      ..options
    ])
    table.ensure_named(names.remains_table, options)
    table.ensure_named(names.roster_table, [
      table.OrderedSet,
      table.Public,
      table.WriteConcurrency(True),
    ])
    table.ensure_named(names.counts_table, [
      table.WriteConcurrency(True),
      ..options
    ])
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select_record(atom.create("ETS-TRANSFER"), 3, Inherited)
    actor.initialised(Nil)
    |> actor.selecting(selector)
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.named(names.ets_owner())
  |> actor.on_message(fn(state, message) {
    case message {
      Inherited(transfer) -> {
        let _ = delete_table(element(2, transfer))
        actor.continue(state)
      }
    }
  })
  |> actor.start
}

pub fn heir() -> Option(Pid) {
  process.named(names.ets_owner()) |> option.from_result
}
