// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/native
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list

@external(erlang, "fluxer_gateway@nif", "gateway_nif_metrics")
fn gateway_nif_metrics() -> List(#(String, Int))

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: a) -> b

@external(erlang, "erlang", "process_info")
fn process_info(pid: process.Pid, item: atom.Atom) -> #(atom.Atom, Int)

fn reductions() -> Int {
  process_info(process.self(), atom.create("reductions")).1
}

pub fn charged(work: fn() -> a) -> #(a, Int) {
  let before = reductions()
  let found = work()
  #(found, reductions() - before)
}

pub fn metric(name: String) -> Int {
  let assert Ok(Nil) = nif.load()
  gateway_nif_metrics()
  |> list.key_find(name)
  |> fn(found) {
    case found {
      Ok(value) -> value
      Error(Nil) -> 0
    }
  }
}

pub fn dirty_calls() -> Int {
  metric("dirty_calls")
}

pub fn reader_handle(reader: perms.Reader) -> native.Reader {
  element(2, reader)
}
