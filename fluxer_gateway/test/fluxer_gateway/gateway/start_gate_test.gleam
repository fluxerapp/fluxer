// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/start_gate
import gleam/erlang/process
import gleam/list
import support/wait

fn holder(acquired: process.Subject(Bool)) -> process.Pid {
  process.spawn_unlinked(fn() {
    process.send(acquired, case start_gate.try_guild_start() {
      Ok(_) -> True
      Error(Nil) -> False
    })
    process.sleep_forever()
  })
}

pub fn guild_start_slot_follows_its_holder_test() {
  let baseline = start_gate.guild_start_count()
  let acquired = process.new_subject()
  let holders = list.map([1, 2, 3], fn(_) { holder(acquired) })
  list.each(holders, fn(_) {
    assert process.receive(acquired, 2000) == Ok(True)
  })
  assert start_gate.guild_start_count() == baseline + 3
  list.each(holders, process.kill)
  assert wait.until(2000, fn() { start_gate.guild_start_count() == baseline })
}

pub fn released_guild_start_frees_its_slot_test() {
  let baseline = start_gate.guild_start_count()
  let assert Ok(permit) = start_gate.try_guild_start()
  assert start_gate.guild_start_count() == baseline + 1
  start_gate.release_guild_start(permit)
  assert wait.until(2000, fn() { start_gate.guild_start_count() == baseline })
  process.sleep(50)
  assert start_gate.guild_start_count() == baseline
}
