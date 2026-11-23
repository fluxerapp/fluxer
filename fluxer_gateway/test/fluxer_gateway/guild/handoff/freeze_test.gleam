// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{GuildRef}
import fluxer_gateway/core/id
import fluxer_gateway/guild/handoff/freeze
import fluxer_gateway/guild/inbox
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}

type Reply =
  Subject(List(#(String, Dynamic)))

fn source(on_request: fn(Reply) -> Nil) -> #(process.Pid, Subject(Reply)) {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let inbox = process.new_subject()
      process.send(ready, inbox)
      on_request(process.receive_forever(inbox))
    })
  let assert Ok(inbox) = process.receive(ready, 1000)
  #(pid, inbox)
}

pub fn await_buffered_returns_the_source_events_test() {
  let events = [#("GUILD_MEMBER_UPDATE", dynamic.int(1))]
  let #(pid, inbox) = source(fn(reply) { process.send(reply, events) })
  let outcome =
    freeze.await_buffered(pid, fn(reply) { process.send(inbox, reply) }, 5000)
  assert outcome == Ok(events)
}

pub fn await_buffered_waits_for_a_slow_source_test() {
  let events = [#("GUILD_ROLE_UPDATE", dynamic.int(2))]
  let #(pid, inbox) =
    source(fn(reply) {
      process.sleep(200)
      process.send(reply, events)
    })
  let outcome =
    freeze.await_buffered(pid, fn(reply) { process.send(inbox, reply) }, 5000)
  assert outcome == Ok(events)
}

pub fn await_buffered_fails_when_the_source_dies_without_events_test() {
  let #(pid, inbox) = source(fn(_) { Nil })
  let outcome =
    freeze.await_buffered(pid, fn(reply) { process.send(inbox, reply) }, 60_000)
  assert outcome == Error(Nil)
}

pub fn await_buffered_fails_after_the_timeout_test() {
  let #(pid, inbox) = source(fn(_) { process.sleep_forever() })
  let outcome =
    freeze.await_buffered(pid, fn(reply) { process.send(inbox, reply) }, 100)
  assert outcome == Error(Nil)
  process.kill(pid)
}

fn fake_handle() -> #(inbox.Handle, Subject(inbox.Control)) {
  let assert Ok(guild) = id.guild_id(1)
  let control = process.new_subject()
  let handle =
    inbox.Handle(
      ref: GuildRef(guild:, inbox: process.new_subject()),
      control:,
      pid: process.self(),
    )
  #(handle, control)
}

pub fn supervise_finishes_the_move_when_the_caller_is_killed_test() {
  let #(handle, control) = fake_handle()
  let gates = process.new_subject()
  let caller =
    process.spawn_unlinked(fn() {
      freeze.supervise(
        handle,
        fn(_started) {
          let gate = process.new_subject()
          process.send(gates, gate)
          process.receive_forever(gate)
          process.send(handle.control, inbox.Unfreeze)
          Error(freeze.TargetUnreachable)
        },
        fn() { Nil },
      )
    })
  let assert Ok(gate) = process.receive(gates, 1000)
  process.kill(caller)
  process.send(gate, Nil)
  assert process.receive(control, 1000) == Ok(inbox.Unfreeze)
}

pub fn supervise_unfreezes_when_the_mover_dies_before_the_target_starts_test() {
  let #(handle, control) = fake_handle()
  let outcome =
    freeze.supervise(handle, fn(_started) { panic as "mover lost" }, fn() { Nil })
  assert outcome == Error(freeze.MoverCrashed)
  assert process.receive(control, 1000) == Ok(inbox.Unfreeze)
}

pub fn supervise_completes_the_handoff_when_the_mover_dies_after_the_target_starts_test() {
  let #(handle, control) = fake_handle()
  let completed = process.new_subject()
  let outcome =
    freeze.supervise(
      handle,
      fn(started) {
        started()
        panic as "mover lost"
      },
      fn() { process.send(completed, Nil) },
    )
  assert outcome == Ok(Nil)
  assert process.receive(completed, 0) == Ok(Nil)
  assert process.receive(control, 100) == Error(Nil)
}

pub fn supervise_returns_the_mover_outcome_test() {
  let #(handle, control) = fake_handle()
  let outcome =
    freeze.supervise(
      handle,
      fn(_started) { Error(freeze.TargetRejected) },
      fn() { Nil },
    )
  assert outcome == Error(freeze.TargetRejected)
  assert process.receive(control, 100) == Error(Nil)
}
