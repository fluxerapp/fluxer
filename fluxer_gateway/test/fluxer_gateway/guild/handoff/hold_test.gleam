// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/handoff/hold.{type Hold}
import gleam/dynamic
import gleam/erlang/process.{type Down, type Pid}
import gleam/option.{None, Some}

fn owner() -> Pid {
  process.spawn_unlinked(fn() { process.sleep_forever() })
}

fn await_down() -> Down {
  let assert Ok(down) =
    process.new_selector()
    |> process.select_monitors(fn(down) { down })
    |> process.selector_receive(1000)
  down
}

fn frozen_by(owners: List(Pid)) -> Hold(Int, Nil) {
  hold.freeze(None, owners, Nil, 1)
  |> hold.record_event("GUILD_UPDATE", dynamic.int(1))
  |> hold.hold_voice(10)
  |> hold.hold_voice(11)
}

pub fn killing_the_owner_releases_the_held_voice_requests_test() {
  let mover = owner()
  let frozen = frozen_by([mover])
  process.kill(mover)
  let assert process.ProcessDown(monitor:, ..) = await_down()
  let assert hold.Abandoned(thawed) = hold.owner_down(frozen, monitor)
  assert hold.release(thawed) == [10, 11]
  assert hold.events(thawed) == [#("GUILD_UPDATE", dynamic.int(1))]
}

pub fn the_hold_stays_while_another_owner_lives_test() {
  let mover = owner()
  let supervisor = owner()
  let frozen = frozen_by([supervisor, mover])
  process.kill(mover)
  let assert process.ProcessDown(monitor:, ..) = await_down()
  let assert hold.StillHeld(frozen) = hold.owner_down(frozen, monitor)
  process.kill(supervisor)
  let assert process.ProcessDown(monitor:, ..) = await_down()
  let assert hold.Abandoned(thawed) = hold.owner_down(frozen, monitor)
  assert hold.release(thawed) == [10, 11]
}

pub fn an_unrelated_monitor_is_not_an_owner_test() {
  let mover = owner()
  let session = owner()
  let watch = process.monitor(session)
  let frozen = frozen_by([mover])
  assert hold.owner_down(frozen, watch) == hold.NotOwner
  let _ = hold.release(frozen)
  process.demonitor_process(watch)
  process.kill(mover)
  process.kill(session)
}

pub fn refreezing_keeps_the_buffer_and_drops_the_old_owner_test() {
  let first = owner()
  let second = owner()
  let frozen =
    frozen_by([first])
    |> Some
    |> hold.freeze([second], Nil, 2)
    |> hold.record_event("GUILD_ROLE_UPDATE", dynamic.int(2))
  assert hold.token(frozen) == 2
  assert hold.events(frozen)
    == [
      #("GUILD_UPDATE", dynamic.int(1)),
      #("GUILD_ROLE_UPDATE", dynamic.int(2)),
    ]
  process.kill(first)
  process.kill(second)
  let assert process.ProcessDown(monitor:, pid:, ..) = await_down()
  assert pid == second
  let assert hold.Abandoned(thawed) = hold.owner_down(frozen, monitor)
  assert hold.release(thawed) == [10, 11]
  let assert Error(Nil) =
    process.new_selector()
    |> process.select_monitors(fn(down) { down })
    |> process.selector_receive(100)
}
