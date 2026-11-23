// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/gateway/abuse_protection
import fluxer_gateway/session/manager/leases
import gleam/erlang/process
import gleam/int
import gleam/list

const max_sessions_per_user = 100

fn user() -> UserId {
  let assert Ok(user) =
    id.user_id(200_000_000_000_000_000 + int.random(700_000_000_000_000_000))
  user
}

fn hold(start: fn(process.Pid) -> a) -> process.Pid {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let _ = start(process.self())
      process.send(ready, Nil)
      process.sleep_forever()
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  pid
}

fn kill_all(pids: List(process.Pid)) -> Nil {
  list.each(pids, fn(pid) {
    let monitor = process.monitor(pid)
    process.kill(pid)
    let down =
      process.new_selector()
      |> process.select_specific_monitor(monitor, fn(_) { Nil })
    let assert Ok(Nil) = process.selector_receive(down, 1000)
  })
}

fn sessions(user: UserId, count: Int) -> List(process.Pid) {
  list.repeat(Nil, count)
  |> list.map(fn(_) { hold(leases.open(_, user)) })
}

pub fn killed_sessions_stop_counting_test() {
  let user = user()
  let live = sessions(user, max_sessions_per_user)
  assert leases.reserve(process.self(), user)
    == Error(abuse_protection.TooManySessions)
  kill_all(live)
  leases.sweep()
  assert leases.reserve(process.self(), user) == Ok(Nil)
  leases.release(process.self())
}

pub fn killed_reservation_holder_stops_counting_test() {
  let user = user()
  let live = sessions(user, max_sessions_per_user - 1)
  let reserver =
    hold(fn(pid) {
      let assert Ok(Nil) = leases.reserve(pid, user)
    })
  kill_all([reserver])
  leases.sweep()
  assert leases.reserve(process.self(), user) == Ok(Nil)
  leases.release(process.self())
  kill_all(live)
  leases.sweep()
}
