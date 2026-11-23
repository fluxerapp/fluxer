// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/push/engagement
import gleam/erlang/process

fn ref(user: Int) -> session.SessionRef {
  let assert Ok(user_id) = id.user_id(user)
  session.SessionRef(
    id: session.new_session_id(),
    user: user_id,
    inbox: process.new_subject(),
  )
}

pub fn presence_active_user_is_engaged_test() {
  let a = ref(10)
  let b = ref(20)
  let e = engagement.new() |> engagement.set_active(10, True)
  let #(_, snapshot) = engagement.snapshot(e, [a, b])
  let assert True = engagement.is_engaged(snapshot, 10)
  let assert False = engagement.is_engaged(snapshot, 20)
  let assert True = engagement.is_connected(snapshot, 20)
}

pub fn released_hold_is_never_engaged_test() {
  let a = ref(10)
  let e =
    engagement.new()
    |> engagement.set_active(10, True)
    |> engagement.set_hold(a.id, False)
  let #(e, snapshot) = engagement.snapshot(e, [a])
  let assert False = engagement.is_engaged(snapshot, 10)
  let assert [_] = engagement.released_holds(e, [a.id])
}

pub fn hold_engages_an_idle_user_test() {
  let a = ref(10)
  let e = engagement.new() |> engagement.set_hold(a.id, True)
  let #(_, snapshot) = engagement.snapshot(e, [a])
  let assert True = engagement.is_engaged(snapshot, 10)
}

pub fn snapshot_is_reused_until_the_version_moves_test() {
  let a = ref(10)
  let e = engagement.new()
  let #(e, first) = engagement.snapshot(e, [a])
  let #(e, second) = engagement.snapshot(e, [a])
  let assert True = first == second
  let e = engagement.set_active(e, 10, True)
  let #(_, third) = engagement.snapshot(e, [a])
  let assert True = engagement.is_engaged(third, 10)
}
