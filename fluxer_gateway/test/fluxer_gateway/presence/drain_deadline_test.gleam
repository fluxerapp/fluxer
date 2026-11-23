// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key
import fluxer_gateway/core/presence.{Online}
import fluxer_gateway/presence/handoff/adapter as presence_adapter
import fluxer_gateway/session/control
import gleam/int
import gleam/list
import support/drain_rig
import support/presence_scenario as scn

const users = 400

const production_deadline_ms = 57_000

fn user_ids() -> List(Int) {
  int.range(from: users, to: 0, with: [], run: fn(acc, n) {
    [7_930_000_000_000_000 + n, ..acc]
  })
}

fn rebound(session: scn.FakeSession) -> Bool {
  scn.drain_seen(session, 0)
  |> list.any(fn(found) {
    case found {
      scn.Notice(control.PresenceRebind(..)) -> True
      _ -> False
    }
  })
}

pub fn a_drain_moves_every_held_presence_well_under_the_deadline_test() {
  let ids = user_ids()
  let sessions =
    list.map(ids, fn(user) {
      let session = scn.rebinding_session(user, 1)
      let _ = scn.connect(session, Online, [])
      session
    })
  let sources = list.filter_map(ids, scn.presence_pid)
  assert list.length(sources) == users
  let rig =
    drain_rig.start(
      presence_adapter.adapter(),
      drain_rig.buckets(key.presence, ids),
    )
  let drained = drain_rig.drain(rig, production_deadline_ms)
  assert drain_rig.moved_all(drained, ids)
  assert drained.elapsed_ms * 10 < production_deadline_ms
  let moved = list.filter_map(ids, scn.presence_pid)
  assert list.length(moved) == users
  assert list.all(moved, fn(pid) { !list.contains(sources, pid) })
  assert list.all(sessions, rebound)
  drain_rig.stop(rig)
  list.each(sessions, scn.exit_session(_, scn.closed_reason()))
}
