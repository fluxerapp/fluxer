// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/codec_range.{type CodecRange}
import bouncer_proto/wire_epoch
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/order
import gleam/string

pub type WorkerId {
  WorkerId(node: Atom, incarnation: Int)
}

pub type Status {
  Joining
  Ready
  Draining
  Gone
}

pub type Profile {
  Profile(
    role: String,
    pool: String,
    build: String,
    template_hash: String,
    wire_epoch: Int,
    codecs: List(CodecRange),
  )
}

pub type Entry {
  Entry(worker: WorkerId, profile: Profile, status: Status, last_beat_ms: Int)
}

pub type Roster {
  Roster(hash: Int, members: List(WorkerId))
}

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

fn compare(a: WorkerId, b: WorkerId) {
  case string.compare(atom.to_string(a.node), atom.to_string(b.node)) {
    order.Eq -> int.compare(a.incarnation, b.incarnation)
    other -> other
  }
}

pub fn roster(members: List(WorkerId)) -> Roster {
  let sorted = members |> list.unique |> list.sort(compare)
  Roster(hash: phash2(sorted, 4_294_967_295), members: sorted)
}

pub fn majority_of(roster: Roster) -> Int {
  list.length(roster.members) / 2 + 1
}

pub fn same_build_line(a: Profile, b: Profile) -> Bool {
  a.role == b.role
  && a.pool == b.pool
  && wire_epoch.compatible(a.wire_epoch, b.wire_epoch)
}
