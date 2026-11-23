// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import fluxer_gateway/handoff/epoch_hold.{Deliver, Dropped, Held, Overflowed}

fn stamp(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

pub fn frames_from_the_linked_owner_pass_straight_through_test() {
  let hold = epoch_hold.new("old", stamp(7), epoch_hold.default_limits())
  let #(_, decision) = epoch_hold.frame(hold, "old", stamp(7), 1, 0)
  assert decision == Deliver([1])
}

pub fn stale_epochs_are_dropped_test() {
  let hold = epoch_hold.new("old", stamp(7), epoch_hold.default_limits())
  let #(_, decision) = epoch_hold.frame(hold, "older", stamp(6), 1, 0)
  assert decision == Dropped
}

pub fn new_owner_output_waits_for_moved_test() {
  let hold = epoch_hold.new("old", stamp(7), epoch_hold.default_limits())
  let #(hold, first) = epoch_hold.frame(hold, "new", stamp(8), 10, 0)
  let #(hold, second) = epoch_hold.frame(hold, "new", stamp(8), 11, 1)
  let #(hold, late) = epoch_hold.frame(hold, "old", stamp(7), 9, 2)
  assert first == Held
  assert second == Held
  assert late == Deliver([9])
  let #(hold, released) = epoch_hold.moved(hold, "old", "new", stamp(8))
  assert released == [10, 11]
  assert epoch_hold.link(hold) == #("new", stamp(8))
  assert epoch_hold.held_count(hold) == 0
  let #(_, after) = epoch_hold.frame(hold, "old", stamp(7), 12, 3)
  assert after == Dropped
}

pub fn moved_from_a_stranger_is_ignored_test() {
  let hold = epoch_hold.new("old", stamp(7), epoch_hold.default_limits())
  let #(hold, _) = epoch_hold.frame(hold, "new", stamp(8), 10, 0)
  let #(hold, released) = epoch_hold.moved(hold, "other", "new", stamp(8))
  assert released == []
  assert epoch_hold.held_count(hold) == 1
}

pub fn the_hold_is_bounded_in_frames_and_time_test() {
  let limits = epoch_hold.Limits(max_frames: 2, max_ms: 100)
  let hold = epoch_hold.new("old", stamp(7), limits)
  let #(hold, _) = epoch_hold.frame(hold, "new", stamp(8), 1, 0)
  let #(hold, _) = epoch_hold.frame(hold, "new", stamp(8), 2, 0)
  let #(hold, third) = epoch_hold.frame(hold, "new", stamp(8), 3, 0)
  assert third == Overflowed([1, 2, 3])
  assert epoch_hold.link(hold) == #("new", stamp(8))
  let hold = epoch_hold.new("old", stamp(7), limits)
  let #(hold, _) = epoch_hold.frame(hold, "new", stamp(8), 1, 10)
  let #(hold, early) = epoch_hold.expire(hold, 50)
  assert early == Held
  let #(_, late) = epoch_hold.expire(hold, 110)
  assert late == Overflowed([1])
}
