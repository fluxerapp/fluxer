// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order

pub type Limits {
  Limits(max_frames: Int, max_ms: Int)
}

pub fn default_limits() -> Limits {
  Limits(max_frames: 1000, max_ms: 2000)
}

pub type Decision(frame) {
  Deliver(frames: List(frame))
  Held
  Dropped
  Overflowed(frames: List(frame))
}

pub opaque type EpochHold(source, frame) {
  EpochHold(
    link: source,
    epoch: Epoch,
    limits: Limits,
    held: List(#(source, Epoch, frame)),
    count: Int,
    since: Option(Int),
  )
}

pub fn new(
  link: source,
  epoch: Epoch,
  limits: Limits,
) -> EpochHold(source, frame) {
  EpochHold(link:, epoch:, limits:, held: [], count: 0, since: None)
}

pub fn link(hold: EpochHold(source, frame)) -> #(source, Epoch) {
  #(hold.link, hold.epoch)
}

pub fn held_count(hold: EpochHold(source, frame)) -> Int {
  hold.count
}

pub fn frame(
  hold: EpochHold(source, frame),
  from: source,
  stamp: Epoch,
  frame: frame,
  now: Int,
) -> #(EpochHold(source, frame), Decision(frame)) {
  case epoch.compare(stamp, hold.epoch), from == hold.link {
    order.Lt, _ -> #(hold, Dropped)
    order.Eq, _ -> #(hold, Deliver([frame]))
    order.Gt, True -> #(EpochHold(..hold, epoch: stamp), Deliver([frame]))
    order.Gt, False -> {
      let since = case hold.since {
        Some(at) -> at
        None -> now
      }
      let hold =
        EpochHold(
          ..hold,
          held: [#(from, stamp, frame), ..hold.held],
          count: hold.count + 1,
          since: Some(since),
        )
      case hold.count > hold.limits.max_frames {
        True -> overflow(hold)
        False -> #(hold, Held)
      }
    }
  }
}

pub fn moved(
  hold: EpochHold(source, frame),
  from: source,
  to: source,
  stamp: Epoch,
) -> #(EpochHold(source, frame), List(frame)) {
  case from == hold.link, epoch.compare(stamp, hold.epoch) {
    True, order.Gt -> {
      let ordered = list.reverse(hold.held)
      let released =
        ordered
        |> list.filter(fn(entry) {
          entry.0 == to && epoch.compare(entry.1, stamp) == order.Eq
        })
        |> list.map(fn(entry) { entry.2 })
      let kept =
        list.filter(ordered, fn(entry) {
          epoch.compare(entry.1, stamp) == order.Gt
        })
      #(
        EpochHold(
          ..hold,
          link: to,
          epoch: stamp,
          held: list.reverse(kept),
          count: list.length(kept),
          since: case kept {
            [] -> None
            _ -> hold.since
          },
        ),
        released,
      )
    }
    _, _ -> #(hold, [])
  }
}

pub fn expire(
  hold: EpochHold(source, frame),
  now: Int,
) -> #(EpochHold(source, frame), Decision(frame)) {
  case hold.since {
    Some(since) if now - since >= hold.limits.max_ms -> overflow(hold)
    _ -> #(hold, Held)
  }
}

fn overflow(
  hold: EpochHold(source, frame),
) -> #(EpochHold(source, frame), Decision(frame)) {
  let ordered = list.reverse(hold.held)
  let #(link, stamp) =
    list.fold(ordered, #(hold.link, hold.epoch), fn(acc, entry) {
      case epoch.compare(entry.1, acc.1) {
        order.Gt -> #(entry.0, entry.1)
        _ -> acc
      }
    })
  #(
    EpochHold(..hold, link:, epoch: stamp, held: [], count: 0, since: None),
    Overflowed(list.map(ordered, fn(entry) { entry.2 })),
  )
}
