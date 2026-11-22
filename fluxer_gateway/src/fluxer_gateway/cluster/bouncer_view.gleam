// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease.{type BucketRecord, type Claim, Free, Held, Moving}
import bouncer_proto/worker.{type Entry, type Roster, type WorkerId}
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}

pub type Bouncer {
  Bouncer(
    node: Atom,
    incarnation: Option(Int),
    directory: Dict(Atom, Entry),
    heard_mono: Int,
  )
}

type Sent {
  Sent(at_mono: Int, matched: Set(Int))
}

pub opaque type View {
  View(
    bouncers: Dict(Int, Bouncer),
    seq: Int,
    sent: Dict(Int, Sent),
    acked_mono: Option(Int),
    roster: Option(Roster),
  )
}

pub type Change {
  Take(bucket: BucketId, epoch: Epoch)
  Drop(bucket: BucketId)
}

pub fn new(nodes: List(Atom)) -> View {
  let bouncers =
    nodes
    |> list.index_map(fn(name, ordinal) {
      #(ordinal, Bouncer(name, None, dict.new(), 0))
    })
    |> dict.from_list
  View(bouncers:, seq: 0, sent: dict.new(), acked_mono: None, roster: None)
}

pub fn configured(view: View) -> Int {
  dict.size(view.bouncers)
}

pub fn nodes(view: View) -> List(#(Int, Atom)) {
  view.bouncers
  |> dict.to_list
  |> list.map(fn(pair) { #(pair.0, { pair.1 }.node) })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
}

pub fn node_of(view: View, ordinal: Int) -> Result(Atom, Nil) {
  dict.get(view.bouncers, ordinal) |> result.map(fn(found) { found.node })
}

pub fn welcomed(view: View) -> List(Int) {
  view.bouncers
  |> dict.to_list
  |> list.filter(fn(pair) { option.is_some({ pair.1 }.incarnation) })
  |> list.map(fn(pair) { pair.0 })
  |> list.sort(int.compare)
}

pub fn unwelcomed(view: View) -> List(Int) {
  view.bouncers
  |> dict.to_list
  |> list.filter(fn(pair) { option.is_none({ pair.1 }.incarnation) })
  |> list.map(fn(pair) { pair.0 })
  |> list.sort(int.compare)
}

pub fn majority(view: View) -> Bool {
  lease.has_quorum(list.length(welcomed(view)), configured(view))
}

pub fn roster(view: View) -> Option(Roster) {
  view.roster
}

pub fn acked_mono(view: View) -> Option(Int) {
  view.acked_mono
}

pub fn beat(view: View, now_mono: Int, keep_ms: Int) -> #(View, Int) {
  let seq = view.seq + 1
  let sent =
    view.sent
    |> dict.filter(fn(_, sent) { sent.at_mono >= now_mono - keep_ms })
    |> dict.insert(seq, Sent(now_mono, set.new()))
  #(View(..view, seq:, sent:), seq)
}

fn heard(view: View, ordinal: Int, now_mono: Int) -> View {
  case dict.get(view.bouncers, ordinal) {
    Ok(found) ->
      View(
        ..view,
        bouncers: dict.insert(
          view.bouncers,
          ordinal,
          Bouncer(..found, heard_mono: now_mono),
        ),
      )
    Error(Nil) -> view
  }
}

pub fn welcome(
  view: View,
  ordinal: Int,
  incarnation: Int,
  directory: List(Entry),
  roster: Roster,
  now_mono: Int,
) -> View {
  case dict.get(view.bouncers, ordinal) {
    Error(Nil) -> view
    Ok(found) -> {
      let entries =
        directory
        |> list.map(fn(entry) { #(entry.worker.node, entry) })
        |> dict.from_list
      View(
        ..view,
        bouncers: dict.insert(
          view.bouncers,
          ordinal,
          Bouncer(
            ..found,
            incarnation: Some(incarnation),
            directory: entries,
            heard_mono: now_mono,
          ),
        ),
        roster: Some(roster),
      )
    }
  }
}

pub fn ack(
  view: View,
  ordinal: Int,
  seq: Int,
  matched: Bool,
  roster: Roster,
  now_mono: Int,
) -> #(View, Option(Int)) {
  let view = heard(view, ordinal, now_mono)
  let counted = matched && list.contains(welcomed(view), ordinal)
  case counted, dict.get(view.sent, seq) {
    True, Ok(sent) -> {
      let sent = Sent(..sent, matched: set.insert(sent.matched, ordinal))
      let view =
        View(
          ..view,
          sent: dict.insert(view.sent, seq, sent),
          roster: Some(roster),
        )
      case
        lease.has_quorum(set.size(sent.matched), configured(view))
        && later(sent.at_mono, view.acked_mono)
      {
        True -> #(
          View(..view, acked_mono: Some(sent.at_mono)),
          Some(sent.at_mono),
        )
        False -> #(view, None)
      }
    }
    _, _ -> #(view, None)
  }
}

fn later(candidate: Int, current: Option(Int)) -> Bool {
  case current {
    Some(known) -> candidate > known
    None -> True
  }
}

pub fn directory_delta(
  view: View,
  ordinal: Int,
  changed: List(Entry),
  removed: List(WorkerId),
  now_mono: Int,
) -> View {
  let view = heard(view, ordinal, now_mono)
  case dict.get(view.bouncers, ordinal) {
    Ok(Bouncer(incarnation: Some(_), ..) as found) -> {
      let directory =
        list.fold(removed, found.directory, fn(directory, gone) {
          case dict.get(directory, gone.node) {
            Ok(entry) if entry.worker == gone ->
              dict.delete(directory, gone.node)
            _ -> directory
          }
        })
        |> fn(directory) {
          list.fold(changed, directory, fn(directory, entry) {
            dict.insert(directory, entry.worker.node, entry)
          })
        }
      View(
        ..view,
        bouncers: dict.insert(
          view.bouncers,
          ordinal,
          Bouncer(..found, directory:),
        ),
      )
    }
    _ -> view
  }
}

pub fn drop(view: View, ordinal: Int) -> View {
  case dict.get(view.bouncers, ordinal) {
    Ok(found) ->
      View(
        ..view,
        bouncers: dict.insert(
          view.bouncers,
          ordinal,
          Bouncer(..found, incarnation: None, directory: dict.new()),
        ),
      )
    Error(Nil) -> view
  }
}

pub fn silent(view: View, now_mono: Int, silence_ms: Int) -> List(Int) {
  view.bouncers
  |> dict.to_list
  |> list.filter(fn(pair) {
    option.is_some({ pair.1 }.incarnation)
    && now_mono - { pair.1 }.heard_mono > silence_ms
  })
  |> list.map(fn(pair) { pair.0 })
  |> list.sort(int.compare)
}

pub fn directory(view: View) -> List(Entry) {
  view.bouncers
  |> dict.values
  |> list.filter(fn(found) { option.is_some(found.incarnation) })
  |> list.flat_map(fn(found) { dict.values(found.directory) })
  |> list.fold(dict.new(), fn(union: Dict(Atom, Entry), entry: Entry) {
    case dict.get(union, entry.worker.node) {
      Ok(known) if known.last_beat_ms >= entry.last_beat_ms -> union
      _ -> dict.insert(union, entry.worker.node, entry)
    }
  })
  |> dict.values
}

pub fn reconcile(
  self: WorkerId,
  held: List(Claim),
  records: List(BucketRecord),
  fenced: Dict(BucketId, Epoch),
) -> List(Change) {
  let held =
    held
    |> list.map(fn(claim) { #(claim.bucket, claim.epoch) })
    |> dict.from_list
  list.filter_map(records, fn(found) {
    let current = dict.get(held, found.bucket)
    case wanted(self, found) {
      Some(epoch) ->
        case
          newer_than(epoch, current),
          newer_than(epoch, dict.get(fenced, found.bucket))
        {
          True, True -> Ok(Take(found.bucket, epoch))
          _, _ -> Error(Nil)
        }
      None ->
        case current {
          Ok(held_epoch) ->
            case epoch.compare(held_epoch, found.epoch) {
              order.Gt -> Error(Nil)
              order.Lt | order.Eq -> Ok(Drop(found.bucket))
            }
          Error(Nil) -> Error(Nil)
        }
    }
  })
}

pub fn accepts_grant(
  bucket: BucketId,
  granted: Epoch,
  known: Result(BucketRecord, Nil),
  fenced: Dict(BucketId, Epoch),
) -> Bool {
  let overtaken = case known {
    Ok(found) -> epoch.compare(lease.top_epoch(found), granted) == order.Gt
    Error(Nil) -> False
  }
  !overtaken && newer_than(granted, dict.get(fenced, bucket))
}

fn newer_than(candidate: Epoch, current: Result(Epoch, Nil)) -> Bool {
  case current {
    Ok(known) -> epoch.compare(candidate, known) == order.Gt
    Error(Nil) -> True
  }
}

fn wanted(self: WorkerId, found: BucketRecord) -> Option(Epoch) {
  case found.holder, found.state {
    _, Moving(to:, to_epoch:, ..) if to == self -> Some(to_epoch)
    Some(holder), Held | Some(holder), Moving(..) if holder == self ->
      Some(found.epoch)
    _, Free | _, Held | _, Moving(..) -> None
  }
}
