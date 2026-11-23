// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease.{
  type BucketRecord, type Claim, type Writer, BucketRecord, Claim, Held, Moving,
  Writer,
}
import bouncer_proto/message.{
  type WriteRefusal, HolderHeard, RosterAlive, StillRecovering, Superseded,
}
import bouncer_proto/worker.{type WorkerId}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/set

type Slot {
  Slot(record: BucketRecord, confirmed: Bool)
}

pub opaque type Leases {
  Leases(
    ordinal: Int,
    incarnation: Int,
    slots: Dict(BucketId, Slot),
    confirmed: Int,
    recovering: Bool,
  )
}

pub type HolderView {
  HolderView(
    silent_ms: Int,
    roster_majority_beating: Bool,
    said_bye: Bool,
    new_incarnation: Bool,
  )
}

pub type Fate {
  Retracting
  Departed
  Present
}

pub type LeaseError {
  UnknownBucket(bucket: BucketId)
  StillInRecovery
}

pub type Proposal {
  Proposal(
    expect_epoch: Epoch,
    expect_writer: Writer,
    record: BucketRecord,
    previous: Option(BucketRecord),
    previous_confirmed: Bool,
  )
}

pub fn new(ordinal: Int, incarnation: Int) -> Leases {
  Leases(
    ordinal: ordinal,
    incarnation: incarnation,
    slots: dict.new(),
    confirmed: 0,
    recovering: True,
  )
}

pub fn absent_writer() -> Writer {
  Writer(ordinal: -1, incarnation: 0, seq: 0)
}

pub fn ready(leases: Leases) -> Bool {
  !leases.recovering
}

pub fn finish_recovery(leases: Leases) -> Leases {
  Leases(..leases, recovering: False)
}

pub fn record(
  leases: Leases,
  bucket: BucketId,
) -> Result(BucketRecord, LeaseError) {
  case dict.get(leases.slots, bucket) {
    Ok(slot) -> Ok(slot.record)
    Error(Nil) -> Error(UnknownBucket(bucket))
  }
}

pub fn size(leases: Leases) -> Int {
  dict.size(leases.slots)
}

pub fn confirmed_count(leases: Leases) -> Int {
  leases.confirmed
}

pub fn records(leases: Leases) -> List(BucketRecord) {
  dict.fold(leases.slots, [], fn(acc, _, slot) { [slot.record, ..acc] })
}

pub fn confirmed_records(leases: Leases) -> List(BucketRecord) {
  dict.fold(leases.slots, [], fn(acc, _, slot) {
    case slot.confirmed {
      True -> [slot.record, ..acc]
      False -> acc
    }
  })
}

pub fn is_confirmed(leases: Leases, bucket: BucketId) -> Bool {
  case dict.get(leases.slots, bucket) {
    Ok(slot) -> slot.confirmed
    Error(Nil) -> False
  }
}

fn same(a: BucketRecord, b: BucketRecord) -> Bool {
  a.epoch == b.epoch && a.written_by == b.written_by
}

fn order(a: BucketRecord, b: BucketRecord) -> Order {
  case epoch.compare(lease.top_epoch(a), lease.top_epoch(b)) {
    order.Eq -> int.compare(a.written_by.seq, b.written_by.seq)
    other -> other
  }
}

fn weight(confirmed: Bool) -> Int {
  case confirmed {
    True -> 1
    False -> 0
  }
}

fn was_confirmed(leases: Leases, bucket: BucketId) -> Int {
  case dict.get(leases.slots, bucket) {
    Ok(slot) -> weight(slot.confirmed)
    Error(Nil) -> 0
  }
}

fn put(leases: Leases, record: BucketRecord, confirmed: Bool) -> Leases {
  Leases(
    ..leases,
    confirmed: leases.confirmed
      - was_confirmed(leases, record.bucket)
      + weight(confirmed),
    slots: dict.insert(
      leases.slots,
      record.bucket,
      Slot(record: record, confirmed: confirmed),
    ),
  )
}

pub fn propose(
  leases: Leases,
  record: BucketRecord,
) -> Result(#(Leases, Proposal), LeaseError) {
  case leases.recovering {
    True -> Error(StillInRecovery)
    False -> {
      let previous = dict.get(leases.slots, record.bucket)
      let #(expect_epoch, expect_writer) = case previous {
        Ok(slot) -> #(slot.record.epoch, slot.record.written_by)
        Error(Nil) -> #(epoch.zero(), absent_writer())
      }
      let stamped =
        BucketRecord(
          ..record,
          written_by: Writer(
            ordinal: leases.ordinal,
            incarnation: leases.incarnation,
            seq: expect_writer.seq + 1,
          ),
        )
      let proposal = case previous {
        Ok(slot) ->
          Proposal(
            expect_epoch: expect_epoch,
            expect_writer: expect_writer,
            record: stamped,
            previous: Some(slot.record),
            previous_confirmed: slot.confirmed,
          )
        Error(Nil) ->
          Proposal(
            expect_epoch: expect_epoch,
            expect_writer: expect_writer,
            record: stamped,
            previous: None,
            previous_confirmed: False,
          )
      }
      Ok(#(put(leases, stamped, False), proposal))
    }
  }
}

pub fn is_regrant(current: BucketRecord, next: BucketRecord) -> Bool {
  case current.holder {
    None -> False
    Some(holder) ->
      case next.holder == Some(holder), current.state {
        True, _ -> False
        False, Moving(to: to, pending: pending, ..) ->
          next.holder != Some(to) || !set.is_empty(pending)
        False, _ -> True
      }
  }
}

pub fn regrant_allowed(
  holder: HolderView,
  regrant_silence_ms: Int,
  isolation_cap_ms: Int,
) -> Result(Nil, WriteRefusal) {
  case
    holder.said_bye || holder.new_incarnation,
    holder.silent_ms >= regrant_silence_ms,
    holder.roster_majority_beating || holder.silent_ms >= isolation_cap_ms
  {
    True, _, _ -> Ok(Nil)
    False, False, _ -> Error(HolderHeard(holder.silent_ms))
    False, True, False -> Error(RosterAlive)
    False, True, True -> Ok(Nil)
  }
}

pub fn apply_write(
  leases: Leases,
  expect_epoch: Epoch,
  expect_writer: Writer,
  record: BucketRecord,
  holder: HolderView,
  regrant_silence_ms: Int,
  isolation_cap_ms: Int,
) -> #(Leases, Result(Nil, WriteRefusal)) {
  case leases.recovering, dict.get(leases.slots, record.bucket) {
    True, _ -> #(leases, Error(StillRecovering))
    False, Error(Nil) ->
      case expect_epoch == epoch.zero() && expect_writer == absent_writer() {
        True -> #(put(leases, record, False), Ok(Nil))
        False -> #(leases, Error(StillRecovering))
      }
    False, Ok(slot) ->
      case
        same(slot.record, record),
        slot.record.epoch == expect_epoch
        && slot.record.written_by == expect_writer
      {
        True, _ -> #(leases, Ok(Nil))
        False, False -> #(
          leases,
          Error(Superseded(slot.record.epoch, slot.record.written_by)),
        )
        False, True ->
          case is_regrant(slot.record, record) {
            False -> #(put(leases, record, False), Ok(Nil))
            True ->
              case
                regrant_allowed(holder, regrant_silence_ms, isolation_cap_ms)
              {
                Ok(Nil) -> #(put(leases, record, False), Ok(Nil))
                Error(refusal) -> #(leases, Error(refusal))
              }
          }
      }
  }
}

pub fn commit(leases: Leases, record: BucketRecord) -> Leases {
  case dict.get(leases.slots, record.bucket) {
    Ok(slot) ->
      case same(slot.record, record) {
        True -> put(leases, record, True)
        False -> leases
      }
    Error(Nil) -> leases
  }
}

pub fn revert(leases: Leases, proposal: Proposal) -> Leases {
  case dict.get(leases.slots, proposal.record.bucket) {
    Ok(slot) ->
      case same(slot.record, proposal.record), proposal.previous {
        True, Some(previous) ->
          put(leases, previous, proposal.previous_confirmed)
        True, None ->
          Leases(
            ..leases,
            confirmed: leases.confirmed - weight(slot.confirmed),
            slots: dict.delete(leases.slots, proposal.record.bucket),
          )
        False, _ -> leases
      }
    Error(Nil) -> leases
  }
}

pub fn adopt(
  leases: Leases,
  record: BucketRecord,
  fate: fn(Writer) -> Fate,
) -> Leases {
  case dict.get(leases.slots, record.bucket) {
    Error(Nil) -> put(leases, record, True)
    Ok(slot) -> {
      let local = slot.record
      case same(local, record), order(record, local), slot.confirmed {
        True, _, _ -> put(leases, record, True)
        False, order.Gt, _ -> put(leases, record, True)
        False, order.Eq, False -> put(leases, record, True)
        False, _, True -> leases
        False, _, False ->
          case fate(local.written_by) {
            Retracting -> put(leases, record, True)
            Departed -> put(leases, local, True)
            Present -> leases
          }
      }
    }
  }
}

pub fn sync(
  leases: Leases,
  records: List(BucketRecord),
  fate: fn(Writer) -> Fate,
) -> Leases {
  list.fold(records, leases, fn(acc, record) { adopt(acc, record, fate) })
}

pub fn settle_orphans(
  leases: Leases,
  fate: fn(Writer) -> Fate,
) -> #(Leases, List(BucketRecord)) {
  dict.fold(leases.slots, #(leases, []), fn(acc, _, slot) {
    case slot.confirmed, fate(slot.record.written_by) {
      False, Departed -> #(put(acc.0, slot.record, True), [slot.record, ..acc.1])
      _, _ -> acc
    }
  })
}

pub fn claim(
  leases: Leases,
  worker: WorkerId,
  pool: String,
  held: List(Claim),
) -> Leases {
  list.fold(held, leases, fn(acc, claim) {
    let claimed =
      BucketRecord(
        bucket: claim.bucket,
        holder: Some(worker),
        epoch: claim.epoch,
        pool: pool,
        state: Held,
        written_by: absent_writer(),
      )
    case dict.get(acc.slots, claim.bucket) {
      Error(Nil) -> put(acc, claimed, True)
      Ok(Slot(record: local, confirmed: True)) ->
        case epoch.compare(claim.epoch, lease.top_epoch(local)) {
          order.Gt -> put(acc, claimed, True)
          order.Eq | order.Lt -> acc
        }
      Ok(Slot(confirmed: False, ..)) -> acc
    }
  })
}

pub fn held_by(leases: Leases, worker: WorkerId) -> List(Claim) {
  dict.fold(leases.slots, [], fn(acc, bucket, slot) {
    let record = slot.record
    let as_holder = case record.holder == Some(worker), record.state {
      True, Held -> [Claim(bucket, record.epoch)]
      True, Moving(..) -> [Claim(bucket, record.epoch)]
      _, _ -> []
    }
    let as_target = case record.state {
      Moving(to: to, to_epoch: to_epoch, ..) if to == worker -> [
        Claim(bucket, to_epoch),
      ]
      _ -> []
    }
    list.flatten([as_holder, as_target, acc])
  })
}

pub fn matches(leases: Leases, worker: WorkerId, held_digest: Int) -> Bool {
  lease.held_digest(held_by(leases, worker)) == held_digest
}
