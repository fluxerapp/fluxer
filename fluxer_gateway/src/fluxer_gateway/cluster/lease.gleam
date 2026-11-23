// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease.{type Claim, Claim}
import bouncer_proto/worker.{type Roster}
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/util/table
import gleam/bool
import gleam/list
import gleam/order
import gleam/result

pub type HoldState {
  Serving
  Closed
  Isolated
}

pub type Validity {
  Validity(epoch: Epoch, valid_until_mono: Int, state: HoldState)
}

pub type LeaseError {
  NotHeld(bucket: BucketId)
  Expired(bucket: BucketId, epoch: Epoch)
  Fenced(bucket: BucketId, epoch: Epoch)
  BucketClosed(bucket: BucketId, retry_after_ms: Int)
  LeaseTableMissing
}

pub type ProbeAnswer {
  SeesBouncers
  NoBouncerMajority
}

type Meta {
  ValidUntil
  GraceUntil
  Incarnation
}

type Row =
  #(BucketId, Epoch, HoldState)

fn held_table() -> table.Table {
  table.named("fluxer_gateway_lease_held")
}

fn meta_table() -> table.Table {
  table.named("fluxer_gateway_lease_meta")
}

fn roster_table() -> table.Table {
  table.named("fluxer_gateway_lease_rosters")
}

pub fn valid_until(sent_mono: Int, ttl_ms: Int, skew_margin_ms: Int) -> Int {
  sent_mono + ttl_ms - skew_margin_ms
}

pub fn start() -> Result(Nil, LeaseError) {
  case
    table.ensure_public(held_table())
    && table.ensure_public(meta_table())
    && table.ensure_public(roster_table())
  {
    True -> Ok(Nil)
    False -> Error(LeaseTableMissing)
  }
}

fn meta(key: Meta) -> Result(Int, Nil) {
  let rows: List(#(Meta, Int)) = table.lookup(meta_table(), key)
  case rows {
    [#(_, value), ..] -> Ok(value)
    [] -> Error(Nil)
  }
}

fn before(now: Int, key: Meta) -> Bool {
  case meta(key) {
    Ok(until) -> now < until
    Error(Nil) -> False
  }
}

fn set_meta(key: Meta, value: Int) -> Nil {
  table.insert(meta_table(), #(key, value))
}

fn row(bucket: BucketId) -> Result(Row, LeaseError) {
  let rows: List(Row) = table.lookup(held_table(), bucket)
  case rows {
    [found, ..] -> Ok(found)
    [] -> Error(NotHeld(bucket))
  }
}

pub fn incarnation() -> Int {
  case start() {
    Error(_) -> clock.system_ms()
    Ok(Nil) -> {
      let _ = table.insert_new(meta_table(), #(Incarnation, clock.system_ms()))
      meta(Incarnation) |> result.unwrap(clock.system_ms())
    }
  }
}

pub fn validity(bucket: BucketId) -> Result(Validity, LeaseError) {
  use #(_, held_epoch, state) <- result.try(row(bucket))
  let now = clock.monotonic_ms()
  let until = meta(ValidUntil) |> result.unwrap(now)
  case before(now, ValidUntil), before(now, GraceUntil) {
    True, _ -> Ok(Validity(held_epoch, until, state))
    False, True -> Ok(Validity(held_epoch, until, Isolated))
    False, False -> Error(Expired(bucket, held_epoch))
  }
}

pub fn valid(bucket: BucketId) -> Result(Epoch, LeaseError) {
  use found <- result.try(validity(bucket))
  Ok(found.epoch)
}

pub fn check(bucket: BucketId) -> Result(Epoch, LeaseError) {
  use found <- result.try(validity(bucket))
  case found.state {
    Closed -> Error(BucketClosed(bucket, durations.bouncer().unplaced_retry_ms))
    Serving | Isolated -> Ok(found.epoch)
  }
}

pub fn held() -> List(Claim) {
  let rows: List(Row) = table.to_list(held_table())
  list.map(rows, fn(found) { Claim(bucket: found.0, epoch: found.1) })
}

pub fn held_epoch(bucket: BucketId) -> Result(Epoch, Nil) {
  case row(bucket) {
    Ok(#(_, held_epoch, _)) -> Ok(held_epoch)
    Error(_) -> Error(Nil)
  }
}

pub fn grant(bucket: BucketId, epoch: Epoch) -> Result(Nil, LeaseError) {
  use Nil <- result.try(start())
  case row(bucket) {
    Ok(#(_, current, _)) ->
      case epoch.compare(epoch, current) {
        order.Lt -> Error(Fenced(bucket, current))
        order.Eq -> Ok(Nil)
        order.Gt -> Ok(table.insert(held_table(), #(bucket, epoch, Serving)))
      }
    Error(_) -> Ok(table.insert(held_table(), #(bucket, epoch, Serving)))
  }
}

pub fn close(bucket: BucketId) -> Result(Nil, LeaseError) {
  use #(_, held_epoch, _) <- result.try(row(bucket))
  Ok(table.insert(held_table(), #(bucket, held_epoch, Closed)))
}

pub fn reopen(bucket: BucketId) -> Result(Nil, LeaseError) {
  use #(_, held_epoch, _) <- result.try(row(bucket))
  Ok(table.insert(held_table(), #(bucket, held_epoch, Serving)))
}

pub fn release(bucket: BucketId) -> Result(Nil, LeaseError) {
  use _ <- result.try(row(bucket))
  Ok(table.delete(held_table(), bucket))
}

pub fn extend(majority_sent_mono: Int) -> Result(Nil, LeaseError) {
  use Nil <- result.try(start())
  let timings = durations.bouncer()
  let until =
    valid_until(
      majority_sent_mono,
      timings.lease_ttl_ms,
      timings.skew_margin_ms,
    )
  case meta(ValidUntil) {
    Ok(current) if current >= until -> Ok(Nil)
    _ -> Ok(set_meta(ValidUntil, until))
  }
}

pub fn remember_roster(
  roster: Roster,
  at_mono: Int,
) -> Result(Nil, LeaseError) {
  use Nil <- result.try(start())
  table.insert(roster_table(), #(roster.hash, roster, at_mono))
  let oldest = at_mono - durations.bouncer().isolation_cap_ms
  let rows: List(#(Int, Roster, Int)) = table.to_list(roster_table())
  list.each(rows, fn(found) {
    case found.2 < oldest {
      True -> table.delete(roster_table(), found.0)
      False -> Nil
    }
  })
  Ok(Nil)
}

pub fn rosters(now_mono: Int) -> List(Roster) {
  let oldest = now_mono - durations.bouncer().isolation_cap_ms
  let rows: List(#(Int, Roster, Int)) = table.to_list(roster_table())
  rows
  |> list.filter(fn(found) { found.2 >= oldest })
  |> list.map(fn(found) { found.1 })
}

pub fn lapsed_at() -> Result(Int, Nil) {
  meta(ValidUntil)
}

pub fn sees_bouncers(now_mono: Int) -> Bool {
  before(now_mono, ValidUntil)
}

pub fn probe_answer() -> ProbeAnswer {
  let timings = durations.bouncer()
  let fresh_for =
    timings.lease_ttl_ms - timings.skew_margin_ms - 2 * timings.beat_interval_ms
  let now = clock.monotonic_ms()
  case meta(ValidUntil) {
    Ok(until) if now < until - fresh_for -> SeesBouncers
    _ -> NoBouncerMajority
  }
}

pub fn isolated() -> Bool {
  let now = clock.monotonic_ms()
  !sees_bouncers(now) && before(now, GraceUntil)
}

pub fn grace(until_mono: Int) -> Nil {
  use <- bool.guard(start() != Ok(Nil), Nil)
  case meta(GraceUntil) {
    Ok(current) if current >= until_mono -> Nil
    _ -> set_meta(GraceUntil, until_mono)
  }
}

pub fn end_grace() -> Nil {
  table.delete(meta_table(), GraceUntil)
}

pub fn fence() -> List(Claim) {
  let fenced = held()
  list.each(fenced, fn(claim) { table.delete(held_table(), claim.bucket) })
  table.delete(meta_table(), ValidUntil)
  end_grace()
  fenced
}
