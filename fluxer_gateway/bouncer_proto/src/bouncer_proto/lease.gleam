// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/worker.{type WorkerId}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set.{type Set}

pub type Writer {
  Writer(ordinal: Int, incarnation: Int, seq: Int)
}

pub type CommitRow {
  CommitRow(id: Int, target_ref: BitArray)
}

pub type BucketState {
  Held
  Moving(
    to: WorkerId,
    to_epoch: Epoch,
    pending: Set(Int),
    commits: List(CommitRow),
  )
  Free
}

pub type BucketRecord {
  BucketRecord(
    bucket: BucketId,
    holder: Option(WorkerId),
    epoch: Epoch,
    pool: String,
    state: BucketState,
    written_by: Writer,
  )
}

pub type Claim {
  Claim(bucket: BucketId, epoch: Epoch)
}

pub fn quorum(configured_count: Int) -> Int {
  int.max(configured_count, 1) / 2 + 1
}

pub fn has_quorum(acks: Int, configured_count: Int) -> Bool {
  acks >= quorum(configured_count)
}

pub fn top_epoch(record: BucketRecord) -> Epoch {
  case record.state {
    Moving(to_epoch:, ..) -> epoch.max(record.epoch, to_epoch)
    Held | Free -> record.epoch
  }
}

pub fn newer(candidate: BucketRecord, current: BucketRecord) -> Bool {
  case epoch.compare(candidate.epoch, current.epoch) {
    order.Gt -> True
    order.Lt -> False
    order.Eq -> candidate.written_by.seq > current.written_by.seq
  }
}

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

pub fn held_digest(claims: List(Claim)) -> Int {
  claims
  |> list.map(fn(claim) {
    #(claim.bucket.ns, claim.bucket.bucket, epoch.to_int(claim.epoch))
  })
  |> list.sort(fn(a, b) {
    case int.compare(a.0, b.0) {
      order.Eq ->
        case int.compare(a.1, b.1) {
          order.Eq -> int.compare(a.2, b.2)
          other -> other
        }
      other -> other
    }
  })
  |> phash2(4_294_967_295)
}
