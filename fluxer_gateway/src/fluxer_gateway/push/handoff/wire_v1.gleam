// SPDX-License-Identifier: AGPL-3.0-or-later

pub const version = 1

pub type WaitingV1 {
  WaitingV1(age_ms: Int, create: BitArray)
}

pub type RecordV1 {
  RecordV1(age_ms: Int, attempts: Int, retry_in_ms: Int, job: BitArray)
}

pub type ReadV1 {
  ReadV1(user_id: Int, channel_id: Int, message_id: Int, age_ms: Int)
}

pub type ThrottleV1 {
  ThrottleV1(kind: String, requests: Int, accepts: Int)
}

pub type PushTailV1 {
  PushTailV1(
    bucket: Int,
    waiting: List(WaitingV1),
    records: List(RecordV1),
    reads: List(ReadV1),
    throttle: List(ThrottleV1),
  )
}

pub type PushTargetV1 {
  PushTargetV1(node: String, outbox: String)
}
