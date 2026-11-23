// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/handoff/codec
import fluxer_gateway/push/handoff/codec as push_codec
import fluxer_gateway/push/job
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import fluxer_gateway/push/throttle
import gleam/option.{None}

pub const directory = "test/fixtures/handoff/push"

pub const fixture_epoch = 7

pub fn path(part: String, version: Int) -> String {
  directory <> "/" <> part <> "_v" <> int_text(version) <> ".bin"
}

@external(erlang, "erlang", "integer_to_binary")
fn int_text(value: Int) -> String

pub fn message_job() -> job.Job {
  let assert Ok(built) =
    job.for_users(
      job.template([], channel: 9_410_001, message: 7001),
      rule.Bulk,
      [11, 12],
    )
  built
}

pub fn expected_tail() -> push_codec.Tail {
  push_codec.Tail(
    bucket: 17,
    waiting: [],
    carried: [
      state.Carried(
        job: message_job(),
        age_ms: 1500,
        attempts: 2,
        retry_in_ms: 4000,
      ),
      state.Carried(
        job: job.clear_job(
          user: 11,
          channel: 9_410_001,
          message: 7001,
          after: None,
        ),
        age_ms: 10,
        attempts: 0,
        retry_in_ms: 0,
      ),
    ],
    marks: [
      state.ReadMark(user: 12, channel: 9_410_001, message: 6999, age_ms: 40),
    ],
    throttle: [#(throttle.UserGuildSettings, 9, 4), #(throttle.ReadState, 2, 2)],
  )
}

pub fn tail_bytes(version: Int) -> BitArray {
  let assert Ok(bytes) =
    codec.encode(push_codec.tail(), expected_tail(), version, fixture_epoch)
  bytes
}
