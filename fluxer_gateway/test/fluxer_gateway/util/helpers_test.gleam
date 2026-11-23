// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/backoff
import fluxer_gateway/util/iso8601
import fluxer_gateway/util/liveness
import fluxer_gateway/util/shard
import gleam/erlang/process

pub fn backoff_without_jitter_doubles_and_caps_test() {
  assert backoff.delay_ms(
      attempt: 0,
      base_ms: 1000,
      max_ms: 30_000,
      jitter_ms: 0,
    )
    == 1000
  assert backoff.delay_ms(
      attempt: 3,
      base_ms: 1000,
      max_ms: 30_000,
      jitter_ms: 0,
    )
    == 8000
  assert backoff.delay_ms(
      attempt: 10,
      base_ms: 1000,
      max_ms: 30_000,
      jitter_ms: 0,
    )
    == 30_000
  assert backoff.delay_ms(
      attempt: -1,
      base_ms: 1000,
      max_ms: 30_000,
      jitter_ms: 0,
    )
    == 1000
  assert backoff.delay_ms(
      attempt: 99,
      base_ms: 1000,
      max_ms: 30_000,
      jitter_ms: 0,
    )
    == 30_000
}

pub fn backoff_jitter_stays_in_range_test() {
  let delay =
    backoff.delay_ms(attempt: 2, base_ms: 1000, max_ms: 30_000, jitter_ms: 2000)
  assert delay >= 3000 && delay <= 5000
}

pub fn iso8601_parses_utc_test() {
  assert iso8601.parse_ms("1970-01-01T00:00:00Z") == Ok(0)
  assert iso8601.parse_ms("2024-03-01T12:30:15.5Z") == Ok(1_709_296_215_500)
  assert iso8601.parse_ms("2024-03-01T12:30:15.123456789Z")
    == Ok(1_709_296_215_123)
  assert iso8601.parse_ms("2024-03-01T12:30:15Z\n") == Ok(1_709_296_215_000)
}

pub fn iso8601_accepts_utc_offset_designators_test() {
  assert iso8601.parse_ms("2024-03-01T12:30:15+00:00") == Ok(1_709_296_215_000)
  assert iso8601.parse_ms("2024-03-01T12:30:15-00:00") == Ok(1_709_296_215_000)
  assert iso8601.parse_ms("2024-03-01T12:30:15+0000") == Ok(1_709_296_215_000)
  assert iso8601.parse_ms("2024-03-01T12:30:15-0000") == Ok(1_709_296_215_000)
  assert iso8601.parse_ms("2024-03-01T12:30:15.5+00:00")
    == Ok(1_709_296_215_500)
  assert iso8601.parse_ms("2024-03-01T12:30:15.123-0000")
    == Ok(1_709_296_215_123)
  assert iso8601.parse_ms("2024-03-01T12:30:15+00:00\n")
    == Ok(1_709_296_215_000)
}

pub fn iso8601_rejects_non_utc_offsets_test() {
  assert iso8601.parse_ms("2024-03-01T12:30:15+00:01") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15+0100") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15+00") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15+00:000") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15Z+00:00") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15.5+01:00") == Error(Nil)
}

pub fn iso8601_rejects_bad_input_test() {
  assert iso8601.parse_ms("2024-03-01 12:30:15Z") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15+01:00") == Error(Nil)
  assert iso8601.parse_ms("2024-13-01T00:00:00Z") == Error(Nil)
  assert iso8601.parse_ms("2024-03-00T00:00:00Z") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15.Z") == Error(Nil)
  assert iso8601.parse_ms("2024-03-01T12:30:15.1234567890Z") == Error(Nil)
}

pub fn iso8601_rolls_day_past_month_end_test() {
  assert iso8601.parse_ms("2023-02-29T00:00:00Z")
    == iso8601.parse_ms("2023-03-01T00:00:00Z")
}

pub fn shard_index_is_phash2_test() {
  let index = shard.shard_index(123_456_789, 16)
  assert index >= 0 && index < 16
  assert index == shard.shard_index(123_456_789, 16)
  assert shard.shard_index(5, 0) == 0
}

pub fn liveness_local_test() {
  let pid = process.self()
  assert liveness.is_alive(pid)
}
