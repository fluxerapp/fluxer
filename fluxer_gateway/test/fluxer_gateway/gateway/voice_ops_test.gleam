// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/codec
import fluxer_gateway/gateway/voice_ops
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}

fn data(text: String) {
  let assert Ok(value) = json.parse(text, decode.dynamic)
  codec.voice_update(value)
}

pub fn two_per_second_go_direct_test() {
  let ops = voice_ops.new()
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 100, False)
  let assert voice_ops.Queued(ops:, arm_timer: True, queue_length: 1) =
    voice_ops.submit(ops, data("{}"), 200, False)
  let assert voice_ops.Queued(arm_timer: False, queue_length: 2, ..) =
    voice_ops.submit(ops, data("{}"), 300, False)
}

pub fn disabled_limits_never_queue_test() {
  let ops = voice_ops.new()
  let assert voice_ops.SendNow(ops) = voice_ops.submit(ops, data("{}"), 0, True)
  let assert voice_ops.SendNow(ops) = voice_ops.submit(ops, data("{}"), 0, True)
  let assert voice_ops.SendNow(_) = voice_ops.submit(ops, data("{}"), 0, True)
}

pub fn queue_keeps_newest_per_key_test() {
  let first = data("{\"guild_id\":\"1\",\"connection_id\":\"a\",\"n\":1}")
  let second = data("{\"guild_id\":\"1\",\"connection_id\":\"a\",\"n\":2}")
  let other = data("{\"guild_id\":\"2\",\"connection_id\":\"a\"}")
  let ops = voice_ops.new()
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let assert voice_ops.Queued(ops:, ..) = voice_ops.submit(ops, first, 1, False)
  let assert voice_ops.Queued(ops:, ..) = voice_ops.submit(ops, other, 2, False)
  let assert voice_ops.Queued(ops:, queue_length: 2, ..) =
    voice_ops.submit(ops, second, 3, False)
  let drained = voice_ops.drain(ops, 2000, False)
  assert drained.send == Some(other)
  assert drained.arm_timer
  let drained = voice_ops.drain(drained.ops, 2001, False)
  assert drained.send == Some(second)
  assert !drained.arm_timer
  assert voice_ops.queue_length(drained.ops) == 0
}

pub fn drain_waits_while_limited_test() {
  let ops = voice_ops.new()
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let assert voice_ops.Queued(ops:, ..) =
    voice_ops.submit(ops, data("{}"), 1, False)
  let drained = voice_ops.drain(ops, 500, False)
  assert drained.send == None
  assert drained.arm_timer
  assert voice_ops.queue_length(drained.ops) == 1
}

pub fn queue_is_bounded_test() {
  let ops = voice_ops.new()
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let assert voice_ops.SendNow(ops) =
    voice_ops.submit(ops, data("{}"), 0, False)
  let ops = fill(ops, 70)
  assert voice_ops.queue_length(ops) == 64
}

fn fill(ops, left) {
  case left {
    0 -> ops
    _ -> {
      let assert voice_ops.Queued(ops:, ..) =
        voice_ops.submit(ops, data("{}"), 1, False)
      fill(ops, left - 1)
    }
  }
}
