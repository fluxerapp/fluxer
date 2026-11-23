// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/gateway/codec.{type Field, type VoiceUpdate}
import gleam/list
import gleam/option.{type Option, None, Some}

const rate_limit = 2

fn rate_window_ms() -> Int {
  durations.gateway().voice_ops_rate_window_ms
}

const max_queue_length = 64

pub fn drain_interval_ms() -> Int {
  durations.gateway().voice_ops_drain_interval_ms
}

pub opaque type VoiceOps {
  VoiceOps(
    sent_at: Option(List(Int)),
    queue: List(VoiceUpdate),
    timer_armed: Bool,
  )
}

pub type Submitted {
  SendNow(VoiceOps)
  Queued(ops: VoiceOps, arm_timer: Bool, queue_length: Int)
}

pub type Drained {
  Drained(ops: VoiceOps, send: Option(VoiceUpdate), arm_timer: Bool)
}

pub fn new() -> VoiceOps {
  VoiceOps(sent_at: None, queue: [], timer_armed: False)
}

pub fn submit(
  ops: VoiceOps,
  data: VoiceUpdate,
  now_ms: Int,
  limits_disabled: Bool,
) -> Submitted {
  let admitted = case ops.queue {
    [] -> admit(ops, now_ms, limits_disabled)
    [_, ..] -> Error(Nil)
  }
  case admitted {
    Ok(admitted) -> SendNow(admitted)
    Error(Nil) -> {
      let queue = enqueue(ops.queue, data)
      Queued(
        ops: VoiceOps(..ops, queue:, timer_armed: True),
        arm_timer: !ops.timer_armed,
        queue_length: list.length(queue),
      )
    }
  }
}

pub fn drain(ops: VoiceOps, now_ms: Int, limits_disabled: Bool) -> Drained {
  let ops = VoiceOps(..ops, timer_armed: False)
  case ops.queue {
    [] -> Drained(ops:, send: None, arm_timer: False)
    [next, ..rest] ->
      case admit(ops, now_ms, limits_disabled) {
        Ok(admitted) -> {
          let more = rest != []
          Drained(
            ops: VoiceOps(..admitted, queue: rest, timer_armed: more),
            send: Some(next),
            arm_timer: more,
          )
        }
        Error(Nil) ->
          Drained(
            ops: VoiceOps(..ops, timer_armed: True),
            send: None,
            arm_timer: True,
          )
      }
  }
}

pub fn queue_length(ops: VoiceOps) -> Int {
  list.length(ops.queue)
}

fn admit(
  ops: VoiceOps,
  now_ms: Int,
  limits_disabled: Bool,
) -> Result(VoiceOps, Nil) {
  case limits_disabled, ops.sent_at {
    True, _ -> Ok(ops)
    False, None -> Ok(VoiceOps(..ops, sent_at: Some([now_ms])))
    False, Some(sent_at) -> {
      let recent =
        list.filter(sent_at, fn(at) { now_ms - at < rate_window_ms() })
      case list.length(recent) >= rate_limit {
        True -> Error(Nil)
        False -> Ok(VoiceOps(..ops, sent_at: Some([now_ms, ..recent])))
      }
    }
  }
}

fn enqueue(queue: List(VoiceUpdate), data: VoiceUpdate) -> List(VoiceUpdate) {
  let deduped = case debounce_key(data) {
    None -> queue
    Some(key) ->
      list.filter(queue, fn(queued) { debounce_key(queued) != Some(key) })
  }
  let trimmed = case list.length(deduped) >= max_queue_length {
    True -> list.drop(deduped, 1)
    False -> deduped
  }
  list.append(trimmed, [data])
}

type DebounceKey {
  DebounceKey(guild: Field, connection: Field)
}

fn debounce_key(data: VoiceUpdate) -> Option(DebounceKey) {
  case codec.is_absent(data.guild) && codec.is_absent(data.connection) {
    True -> None
    False -> Some(DebounceKey(guild: data.guild, connection: data.connection))
  }
}
