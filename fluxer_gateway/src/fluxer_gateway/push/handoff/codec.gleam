// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range.{type CodecRange, CodecRange}
import bouncer_proto/key
import fluxer_gateway/handoff/codec.{type Codec, type CodecError, Codec}
import fluxer_gateway/presence/handoff/terms
import fluxer_gateway/push/handoff/wire_v1.{
  type PushTailV1, type ReadV1, type RecordV1, type ThrottleV1, type WaitingV1,
}
import fluxer_gateway/push/outbox/state.{type Carried, type ReadMark}
import fluxer_gateway/push/server.{type Waiting}
import fluxer_gateway/push/throttle.{type RequestType}
import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/result

pub type Tail {
  Tail(
    bucket: Int,
    waiting: List(Waiting),
    carried: List(Carried),
    marks: List(ReadMark),
    throttle: List(#(RequestType, Int, Int)),
  )
}

pub fn range() -> CodecRange {
  CodecRange(
    ns: key.push,
    oldest_read: wire_v1.version,
    max_read: wire_v1.version,
    max_write: wire_v1.version,
  )
}

pub fn tail() -> Codec(Tail) {
  Codec(
    ns: key.push,
    part: blob.Tail,
    range: range(),
    encode_body: encode_tail,
    decode_body: decode_tail,
  )
}

pub fn encode_target(target: state.Target) -> BitArray {
  terms.encode(wire_v1.PushTargetV1(node: target.node, outbox: target.outbox))
}

pub fn decode_target(bytes: BitArray) -> Result(state.Target, Nil) {
  terms.decode(bytes, {
    use <- terms.record("push_target_v1")
    use node <- decode.field(1, decode.string)
    use outbox <- decode.field(2, decode.string)
    decode.success(state.Target(node:, outbox:))
  })
  |> result.replace_error(Nil)
}

fn encode_tail(value: Tail, version: Int) -> Result(BitArray, CodecError) {
  case version == wire_v1.version {
    False -> Error(codec.UnwritableVersion(version))
    True -> Ok(terms.encode(tail_v1(value)))
  }
}

fn decode_tail(version: Int, body: BitArray) -> Result(Tail, CodecError) {
  case version == wire_v1.version {
    False -> Error(codec.UnreadableVersion(version))
    True -> {
      use wire <- result.try(
        terms.decode(body, tail_decoder()) |> result.map_error(codec.Malformed),
      )
      tail_of(wire)
    }
  }
}

fn tail_v1(tail: Tail) -> PushTailV1 {
  wire_v1.PushTailV1(
    bucket: tail.bucket,
    waiting: list.map(tail.waiting, fn(found) {
      wire_v1.WaitingV1(
        age_ms: found.age_ms,
        create: terms.encode(found.create),
      )
    }),
    records: list.map(tail.carried, fn(found) {
      wire_v1.RecordV1(
        age_ms: found.age_ms,
        attempts: found.attempts,
        retry_in_ms: found.retry_in_ms,
        job: terms.encode(found.job),
      )
    }),
    reads: list.map(tail.marks, fn(mark) {
      wire_v1.ReadV1(
        user_id: mark.user,
        channel_id: mark.channel,
        message_id: mark.message,
        age_ms: mark.age_ms,
      )
    }),
    throttle: list.map(tail.throttle, fn(found) {
      wire_v1.ThrottleV1(
        kind: throttle.kind_label(found.0),
        requests: found.1,
        accepts: found.2,
      )
    }),
  )
}

fn tail_of(wire: PushTailV1) -> Result(Tail, CodecError) {
  use waiting <- result.try(
    list.try_map(wire.waiting, fn(found) {
      use create <- result.map(codec.term_of(found.create))
      server.Waiting(age_ms: found.age_ms, create:)
    }),
  )
  use carried <- result.map(
    list.try_map(wire.records, fn(found) {
      use job <- result.map(codec.term_of(found.job))
      state.Carried(
        job:,
        age_ms: found.age_ms,
        attempts: found.attempts,
        retry_in_ms: found.retry_in_ms,
      )
    }),
  )
  Tail(
    bucket: wire.bucket,
    waiting:,
    carried:,
    marks: list.map(wire.reads, fn(found) {
      state.ReadMark(
        user: found.user_id,
        channel: found.channel_id,
        message: found.message_id,
        age_ms: found.age_ms,
      )
    }),
    throttle: list.filter_map(wire.throttle, fn(found) {
      throttle.kind_of(found.kind)
      |> result.map(fn(kind) { #(kind, found.requests, found.accepts) })
    }),
  )
}

fn waiting_decoder() -> Decoder(WaitingV1) {
  use <- terms.record("waiting_v1")
  use age_ms <- decode.field(1, decode.int)
  use create <- decode.field(2, decode.bit_array)
  decode.success(wire_v1.WaitingV1(age_ms:, create:))
}

fn record_decoder() -> Decoder(RecordV1) {
  use <- terms.record("record_v1")
  use age_ms <- decode.field(1, decode.int)
  use attempts <- decode.field(2, decode.int)
  use retry_in_ms <- decode.field(3, decode.int)
  use job <- decode.field(4, decode.bit_array)
  decode.success(wire_v1.RecordV1(age_ms:, attempts:, retry_in_ms:, job:))
}

fn read_decoder() -> Decoder(ReadV1) {
  use <- terms.record("read_v1")
  use user_id <- decode.field(1, decode.int)
  use channel_id <- decode.field(2, decode.int)
  use message_id <- decode.field(3, decode.int)
  use age_ms <- decode.field(4, decode.int)
  decode.success(wire_v1.ReadV1(user_id:, channel_id:, message_id:, age_ms:))
}

fn throttle_decoder() -> Decoder(ThrottleV1) {
  use <- terms.record("throttle_v1")
  use kind <- decode.field(1, decode.string)
  use requests <- decode.field(2, decode.int)
  use accepts <- decode.field(3, decode.int)
  decode.success(wire_v1.ThrottleV1(kind:, requests:, accepts:))
}

fn tail_decoder() -> Decoder(PushTailV1) {
  use <- terms.record("push_tail_v1")
  use bucket <- decode.field(1, decode.int)
  use waiting <- decode.field(2, decode.list(waiting_decoder()))
  use records <- decode.field(3, decode.list(record_decoder()))
  use reads <- decode.field(4, decode.list(read_decoder()))
  use throttle <- decode.field(5, decode.list(throttle_decoder()))
  decode.success(wire_v1.PushTailV1(
    bucket:,
    waiting:,
    records:,
    reads:,
    throttle:,
  ))
}
