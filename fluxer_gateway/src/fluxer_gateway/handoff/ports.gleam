// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/codec_range.{type CodecRange}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/message.{type Refusal, type ToBouncer}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/core/clock
import fluxer_gateway/handoff/source.{type Timings}
import gleam/erlang/atom

pub type Ports {
  Ports(
    me: WorkerId,
    to_home: fn(BucketId, ToBouncer) -> Result(Nil, Nil),
    holding: fn(BucketId) -> Result(Epoch, Nil),
    held: fn() -> List(BucketId),
    place: fn(BucketId) -> Result(WorkerId, Nil),
    target_codecs: fn(WorkerId) -> List(CodecRange),
    close_bucket: fn(BucketId, Epoch, WorkerId, List(Int)) ->
      Result(Epoch, Refusal),
    bucket_emptied: fn(BucketId, Epoch) -> Nil,
    now_ms: fn() -> Int,
    timings: Timings,
    arrival_status_ms: Int,
  )
}

pub fn unwired() -> Ports {
  Ports(
    me: worker.WorkerId(node: atom.create("nonode@nohost"), incarnation: 0),
    to_home: fn(_, _) { Error(Nil) },
    holding: fn(_) { Error(Nil) },
    held: fn() { [] },
    place: fn(_) { Error(Nil) },
    target_codecs: fn(_) { [] },
    close_bucket: fn(_, _, _, _) { Error(message.Recovering) },
    bucket_emptied: fn(_, _) { Nil },
    now_ms: clock.monotonic_ms,
    timings: source.default_timings(),
    arrival_status_ms: 4000,
  )
}
