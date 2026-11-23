// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/codec_range.{type CodecRange}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId}
import bouncer_proto/worker.{type WorkerId}
import gleam/erlang/process.{type Pid}
import gleam/option.{type Option}

pub type AdapterError {
  EntityMissing(key: Key)
  EntityBusy(key: Key)
  EncodeFailed(reason: String)
  DecodeFailed(reason: String)
  InstallFailed(reason: String)
}

pub type Frozen {
  Frozen(tail: BitArray, stub: Pid)
}

pub type Arrival {
  Arrival(entity: Pid, target_ref: BitArray)
}

pub type Precopy {
  Precopy(
    from_version: Int,
    snapshot: fn(Key, MoveId, Int) -> Result(BitArray, AdapterError),
    changes: fn(Key, MoveId, Int, Int) -> Result(#(BitArray, Int), AdapterError),
    cancel: fn(Key, MoveId) -> Nil,
    stage: fn(Key, MoveId, BitArray) -> Result(Pid, AdapterError),
    restage: fn(Pid, Int, BitArray) -> Result(Nil, AdapterError),
    unstage: fn(Pid) -> Nil,
  )
}

pub type Adapter {
  Adapter(
    ns: Int,
    codecs: CodecRange,
    entities_in: fn(Int) -> List(Int),
    freeze: fn(Key, MoveId, Int, Epoch) -> Result(Frozen, AdapterError),
    install: fn(Key, WorkerId, Epoch, BitArray) -> Result(Arrival, AdapterError),
    release: fn(Pid, Int, BitArray) -> Result(Nil, AdapterError),
    seal: fn(Pid, Int) -> Result(Nil, AdapterError),
    abort_arrival: fn(Pid) -> Nil,
    committed: fn(Pid, BitArray, Epoch) -> Result(Nil, AdapterError),
    thaw: fn(Pid, List(#(Int, BitArray))) -> Result(Nil, AdapterError),
    abandon: fn(Key) -> Nil,
    precopy: Option(Precopy),
  )
}

pub fn describe(error: AdapterError) -> String {
  case error {
    EntityMissing(_) -> "entity missing"
    EntityBusy(_) -> "entity busy"
    EncodeFailed(reason) -> "encode failed: " <> reason
    DecodeFailed(reason) -> "decode failed: " <> reason
    InstallFailed(reason) -> "install failed: " <> reason
  }
}
