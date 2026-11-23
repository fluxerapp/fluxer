// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto.{type ProtocolRange}
import bouncer_proto/blob.{type Part}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId, type Key}
import bouncer_proto/lease.{type BucketRecord, type Claim, type Writer}
import bouncer_proto/worker.{
  type Entry, type Profile, type Roster, type WorkerId,
}
import gleam/erlang/atom.{type Atom}

pub type MoveId {
  MoveId(source: WorkerId, serial: Int)
}

pub type Hello {
  Hello(
    worker: WorkerId,
    profile: Profile,
    protocol: ProtocolRange,
    pinned: List(Int),
  )
}

pub type Beat {
  Beat(
    worker: WorkerId,
    seq: Int,
    sent_mono: Int,
    held_digest: Int,
    roster_hash: Int,
    draining: Bool,
    load: Int,
    unreachable: List(Atom),
  )
}

pub type Refusal {
  RetryAfter(ms: Int)
  Budget
  NotHome(home: Int)
  WrongEpoch(current: Epoch)
  EpochMismatch
  TargetNotEligible
  NoCommonCodec
  Recovering
}

pub type WriteRefusal {
  Superseded(epoch: Epoch, writer: Writer)
  HolderHeard(silent_ms: Int)
  RosterAlive
  StillRecovering
}

pub type MovePhase {
  Admitted
  Streaming
  Installing
  CommitWritten
  Aborting
}

pub type MoveRecord {
  MoveRecord(
    move: MoveId,
    key: Key,
    from_epoch: Epoch,
    to: WorkerId,
    to_epoch: Epoch,
    codec_version: Int,
    phase: MovePhase,
    received_seq: Int,
  )
}

pub type Verdict {
  VerdictCommitted(target_ref: BitArray, epoch: Epoch)
  VerdictAborted
  VerdictUnknown
}

pub type InstallFailure {
  DecodeFailed
  UnsupportedVersion(version: Int)
  TargetDraining
  TargetBusy
}

pub type ToBouncer {
  WorkerHello(hello: Hello)
  WorkerBeat(beat: Beat)
  WorkerClaim(worker: WorkerId, held: List(Claim))
  WorkerBye(worker: WorkerId)
  CloseBucket(
    worker: WorkerId,
    bucket: BucketId,
    epoch: Epoch,
    to: WorkerId,
    ids: List(Int),
  )
  BucketEmptied(worker: WorkerId, bucket: BucketId, epoch: Epoch)
  BeginMove(
    move: MoveId,
    key: Key,
    epoch: Epoch,
    to: WorkerId,
    codec_version: Int,
    bytes_estimate: Int,
  )
  BlobChunk(move: MoveId, part: Part, index: Int, count: Int, bytes: BitArray)
  Forward(move: MoveId, seq: Int, bytes: BitArray)
  SealSource(move: MoveId, last_seq: Int)
  Installed(move: MoveId, target_ref: BitArray)
  InstallRefused(move: MoveId, reason: InstallFailure)
  Applied(move: MoveId, last_seq: Int)
  AskMoveStatus(move: MoveId, reply_to: WorkerId)
  FromPeer(ordinal: Int, message: PeerMessage)
}

pub type PeerMessage {
  PeerHello(ordinal: Int, incarnation: Int, protocol: ProtocolRange)
  Write(
    request: Int,
    bucket: BucketId,
    expect_epoch: Epoch,
    expect_writer: Writer,
    record: BucketRecord,
  )
  WriteAck(request: Int, bucket: BucketId, outcome: Result(Nil, WriteRefusal))
  SyncRequest
  SyncReply(records: List(BucketRecord))
  SyncAnswer(records: List(BucketRecord))
  FreshAnswer(records: List(BucketRecord))
  MirrorMove(record: MoveRecord)
  MirrorChunk(move: MoveId, part: Part, index: Int, count: Int, bytes: BitArray)
  MirrorAck(move: MoveId, part: Part, index: Int)
  DropMove(move: MoveId)
  PeerLeaving(ordinal: Int)
  Handover(records: List(MoveRecord))
}

pub type ToWorker {
  Welcome(
    bouncer: Int,
    incarnation: Int,
    protocol: Int,
    directory: List(Entry),
    records: List(BucketRecord),
    roster: Roster,
  )
  BeatAck(bouncer: Int, seq: Int, matched: Bool, roster: Roster)
  DirectoryDelta(bouncer: Int, changed: List(Entry), removed: List(WorkerId))
  BucketDelta(bouncer: Int, records: List(BucketRecord))
  Granted(bucket: BucketId, epoch: Epoch, pending: List(Int))
  CloseRefused(bucket: BucketId, reason: Refusal)
  BouncerLeaving(ordinal: Int)
}

pub type ToMover {
  MoveAccepted(move: MoveId)
  MoveRefused(move: MoveId, reason: Refusal)
  ChunkAck(move: MoveId, part: Part, index: Int)
  Install(
    move: MoveId,
    key: Key,
    epoch: Epoch,
    codec_version: Int,
    part: Part,
    index: Int,
    count: Int,
    bytes: BitArray,
  )
  Committed(move: MoveId, target_ref: BitArray, epoch: Epoch)
  Release(move: MoveId, seq: Int, bytes: BitArray)
  Seal(move: MoveId, last_seq: Int)
  Abort(move: MoveId)
  Aborted(move: MoveId)
  ResendFrom(move: MoveId, seq: Int)
  MoveStatus(move: MoveId, verdict: Verdict)
  AppliedThrough(move: MoveId, last_seq: Int)
}
