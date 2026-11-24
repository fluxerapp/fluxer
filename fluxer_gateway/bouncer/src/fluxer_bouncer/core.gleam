// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/blob.{type Part, Tail}
import bouncer_proto/codec_range
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId, BucketId}
import bouncer_proto/lease.{
  type BucketRecord, type Writer, BucketRecord, CommitRow, Held, Moving,
}
import bouncer_proto/message.{
  type Beat, type Hello, type MoveId, type MoveRecord, type PeerMessage,
  type Refusal, type ToBouncer, type ToMover, type ToWorker, type Verdict,
  type WriteRefusal, Abort, Aborted, Aborting, Admitted, Applied, AppliedThrough,
  AskMoveStatus, BeatAck, BeginMove, BlobChunk, BouncerLeaving, BucketDelta,
  BucketEmptied, ChunkAck, CloseBucket, CloseRefused, CommitWritten, Committed,
  DirectoryDelta, DropMove, EpochMismatch, Forward, FreshAnswer, FromPeer,
  Granted, Handover, Install, InstallRefused, Installed, Installing, MirrorAck,
  MirrorChunk, MirrorMove, MoveAccepted, MoveRecord, MoveRefused, MoveStatus,
  NoCommonCodec, NotHome, PeerHello, PeerLeaving, Recovering, Release,
  ResendFrom, RetryAfter, Seal, SealSource, Streaming, Superseded, SyncAnswer,
  SyncReply, SyncRequest, TargetNotEligible, VerdictAborted, VerdictCommitted,
  VerdictUnknown, Welcome, WorkerBeat, WorkerBye, WorkerClaim, WorkerHello,
  Write, WriteAck, WrongEpoch,
}
import bouncer_proto/rendezvous
import bouncer_proto/wire_epoch
import bouncer_proto/worker.{type Roster, type WorkerId, Ready}
import fluxer_bouncer/blob_store.{type BlobStore}
import fluxer_bouncer/config.{type Config}
import fluxer_bouncer/directory.{type Directory}
import fluxer_bouncer/freeze_buffer.{type FreezeBuffer}
import fluxer_bouncer/leases.{type HolderView, type Leases, type Proposal}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

const write_attempts = 4

const retry_after_ms = 50

const grants_per_scan = 1024

pub type Input {
  Received(message: ToBouncer)
  NodeUp(node: Atom)
  NodeDown(node: Atom)
  Tick
  Leave
}

pub type Effect {
  ToPeer(ordinal: Int, message: PeerMessage)
  ToWorker(node: Atom, message: ToWorker)
  ToMover(node: Atom, message: ToMover)
}

type MoveRole {
  HomeRole
  MirrorRole(home: Int)
}

type Move {
  Move(
    record: MoveRecord,
    bucket: BucketId,
    role: MoveRole,
    mirror: Option(Int),
    admitted_at: Int,
    frozen_at: Option(Int),
    buffer: FreezeBuffer,
    released: Int,
    seal: Option(Int),
    seal_sent: Bool,
    applied: Option(Int),
    target_ref: Option(BitArray),
    deciding: Bool,
  )
}

type Intent {
  Grant(to: WorkerId, pool: String)
  Close(source: WorkerId, epoch: Epoch, to: WorkerId, ids: List(Int))
  Empty(source: WorkerId, epoch: Epoch)
  CommitEntity(move: MoveId)
  AbortEntity(move: MoveId)
}

type Pending {
  Pending(
    request: Int,
    intent: Intent,
    proposal: Proposal,
    acks: Set(Int),
    refused: Dict(Int, WriteRefusal),
    sent_at: Int,
    attempts: Int,
  )
}

type Failure {
  Rejected(reason: Refusal)
  Lost
}

type Built {
  Propose(record: BucketRecord)
  Unchanged(record: BucketRecord)
  Invalid(reason: Refusal)
}

pub opaque type State {
  State(
    config: Config,
    incarnation: Int,
    started: Int,
    system_at_start: Int,
    peer_nodes: Dict(Atom, Int),
    peers: Dict(Int, Int),
    protocols: Dict(Int, Int),
    leaving_peers: Set(Int),
    asked: Set(Int),
    answered: Set(Int),
    answered_fresh: Set(Int),
    fresh: Bool,
    caught_up: Bool,
    needed: Int,
    leaving: Bool,
    directory: Directory,
    leases: Leases,
    writes: Dict(BucketId, Pending),
    queued: Dict(BucketId, List(Intent)),
    next_request: Int,
    moves: Dict(MoveId, Move),
    verdicts: Dict(MoveId, #(Verdict, Int)),
    blobs: BlobStore,
    buffered: Int,
    rosters: Dict(Int, #(Roster, Int)),
    roster: Roster,
    delta: List(BucketRecord),
    last_scan: Int,
    refusals: Refusals,
  )
}

pub type Refusals {
  Refusals(
    superseded: Int,
    holder_heard: Int,
    roster_alive: Int,
    still_recovering: Int,
    lost: Int,
  )
}

pub fn no_refusals() -> Refusals {
  Refusals(
    superseded: 0,
    holder_heard: 0,
    roster_alive: 0,
    still_recovering: 0,
    lost: 0,
  )
}

fn refused(refusals: Refusals, refusal: WriteRefusal) -> Refusals {
  case refusal {
    Superseded(..) -> Refusals(..refusals, superseded: refusals.superseded + 1)
    message.HolderHeard(..) ->
      Refusals(..refusals, holder_heard: refusals.holder_heard + 1)
    message.RosterAlive ->
      Refusals(..refusals, roster_alive: refusals.roster_alive + 1)
    message.StillRecovering ->
      Refusals(..refusals, still_recovering: refusals.still_recovering + 1)
  }
}

type Step =
  #(State, List(Effect))

pub fn new(
  config: Config,
  incarnation: Int,
  now_ms: Int,
  system_ms: Int,
) -> State {
  let peer_nodes =
    int.range(from: 0, to: config.count, with: [], run: fn(acc, ordinal) {
      case ordinal == config.ordinal {
        True -> acc
        False -> [#(config.peer_node(config, ordinal), ordinal), ..acc]
      }
    })
    |> dict.from_list
  let roster = worker.roster([])
  State(
    config: config,
    incarnation: incarnation,
    started: now_ms,
    system_at_start: system_ms,
    peer_nodes: peer_nodes,
    peers: dict.new(),
    protocols: dict.new(),
    leaving_peers: set.new(),
    asked: set.new(),
    answered: set.new(),
    answered_fresh: set.new(),
    fresh: True,
    caught_up: False,
    needed: answers_needed(config, fresh: True),
    leaving: False,
    directory: directory.new(),
    leases: leases.new(config.ordinal, incarnation),
    writes: dict.new(),
    queued: dict.new(),
    next_request: 1,
    moves: dict.new(),
    verdicts: dict.new(),
    blobs: blob_store.new(config.budgets.blob_bytes),
    buffered: 0,
    rosters: dict.from_list([#(roster.hash, #(roster, now_ms))]),
    roster: roster,
    delta: [],
    last_scan: now_ms,
    refusals: no_refusals(),
  )
}

pub type Status {
  Status(
    ordinal: Int,
    count: Int,
    quorum: Int,
    synced: Bool,
    recovering: Bool,
    leaving: Bool,
    buckets: Int,
    buckets_synced: Int,
    peers_seen: Int,
    peers_synced: Int,
    workers: Int,
    moves_in_flight: Int,
    refusals: Refusals,
  )
}

pub fn ready(state: State) -> Bool {
  leases.ready(state.leases)
}

pub fn synced(state: State) -> Bool {
  writable(state) && !state.leaving && in_quorum(state)
}

fn writable(state: State) -> Bool {
  state.caught_up && leases.ready(state.leases)
}

pub fn status(state: State) -> Status {
  Status(
    ordinal: ordinal(state),
    count: state.config.count,
    quorum: state.config.quorum,
    synced: synced(state),
    recovering: !leases.ready(state.leases),
    leaving: state.leaving,
    buckets: leases.size(state.leases),
    buckets_synced: leases.confirmed_count(state.leases),
    peers_seen: set.size(set.difference(
      set.from_list(dict.keys(state.peers)),
      state.leaving_peers,
    )),
    peers_synced: peers_synced(state),
    workers: list.length(directory.entries(state.directory)),
    moves_in_flight: moves_in_flight(state),
    refusals: state.refusals,
  )
}

fn answers_needed(config: Config, fresh fresh: Bool) -> Int {
  let lost = case fresh {
    True -> 1
    False -> 0
  }
  int.max(0, int.min(config.count - 1, config.count - config.quorum + lost))
}

fn peers_synced(state: State) -> Int {
  set.size(set.difference(state.answered, state.leaving_peers))
}

fn full_answers(state: State) -> Int {
  state.answered
  |> set.difference(state.answered_fresh)
  |> set.difference(state.leaving_peers)
  |> set.size
}

fn fresh_quorum(state: State) -> Bool {
  let fresh_peers =
    state.answered
    |> set.intersection(state.answered_fresh)
    |> set.difference(state.leaving_peers)
    |> set.size
  let unanswered = set.difference(state.asked, state.leaving_peers)
  state.fresh
  && set.is_empty(unanswered)
  && 1 + fresh_peers >= state.config.quorum
}

fn in_quorum(state: State) -> Bool {
  1 + peers_synced(state) >= state.config.quorum
}

fn check_sync(state: State) -> Step {
  case state.caught_up, in_quorum(state) {
    True, True -> #(state, [])
    True, False -> {
      let again = set.to_list(state.answered)
      #(
        State(
          ..state,
          caught_up: False,
          needed: answers_needed(state.config, fresh: False),
          answered: set.new(),
          answered_fresh: set.new(),
          asked: set.union(state.asked, state.answered),
        ),
        list.map(again, fn(peer) { ToPeer(peer, SyncRequest) }),
      )
    }
    False, quorate ->
      case
        quorate
        && leases.ready(state.leases)
        && { full_answers(state) >= state.needed || fresh_quorum(state) }
      {
        False -> #(state, [])
        True -> {
          let told = case state.fresh {
            True ->
              to_peers(state, SyncReply(leases.confirmed_records(state.leases)))
            False -> []
          }
          #(State(..state, caught_up: True, fresh: False), told)
        }
      }
  }
}

fn ask_sync(state: State, peer: Int) -> Step {
  case set.contains(state.answered, peer) || set.contains(state.asked, peer) {
    True -> #(state, [])
    False -> #(State(..state, asked: set.insert(state.asked, peer)), [
      ToPeer(peer, SyncRequest),
    ])
  }
}

fn answer_sync(state: State, peer: Int) -> PeerMessage {
  let records = leases.confirmed_records(state.leases)
  case dict.get(state.protocols, peer) {
    Ok(version)
      if state.fresh && version >= bouncer_proto.fresh_answer_protocol
    -> FreshAnswer(records)
    Ok(version) if version >= bouncer_proto.sync_answer_protocol ->
      SyncAnswer(records)
    _ -> SyncReply(records)
  }
}

fn synced_from(
  state: State,
  peer: Int,
  found: List(BucketRecord),
  fresh fresh: Bool,
) -> Step {
  let known = dict.has_key(state.peers, peer)
  let state =
    State(
      ..state,
      leases: leases.sync(state.leases, found, fate(state, Some(peer))),
      asked: set.delete(state.asked, peer),
      answered: case known {
        True -> set.insert(state.answered, peer)
        False -> state.answered
      },
      answered_fresh: case known && fresh {
        True -> set.insert(state.answered_fresh, peer)
        False -> set.delete(state.answered_fresh, peer)
      },
    )
  check_sync(state)
}

pub fn records(state: State) -> List(BucketRecord) {
  leases.records(state.leases)
}

pub fn record(state: State, bucket: BucketId) -> Result(BucketRecord, Nil) {
  leases.record(state.leases, bucket) |> result.replace_error(Nil)
}

pub fn moves_in_flight(state: State) -> Int {
  dict.size(state.moves)
}

pub fn handed_over(state: State) -> Bool {
  state.leaving
  && dict.is_empty(state.writes)
  && dict.is_empty(state.queued)
  && !list.any(dict.values(state.moves), fn(move) { move.role == HomeRole })
}

fn home_of(state: State, bucket: BucketId) -> Result(Int, Nil) {
  homes_of(state, bucket) |> result.map(fn(homes) { homes.home })
}

pub fn step(state: State, input: Input, now_ms: Int) -> Step {
  case input {
    Received(message) -> receive(state, message, now_ms)
    NodeUp(node) -> node_up(state, node)
    NodeDown(node) -> node_down(state, node, now_ms)
    Tick -> tick(state, now_ms)
    Leave -> leave(state, now_ms)
  }
}

fn then(first: Step, next: fn(State) -> Step) -> Step {
  let #(state, effects) = first
  let #(state, more) = next(state)
  #(state, list.append(effects, more))
}

fn ordinal(state: State) -> Int {
  state.config.ordinal
}

fn live_ordinals(state: State) -> List(Int) {
  let others =
    state.peers
    |> dict.keys
    |> list.filter(fn(peer) { !set.contains(state.leaving_peers, peer) })
  case state.leaving {
    True -> others
    False -> [ordinal(state), ..others]
  }
}

fn homes_of(state: State, bucket: BucketId) -> Result(rendezvous.Homes, Nil) {
  rendezvous.homes(bucket, live_ordinals(state))
}

fn is_home(state: State, bucket: BucketId) -> Bool {
  home_of(state, bucket) == Ok(ordinal(state))
}

fn to_peers(state: State, message: PeerMessage) -> List(Effect) {
  list.map(dict.keys(state.peers), fn(peer) { ToPeer(peer, message) })
}

fn to_workers(state: State, message: ToWorker) -> List(Effect) {
  list.map(directory.entries(state.directory), fn(entry) {
    ToWorker(entry.worker.node, message)
  })
}

fn floor_epoch(state: State, now_ms: Int) -> Epoch {
  epoch.floor_at(state.system_at_start + now_ms - state.started)
}

fn timings(state: State) -> config.Timings {
  state.config.timings
}

fn refresh_roster(state: State, now_ms: Int) -> State {
  let roster = directory.roster(state.directory)
  case roster.hash == state.roster.hash {
    True -> state
    False ->
      State(
        ..state,
        roster: roster,
        rosters: dict.insert(state.rosters, roster.hash, #(roster, now_ms)),
      )
  }
}

fn holder_view(state: State, holder: WorkerId, now_ms: Int) -> HolderView {
  let silent = case directory.silent_ms(state.directory, holder, now_ms) {
    Ok(silent) -> silent
    Error(_) -> now_ms - state.started
  }
  let beating = case
    directory.reported_roster(state.directory, holder)
    |> result.replace_error(Nil)
    |> result.try(dict.get(state.rosters, _))
  {
    Error(Nil) -> False
    Ok(#(roster, _)) -> {
      let alive =
        list.count(roster.members, fn(member) {
          member != holder
          && case directory.silent_ms(state.directory, member, now_ms) {
            Ok(silent) -> silent < timings(state).lease_ttl_ms
            Error(_) -> False
          }
        })
      alive >= worker.majority_of(roster)
    }
  }
  leases.HolderView(
    silent_ms: silent,
    roster_majority_beating: beating,
    said_bye: directory.said_bye(state.directory, holder),
    new_incarnation: directory.superseded(state.directory, holder),
  )
}

fn regrant_check(
  state: State,
  holder: WorkerId,
  now_ms: Int,
) -> Result(Nil, WriteRefusal) {
  leases.regrant_allowed(
    holder_view(state, holder, now_ms),
    timings(state).regrant_silence_ms,
    timings(state).isolation_cap_ms + timings(state).skew_margin_ms,
  )
}

fn receive(state: State, message: ToBouncer, now_ms: Int) -> Step {
  case message {
    WorkerHello(hello) -> on_hello(state, hello, now_ms)
    WorkerBeat(beat) -> on_beat(state, beat, now_ms)
    WorkerClaim(worker, held) -> {
      let pool = case directory.find(state.directory, worker) {
        Ok(entry) -> entry.profile.pool
        Error(_) -> ""
      }
      #(
        State(..state, leases: leases.claim(state.leases, worker, pool, held)),
        [],
      )
    }
    WorkerBye(worker) ->
      case directory.bye(state.directory, worker) {
        Error(_) -> #(state, [])
        Ok(found) -> {
          let state = refresh_roster(State(..state, directory: found), now_ms)
          #(
            state,
            to_workers(state, DirectoryDelta(ordinal(state), [], [worker])),
          )
        }
      }
    CloseBucket(worker, bucket, epoch, to, ids) ->
      case admission(state, bucket) {
        Error(reason) -> #(state, [
          ToWorker(worker.node, CloseRefused(bucket, reason)),
        ])
        Ok(Nil) ->
          request_write(state, bucket, Close(worker, epoch, to, ids), now_ms)
      }
    BucketEmptied(worker, bucket, epoch) ->
      case admission(state, bucket) {
        Error(_) -> #(state, [])
        Ok(Nil) -> request_write(state, bucket, Empty(worker, epoch), now_ms)
      }
    BeginMove(move, key, epoch, to, codec_version, bytes_estimate) ->
      on_begin(
        state,
        MoveRecord(
          move: move,
          key: key,
          from_epoch: epoch,
          to: to,
          to_epoch: epoch,
          codec_version: codec_version,
          phase: Admitted,
          received_seq: 0,
        ),
        bytes_estimate,
        now_ms,
      )
    BlobChunk(move, part, index, count, bytes) ->
      on_chunk(state, move, part, index, count, bytes, now_ms)
    Forward(move, seq, bytes) -> on_forward(state, move, seq, bytes, now_ms)
    SealSource(move, last_seq) -> on_seal(state, move, last_seq, now_ms)
    Installed(move, target_ref) -> on_installed(state, move, target_ref, now_ms)
    InstallRefused(move, _) -> abort_move(state, move, now_ms)
    Applied(move, last_seq) -> on_applied(state, move, last_seq, now_ms)
    AskMoveStatus(move, reply_to) -> on_status(state, move, reply_to)
    FromPeer(peer, message) -> on_peer(state, peer, message, now_ms)
  }
}

fn on_hello(state: State, hello: Hello, now_ms: Int) -> Step {
  case
    directory.hello(state.directory, hello, now_ms),
    bouncer_proto.negotiate(bouncer_proto.own_range(), hello.protocol)
  {
    Ok(#(found, entry)), Ok(protocol) -> {
      let state = refresh_roster(State(..state, directory: found), now_ms)
      let welcome =
        Welcome(
          bouncer: ordinal(state),
          incarnation: state.incarnation,
          protocol: protocol,
          directory: directory.entries(found),
          records: leases.confirmed_records(state.leases),
          roster: state.roster,
        )
      #(state, [
        ToWorker(hello.worker.node, welcome),
        ..to_workers(state, DirectoryDelta(ordinal(state), [entry], []))
      ])
    }
    _, _ -> #(state, [])
  }
}

fn on_beat(state: State, beat: Beat, now_ms: Int) -> Step {
  let before = directory.find(state.directory, beat.worker)
  case directory.beat(state.directory, beat, now_ms) {
    Error(_) -> #(state, [
      ToWorker(
        beat.worker.node,
        BeatAck(ordinal(state), beat.seq, False, state.roster),
      ),
    ])
    Ok(#(found, entry)) -> {
      let changed = case before {
        Ok(previous) -> previous.status != entry.status
        Error(_) -> True
      }
      let state = State(..state, directory: found)
      let state = case changed {
        True -> refresh_roster(state, now_ms)
        False -> state
      }
      let matched =
        writable(state)
        && leases.matches(state.leases, beat.worker, beat.held_digest)
      let ack =
        ToWorker(
          beat.worker.node,
          BeatAck(ordinal(state), beat.seq, matched, state.roster),
        )
      case changed {
        False -> #(state, [ack])
        True -> #(state, [
          ack,
          ..to_workers(state, DirectoryDelta(ordinal(state), [entry], []))
        ])
      }
    }
  }
}

fn admission(state: State, bucket: BucketId) -> Result(Nil, Refusal) {
  case writable(state), homes_of(state, bucket) {
    False, _ -> Error(Recovering)
    True, Error(Nil) -> Error(RetryAfter(retry_after_ms))
    True, Ok(homes) ->
      case homes.home == ordinal(state) {
        True -> Ok(Nil)
        False -> Error(NotHome(homes.home))
      }
  }
}

fn request_write(
  state: State,
  bucket: BucketId,
  intent: Intent,
  now_ms: Int,
) -> Step {
  case dict.has_key(state.writes, bucket) {
    True -> {
      let queue = dict.get(state.queued, bucket) |> result.unwrap([])
      #(
        State(
          ..state,
          queued: dict.insert(
            state.queued,
            bucket,
            list.append(queue, [intent]),
          ),
        ),
        [],
      )
    }
    False -> start_write(state, bucket, intent, now_ms)
  }
}

fn drain(state: State, bucket: BucketId, now_ms: Int) -> Step {
  case dict.has_key(state.writes, bucket), dict.get(state.queued, bucket) {
    False, Ok([next, ..rest]) -> {
      let queued = case rest {
        [] -> dict.delete(state.queued, bucket)
        _ -> dict.insert(state.queued, bucket, rest)
      }
      start_write(State(..state, queued: queued), bucket, next, now_ms)
    }
    _, _ -> #(state, [])
  }
}

fn start_write(
  state: State,
  bucket: BucketId,
  intent: Intent,
  now_ms: Int,
) -> Step {
  case build(state, bucket, intent, now_ms) {
    Unchanged(record) ->
      finish(state, bucket, intent, Ok(record), now_ms)
      |> then(drain(_, bucket, now_ms))
    Invalid(reason) ->
      finish(state, bucket, intent, Error(Rejected(reason)), now_ms)
      |> then(drain(_, bucket, now_ms))
    Propose(record) ->
      case writable(state), leases.propose(state.leases, record) {
        False, _ ->
          finish(state, bucket, intent, Error(Lost), now_ms)
          |> then(drain(_, bucket, now_ms))
        True, Error(_) ->
          finish(state, bucket, intent, Error(Rejected(Recovering)), now_ms)
          |> then(drain(_, bucket, now_ms))
        True, Ok(#(proposed, proposal)) -> {
          let pending =
            Pending(
              request: state.next_request,
              intent: intent,
              proposal: proposal,
              acks: set.from_list([ordinal(state)]),
              refused: dict.new(),
              sent_at: now_ms,
              attempts: 1,
            )
          let state =
            State(
              ..state,
              leases: proposed,
              next_request: state.next_request + 1,
              writes: dict.insert(state.writes, bucket, pending),
            )
          let sends = to_peers(state, write_message(bucket, pending))
          #(state, sends) |> then(settle(_, bucket, pending, now_ms))
        }
      }
  }
}

fn write_message(bucket: BucketId, pending: Pending) -> PeerMessage {
  Write(
    request: pending.request,
    bucket: bucket,
    expect_epoch: pending.proposal.expect_epoch,
    expect_writer: pending.proposal.expect_writer,
    record: pending.proposal.record,
  )
}

fn settle(
  state: State,
  bucket: BucketId,
  pending: Pending,
  now_ms: Int,
) -> Step {
  let quorum = state.config.quorum
  case
    set.size(pending.acks) >= quorum,
    dict.size(pending.refused) > state.config.count - quorum
  {
    True, _ -> commit_write(state, bucket, pending, now_ms)
    False, True -> lose_write(state, bucket, pending, now_ms)
    False, False -> #(
      State(..state, writes: dict.insert(state.writes, bucket, pending)),
      [],
    )
  }
}

fn commit_write(
  state: State,
  bucket: BucketId,
  pending: Pending,
  now_ms: Int,
) -> Step {
  let record = pending.proposal.record
  let state =
    State(
      ..state,
      leases: leases.commit(state.leases, record),
      writes: dict.delete(state.writes, bucket),
      delta: [record, ..state.delta],
    )
  #(state, to_peers(state, SyncReply([record])))
  |> then(flush_delta)
  |> then(finish(_, bucket, pending.intent, Ok(record), now_ms))
  |> then(drain(_, bucket, now_ms))
}

fn lose_write(
  state: State,
  bucket: BucketId,
  pending: Pending,
  now_ms: Int,
) -> Step {
  let state =
    State(
      ..state,
      leases: leases.revert(state.leases, pending.proposal),
      writes: dict.delete(state.writes, bucket),
      refusals: Refusals(..state.refusals, lost: state.refusals.lost + 1),
    )
  let retraction = case
    pending.proposal.previous,
    pending.proposal.previous_confirmed
  {
    Some(previous), True -> to_peers(state, SyncReply([previous]))
    _, _ -> []
  }
  let repairs =
    pending.refused
    |> dict.to_list
    |> list.filter_map(fn(entry) {
      case entry.1 {
        Superseded(..) -> Ok(ToPeer(entry.0, SyncRequest))
        _ -> Error(Nil)
      }
    })
  #(state, list.append(retraction, repairs))
  |> then(finish(_, bucket, pending.intent, Error(Lost), now_ms))
  |> then(drain(_, bucket, now_ms))
}

fn build(state: State, bucket: BucketId, intent: Intent, now_ms: Int) -> Built {
  let current = leases.record(state.leases, bucket)
  case intent, current {
    Grant(to, pool), Error(_) ->
      Propose(BucketRecord(
        bucket: bucket,
        holder: Some(to),
        epoch: floor_epoch(state, now_ms),
        pool: pool,
        state: Held,
        written_by: leases.absent_writer(),
      ))
    Grant(to, pool), Ok(record) -> {
      let free = case record.holder {
        None -> True
        Some(holder) -> regrant_check(state, holder, now_ms) == Ok(Nil)
      }
      case free {
        False -> Invalid(RetryAfter(retry_after_ms))
        True ->
          Propose(
            BucketRecord(
              ..record,
              holder: Some(to),
              epoch: epoch.next(lease.top_epoch(record)),
              pool: pool,
              state: Held,
            ),
          )
      }
    }
    Close(..), Error(_) -> Invalid(WrongEpoch(epoch.zero()))
    Close(source, at, to, ids), Ok(record) ->
      build_close(state, record, source, at, to, ids)
    Empty(..), Error(_) -> Invalid(WrongEpoch(epoch.zero()))
    Empty(source, at), Ok(record) ->
      case record.holder == Some(source) && record.epoch == at, record.state {
        True,
          Moving(to: to, to_epoch: to_epoch, pending: pending, commits: commits)
        ->
          case set.is_empty(pending) {
            True ->
              Propose(
                BucketRecord(
                  ..record,
                  holder: Some(to),
                  epoch: to_epoch,
                  state: Held,
                ),
              )
            False ->
              Propose(
                BucketRecord(
                  ..record,
                  state: Moving(to, to_epoch, set.new(), commits),
                ),
              )
          }
        _, _ -> Invalid(WrongEpoch(record.epoch))
      }
    CommitEntity(move), Ok(record) -> build_commit(state, record, move)
    AbortEntity(move), Ok(record) -> build_abort(state, record, move)
    CommitEntity(_), Error(_) | AbortEntity(_), Error(_) ->
      Invalid(WrongEpoch(epoch.zero()))
  }
}

fn build_close(
  state: State,
  record: BucketRecord,
  source: WorkerId,
  at: Epoch,
  to: WorkerId,
  ids: List(Int),
) -> Built {
  case record.holder == Some(source) && record.epoch == at {
    False -> Invalid(WrongEpoch(record.epoch))
    True ->
      case target_check(state, source, to, record.bucket.ns), record.state {
        Error(reason), _ -> Invalid(reason)
        Ok(Nil), Held ->
          Propose(
            BucketRecord(
              ..record,
              state: Moving(
                to: to,
                to_epoch: epoch.next(record.epoch),
                pending: set.from_list(ids),
                commits: [],
              ),
            ),
          )
        Ok(Nil), Moving(to: current, ..) if current == to -> Unchanged(record)
        Ok(Nil), Moving(to: current, to_epoch: to_epoch, ..) ->
          case directory.find(state.directory, current) {
            Ok(entry) if entry.status == Ready -> Invalid(TargetNotEligible)
            _ ->
              Propose(
                BucketRecord(
                  ..record,
                  state: Moving(
                    to: to,
                    to_epoch: epoch.next(to_epoch),
                    pending: set.from_list(ids),
                    commits: [],
                  ),
                ),
              )
          }
        Ok(Nil), _ -> Invalid(WrongEpoch(record.epoch))
      }
  }
}

fn target_check(
  state: State,
  source: WorkerId,
  to: WorkerId,
  ns: Int,
) -> Result(Nil, Refusal) {
  case
    source == to,
    directory.find(state.directory, source),
    directory.find(state.directory, to)
  {
    False, Ok(from), Ok(target) ->
      case
        target.status == Ready,
        wire_epoch.compatible(
          target.profile.wire_epoch,
          from.profile.wire_epoch,
        ),
        worker.same_build_line(from.profile, target.profile),
        codec_range.can_move(from.profile.codecs, target.profile.codecs, ns)
      {
        False, _, _, _ -> Error(TargetNotEligible)
        True, False, _, _ -> Error(EpochMismatch)
        True, True, False, _ -> Error(TargetNotEligible)
        True, True, True, Error(Nil) -> Error(NoCommonCodec)
        True, True, True, Ok(_) -> Ok(Nil)
      }
    _, _, _ -> Error(TargetNotEligible)
  }
}

fn moving_for(record: BucketRecord, move: Move) -> Bool {
  case record.state {
    Moving(to: to, to_epoch: to_epoch, ..) ->
      record.holder == Some(move.record.move.source)
      && to == move.record.to
      && to_epoch == move.record.to_epoch
    _ -> False
  }
}

fn build_commit(state: State, record: BucketRecord, move_id: MoveId) -> Built {
  case dict.get(state.moves, move_id) {
    Error(Nil) -> Invalid(TargetNotEligible)
    Ok(move) ->
      case moving_for(record, move), record.state, move.target_ref {
        True,
          Moving(to: to, to_epoch: to_epoch, pending: pending, commits: commits),
          Some(target_ref)
        -> {
          let id = move.record.key.id
          case
            list.any(commits, fn(row) { row.id == id }),
            set.contains(pending, id)
          {
            True, _ -> Unchanged(record)
            False, True ->
              Propose(
                BucketRecord(
                  ..record,
                  state: Moving(
                    to: to,
                    to_epoch: to_epoch,
                    pending: set.delete(pending, id),
                    commits: [CommitRow(id, target_ref), ..commits],
                  ),
                ),
              )
            False, False -> Invalid(TargetNotEligible)
          }
        }
        _, _, _ -> Invalid(WrongEpoch(record.epoch))
      }
  }
}

fn build_abort(state: State, record: BucketRecord, move_id: MoveId) -> Built {
  case dict.get(state.moves, move_id) {
    Error(Nil) -> Invalid(TargetNotEligible)
    Ok(move) ->
      case moving_for(record, move), record.state {
        True, Moving(pending: pending, ..) ->
          case set.contains(pending, move.record.key.id) {
            True -> Propose(record)
            False -> Invalid(TargetNotEligible)
          }
        _, _ -> Invalid(WrongEpoch(record.epoch))
      }
  }
}

fn finish(
  state: State,
  bucket: BucketId,
  intent: Intent,
  outcome: Result(BucketRecord, Failure),
  now_ms: Int,
) -> Step {
  case intent, outcome {
    Grant(..), Ok(record) ->
      case record.holder {
        Some(holder) -> #(state, [
          ToWorker(holder.node, Granted(bucket, record.epoch, [])),
        ])
        None -> #(state, [])
      }
    Grant(..), Error(_) -> #(state, [])
    Close(source, at, to, _), Ok(record) ->
      case record.state {
        Moving(to_epoch: to_epoch, pending: pending, ..) -> {
          let granted =
            ToWorker(to.node, Granted(bucket, to_epoch, set.to_list(pending)))
          case set.is_empty(pending) {
            False -> #(state, [granted])
            True ->
              #(state, [granted])
              |> then(request_write(_, bucket, Empty(source, at), now_ms))
          }
        }
        _ -> #(state, [])
      }
    Close(source, ..), Error(Rejected(reason)) -> #(state, [
      ToWorker(source.node, CloseRefused(bucket, reason)),
    ])
    Close(source, ..), Error(Lost) -> #(state, [
      ToWorker(source.node, CloseRefused(bucket, RetryAfter(retry_after_ms))),
    ])
    Empty(source, at), Ok(record) ->
      case record.state {
        Moving(..) -> request_write(state, bucket, Empty(source, at), now_ms)
        _ -> #(state, [])
      }
    Empty(..), Error(_) -> #(state, [])
    CommitEntity(move), Ok(_) -> committed(state, move)
    CommitEntity(move), Error(_) ->
      abort_move(undecide(state, move), move, now_ms)
    AbortEntity(move), Ok(_) -> aborted(state, move, now_ms)
    AbortEntity(move), Error(Lost) -> #(undecide(state, move), [])
    AbortEntity(move), Error(Rejected(_)) ->
      case dict.get(state.moves, move) {
        Error(Nil) -> #(state, [])
        Ok(found) ->
          case commit_row(state, found) {
            Ok(target_ref) ->
              committed(
                put_move(
                  state,
                  Move(
                    ..found,
                    target_ref: option.or(target_ref, found.target_ref),
                  ),
                ),
                move,
              )
            Error(Nil) -> aborted(state, move, now_ms)
          }
      }
  }
}

fn commit_row(state: State, move: Move) -> Result(Option(BitArray), Nil) {
  case leases.record(state.leases, move.bucket) {
    Error(_) -> Error(Nil)
    Ok(record) ->
      case record.state {
        Moving(commits: commits, ..) ->
          commits
          |> list.find(fn(row) { row.id == move.record.key.id })
          |> result.map(fn(row) { Some(row.target_ref) })
        Held ->
          case
            record.holder == Some(move.record.to)
            && record.epoch == move.record.to_epoch
          {
            True -> Ok(None)
            False -> Error(Nil)
          }
        _ -> Error(Nil)
      }
  }
}

fn put_move(state: State, move: Move) -> State {
  State(..state, moves: dict.insert(state.moves, move.record.move, move))
}

fn update_move(
  state: State,
  move_id: MoveId,
  change: fn(Move) -> Move,
) -> State {
  case dict.get(state.moves, move_id) {
    Ok(move) -> put_move(state, change(move))
    Error(Nil) -> state
  }
}

fn undecide(state: State, move_id: MoveId) -> State {
  update_move(state, move_id, fn(move) { Move(..move, deciding: False) })
}

fn with_phase(move: Move, phase: message.MovePhase) -> Move {
  Move(..move, record: MoveRecord(..move.record, phase: phase))
}

fn forget_move(
  state: State,
  move: Move,
  verdict: Verdict,
  now_ms: Int,
) -> State {
  State(
    ..state,
    moves: dict.delete(state.moves, move.record.move),
    blobs: blob_store.drop(state.blobs, move.record.move),
    buffered: state.buffered - freeze_buffer.size_bytes(move.buffer),
    verdicts: dict.insert(state.verdicts, move.record.move, #(verdict, now_ms)),
  )
}

fn mirror_effects(move: Move, message: PeerMessage) -> List(Effect) {
  case move.mirror {
    Some(peer) -> [ToPeer(peer, message)]
    None -> []
  }
}

fn on_begin(
  state: State,
  record: MoveRecord,
  bytes_estimate: Int,
  now_ms: Int,
) -> Step {
  let move_id = record.move
  let source = move_id.source.node
  let refuse = fn(reason) {
    #(state, [ToMover(source, MoveRefused(move_id, reason))])
  }
  let bucket = key.bucket_of(record.key)
  case dict.get(state.moves, move_id) {
    Ok(existing) if existing.role == HomeRole -> #(state, [
      ToMover(source, MoveAccepted(move_id)),
    ])
    _ ->
      case admission(state, bucket) {
        Error(reason) -> refuse(reason)
        Ok(Nil) ->
          case begin_check(state, bucket, record) {
            Error(reason) -> refuse(reason)
            Ok(to_epoch) ->
              case
                state.buffered >= state.config.budgets.buffer_bytes,
                blob_store.admit(state.blobs, move_id, bytes_estimate, now_ms)
              {
                False, Ok(blobs) -> {
                  let mirror = case homes_of(state, bucket) {
                    Ok(homes) -> homes.successor
                    Error(Nil) -> None
                  }
                  let move =
                    Move(
                      record: MoveRecord(..record, to_epoch: to_epoch),
                      bucket: bucket,
                      role: HomeRole,
                      mirror: mirror,
                      admitted_at: now_ms,
                      frozen_at: None,
                      buffer: freeze_buffer.new(
                        state.config.budgets.entity_buffer_bytes,
                      ),
                      released: 0,
                      seal: None,
                      seal_sent: False,
                      applied: None,
                      target_ref: None,
                      deciding: False,
                    )
                  let state = put_move(State(..state, blobs: blobs), move)
                  #(
                    state,
                    list.append(mirror_effects(move, MirrorMove(move.record)), [
                      ToMover(source, MoveAccepted(move_id)),
                    ]),
                  )
                }
                _, _ -> refuse(message.Budget)
              }
          }
      }
  }
}

fn begin_check(
  state: State,
  bucket: BucketId,
  move: MoveRecord,
) -> Result(Epoch, Refusal) {
  case leases.record(state.leases, bucket) {
    Error(_) -> Error(WrongEpoch(epoch.zero()))
    Ok(record) ->
      case
        record.holder == Some(move.move.source)
        && record.epoch == move.from_epoch,
        record.state
      {
        True, Moving(to: to, to_epoch: to_epoch, pending: pending, ..) ->
          case to == move.to && set.contains(pending, move.key.id) {
            True -> Ok(to_epoch)
            False -> Error(TargetNotEligible)
          }
        _, _ -> Error(WrongEpoch(record.epoch))
      }
  }
}

fn home_move(state: State, move_id: MoveId) -> Result(Move, Nil) {
  case dict.get(state.moves, move_id) {
    Ok(move) if move.role == HomeRole -> Ok(move)
    _ -> Error(Nil)
  }
}

fn open(move: Move) -> Bool {
  move.record.phase != CommitWritten && move.record.phase != Aborting
}

fn on_chunk(
  state: State,
  move_id: MoveId,
  part: Part,
  index: Int,
  count: Int,
  bytes: BitArray,
  now_ms: Int,
) -> Step {
  case home_move(state, move_id) {
    Ok(move) ->
      case
        open(move),
        blob_store.put_chunk(state.blobs, move_id, part, index, count, bytes)
      {
        True, Ok(blobs) -> {
          let frozen_at = case part, move.frozen_at {
            Tail, None -> Some(now_ms)
            _, known -> known
          }
          let phase = case blob_store.complete(blobs, move_id, Tail) {
            True -> Installing
            False -> Streaming
          }
          let move = Move(..with_phase(move, phase), frozen_at: frozen_at)
          let state = put_move(State(..state, blobs: blobs), move)
          case move.mirror {
            Some(peer) -> #(state, [
              ToPeer(peer, MirrorChunk(move_id, part, index, count, bytes)),
            ])
            None -> #(state, chunk_safe(move, part, index, count, bytes))
          }
        }
        _, _ -> #(state, [])
      }
    Error(Nil) -> #(state, [])
  }
}

fn chunk_safe(
  move: Move,
  part: Part,
  index: Int,
  count: Int,
  bytes: BitArray,
) -> List(Effect) {
  let record = move.record
  [
    ToMover(record.move.source.node, ChunkAck(record.move, part, index)),
    ToMover(
      record.to.node,
      Install(
        move: record.move,
        key: record.key,
        epoch: record.to_epoch,
        codec_version: record.codec_version,
        part: part,
        index: index,
        count: count,
        bytes: bytes,
      ),
    ),
  ]
}

fn on_forward(
  state: State,
  move_id: MoveId,
  seq: Int,
  bytes: BitArray,
  now_ms: Int,
) -> Step {
  case home_move(state, move_id) {
    Error(Nil) -> #(state, [])
    Ok(move) ->
      case move.record.phase {
        CommitWritten -> pass(state, move, seq, bytes)
        Aborting -> #(state, [])
        _ -> {
          let size = bit_array.byte_size(bytes)
          case
            state.buffered + size > state.config.budgets.buffer_bytes,
            freeze_buffer.push(move.buffer, seq, bytes)
          {
            True, _ | _, Error(freeze_buffer.Overflow(_)) ->
              abort_move(state, move_id, now_ms)
            False, Ok(buffer) -> {
              let move =
                Move(
                  ..move,
                  buffer: buffer,
                  frozen_at: option.or(move.frozen_at, Some(now_ms)),
                )
              #(
                put_move(State(..state, buffered: state.buffered + size), move),
                [],
              )
            }
            False, Error(freeze_buffer.OutOfOrder(expected, got))
              if got > expected
            -> #(state, [
              ToMover(move_id.source.node, ResendFrom(move_id, expected)),
            ])
            False, Error(_) -> #(state, [])
          }
        }
      }
  }
}

fn pass(state: State, move: Move, seq: Int, bytes: BitArray) -> Step {
  let move_id = move.record.move
  case seq <= move.released, seq == move.released + 1 {
    True, _ -> #(state, [])
    False, True -> {
      let move = Move(..move, released: seq)
      let #(move, sealing) = maybe_seal(move)
      #(put_move(state, move), [
        ToMover(move.record.to.node, Release(move_id, seq, bytes)),
        ..sealing
      ])
    }
    False, False -> #(state, [
      ToMover(move_id.source.node, ResendFrom(move_id, move.released + 1)),
    ])
  }
}

fn maybe_seal(move: Move) -> #(Move, List(Effect)) {
  case move.record.phase, move.seal, move.seal_sent {
    CommitWritten, Some(last), False if move.released >= last -> #(
      Move(..move, seal_sent: True),
      [ToMover(move.record.to.node, Seal(move.record.move, last))],
    )
    _, _, _ -> #(move, [])
  }
}

fn on_seal(state: State, move_id: MoveId, last_seq: Int, now_ms: Int) -> Step {
  case home_move(state, move_id) {
    Error(Nil) -> #(state, [])
    Ok(move) -> {
      let buffer = case move.record.phase {
        CommitWritten -> move.buffer
        _ ->
          freeze_buffer.seal(move.buffer, last_seq)
          |> result.unwrap(move.buffer)
      }
      seal_or_finish(
        state,
        Move(..move, buffer: buffer, seal: Some(last_seq)),
        now_ms,
      )
    }
  }
}

fn seal_or_finish(state: State, move: Move, now_ms: Int) -> Step {
  case move.record.phase, move.seal, move.applied {
    CommitWritten, Some(sealed), Some(applied) if applied >= sealed ->
      finish_applied(state, move, sealed, now_ms)
    _, _, _ -> {
      let #(move, effects) = maybe_seal(move)
      #(put_move(state, move), effects)
    }
  }
}

fn finish_applied(state: State, move: Move, sealed: Int, now_ms: Int) -> Step {
  let move_id = move.record.move
  #(
    forget_move(state, move, verdict_of(move), now_ms),
    list.append(mirror_effects(move, DropMove(move_id)), [
      ToMover(move_id.source.node, AppliedThrough(move_id, sealed)),
    ]),
  )
}

fn on_installed(
  state: State,
  move_id: MoveId,
  target_ref: BitArray,
  now_ms: Int,
) -> Step {
  case home_move(state, move_id) {
    Ok(move) ->
      case open(move) && !move.deciding {
        False -> #(state, [])
        True -> {
          let move =
            Move(
              ..with_phase(move, Installing),
              target_ref: Some(target_ref),
              deciding: True,
            )
          request_write(
            put_move(state, move),
            move.bucket,
            CommitEntity(move_id),
            now_ms,
          )
        }
      }
    Error(Nil) -> #(state, [])
  }
}

fn committed(state: State, move_id: MoveId) -> Step {
  case dict.get(state.moves, move_id) {
    Error(Nil) -> #(state, [])
    Ok(move) -> {
      let record = move.record
      let releases =
        freeze_buffer.entries_from(move.buffer, move.released + 1)
        |> list.map(fn(entry) {
          ToMover(record.to.node, Release(record.move, entry.0, entry.1))
        })
      let released = int.max(move.released, freeze_buffer.last_seq(move.buffer))
      let buffer = freeze_buffer.drop_through(move.buffer, released)
      let freed =
        freeze_buffer.size_bytes(move.buffer) - freeze_buffer.size_bytes(buffer)
      let move =
        Move(
          ..with_phase(move, CommitWritten),
          role: HomeRole,
          deciding: False,
          released: released,
          buffer: buffer,
        )
      let #(move, sealing) = maybe_seal(move)
      let state =
        State(
          ..put_move(state, move),
          blobs: blob_store.drop(state.blobs, record.move),
          buffered: state.buffered - freed,
        )
      let notify = case move.target_ref {
        Some(target_ref) -> [
          ToMover(
            record.move.source.node,
            Committed(record.move, target_ref, record.to_epoch),
          ),
        ]
        None -> []
      }
      #(
        state,
        list.flatten([
          mirror_effects(
            move,
            MirrorMove(MoveRecord(..move.record, received_seq: released)),
          ),
          notify,
          releases,
          sealing,
        ]),
      )
    }
  }
}

fn abort_move(state: State, move_id: MoveId, now_ms: Int) -> Step {
  case home_move(state, move_id) {
    Ok(move) ->
      case open(move) && !move.deciding {
        False -> #(state, [])
        True ->
          request_write(
            put_move(state, Move(..with_phase(move, Aborting), deciding: True)),
            move.bucket,
            AbortEntity(move_id),
            now_ms,
          )
      }
    Error(Nil) -> #(state, [])
  }
}

fn aborted(state: State, move_id: MoveId, now_ms: Int) -> Step {
  case dict.get(state.moves, move_id) {
    Error(Nil) -> #(state, [])
    Ok(move) -> {
      #(forget_move(state, move, VerdictAborted, now_ms), [
        ToMover(move.record.to.node, Abort(move_id)),
        ToMover(move_id.source.node, Aborted(move_id)),
        ..mirror_effects(move, DropMove(move_id))
      ])
    }
  }
}

fn on_applied(
  state: State,
  move_id: MoveId,
  last_seq: Int,
  now_ms: Int,
) -> Step {
  case home_move(state, move_id) {
    Ok(move) if move.record.phase == CommitWritten -> {
      let applied = case move.applied {
        Some(seen) -> int.max(seen, last_seq)
        None -> last_seq
      }
      let move =
        Move(
          ..move,
          applied: Some(applied),
          released: int.max(move.released, last_seq),
        )
      case move.seal {
        Some(sealed) if applied >= sealed ->
          finish_applied(state, move, sealed, now_ms)
        _ -> #(put_move(state, move), [
          ToMover(move_id.source.node, ResendFrom(move_id, move.released + 1)),
        ])
      }
    }
    _ -> #(state, [])
  }
}

fn settled_verdict(move: Move) -> Verdict {
  case move.record.phase {
    CommitWritten -> verdict_of(move)
    _ -> VerdictUnknown
  }
}

fn retry_abort(state: State, move: Move, now_ms: Int) -> Step {
  request_write(
    put_move(state, Move(..move, deciding: True)),
    move.bucket,
    AbortEntity(move.record.move),
    now_ms,
  )
}

fn verdict_of(move: Move) -> Verdict {
  VerdictCommitted(option.unwrap(move.target_ref, <<>>), move.record.to_epoch)
}

fn on_status(state: State, move_id: MoveId, reply_to: WorkerId) -> Step {
  let verdict = case dict.get(state.moves, move_id) {
    Ok(move) ->
      case move.record.phase {
        CommitWritten -> Some(verdict_of(move))
        _ -> None
      }
    Error(Nil) ->
      case dict.get(state.verdicts, move_id), writable(state) {
        Ok(#(verdict, _)), _ -> Some(verdict)
        Error(Nil), True -> Some(VerdictUnknown)
        Error(Nil), False -> None
      }
  }
  case verdict {
    Some(verdict) -> #(state, [
      ToMover(reply_to.node, MoveStatus(move_id, verdict)),
    ])
    None -> #(state, [])
  }
}

fn fate(state: State, sender: Option(Int)) -> fn(Writer) -> leases.Fate {
  fn(writer: Writer) {
    let alive = case writer.ordinal == ordinal(state) {
      True -> writer.incarnation == state.incarnation
      False -> dict.get(state.peers, writer.ordinal) == Ok(writer.incarnation)
    }
    case alive, sender == Some(writer.ordinal) {
      False, _ -> leases.Departed
      True, True -> leases.Retracting
      True, False -> leases.Present
    }
  }
}

fn settle_orphans(state: State) -> Step {
  let #(found, settled) = leases.settle_orphans(state.leases, fate(state, None))
  #(State(..state, leases: found, delta: list.append(settled, state.delta)), [])
  |> then(flush_delta)
}

fn on_peer(state: State, peer: Int, message: PeerMessage, now_ms: Int) -> Step {
  case message {
    PeerHello(_, incarnation, protocol) ->
      case bouncer_proto.negotiate(bouncer_proto.own_range(), protocol) {
        Error(Nil) -> #(state, [])
        Ok(version) -> {
          let known = dict.get(state.peers, peer) == Ok(incarnation)
          let state = case known {
            True -> state
            False -> forget_peer(state, peer)
          }
          let state =
            State(
              ..state,
              peers: dict.insert(state.peers, peer, incarnation),
              protocols: dict.insert(state.protocols, peer, version),
              leaving_peers: set.delete(state.leaving_peers, peer),
            )
          let hello = case known {
            True -> []
            False -> [ToPeer(peer, own_hello(state))]
          }
          #(state, hello)
          |> then(settle_orphans)
          |> then(ask_sync(_, peer))
          |> then(check_sync)
        }
      }
    Write(request, bucket, expect_epoch, expect_writer, record) -> {
      let view = case leases.record(state.leases, bucket) {
        Ok(BucketRecord(holder: Some(holder), ..)) ->
          holder_view(state, holder, now_ms)
        _ -> leases.HolderView(0, False, False, False)
      }
      let #(applied, outcome) = case writable(state) {
        False -> #(state.leases, Error(message.StillRecovering))
        True ->
          leases.apply_write(
            state.leases,
            expect_epoch,
            expect_writer,
            record,
            view,
            timings(state).regrant_silence_ms,
            timings(state).isolation_cap_ms + timings(state).skew_margin_ms,
          )
      }
      #(State(..state, leases: applied), [
        ToPeer(peer, WriteAck(request, bucket, outcome)),
      ])
    }
    WriteAck(request, bucket, outcome) ->
      case dict.get(state.writes, bucket) {
        Ok(pending) if pending.request == request -> {
          let pending = case outcome {
            Ok(Nil) -> Pending(..pending, acks: set.insert(pending.acks, peer))
            Error(refusal) ->
              Pending(
                ..pending,
                refused: dict.insert(pending.refused, peer, refusal),
              )
          }
          let state = case outcome {
            Ok(Nil) -> state
            Error(refusal) ->
              State(..state, refusals: refused(state.refusals, refusal))
          }
          settle(state, bucket, pending, now_ms)
        }
        _ -> #(state, [])
      }
    SyncRequest -> #(state, [ToPeer(peer, answer_sync(state, peer))])
    SyncAnswer(found) -> synced_from(state, peer, found, fresh: False)
    FreshAnswer(found) -> synced_from(state, peer, found, fresh: True)
    SyncReply(found) ->
      case set.contains(state.asked, peer), dict.get(state.protocols, peer) {
        True, Ok(version) if version < bouncer_proto.sync_answer_protocol ->
          synced_from(state, peer, found, fresh: False)
        _, _ -> #(
          State(
            ..state,
            leases: leases.sync(state.leases, found, fate(state, Some(peer))),
          ),
          [],
        )
      }
    MirrorMove(record) -> on_mirror_move(state, peer, record, now_ms)
    MirrorChunk(move_id, part, index, count, bytes) ->
      case dict.get(state.moves, move_id) {
        Ok(Move(role: MirrorRole(home), ..)) if home == peer ->
          case
            blob_store.put_chunk(
              state.blobs,
              move_id,
              part,
              index,
              count,
              bytes,
            )
          {
            Ok(blobs) -> #(State(..state, blobs: blobs), [
              ToPeer(peer, MirrorAck(move_id, part, index)),
            ])
            Error(_) -> #(state, [])
          }
        _ -> #(state, [])
      }
    MirrorAck(move_id, part, index) ->
      case home_move(state, move_id) {
        Ok(move) if move.mirror == Some(peer) ->
          case blob_store.chunk(state.blobs, move_id, part, index) {
            Ok(#(bytes, count)) -> #(
              state,
              chunk_safe(move, part, index, count, bytes),
            )
            Error(_) -> #(state, [])
          }
        _ -> #(state, [])
      }
    DropMove(move_id) ->
      case dict.get(state.moves, move_id) {
        Ok(Move(role: MirrorRole(home), ..) as move) if home == peer -> #(
          forget_move(state, move, verdict_of(move), now_ms),
          [],
        )
        _ -> #(state, [])
      }
    PeerLeaving(leaving) ->
      check_sync(
        State(..state, leaving_peers: set.insert(state.leaving_peers, leaving)),
      )
    Handover(records) ->
      list.fold(records, #(state, []), fn(acc, record) {
        then(acc, fn(state) {
          let move = fresh_move(state, record, HomeRole, now_ms)
          take_over(put_move(state, move), move, now_ms)
        })
      })
  }
}

fn own_hello(state: State) -> PeerMessage {
  PeerHello(ordinal(state), state.incarnation, bouncer_proto.own_range())
}

fn fresh_move(
  state: State,
  record: MoveRecord,
  role: MoveRole,
  now_ms: Int,
) -> Move {
  Move(
    record: record,
    bucket: key.bucket_of(record.key),
    role: role,
    mirror: None,
    admitted_at: now_ms,
    frozen_at: None,
    buffer: freeze_buffer.new(state.config.budgets.entity_buffer_bytes),
    released: 0,
    seal: None,
    seal_sent: False,
    applied: None,
    target_ref: None,
    deciding: False,
  )
}

fn on_mirror_move(
  state: State,
  peer: Int,
  record: MoveRecord,
  now_ms: Int,
) -> Step {
  case dict.get(state.moves, record.move) {
    Ok(Move(role: MirrorRole(home), ..) as move) if home == peer -> {
      let blobs = case record.phase {
        CommitWritten -> blob_store.drop(state.blobs, record.move)
        _ -> state.blobs
      }
      #(
        put_move(State(..state, blobs: blobs), Move(..move, record: record)),
        [],
      )
    }
    Ok(_) -> #(state, [])
    Error(Nil) ->
      case blob_store.admit(state.blobs, record.move, 0, now_ms) {
        Ok(blobs) -> #(
          put_move(
            State(..state, blobs: blobs),
            fresh_move(state, record, MirrorRole(peer), now_ms),
          ),
          [],
        )
        Error(_) -> #(state, [])
      }
  }
}

fn take_over(state: State, move: Move, now_ms: Int) -> Step {
  let move =
    Move(
      ..move,
      role: HomeRole,
      mirror: None,
      deciding: False,
      frozen_at: option.or(move.frozen_at, Some(now_ms)),
    )
  let state = put_move(state, move)
  let resend = [
    ToMover(move.record.move.source.node, ResendFrom(move.record.move, 1)),
  ]
  case move.record.phase, commit_row(state, move) {
    CommitWritten, found -> {
      let target_ref = case found {
        Ok(Some(target_ref)) -> Some(target_ref)
        _ -> move.target_ref
      }
      let state =
        put_move(state, Move(..move, target_ref: target_ref, released: 0))
      #(state, list.append(committed_notice(move, target_ref), resend))
    }
    _, Ok(target_ref) -> {
      let move =
        Move(
          ..with_phase(move, CommitWritten),
          target_ref: target_ref,
          released: 0,
        )
      #(
        put_move(state, move),
        list.append(committed_notice(move, target_ref), resend),
      )
    }
    _, Error(Nil) -> abort_move(state, move.record.move, now_ms)
  }
}

fn committed_notice(move: Move, target_ref: Option(BitArray)) -> List(Effect) {
  case target_ref {
    Some(target_ref) -> [
      ToMover(
        move.record.move.source.node,
        Committed(move.record.move, target_ref, move.record.to_epoch),
      ),
    ]
    None -> []
  }
}

fn node_up(state: State, node: Atom) -> Step {
  case dict.get(state.peer_nodes, node) {
    Ok(peer) -> #(state, [ToPeer(peer, own_hello(state))])
    Error(Nil) -> #(state, [])
  }
}

fn node_down(state: State, node: Atom, now_ms: Int) -> Step {
  case dict.get(state.peer_nodes, node) {
    Ok(peer) ->
      forget_peer(state, peer)
      |> settle_orphans
      |> then(check_sync)
      |> then(
        each_move(
          _,
          fn(move) { move.role == MirrorRole(peer) },
          fn(state, move) { take_over(state, move, now_ms) },
        ),
      )
    Error(Nil) ->
      each_move(state, fn(move) { move.role == HomeRole }, fn(state, move) {
        worker_lost(state, move, node, now_ms)
      })
  }
}

fn forget_peer(state: State, peer: Int) -> State {
  State(
    ..state,
    peers: dict.delete(state.peers, peer),
    protocols: dict.delete(state.protocols, peer),
    leaving_peers: set.delete(state.leaving_peers, peer),
    asked: set.delete(state.asked, peer),
    answered: set.delete(state.answered, peer),
    answered_fresh: set.delete(state.answered_fresh, peer),
  )
}

fn worker_lost(state: State, move: Move, node: Atom, now_ms: Int) -> Step {
  let move_id = move.record.move
  let committed = move.record.phase == CommitWritten
  case move.record.to.node == node, move_id.source.node == node, committed {
    True, _, True -> #(forget_move(state, move, verdict_of(move), now_ms), [])
    True, _, False -> abort_move(state, move_id, now_ms)
    False, True, True ->
      seal_or_finish(
        state,
        Move(..move, seal: Some(option.unwrap(move.seal, move.released))),
        now_ms,
      )
    False, True, False ->
      case blob_store.complete(state.blobs, move_id, Tail) {
        True -> {
          let last = freeze_buffer.last_seq(move.buffer)
          let buffer =
            freeze_buffer.seal(move.buffer, last) |> result.unwrap(move.buffer)
          #(put_move(state, Move(..move, buffer: buffer, seal: Some(last))), [])
        }
        False -> abort_move(state, move_id, now_ms)
      }
    False, False, _ -> #(state, [])
  }
}

fn leave(state: State, now_ms: Int) -> Step {
  let notices =
    list.append(
      to_peers(state, PeerLeaving(ordinal(state))),
      to_workers(state, BouncerLeaving(ordinal(state))),
    )
  let state = State(..state, leaving: True)
  let homed =
    state.moves
    |> dict.values
    |> list.filter(fn(move) { move.role == HomeRole })
  let #(done, open_moves) =
    list.partition(homed, fn(move) { move.record.phase == CommitWritten })
  let handovers =
    done
    |> list.fold(dict.new(), fn(groups, move) {
      case homes_of(state, move.bucket) {
        Ok(homes) ->
          dict.upsert(groups, homes.home, fn(found) {
            [move.record, ..option.unwrap(found, [])]
          })
        Error(Nil) -> groups
      }
    })
    |> dict.to_list
    |> list.map(fn(group) { ToPeer(group.0, Handover(group.1)) })
  let state =
    list.fold(done, state, fn(acc, move) {
      State(..acc, moves: dict.delete(acc.moves, move.record.move))
    })
  list.fold(
    open_moves,
    #(state, list.append(notices, handovers)),
    fn(acc, move) { then(acc, abort_move(_, move.record.move, now_ms)) },
  )
}

fn tick(state: State, now_ms: Int) -> Step {
  let state = case
    leases.ready(state.leases),
    now_ms - state.started >= timings(state).claim_window_ms
  {
    False, True -> State(..state, leases: leases.finish_recovery(state.leases))
    _, _ -> state
  }
  check_sync(state)
  |> then(retry_writes(_, now_ms))
  |> then(expire_workers(_, now_ms))
  |> then(move_timers(_, now_ms))
  |> then(scan(_, now_ms))
  |> then(flush_delta)
}

fn retry_writes(state: State, now_ms: Int) -> Step {
  state.writes
  |> dict.to_list
  |> list.fold(#(state, []), fn(acc, entry) {
    let #(bucket, pending) = entry
    case now_ms - pending.sent_at >= timings(state).request_timeout_ms {
      False -> acc
      True ->
        then(acc, fn(state) {
          case pending.attempts >= write_attempts {
            True -> lose_write(state, bucket, pending, now_ms)
            False -> {
              let pending =
                Pending(
                  ..pending,
                  attempts: pending.attempts + 1,
                  sent_at: now_ms,
                )
              let silent =
                state.peers
                |> dict.keys
                |> list.filter(fn(peer) {
                  !set.contains(pending.acks, peer)
                  && !dict.has_key(pending.refused, peer)
                })
              #(
                State(
                  ..state,
                  writes: dict.insert(state.writes, bucket, pending),
                ),
                list.map(silent, fn(peer) {
                  ToPeer(peer, write_message(bucket, pending))
                }),
              )
            }
          }
        })
    }
  })
}

fn expire_workers(state: State, now_ms: Int) -> Step {
  let #(found, gone) =
    directory.expire(state.directory, now_ms, timings(state).regrant_silence_ms)
  let found =
    directory.prune(
      found,
      now_ms,
      2
        * {
        timings(state).isolation_cap_ms + timings(state).regrant_silence_ms
      },
    )
  let rosters =
    dict.filter(state.rosters, fn(hash, entry) {
      hash == state.roster.hash
      || now_ms - entry.1 <= timings(state).isolation_cap_ms
    })
  let verdicts =
    dict.filter(state.verdicts, fn(_, entry) {
      now_ms - entry.1 <= timings(state).blob_ttl_ms
    })
  let state =
    refresh_roster(
      State(..state, directory: found, rosters: rosters, verdicts: verdicts),
      now_ms,
    )
  case gone {
    [] -> #(state, [])
    _ -> #(state, to_workers(state, DirectoryDelta(ordinal(state), [], gone)))
  }
}

fn move_timers(state: State, now_ms: Int) -> Step {
  let #(blobs, expired) =
    blob_store.expire(state.blobs, now_ms, timings(state).blob_ttl_ms)
  let state = State(..state, blobs: blobs)
  let after_ttl =
    list.fold(expired, #(state, []), fn(acc, move_id) {
      then(acc, fn(state) {
        case dict.get(state.moves, move_id) {
          Ok(Move(role: MirrorRole(_), ..) as move) -> #(
            forget_move(state, move, VerdictUnknown, now_ms),
            [],
          )
          Ok(_) -> abort_move(state, move_id, now_ms)
          Error(Nil) -> #(state, [])
        }
      })
    })
  use state <- then(after_ttl)
  each_move(state, fn(_) { True }, fn(state, move) {
    let age = now_ms - move.admitted_at
    let stale = age > 2 * timings(state).blob_ttl_ms
    let abandoned = age > 4 * timings(state).blob_ttl_ms
    let committed = move.record.phase == CommitWritten
    let frozen_too_long = case move.frozen_at {
      Some(at) -> now_ms - at > timings(state).freeze_bound_ms
      None -> False
    }
    case move.role, open(move) {
      HomeRole, _ if stale && committed || abandoned -> #(
        forget_move(state, move, settled_verdict(move), now_ms),
        [],
      )
      MirrorRole(_), _ if stale -> #(
        forget_move(state, move, settled_verdict(move), now_ms),
        [],
      )
      HomeRole, True if frozen_too_long || stale ->
        abort_move(state, move.record.move, now_ms)
      HomeRole, False if move.record.phase == Aborting && !move.deciding ->
        retry_abort(state, move, now_ms)
      _, _ -> #(state, [])
    }
  })
}

fn each_move(
  state: State,
  keep: fn(Move) -> Bool,
  run: fn(State, Move) -> Step,
) -> Step {
  state.moves
  |> dict.keys
  |> list.fold(#(state, []), fn(acc, move_id) {
    then(acc, fn(state) {
      case dict.get(state.moves, move_id) {
        Ok(move) ->
          case keep(move) {
            True -> run(state, move)
            False -> #(state, [])
          }
        Error(Nil) -> #(state, [])
      }
    })
  })
}

fn scan(state: State, now_ms: Int) -> Step {
  case
    writable(state)
    && !state.leaving
    && now_ms - state.last_scan >= timings(state).beat_interval_ms
  {
    False -> #(state, [])
    True -> {
      let state = State(..state, last_scan: now_ms)
      let wanted =
        directory.namespaces(state.directory)
        |> list.flat_map(fn(space) {
          int.range(from: 0, to: space.1, with: [], run: fn(acc, index) {
            [BucketId(space.0, index), ..acc]
          })
        })
        |> list.filter(fn(bucket) {
          !dict.has_key(state.writes, bucket) && is_home(state, bucket)
        })
        |> list.filter_map(fn(bucket) {
          decide(state, bucket, now_ms)
          |> result.map(fn(intent) { #(bucket, intent) })
        })
        |> list.take(grants_per_scan)
      list.fold(wanted, #(state, []), fn(acc, entry) {
        then(acc, request_write(_, entry.0, entry.1, now_ms))
      })
    }
  }
}

fn decide(state: State, bucket: BucketId, now_ms: Int) -> Result(Intent, Nil) {
  case leases.record(state.leases, bucket) {
    Error(_) ->
      case now_ms - state.started >= timings(state).regrant_silence_ms {
        True -> pick(state, bucket, None)
        False -> Error(Nil)
      }
    Ok(record) ->
      case record.holder, record.state {
        Some(holder), Moving(pending: pending, ..) ->
          case set.is_empty(pending) {
            True -> Ok(Empty(holder, record.epoch))
            False -> regrant(state, bucket, record, holder, now_ms)
          }
        Some(holder), _ -> regrant(state, bucket, record, holder, now_ms)
        None, _ -> pick(state, bucket, None)
      }
  }
}

fn regrant(
  state: State,
  bucket: BucketId,
  record: BucketRecord,
  holder: WorkerId,
  now_ms: Int,
) -> Result(Intent, Nil) {
  case regrant_check(state, holder, now_ms) {
    Error(_) -> Error(Nil)
    Ok(Nil) ->
      case record.state {
        Moving(to: to, ..) ->
          case directory.find(state.directory, to) {
            Ok(entry) if entry.status == Ready ->
              Ok(Grant(to, entry.profile.pool))
            _ -> pick(state, bucket, Some(holder))
          }
        _ -> pick(state, bucket, Some(holder))
      }
  }
}

fn pick(
  state: State,
  bucket: BucketId,
  exclude: Option(WorkerId),
) -> Result(Intent, Nil) {
  let candidates =
    directory.candidates(state.directory, bucket)
    |> list.filter(fn(entry) { Some(entry.worker) != exclude })
  use node <- result.try(rendezvous.select(
    bucket.ns * key.bucket_count + bucket.bucket,
    list.map(candidates, fn(entry) { entry.worker.node }),
  ))
  use entry <- result.map(
    list.find(candidates, fn(entry) { entry.worker.node == node }),
  )
  Grant(entry.worker, entry.profile.pool)
}

fn flush_delta(state: State) -> Step {
  case state.delta {
    [] -> #(state, [])
    changed -> {
      let state = State(..state, delta: [])
      #(
        state,
        to_workers(state, BucketDelta(ordinal(state), list.reverse(changed))),
      )
    }
  }
}
