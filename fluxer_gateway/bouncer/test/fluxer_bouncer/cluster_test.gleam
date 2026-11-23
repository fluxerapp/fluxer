// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/blob.{Tail}
import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/lease.{Held, Moving}
import bouncer_proto/message.{
  Abort, Aborted, Applied, AppliedThrough, AskMoveStatus, BeatAck, BeginMove,
  BlobChunk, BucketEmptied, ChunkAck, CloseBucket, CloseRefused, Committed,
  EpochMismatch, Forward, Granted, Install, Installed, MoveAccepted, MoveId,
  MoveStatus, NotHome, Release, ResendFrom, Seal, SealSource, VerdictCommitted,
}
import bouncer_proto/rendezvous
import bouncer_proto/worker.{type Profile, type WorkerId, Profile}
import fluxer_bouncer/config.{Timings}
import fluxer_bouncer/core
import fluxer_bouncer/sim.{type Sim}
import gleam/dict
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/order
import gleam/set

fn ready_cluster(quorum: Int) -> #(Sim, WorkerId) {
  let cluster = sim.new(3, quorum, sim.timings())
  let #(cluster, source) = sim.add_worker(cluster, "s@sim", sim.profile())
  #(sim.advance(cluster, 10_000), source)
}

fn held_count(cluster: Sim, who: WorkerId) -> Int {
  dict.size(sim.worker(cluster, who.node).held)
}

pub fn first_worker_gets_every_bucket_with_one_epoch_test() {
  let #(cluster, source) = ready_cluster(2)
  assert held_count(cluster, source) == 1024
  let bucket = key.bucket_of(Key(0, 7))
  let assert Ok(on_zero) = sim.record(cluster, 0, bucket)
  let assert Ok(on_one) = sim.record(cluster, 1, bucket)
  let assert Ok(on_two) = sim.record(cluster, 2, bucket)
  assert on_zero == on_one
  assert on_one == on_two
  assert on_zero.holder == Some(source)
  assert list.length(core.records(sim.bouncer(cluster, 2))) == 1024
  let cluster = sim.clear(cluster) |> sim.beat(source.node)
  let acks =
    sim.to_worker(cluster, source.node)
    |> list.filter_map(fn(message) {
      case message {
        BeatAck(matched: matched, ..) -> Ok(matched)
        _ -> Error(Nil)
      }
    })
  assert acks == [True, True, True]
}

fn moving_setup() {
  let #(cluster, source) = ready_cluster(2)
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let id = 42
  let entity = Key(0, id)
  let bucket = key.bucket_of(entity)
  let assert Ok(current) = sim.record(cluster, 0, bucket)
  let cluster =
    sim.send_home(
      cluster,
      source.node,
      bucket,
      CloseBucket(source, bucket, current.epoch, target, [id]),
    )
  #(cluster, source, target, entity, bucket, current.epoch)
}

pub fn close_and_move_one_entity_in_order_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let next = epoch.next(at)
  assert list.contains(
    sim.to_worker(cluster, target.node),
    Granted(bucket, next, [entity.id]),
  )
  let move = MoveId(source, 1)
  let send = fn(cluster, message) {
    sim.send_home(cluster, source.node, bucket, message)
  }
  let cluster = send(cluster, BeginMove(move, entity, at, target, 1, 64))
  assert list.contains(sim.to_mover(cluster, source.node), MoveAccepted(move))
  let cluster = send(cluster, BlobChunk(move, Tail, 0, 1, <<1, 2, 3>>))
  assert list.contains(
    sim.to_mover(cluster, source.node),
    ChunkAck(move, Tail, 0),
  )
  assert list.contains(
    sim.to_mover(cluster, target.node),
    Install(move, entity, next, 1, Tail, 0, 1, <<1, 2, 3>>),
  )
  let cluster =
    cluster
    |> send(Forward(move, 1, <<"a">>))
    |> send(Forward(move, 2, <<"b">>))
  assert releases(cluster, target) == []
  let cluster = send(cluster, Installed(move, <<"ref">>))
  assert list.contains(
    sim.to_mover(cluster, source.node),
    Committed(move, <<"ref">>, next),
  )
  assert releases(cluster, target) == [#(1, <<"a">>), #(2, <<"b">>)]
  let cluster =
    cluster
    |> send(Forward(move, 3, <<"c">>))
    |> send(SealSource(move, 3))
  assert releases(cluster, target)
    == [#(1, <<"a">>), #(2, <<"b">>), #(3, <<"c">>)]
  assert list.contains(sim.to_mover(cluster, target.node), Seal(move, 3))
  let cluster =
    cluster
    |> send(Applied(move, 3))
  assert list.contains(
    sim.to_mover(cluster, source.node),
    AppliedThrough(move, 3),
  )
  let cluster = send(cluster, BucketEmptied(source, bucket, at))
  list.each([0, 1, 2], fn(ordinal) {
    let assert Ok(done) = sim.record(cluster, ordinal, bucket)
    assert done.holder == Some(target)
    assert done.epoch == next
    assert done.state == Held
  })
  assert core.moves_in_flight(sim.bouncer(
      cluster,
      sim.home(cluster, source.node, bucket),
    ))
    == 0
}

fn beat_matches(cluster: Sim, who: WorkerId) -> List(Bool) {
  sim.clear(cluster)
  |> sim.beat(who.node)
  |> sim.to_worker(who.node)
  |> list.filter_map(fn(message) {
    case message {
      BeatAck(matched: matched, ..) -> Ok(matched)
      _ -> Error(Nil)
    }
  })
}

pub fn workers_learn_a_bucket_hand_over_before_their_next_beat_test() {
  let #(cluster, source, target, _, bucket, at) = moving_setup()
  assert beat_matches(cluster, source) == [True, True, True]
  assert beat_matches(cluster, target) == [True, True, True]
  let cluster =
    sim.send_home(
      cluster,
      source.node,
      bucket,
      BucketEmptied(source, bucket, at),
    )
  assert beat_matches(cluster, source) == [True, True, True]
  assert beat_matches(cluster, target) == [True, True, True]
}

pub fn welcome_lists_only_confirmed_records_test() {
  let #(cluster, source) = ready_cluster(2)
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let bucket = key.bucket_of(Key(0, 42))
  let home = sim.home(cluster, source.node, bucket)
  let others = list.filter([0, 1, 2], fn(ordinal) { ordinal != home })
  let assert Ok(current) = sim.record(cluster, home, bucket)
  let cluster =
    sim.drop_between(cluster, [home], others)
    |> sim.send(
      source.node,
      home,
      CloseBucket(source, bucket, current.epoch, target, [42]),
    )
  let assert Ok(pending) = sim.record(cluster, home, bucket)
  assert pending.state != Held
  let welcomed =
    sim.clear(cluster)
    |> sim.send(target.node, home, message.WorkerHello(hello_of(target)))
    |> sim.to_worker(target.node)
    |> list.flat_map(fn(found) {
      case found {
        message.Welcome(records: records, ..) -> records
        _ -> []
      }
    })
    |> list.filter(fn(record) { record.bucket == bucket })
  assert !list.contains(welcomed, pending)
  assert welcomed == []
}

fn hello_of(who: WorkerId) -> message.Hello {
  message.Hello(
    worker: who,
    profile: sim.profile(),
    protocol: bouncer_proto.own_range(),
    pinned: [],
  )
}

fn releases(cluster: Sim, target: WorkerId) -> List(#(Int, BitArray)) {
  sim.to_mover(cluster, target.node)
  |> list.filter_map(fn(message) {
    case message {
      Release(_, seq, bytes) -> Ok(#(seq, bytes))
      _ -> Error(Nil)
    }
  })
}

pub fn freeze_bound_aborts_and_leaves_the_buffer_to_the_source_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let send = fn(cluster, message) {
    sim.send_home(cluster, source.node, bucket, message)
  }
  let cluster =
    cluster
    |> send(BeginMove(move, entity, at, target, 1, 64))
    |> send(BlobChunk(move, Tail, 0, 1, <<9>>))
    |> send(Forward(move, 1, <<"a">>))
    |> sim.advance(1500)
  assert !list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  let cluster = sim.advance(cluster, 1000)
  assert list.contains(sim.to_mover(cluster, target.node), Abort(move))
  let to_source = sim.to_mover(cluster, source.node)
  assert list.contains(to_source, Aborted(move))
  assert list.all(to_source, fn(message) {
    case message {
      Release(..) | Committed(..) -> False
      _ -> True
    }
  })
  assert releases(cluster, target) == []
  let assert Ok(after) = sim.record(cluster, 1, bucket)
  assert after.holder == Some(source)
  let assert Moving(pending: pending, ..) = after.state
  assert set.contains(pending, entity.id)
}

pub fn an_abort_whose_write_was_lost_is_written_again_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let others = list.filter([0, 1, 2], fn(ordinal) { ordinal != home })
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.drop_between([home], others)
    |> sim.advance(6000)
  assert !list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  let cluster =
    cluster
    |> sim.pass_between([home], others)
    |> sim.advance(3000)
  assert list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  assert list.contains(sim.to_mover(cluster, target.node), Abort(move))
  assert core.moves_in_flight(sim.bouncer(cluster, home)) == 0
}

pub fn a_stale_move_that_never_committed_is_not_reported_committed_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let others = list.filter([0, 1, 2], fn(ordinal) { ordinal != home })
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.drop_between([home], others)
    |> sim.advance(130_000)
    |> sim.clear
    |> sim.send(source.node, home, AskMoveStatus(move, source))
  assert list.all(sim.to_mover(cluster, source.node), fn(message) {
    case message {
      MoveStatus(_, VerdictCommitted(..)) | Committed(..) -> False
      _ -> True
    }
  })
}

pub fn a_stale_move_whose_abort_cannot_be_written_is_aborted_once_it_can_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let others = list.filter([0, 1, 2], fn(ordinal) { ordinal != home })
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.drop_between([home], others)
    |> sim.advance(130_000)
  assert !list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  let cluster =
    cluster
    |> sim.pass_between([home], others)
    |> sim.advance(5000)
  assert list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  assert list.contains(sim.to_mover(cluster, target.node), Abort(move))
  assert core.moves_in_flight(sim.bouncer(cluster, home)) == 0
}

pub fn home_loss_before_commit_aborts_from_the_successor_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.send(source.node, home, Forward(move, 1, <<"a">>))
    |> sim.crash(home)
  assert list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  assert list.contains(sim.to_mover(cluster, target.node), Abort(move))
  assert releases(cluster, target) == []
}

pub fn home_loss_after_commit_completes_from_the_successor_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.send(source.node, home, Installed(move, <<"ref">>))
    |> sim.clear
    |> sim.crash(home)
  let to_source = sim.to_mover(cluster, source.node)
  assert list.contains(to_source, ResendFrom(move, 1))
  assert list.contains(to_source, Committed(move, <<"ref">>, epoch.next(at)))
  assert !list.contains(to_source, Aborted(move))
  let cluster =
    cluster
    |> sim.send_home(source.node, bucket, Forward(move, 1, <<"a">>))
    |> sim.send_home(source.node, bucket, SealSource(move, 1))
  assert releases(cluster, target) == [#(1, <<"a">>)]
  assert list.contains(sim.to_mover(cluster, target.node), Seal(move, 1))
}

pub fn applied_progress_after_a_home_change_resumes_the_stream_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.send(source.node, home, Installed(move, <<"ref">>))
    |> sim.send(source.node, home, Forward(move, 1, <<"a">>))
    |> sim.send(source.node, home, Forward(move, 2, <<"b">>))
    |> sim.crash(home)
    |> sim.clear
    |> sim.send_home(target.node, bucket, Applied(move, 2))
  assert sim.to_mover(cluster, source.node) == [ResendFrom(move, 3)]
  let cluster =
    cluster
    |> sim.send_home(source.node, bucket, Forward(move, 2, <<"b">>))
    |> sim.send_home(source.node, bucket, Forward(move, 3, <<"c">>))
    |> sim.send_home(source.node, bucket, SealSource(move, 3))
  assert releases(cluster, target) == [#(3, <<"c">>)]
  assert list.contains(sim.to_mover(cluster, target.node), Seal(move, 3))
  let new_home = sim.home(cluster, source.node, bucket)
  let cluster = sim.send_home(cluster, target.node, bucket, Applied(move, 3))
  assert list.contains(
    sim.to_mover(cluster, source.node),
    AppliedThrough(move, 3),
  )
  assert core.moves_in_flight(sim.bouncer(cluster, new_home)) == 0
}

pub fn a_final_applied_before_the_seal_finishes_once_the_seal_arrives_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.send(source.node, home, Installed(move, <<"ref">>))
    |> sim.send(source.node, home, Forward(move, 1, <<"a">>))
    |> sim.send(source.node, home, SealSource(move, 1))
    |> sim.crash(home)
    |> sim.clear
    |> sim.send_home(target.node, bucket, Applied(move, 1))
  let new_home = sim.home(cluster, source.node, bucket)
  assert !list.contains(
    sim.to_mover(cluster, source.node),
    AppliedThrough(move, 1),
  )
  assert core.moves_in_flight(sim.bouncer(cluster, new_home)) == 1
  let cluster =
    cluster
    |> sim.clear
    |> sim.send_home(source.node, bucket, SealSource(move, 1))
  assert sim.to_mover(cluster, source.node) == [AppliedThrough(move, 1)]
  assert core.moves_in_flight(sim.bouncer(cluster, new_home)) == 0
}

pub fn only_the_home_accepts_close_and_wire_epochs_must_match_test() {
  let #(cluster, source) = ready_cluster(2)
  let #(cluster, other_build) =
    sim.add_worker(cluster, "x@sim", Profile(..sim.profile(), wire_epoch: 2))
  let cluster = sim.advance(cluster, 1000)
  let bucket = key.bucket_of(Key(0, 5))
  let assert Ok(current) = sim.record(cluster, 0, bucket)
  let home = sim.home(cluster, source.node, bucket)
  let elsewhere = { home + 1 } % 3
  let close = CloseBucket(source, bucket, current.epoch, other_build, [5])
  let cluster =
    cluster
    |> sim.send(source.node, elsewhere, close)
    |> sim.send(source.node, home, close)
  let refusals = sim.to_worker(cluster, source.node)
  assert list.contains(refusals, CloseRefused(bucket, NotHome(home)))
  assert list.contains(refusals, CloseRefused(bucket, EpochMismatch))
}

pub fn a_bridged_neighbour_epoch_takes_a_close_and_a_wider_gap_is_refused_test() {
  let close_to = fn(target_epoch) {
    let cluster = sim.new(3, 2, sim.timings())
    let #(cluster, source) = sim.add_worker(cluster, "s@sim", bridged(5))
    let cluster = sim.advance(cluster, 10_000)
    let #(cluster, target) =
      sim.add_worker(cluster, "x@sim", bridged(target_epoch))
    let cluster = sim.advance(cluster, 1000)
    let bucket = key.bucket_of(Key(0, 5))
    let assert Ok(current) = sim.record(cluster, 0, bucket)
    let home = sim.home(cluster, source.node, bucket)
    let cluster =
      cluster
      |> sim.clear
      |> sim.send(
        source.node,
        home,
        CloseBucket(source, bucket, current.epoch, target, [5]),
      )
    let refused =
      list.contains(
        sim.to_worker(cluster, source.node),
        CloseRefused(bucket, EpochMismatch),
      )
    let granted =
      list.any(sim.to_worker(cluster, target.node), fn(message) {
        case message {
          Granted(granted, _, _) -> granted == bucket
          _ -> False
        }
      })
    #(refused, granted)
  }
  assert close_to(6) == #(False, True)
  assert close_to(4) == #(False, True)
  assert close_to(7) == #(True, False)
  assert close_to(3) == #(True, False)
}

fn bridged(epoch: Int) -> Profile {
  Profile(..sim.profile(), wire_epoch: epoch)
}

pub fn a_restarted_bouncer_resyncs_every_record_test() {
  let #(cluster, _) = ready_cluster(2)
  let cluster =
    cluster
    |> sim.crash(2)
    |> sim.advance(2000)
    |> sim.restart(2)
    |> sim.advance(4000)
  assert core.ready(sim.bouncer(cluster, 2))
  let sorted = fn(ordinal) {
    core.records(sim.bouncer(cluster, ordinal))
    |> list.sort(fn(a, b) { int.compare(a.bucket.bucket, b.bucket.bucket) })
  }
  assert sorted(2) == sorted(0)
}

pub fn whole_tier_restart_rebuilds_from_claims_without_regrants_test() {
  let #(cluster, source) = ready_cluster(2)
  let before = sim.worker(cluster, source.node).held
  let cluster =
    cluster
    |> sim.crash(0)
    |> sim.crash(1)
    |> sim.crash(2)
    |> sim.restart(0)
    |> sim.restart(1)
    |> sim.restart(2)
    |> sim.clear
    |> sim.hello(source.node)
    |> sim.claim(source.node)
    |> sim.advance(12_000)
  let granted =
    sim.to_worker(cluster, source.node)
    |> list.filter(fn(message) {
      case message {
        Granted(..) -> True
        _ -> False
      }
    })
  assert granted == []
  assert sim.worker(cluster, source.node).held == before
  let bucket = key.bucket_of(Key(0, 3))
  let assert Ok(rebuilt) = sim.record(cluster, 1, bucket)
  assert rebuilt.holder == Some(source)
  assert dict.get(before, bucket) == Ok(rebuilt.epoch)
}

fn first_regrant_ms(cluster: Sim, to: List(WorkerId)) -> Int {
  wait_regrant(cluster, to, 0)
}

fn wait_regrant(cluster: Sim, to: List(WorkerId), waited: Int) -> Int {
  case
    list.any(to, fn(who) { held_count(cluster, who) > 0 }) || waited > 200_000
  {
    True -> waited
    False -> wait_regrant(sim.advance(cluster, 500), to, waited + 500)
  }
}

fn roster_cluster(names: List(String), isolation_cap_ms: Int) {
  let timings = Timings(..sim.timings(), isolation_cap_ms: isolation_cap_ms)
  let cluster = sim.new(3, 2, timings)
  let #(cluster, source) = sim.add_worker(cluster, "s@sim", sim.profile())
  let cluster = sim.advance(cluster, 10_000)
  let #(cluster, others) =
    list.fold(names, #(cluster, []), fn(acc, name) {
      let #(cluster, found) = sim.add_worker(acc.0, name, sim.profile())
      #(cluster, [found, ..acc.1])
    })
  #(sim.advance(cluster, 3000), source, list.reverse(others))
}

pub fn a_crashed_holder_is_regranted_after_the_silence_test() {
  let #(cluster, source, others) = roster_cluster(["t@sim", "u@sim"], 120_000)
  let waited =
    cluster
    |> sim.silence(source.node)
    |> first_regrant_ms(others)
  assert waited >= 7000
  assert waited <= 10_000
}

pub fn writes_refused_by_peers_that_hear_the_holder_are_counted_test() {
  let #(cluster, source, _) = roster_cluster(["t@sim", "u@sim"], 120_000)
  let cluster =
    cluster
    |> sim.deafen(source.node, [0])
    |> sim.advance(20_000)
  let found = core.status(sim.bouncer(cluster, 0)).refusals
  assert found.holder_heard > 0
  assert found.lost > 0
  assert found.superseded == 0
  assert core.status(sim.bouncer(cluster, 1)).refusals == core.no_refusals()
}

pub fn roster_rule_measurement_test() {
  let names = ["t@sim", "u@sim", "v@sim", "w@sim"]
  let mass_crash = fn(cap) {
    let #(cluster, source, others) = roster_cluster(names, cap)
    let assert [t, u, v, w] = others
    cluster
    |> sim.silence(source.node)
    |> sim.silence(t.node)
    |> sim.silence(u.node)
    |> first_regrant_ms([v, w])
  }
  let with_rule = mass_crash(120_000)
  let without_rule = mass_crash(7000)
  assert with_rule >= 120_000
  assert with_rule <= 123_000
  assert without_rule >= 7000
  assert without_rule <= 10_000
  let isolated = fn(cap) {
    let #(cluster, source, others) = roster_cluster(names, cap)
    let assert [t, u, v, w] = others
    cluster
    |> sim.deafen(source.node, [0, 1, 2])
    |> sim.deafen(t.node, [0, 1, 2])
    |> sim.deafen(u.node, [0, 1, 2])
    |> first_regrant_ms([v, w])
  }
  assert isolated(120_000) >= 120_000
  assert isolated(7000) <= 10_000
}

pub fn majority_accept_measurement_test() {
  let doubles = fn(quorum) {
    let cluster =
      sim.new(3, quorum, sim.timings())
      |> sim.advance(sim.timings().claim_window_ms + 200)
      |> sim.split([0], [1, 2])
      |> sim.deafen(atom.create("a@sim"), [1, 2])
      |> sim.deafen(atom.create("b@sim"), [0])
    let #(cluster, a) = sim.add_worker(cluster, "a@sim", sim.profile())
    let #(cluster, b) = sim.add_worker(cluster, "b@sim", sim.profile())
    let cluster = sim.advance(cluster, 12_000)
    let on_a = sim.worker(cluster, a.node).held
    let on_b = sim.worker(cluster, b.node).held
    dict.keys(on_a) |> list.count(dict.has_key(on_b, _))
  }
  assert doubles(1) == 1024
  assert doubles(2) == 0
}

pub fn a_source_lost_mid_bucket_move_hands_the_bucket_to_its_target_test() {
  let #(cluster, source, target, _, bucket, at) = moving_setup()
  let #(cluster, _) = sim.add_worker(cluster, "u@sim", sim.profile())
  let cluster = sim.advance(cluster, 2000)
  let cluster = sim.silence(cluster, source.node) |> sim.advance(5000)
  let assert Ok(waiting) = sim.record(cluster, 0, bucket)
  assert waiting.holder == Some(source)
  let cluster = sim.advance(cluster, 5000)
  list.each([0, 1, 2], fn(ordinal) {
    let assert Ok(regranted) = sim.record(cluster, ordinal, bucket)
    assert regranted.holder == Some(target)
    assert regranted.state == Held
    assert epoch.compare(regranted.epoch, epoch.next(at)) == order.Gt
  })
}

pub fn a_commit_acked_by_a_majority_survives_its_home_dying_unconfirmed_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let assert Ok(homes) = rendezvous.homes(bucket, [0, 1, 2])
  let home = homes.home
  let assert Some(mirror) = homes.successor
  let assert [other] =
    list.filter([0, 1, 2], fn(ordinal) { ordinal != home && ordinal != mirror })
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.lose(fn(from, to, message) {
      from == home
      && {
        to == mirror
        || case message {
          message.SyncReply(..) -> True
          _ -> False
        }
      }
    })
    |> sim.send(source.node, home, Installed(move, <<"ref">>))
  let next = epoch.next(at)
  assert list.contains(
    sim.to_mover(cluster, source.node),
    Committed(move, <<"ref">>, next),
  )
  let cluster =
    cluster
    |> sim.stop_losing
    |> sim.clear
    |> sim.crash(home)
    |> sim.advance(15_000)
    |> sim.restart(home)
    |> sim.advance(15_000)
  assert !list.contains(sim.to_mover(cluster, source.node), Aborted(move))
  assert !list.contains(sim.to_mover(cluster, target.node), Abort(move))
  list.each([home, mirror, other], fn(ordinal) {
    let assert Ok(found) = sim.record(cluster, ordinal, bucket)
    assert found.holder == Some(target)
    assert found.epoch == next
    assert found.state == Held
  })
}

pub fn a_home_cut_off_mid_commit_write_ends_with_one_verdict_after_healing_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_setup()
  let move = MoveId(source, 1)
  let assert Ok(homes) = rendezvous.homes(bucket, [0, 1, 2])
  let home = homes.home
  let others = list.filter([0, 1, 2], fn(ordinal) { ordinal != home })
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.lose(fn(from, to, _message) { to == home && from != home })
    |> sim.send(source.node, home, Installed(move, <<"ref">>))
    |> sim.split([home], others)
    |> sim.stop_losing
    |> sim.advance(5000)
    |> sim.heal([home], others)
    |> sim.advance(30_000)
  let to_source = sim.to_mover(cluster, source.node)
  let to_target = sim.to_mover(cluster, target.node)
  let committed =
    list.any(to_source, fn(found) {
      case found {
        Committed(found_move, ..) -> found_move == move
        _ -> False
      }
    })
  let aborted = list.contains(to_source, Aborted(move))
  assert committed != aborted
  assert !{ committed && list.contains(to_target, Abort(move)) }
  let assert Ok(settled) = sim.record(cluster, home, bucket)
  list.each(others, fn(ordinal) {
    assert sim.record(cluster, ordinal, bucket) == Ok(settled)
  })
  case committed {
    True -> {
      assert settled.holder == Some(target)
      assert settled.state == Held
    }
    False -> {
      assert settled.holder == Some(source)
    }
  }
}
