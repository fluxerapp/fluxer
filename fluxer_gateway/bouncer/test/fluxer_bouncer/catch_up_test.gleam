// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{BucketId}
import bouncer_proto/lease.{BucketRecord, Held, Writer}
import bouncer_proto/message.{
  AskMoveStatus, BeatAck, BucketEmptied, CloseBucket, CloseRefused, FromPeer,
  MoveId, MoveStatus, Recovering, StillRecovering, Superseded, VerdictUnknown,
  Write, WriteAck,
}
import bouncer_proto/rendezvous
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_bouncer/core
import fluxer_bouncer/leases
import fluxer_bouncer/sim.{type Sim}
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result

fn ready_cluster() -> #(Sim, WorkerId) {
  let cluster = sim.new(3, 2, sim.timings())
  let #(cluster, source) = sim.add_worker(cluster, "s@sim", sim.profile())
  #(sim.advance(cluster, 10_000), source)
}

fn restarted_alone(cluster: Sim) -> Sim {
  cluster
  |> sim.crash(2)
  |> sim.split([2], [0, 1])
  |> sim.restart(2)
  |> sim.advance(sim.timings().claim_window_ms + 1000)
}

fn restarted_with_one_answer(cluster: Sim) -> Sim {
  let cluster =
    restarted_alone(cluster)
    |> sim.heal([2], [0])
    |> sim.advance(200)
  assert core.status(sim.bouncer(cluster, 2)).peers_synced == 1
  cluster
}

fn step(cluster: Sim, ordinal: Int, input: core.Input) -> List(core.Effect) {
  let #(_, effects) = core.step(sim.bouncer(cluster, ordinal), input, 0)
  effects
}

fn fresh_record() -> lease.BucketRecord {
  BucketRecord(
    bucket: BucketId(0, 7),
    holder: Some(WorkerId(node: atom.create("x@sim"), incarnation: 1)),
    epoch: epoch.next(epoch.zero()),
    pool: "default",
    state: Held,
    written_by: Writer(ordinal: 0, incarnation: 9, seq: 1),
  )
}

fn peer_write(from: Int) -> core.Input {
  let record = fresh_record()
  core.Received(FromPeer(
    from,
    Write(
      request: 1,
      bucket: record.bucket,
      expect_epoch: epoch.zero(),
      expect_writer: leases.absent_writer(),
      record: record,
    ),
  ))
}

fn acks_from(cluster: Sim, node: atom.Atom, ordinal: Int) -> List(Bool) {
  sim.to_worker(cluster, node)
  |> list.filter_map(fn(message) {
    case message {
      BeatAck(bouncer: bouncer, matched: matched, ..) if bouncer == ordinal ->
        Ok(matched)
      _ -> Error(Nil)
    }
  })
}

pub fn a_restarted_bouncer_refuses_peer_writes_until_it_caught_up_test() {
  let #(cluster, _) = ready_cluster()
  let cluster = restarted_alone(cluster)
  assert core.ready(sim.bouncer(cluster, 2))
  assert !sim.synced(cluster, 2)
  assert step(cluster, 2, peer_write(0))
    == [core.ToPeer(0, WriteAck(1, BucketId(0, 7), Error(StillRecovering)))]
  let cluster = restarted_with_one_answer(cluster)
  assert !sim.synced(cluster, 2)
  assert step(cluster, 2, peer_write(0))
    == [core.ToPeer(0, WriteAck(1, BucketId(0, 7), Error(StillRecovering)))]
  let cluster =
    cluster
    |> sim.heal([2], [1])
    |> sim.advance(200)
  assert sim.synced(cluster, 2)
  let assert Ok(known) = sim.record(cluster, 2, BucketId(0, 7))
  assert step(cluster, 2, peer_write(0))
    == [
      core.ToPeer(
        0,
        WriteAck(
          1,
          BucketId(0, 7),
          Error(Superseded(known.epoch, known.written_by)),
        ),
      ),
    ]
}

pub fn a_restarted_bouncer_does_not_take_worker_writes_until_it_caught_up_test() {
  let #(cluster, source) = ready_cluster()
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster =
    restarted_with_one_answer(cluster)
    |> sim.hello(source.node)
    |> sim.hello(target.node)
    |> sim.beat(target.node)
  let assert Ok(bucket) =
    int.range(from: 0, to: 1024, with: [], run: fn(acc, index) {
      [BucketId(0, index), ..acc]
    })
    |> list.find(fn(bucket) {
      rendezvous.homes(bucket, [0, 2]) |> result.map(fn(homes) { homes.home })
      == Ok(2)
    })
  let assert Ok(current) = sim.record(cluster, 2, bucket)
  assert current.holder == Some(source)
  let close =
    core.Received(CloseBucket(source, bucket, current.epoch, target, []))
  assert step(cluster, 2, close)
    == [core.ToWorker(source.node, CloseRefused(bucket, Recovering))]
  let emptied = core.Received(BucketEmptied(source, bucket, current.epoch))
  assert step(cluster, 2, emptied) == []
}

pub fn a_restarted_bouncer_does_not_match_beats_until_it_caught_up_test() {
  let #(cluster, source) = ready_cluster()
  let cluster =
    restarted_alone(cluster)
    |> sim.hello(source.node)
    |> sim.claim(source.node)
    |> sim.clear
    |> sim.beat(source.node)
  assert acks_from(cluster, source.node, 2) == [False]
  assert acks_from(cluster, source.node, 0) == [True]
  let cluster =
    cluster
    |> sim.heal([2], [0, 1])
    |> sim.advance(200)
    |> sim.clear
    |> sim.beat(source.node)
  assert sim.synced(cluster, 2)
  assert acks_from(cluster, source.node, 2) == [True]
}

pub fn a_restarted_bouncer_does_not_answer_an_unknown_verdict_until_it_caught_up_test() {
  let #(cluster, source) = ready_cluster()
  let ask = core.Received(AskMoveStatus(MoveId(source, 41), source))
  let cluster = restarted_alone(cluster)
  assert step(cluster, 2, ask) == []
  let cluster =
    cluster
    |> sim.heal([2], [0, 1])
    |> sim.advance(200)
  assert step(cluster, 2, ask)
    == [
      core.ToMover(source.node, MoveStatus(MoveId(source, 41), VerdictUnknown)),
    ]
}
