// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/epoch
import bouncer_proto/key.{type BucketId, Key}
import bouncer_proto/lease.{type BucketRecord, Held}
import bouncer_proto/message.{
  BucketEmptied, CloseBucket, FromPeer, PeerHello, SyncAnswer, SyncReply,
  SyncRequest, WorkerHello,
}
import bouncer_proto/worker.{type WorkerId}
import fluxer_bouncer/config
import fluxer_bouncer/core
import fluxer_bouncer/sim.{type Sim}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/order

fn ready_cluster() -> #(Sim, WorkerId) {
  let cluster = sim.new(3, 2, sim.timings())
  let #(cluster, source) = sim.add_worker(cluster, "s@sim", sim.profile())
  #(sim.advance(cluster, 10_000), source)
}

fn sorted(cluster: Sim, ordinal: Int) -> List(BucketRecord) {
  core.records(sim.bouncer(cluster, ordinal))
  |> list.sort(fn(a, b) {
    case int.compare(a.bucket.ns, b.bucket.ns) {
      order.Eq -> int.compare(a.bucket.bucket, b.bucket.bucket)
      other -> other
    }
  })
}

fn status(cluster: Sim, ordinal: Int) -> core.Status {
  core.status(sim.bouncer(cluster, ordinal))
}

fn restart_unreachable(cluster: Sim, ordinal: Int, others: List(Int)) -> Sim {
  cluster
  |> sim.crash(ordinal)
  |> sim.split([ordinal], others)
  |> sim.restart(ordinal)
}

pub fn a_bouncer_whose_peers_connect_after_the_claim_window_resyncs_test() {
  let #(cluster, _) = ready_cluster()
  let cluster =
    restart_unreachable(cluster, 2, [0, 1])
    |> sim.advance(4000)
  assert core.ready(sim.bouncer(cluster, 2))
  let cluster =
    cluster
    |> sim.heal([2], [0, 1])
    |> sim.advance(1000)
  assert list.length(sorted(cluster, 2)) == 1024
  assert sorted(cluster, 2) == sorted(cluster, 0)
}

pub fn a_fresh_bouncer_is_not_synced_until_the_majority_answered_test() {
  let #(cluster, _) = ready_cluster()
  assert list.all([0, 1, 2], sim.synced(cluster, _))
  let cluster =
    restart_unreachable(cluster, 2, [0, 1])
    |> sim.advance(4000)
  assert core.ready(sim.bouncer(cluster, 2))
  assert !sim.synced(cluster, 2)
  assert status(cluster, 2)
    == core.Status(
      ordinal: 2,
      count: 3,
      quorum: 2,
      synced: False,
      recovering: False,
      leaving: False,
      buckets: 0,
      buckets_synced: 0,
      peers_seen: 0,
      peers_synced: 0,
      workers: 0,
      moves_in_flight: 0,
      refusals: core.no_refusals(),
    )
  assert sim.synced(cluster, 0) && sim.synced(cluster, 1)
  let cluster =
    cluster
    |> sim.heal([2], [0])
    |> sim.advance(1000)
  assert list.length(sorted(cluster, 2)) == 1024
  assert status(cluster, 2).peers_synced == 1
  assert !sim.synced(cluster, 2)
  let cluster =
    cluster
    |> sim.heal([2], [1])
    |> sim.advance(200)
  assert sim.synced(cluster, 2)
  let found = status(cluster, 2)
  assert found.buckets_synced == 1024
  assert found.peers_seen == 2
  assert found.peers_synced == 2
  assert sorted(cluster, 2) == sorted(cluster, 1)
}

pub fn a_fresh_bouncer_is_not_synced_during_the_claim_window_test() {
  let #(cluster, _) = ready_cluster()
  let cluster =
    cluster
    |> sim.crash(2)
    |> sim.restart(2)
  assert status(cluster, 2).peers_synced == 2
  assert !sim.synced(cluster, 2)
  let cluster = sim.advance(cluster, sim.timings().claim_window_ms + 200)
  assert sim.synced(cluster, 2)
}

pub fn a_bouncer_cut_off_from_the_majority_loses_sync_and_catches_up_test() {
  let #(cluster, source) = ready_cluster()
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster =
    sim.advance(cluster, 1000)
    |> sim.split([0], [1, 2])
    |> sim.deafen(source.node, [0])
    |> sim.deafen(target.node, [0])
  assert !sim.synced(cluster, 0)
  assert sim.synced(cluster, 1) && sim.synced(cluster, 2)
  let #(cluster, moved) =
    list.fold(span(1, 20), #(cluster, dict.new()), fn(acc, id) {
      move(acc.0, acc.1, source, target, id)
    })
  assert dict.size(moved) >= 1
  let cluster =
    cluster
    |> sim.hear(source.node, [0])
    |> sim.hear(target.node, [0])
    |> sim.heal([0], [1, 2])
    |> sim.advance(500)
  assert sim.synced(cluster, 0)
  assert sorted(cluster, 0) == sorted(cluster, 1)
  assert_kept(cluster, moved)
}

pub fn a_bouncer_answers_an_old_peer_in_the_old_shape_test() {
  let assert Ok(built) =
    config.build(hostname: "b-0", count: "3", basename: "b", dns_name: "sim")
  let state = core.new(built, 1, 0, 1_000_000)
  let old = bouncer_proto.ProtocolRange(oldest: 1, current: 1)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(1, PeerHello(1, 7, old))), 10)
  let #(state, effects) =
    core.step(state, core.Received(FromPeer(1, SyncRequest)), 20)
  assert effects == [core.ToPeer(1, SyncReply([]))]
  let #(state, _) =
    core.step(
      state,
      core.Received(FromPeer(
        2,
        PeerHello(2, 9, bouncer_proto.ProtocolRange(oldest: 1, current: 2)),
      )),
      30,
    )
  let #(state, effects) =
    core.step(state, core.Received(FromPeer(2, SyncRequest)), 40)
  assert effects == [core.ToPeer(2, SyncAnswer([]))]
  let #(state, _) =
    core.step(state, core.Received(FromPeer(1, SyncReply([]))), 50)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(2, SyncAnswer([]))), 60)
  assert core.status(state).peers_synced == 2
  let #(state, _) = core.step(state, core.Tick, 5000)
  assert core.synced(state)
}

fn span(from: Int, count: Int) -> List(Int) {
  int.range(from: from + count - 1, to: from - 1, with: [], run: fn(acc, id) {
    [id, ..acc]
  })
}

fn assert_kept(cluster: Sim, moved: Dict(BucketId, BucketRecord)) -> Nil {
  dict.each(moved, fn(bucket, committed) {
    list.each([0, 1, 2], fn(ordinal) {
      let assert Ok(found) = sim.record(cluster, ordinal, bucket)
      assert found.holder == committed.holder
      assert found.epoch == committed.epoch
      assert found.state == Held
    })
  })
}

fn move(
  cluster: Sim,
  moved: Dict(BucketId, BucketRecord),
  source: WorkerId,
  target: WorkerId,
  id: Int,
) -> #(Sim, Dict(BucketId, BucketRecord)) {
  let bucket = key.bucket_of(Key(0, id))
  let home = sim.home(cluster, source.node, bucket)
  case dict.has_key(moved, bucket), sim.record(cluster, home, bucket) {
    False, Ok(current) if current.holder == Some(source) -> {
      let cluster =
        cluster
        |> sim.send(
          source.node,
          home,
          CloseBucket(source, bucket, current.epoch, target, [id]),
        )
        |> sim.send(
          source.node,
          home,
          BucketEmptied(source, bucket, current.epoch),
        )
      case sim.record(cluster, home, bucket) {
        Ok(done) ->
          case
            done.holder == Some(target)
            && done.state == Held
            && done.epoch == epoch.next(current.epoch)
          {
            True -> #(cluster, dict.insert(moved, bucket, done))
            False -> #(cluster, moved)
          }
        Error(Nil) -> #(cluster, moved)
      }
    }
    _, _ -> #(cluster, moved)
  }
}

fn moves(
  cluster: Sim,
  moved: Dict(BucketId, BucketRecord),
  source: WorkerId,
  target: WorkerId,
  ids: List(Int),
) -> #(Sim, Dict(BucketId, BucketRecord)) {
  list.fold(ids, #(cluster, moved), fn(acc, id) {
    move(sim.advance(acc.0, 100), acc.1, source, target, id)
  })
}

fn wait_synced(cluster: Sim, ordinal: Int, waited: Int) -> #(Sim, Int) {
  case sim.synced(cluster, ordinal) || waited >= 30_000 {
    True -> #(cluster, waited)
    False -> wait_synced(sim.advance(cluster, 100), ordinal, waited + 100)
  }
}

fn rehello(cluster: Sim, who: WorkerId, ordinal: Int) -> Sim {
  sim.send(
    cluster,
    who.node,
    ordinal,
    WorkerHello(
      message.Hello(
        worker: who,
        profile: sim.profile(),
        protocol: bouncer_proto.own_range(),
        pinned: [],
      ),
    ),
  )
}

fn roll_one(
  acc: #(Sim, Dict(BucketId, BucketRecord), Int),
  ordinal: Int,
  source: WorkerId,
  target: WorkerId,
) -> #(Sim, Dict(BucketId, BucketRecord), Int) {
  let #(cluster, moved, next_id) = acc
  let others = list.filter([0, 1, 2], fn(other) { other != ordinal })
  let ids = fn(from) { span(from, 10) }
  let #(cluster, moved) = moves(cluster, moved, source, target, ids(next_id))
  let cluster =
    cluster
    |> sim.crash(ordinal)
    |> sim.deafen(source.node, [ordinal])
    |> sim.deafen(target.node, [ordinal])
  let #(cluster, moved) =
    moves(cluster, moved, source, target, ids(next_id + 10))
  let cluster =
    cluster
    |> sim.split([ordinal], others)
    |> sim.restart(ordinal)
  let #(cluster, moved) =
    moves(cluster, moved, source, target, ids(next_id + 20))
  let cluster = sim.advance(cluster, sim.timings().claim_window_ms)
  assert core.ready(sim.bouncer(cluster, ordinal))
  assert !sim.synced(cluster, ordinal)
  let cluster =
    cluster
    |> sim.heal([ordinal], others)
    |> sim.hear(source.node, [ordinal])
    |> sim.hear(target.node, [ordinal])
    |> rehello(source, ordinal)
    |> rehello(target, ordinal)
    |> sim.claim(source.node)
    |> sim.claim(target.node)
  let #(cluster, moved) =
    moves(cluster, moved, source, target, ids(next_id + 30))
  let #(cluster, waited) = wait_synced(cluster, ordinal, 0)
  assert waited < 30_000
  assert list.all([0, 1, 2], sim.synced(cluster, _))
  #(cluster, moved, next_id + 40)
}

pub fn a_roll_gated_on_synced_with_live_writes_loses_no_record_test() {
  let #(cluster, source) = ready_cluster()
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let #(cluster, moved, _) =
    list.fold([2, 1, 0], #(cluster, dict.new(), 1), fn(acc, ordinal) {
      roll_one(acc, ordinal, source, target)
    })
  let cluster = sim.advance(cluster, 2000)
  assert dict.size(moved) >= 60
  assert_kept(cluster, moved)
  assert list.length(sorted(cluster, 0)) == 1024
  assert sorted(cluster, 0) == sorted(cluster, 1)
  assert sorted(cluster, 1) == sorted(cluster, 2)
  list.each(sorted(cluster, 0), fn(record) {
    let expected = case dict.has_key(moved, record.bucket) {
      True -> Some(target)
      False -> Some(source)
    }
    assert record.holder == expected
  })
}
