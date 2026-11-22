// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/epoch
import bouncer_proto/key.{type BucketId, Key}
import bouncer_proto/lease.{type BucketRecord, Held}
import bouncer_proto/message.{
  type PeerMessage, BeatAck, BucketEmptied, CloseBucket, FreshAnswer, FromPeer,
  Granted, PeerHello, SyncAnswer, SyncReply, SyncRequest,
}
import bouncer_proto/worker.{type WorkerId}
import fluxer_bouncer/config
import fluxer_bouncer/core
import fluxer_bouncer/sim.{type Sim}
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/order
import gleam/result

fn ready_cluster(count: Int, quorum: Int) -> #(Sim, WorkerId) {
  let cluster = sim.new(count, quorum, sim.timings())
  let #(cluster, source) = sim.add_worker(cluster, "s@sim", sim.profile())
  #(sim.advance(cluster, 10_000), source)
}

fn past_claim_window(cluster: Sim) -> Sim {
  sim.advance(cluster, sim.timings().claim_window_ms + 1000)
}

fn acks_from(cluster: Sim, node: Atom, ordinal: Int) -> List(Bool) {
  sim.to_worker(cluster, node)
  |> list.filter_map(fn(message) {
    case message {
      BeatAck(bouncer: bouncer, matched: matched, ..) if bouncer == ordinal ->
        Ok(matched)
      _ -> Error(Nil)
    }
  })
}

fn grants(cluster: Sim, node: Atom) -> Int {
  sim.to_worker(cluster, node)
  |> list.count(fn(message) {
    case message {
      Granted(..) -> True
      _ -> False
    }
  })
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

fn rejoin(cluster: Sim, who: WorkerId) -> Sim {
  cluster
  |> sim.hello(who.node)
  |> sim.claim(who.node)
}

fn move_where(
  cluster: Sim,
  source: WorkerId,
  target: WorkerId,
  homes: List(Int),
  wanted: Int,
) -> #(Sim, Dict(BucketId, BucketRecord)) {
  int.range(from: 0, to: 400, with: [], run: fn(acc, id) { [id, ..acc] })
  |> list.reverse
  |> list.fold(#(cluster, dict.new()), fn(acc, id) {
    let #(cluster, moved) = acc
    let bucket = key.bucket_of(Key(0, id))
    let home = sim.home(cluster, source.node, bucket)
    case
      dict.size(moved) < wanted
      && list.contains(homes, home)
      && !dict.has_key(moved, bucket),
      sim.record(cluster, home, bucket)
    {
      True, Ok(current) if current.holder == Some(source) -> {
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
        let next = epoch.next(current.epoch)
        case sim.record(cluster, home, bucket) {
          Ok(done)
            if done.holder == Some(target)
            && done.state == Held
            && done.epoch == next
          -> #(cluster, dict.insert(moved, bucket, done))
          _ -> #(cluster, moved)
        }
      }
      _, _ -> #(cluster, moved)
    }
  })
}

pub fn a_full_restart_with_one_bouncer_missing_recovers_within_the_grace_test() {
  let #(cluster, source) = ready_cluster(3, 2)
  let before = sim.worker(cluster, source.node).held
  let cluster =
    cluster
    |> sim.crash(0)
    |> sim.crash(1)
    |> sim.crash(2)
    |> sim.restart(0)
    |> sim.restart(1)
    |> sim.clear
    |> rejoin(source)
    |> past_claim_window
  assert sim.synced(cluster, 0)
  assert sim.synced(cluster, 1)
  let cluster = cluster |> sim.clear |> sim.beat(source.node)
  assert acks_from(cluster, source.node, 0) == [True]
  assert acks_from(cluster, source.node, 1) == [True]
  let cluster = sim.advance(cluster, 12_000)
  assert grants(cluster, source.node) == 0
  assert sim.worker(cluster, source.node).held == before
  let bucket = key.bucket_of(Key(0, 3))
  let assert Ok(rebuilt) = sim.record(cluster, 0, bucket)
  assert rebuilt.holder == Some(source)
  assert dict.get(before, bucket) == Ok(rebuilt.epoch)
}

pub fn a_bouncer_restarted_alone_does_not_take_a_caught_up_restarted_peer_for_fresh_test() {
  let #(cluster, _) = ready_cluster(3, 2)
  let cluster =
    cluster
    |> sim.crash(2)
    |> sim.restart(2)
    |> past_claim_window
  assert sim.synced(cluster, 2)
  let cluster =
    cluster
    |> sim.crash(1)
    |> sim.split([1], [0])
    |> sim.restart(1)
    |> past_claim_window
  assert core.ready(sim.bouncer(cluster, 1))
  assert core.status(sim.bouncer(cluster, 1)).peers_synced == 1
  assert !sim.synced(cluster, 1)
  let cluster =
    cluster
    |> sim.heal([1], [0])
    |> sim.advance(200)
  assert sim.synced(cluster, 1)
}

pub fn a_fresh_pair_with_the_old_third_cut_off_grants_nothing_twice_test() {
  let #(cluster, source) = ready_cluster(3, 2)
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let before = sim.worker(cluster, source.node).held
  let cluster =
    cluster
    |> sim.crash(0)
    |> sim.crash(1)
    |> sim.split([0, 1], [2])
    |> sim.restart(0)
    |> sim.restart(1)
    |> sim.clear
    |> rejoin(source)
    |> rejoin(target)
    |> past_claim_window
  assert sim.synced(cluster, 0)
  assert sim.synced(cluster, 1)
  assert !sim.synced(cluster, 2)
  let cluster = sim.advance(cluster, 12_000)
  assert grants(cluster, source.node) == 0
  assert grants(cluster, target.node) == 0
  let cluster =
    cluster
    |> sim.heal([0, 1], [2])
    |> sim.advance(1000)
  assert list.all([0, 1, 2], sim.synced(cluster, _))
  assert grants(cluster, source.node) == 0
  assert grants(cluster, target.node) == 0
  assert sim.worker(cluster, source.node).held == before
  assert sorted(cluster, 0) == sorted(cluster, 2)
  assert sorted(cluster, 1) == sorted(cluster, 2)
  list.each(sorted(cluster, 0), fn(record) {
    assert record.holder == Some(source)
    assert dict.get(before, record.bucket) == Ok(record.epoch)
  })
}

pub fn a_fresh_bouncer_learns_what_the_live_third_told_its_fresh_peer_test() {
  let #(cluster, source) = ready_cluster(3, 2)
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let #(cluster, moved) = move_where(cluster, source, target, [0, 1, 2], 8)
  assert dict.size(moved) == 8
  let cluster =
    cluster
    |> sim.silence(target.node)
    |> sim.crash(0)
    |> sim.crash(1)
    |> sim.split([0], [2])
    |> sim.restart(0)
    |> sim.restart(1)
    |> rejoin(source)
    |> past_claim_window
  assert sim.synced(cluster, 0)
  assert sim.synced(cluster, 1)
  dict.each(moved, fn(bucket, committed) {
    assert sim.record(cluster, 2, bucket) == Ok(committed)
    assert sim.record(cluster, 1, bucket) == Ok(committed)
    assert sim.record(cluster, 0, bucket) == Ok(committed)
  })
}

fn answers_from_two(from: Int, _to: Int, message: PeerMessage) -> Bool {
  case message {
    SyncAnswer(_) -> from == 2
    _ -> False
  }
}

pub fn a_fresh_pair_waits_for_a_live_third_that_has_not_answered_yet_test() {
  let #(cluster, source) = ready_cluster(3, 2)
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let #(cluster, moved) = move_where(cluster, source, target, [0, 1, 2], 8)
  assert dict.size(moved) == 8
  let cluster =
    cluster
    |> sim.silence(target.node)
    |> sim.crash(0)
    |> sim.crash(1)
    |> sim.lose(answers_from_two)
    |> sim.restart(0)
    |> sim.restart(1)
    |> rejoin(source)
    |> past_claim_window
  assert !sim.synced(cluster, 0)
  assert !sim.synced(cluster, 1)
  let late = SyncAnswer(core.records(sim.bouncer(cluster, 2)))
  let cluster =
    cluster
    |> sim.stop_losing
    |> sim.deliver_input(0, core.Received(FromPeer(2, late)))
    |> sim.deliver_input(1, core.Received(FromPeer(2, late)))
    |> sim.advance(200)
  assert sim.synced(cluster, 0)
  assert sim.synced(cluster, 1)
  dict.each(moved, fn(bucket, committed) {
    assert sim.record(cluster, 0, bucket) == Ok(committed)
    assert sim.record(cluster, 1, bucket) == Ok(committed)
  })
}

pub fn a_fresh_answer_does_not_count_toward_the_majority_a_restart_needs_test() {
  let #(cluster, source) = ready_cluster(5, 3)
  let #(cluster, target) = sim.add_worker(cluster, "t@sim", sim.profile())
  let cluster = sim.advance(cluster, 1000)
  let cluster = sim.drop_between(cluster, [0, 3, 4], [1, 2])
  let #(cluster, moved) = move_where(cluster, source, target, [0, 3, 4], 4)
  assert dict.size(moved) == 4
  dict.each(moved, fn(bucket, committed) {
    assert sim.record(cluster, 4, bucket) == Ok(committed)
    assert sim.record(cluster, 1, bucket)
      |> result.map(fn(found) { found.holder })
      == Ok(Some(source))
  })
  let cluster =
    cluster
    |> sim.pass_between([0, 3, 4], [1, 2])
    |> sim.silence(target.node)
    |> sim.crash(0)
    |> sim.crash(3)
    |> sim.split([0, 3], [4])
    |> sim.restart(0)
    |> sim.restart(3)
    |> rejoin(source)
    |> past_claim_window
  assert core.status(sim.bouncer(cluster, 0)).peers_synced == 3
  assert !sim.synced(cluster, 0)
  assert !sim.synced(cluster, 3)
  assert list.all([1, 2, 4], sim.synced(cluster, _))
  let cluster =
    cluster
    |> sim.heal([0, 3], [4])
    |> sim.advance(200)
  assert sim.synced(cluster, 0)
  assert sim.synced(cluster, 3)
  dict.each(moved, fn(bucket, committed) {
    assert sim.record(cluster, 0, bucket) == Ok(committed)
    assert sim.record(cluster, 3, bucket) == Ok(committed)
  })
}

pub fn three_fresh_answers_of_five_rebuild_from_claims_test() {
  let #(cluster, source) = ready_cluster(5, 3)
  let cluster =
    cluster
    |> sim.crash(0)
    |> sim.crash(1)
    |> sim.crash(2)
    |> sim.crash(3)
    |> sim.crash(4)
    |> sim.restart(0)
    |> sim.restart(1)
    |> rejoin(source)
    |> past_claim_window
  assert !sim.synced(cluster, 0)
  assert !sim.synced(cluster, 1)
  let cluster =
    cluster
    |> sim.restart(2)
    |> rejoin(source)
    |> past_claim_window
  assert list.all([0, 1, 2], sim.synced(cluster, _))
}

fn fresh_bouncer() -> core.State {
  let assert Ok(built) =
    config.build(hostname: "b-0", count: "3", basename: "b", dns_name: "sim")
  core.new(built, 1, 0, 1_000_000)
}

fn hello_from(
  state: core.State,
  peer: Int,
  range: bouncer_proto.ProtocolRange,
) {
  let #(state, _) =
    core.step(
      state,
      core.Received(FromPeer(peer, PeerHello(peer, 7 + peer, range))),
      10,
    )
  state
}

pub fn a_fresh_bouncer_says_so_only_to_peers_that_understand_it_test() {
  let v2 = bouncer_proto.ProtocolRange(oldest: 1, current: 2)
  let state =
    fresh_bouncer()
    |> hello_from(1, v2)
    |> hello_from(2, bouncer_proto.own_range())
  let #(state, effects) =
    core.step(state, core.Received(FromPeer(1, SyncRequest)), 20)
  assert effects == [core.ToPeer(1, SyncAnswer([]))]
  let #(state, effects) =
    core.step(state, core.Received(FromPeer(2, SyncRequest)), 20)
  assert effects == [core.ToPeer(2, FreshAnswer([]))]
  let #(state, _) =
    core.step(state, core.Received(FromPeer(2, FreshAnswer([]))), 30)
  let #(state, _) = core.step(state, core.Tick, 5000)
  assert !core.synced(state)
  let #(state, effects) =
    core.step(state, core.Received(FromPeer(1, SyncAnswer([]))), 5050)
  assert core.synced(state)
  assert list.contains(effects, core.ToPeer(1, SyncReply([])))
  assert list.contains(effects, core.ToPeer(2, SyncReply([])))
  let #(_, effects) =
    core.step(state, core.Received(FromPeer(2, SyncRequest)), 5100)
  assert effects == [core.ToPeer(2, SyncAnswer([]))]
}

pub fn a_v2_answer_from_a_fresh_peer_still_counts_as_before_test() {
  let v2 = bouncer_proto.ProtocolRange(oldest: 1, current: 2)
  let state =
    fresh_bouncer()
    |> hello_from(1, v2)
    |> hello_from(2, v2)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(1, SyncAnswer([]))), 30)
  let #(state, _) = core.step(state, core.Tick, 5000)
  assert !core.synced(state)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(2, SyncAnswer([]))), 5050)
  let #(state, _) = core.step(state, core.Tick, 5100)
  assert core.synced(state)
}

pub fn a_fresh_quorum_of_five_needs_three_fresh_bouncers_test() {
  let assert Ok(built) =
    config.build(hostname: "b-0", count: "5", basename: "b", dns_name: "sim")
  let state =
    core.new(built, 1, 0, 1_000_000)
    |> hello_from(1, bouncer_proto.own_range())
    |> hello_from(2, bouncer_proto.own_range())
    |> hello_from(3, bouncer_proto.own_range())
    |> hello_from(4, bouncer_proto.own_range())
  let #(state, _) =
    core.step(state, core.Received(FromPeer(1, FreshAnswer([]))), 30)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(2, SyncAnswer([]))), 30)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(3, SyncAnswer([]))), 30)
  let #(state, _) = core.step(state, core.Tick, 5000)
  assert !core.synced(state)
  let #(state, _) =
    core.step(state, core.Received(FromPeer(4, FreshAnswer([]))), 5050)
  let #(state, _) = core.step(state, core.Tick, 5100)
  assert core.synced(state)
}
