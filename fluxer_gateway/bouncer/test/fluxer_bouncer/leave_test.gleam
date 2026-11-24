// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob.{Tail}
import bouncer_proto/epoch
import bouncer_proto/key.{type BucketId, Key}
import bouncer_proto/message.{
  type MoveId, type PeerMessage, BeatAck, BeginMove, BlobChunk, BouncerLeaving,
  CloseBucket, Committed, Handover, Installed, MoveId, PeerLeaving, ResendFrom,
}
import bouncer_proto/rendezvous
import bouncer_proto/worker.{type WorkerId}
import fluxer_bouncer/config.{Timings}
import fluxer_bouncer/core
import fluxer_bouncer/server
import fluxer_bouncer/shutdown
import fluxer_bouncer/sim.{type Sim}
import fluxer_bouncer/status
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Subject}
import gleam/list

const bound_ms = 600

type TimeUnit {
  Millisecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "os", "getpid")
fn os_pid() -> Charlist

@external(erlang, "os", "cmd")
fn os_cmd(command: Charlist) -> Charlist

type Report {
  Report(outcome: shutdown.Outcome, elapsed: Int)
}

type Seen {
  Seen(from: Int, to: Int, message: PeerMessage)
}

fn sigterm() -> Dynamic {
  atom.to_dynamic(atom.create("sigterm"))
}

fn now() -> Int {
  monotonic_time(Millisecond)
}

fn drain_after(
  signal: fn() -> Nil,
  setup: fn() -> Result(Nil, Nil),
  leave: fn() -> Nil,
) -> Report {
  let reply = process.new_subject()
  process.spawn_unlinked(fn() {
    let assert Ok(Nil) = setup()
    signal()
    shutdown.await_term()
    let started = now()
    let outcome = shutdown.drain(bound_ms, leave)
    process.send(reply, Report(outcome, now() - started))
  })
  let assert Ok(report) = process.receive(reply, bound_ms + 3000)
  report
}

fn invoke_handler() -> Nil {
  let assert Ok(Nil) = shutdown.handle_event(sigterm(), Nil)
  Nil
}

fn drive(
  cluster: Sim,
  home: Int,
  left: Bool,
  rounds: Int,
  reply: Subject(Sim),
) -> Nil {
  let left = case !left && core.handed_over(sim.bouncer(cluster, home)) {
    True -> {
      shutdown.announce_left()
      True
    }
    False -> left
  }
  case rounds {
    0 -> process.send(reply, cluster)
    _ -> {
      process.sleep(5)
      drive(sim.advance(cluster, 100), home, left, rounds - 1, reply)
    }
  }
}

fn leave_in_sim(
  cluster: Sim,
  home: Int,
  rounds: Int,
  reply: Subject(Sim),
) -> fn() -> Nil {
  fn() {
    let _ =
      process.spawn_unlinked(fn() {
        drive(
          sim.deliver_input(cluster, home, core.Leave),
          home,
          False,
          rounds,
          reply,
        )
      })
    Nil
  }
}

fn spy(seen: Subject(Seen)) -> fn(Int, Int, PeerMessage) -> Bool {
  fn(from, to, message) {
    case message {
      PeerLeaving(..) | Handover(..) ->
        process.send(seen, Seen(from, to, message))
      _ -> Nil
    }
    False
  }
}

fn collect(seen: Subject(Seen), found: List(Seen)) -> List(Seen) {
  case process.receive(seen, 0) {
    Ok(one) -> collect(seen, [one, ..found])
    Error(Nil) -> list.reverse(found)
  }
}

fn moving_cluster() -> #(
  Sim,
  WorkerId,
  WorkerId,
  key.Key,
  BucketId,
  epoch.Epoch,
) {
  let cluster = sim.new(3, 2, sim.timings())
  let #(cluster, source) = sim.add_worker(cluster, "s@sim", sim.profile())
  let cluster = sim.advance(cluster, 10_000)
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

fn others(home: Int) -> List(Int) {
  list.filter([0, 1, 2], fn(ordinal) { ordinal != home })
}

fn matched_acks(cluster: Sim, node: Atom, from: List(Int)) -> Int {
  sim.to_worker(cluster, node)
  |> list.count(fn(message) {
    case message {
      BeatAck(bouncer: bouncer, matched: True, ..) ->
        list.contains(from, bouncer)
      _ -> False
    }
  })
}

fn handed(seen: List(Seen), from: Int, to: Int, move: MoveId) -> Bool {
  list.any(seen, fn(one) {
    case one {
      Seen(from: f, to: t, message: Handover(records)) ->
        f == from && t == to && list.any(records, fn(r) { r.move == move })
      _ -> False
    }
  })
}

fn told_leaving(seen: List(Seen), from: Int, to: Int) -> Bool {
  list.contains(seen, Seen(from, to, PeerLeaving(from)))
}

pub fn a_sigterm_while_home_for_committed_moves_hands_them_over_and_exits_within_the_bound_test() {
  let #(cluster, source, target, entity, bucket, at) = moving_cluster()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.send(source.node, home, Installed(move, <<"ref">>))
  assert list.contains(
    sim.to_mover(cluster, source.node),
    Committed(move, <<"ref">>, epoch.next(at)),
  )
  assert core.moves_in_flight(sim.bouncer(cluster, home)) == 1
  let remaining = others(home)
  let assert Ok(successor) = rendezvous.homes(bucket, remaining)
  let seen = process.new_subject()
  let cluster = cluster |> sim.clear |> sim.lose(spy(seen))
  let finished = process.new_subject()
  let report =
    drain_after(
      invoke_handler,
      shutdown.claim,
      leave_in_sim(cluster, home, 3, finished),
    )
  assert report.outcome == shutdown.HandedOver
  assert report.elapsed < bound_ms
  let assert Ok(cluster) = process.receive(finished, 3000)
  let seen = collect(seen, [])
  assert handed(seen, home, successor.home, move)
  assert list.all(remaining, fn(peer) { told_leaving(seen, home, peer) })
  let leaving = sim.bouncer(cluster, home)
  assert core.handed_over(leaving)
  assert core.moves_in_flight(leaving) == 0
  assert core.status(leaving).leaving
  assert !core.synced(leaving)
  assert core.moves_in_flight(sim.bouncer(cluster, successor.home)) == 1
  let to_source = sim.to_mover(cluster, source.node)
  assert list.contains(to_source, ResendFrom(move, 1))
  assert list.contains(to_source, Committed(move, <<"ref">>, epoch.next(at)))
  assert list.contains(
    sim.to_worker(cluster, source.node),
    BouncerLeaving(home),
  )
  assert list.contains(
    sim.to_worker(cluster, target.node),
    BouncerLeaving(home),
  )
  list.each(remaining, fn(peer) {
    assert sim.synced(cluster, peer)
    assert core.status(sim.bouncer(cluster, peer)).peers_seen == 1
  })
  let cluster = cluster |> sim.clear |> sim.beat(source.node)
  assert matched_acks(cluster, source.node, remaining) == 2
}

pub fn a_sigterm_with_no_moves_leaves_and_exits_promptly_test() {
  let assert Ok(built) =
    config.build(
      hostname: "leave-0",
      count: "1",
      basename: "leave",
      dns_name: "invalid",
    )
  let timings =
    Timings(
      ..sim.timings(),
      beat_interval_ms: 50,
      claim_window_ms: 50,
      request_timeout_ms: 100,
      tick_ms: 10,
    )
  let assert Ok(started) =
    server.start(config.Config(..built, timings: timings))
  let signal = fn() {
    assert shutdown.installed()
    let _ =
      os_cmd(charlist.from_string("kill -TERM " <> charlist.to_string(os_pid())))
    Nil
  }
  let report = drain_after(signal, shutdown.install, server.leave)
  let summary = status.current()
  process.unlink(started.pid)
  process.kill(started.pid)
  assert report.outcome == shutdown.HandedOver
  assert report.elapsed < 200
  assert shutdown.installed()
  let assert Ok(summary) = summary
  assert summary.leaving
  assert !summary.synced
  assert summary.moves_in_flight == 0
}

fn stuck_cluster() -> #(Sim, Int, MoveId) {
  let #(cluster, source, target, entity, bucket, at) = moving_cluster()
  let move = MoveId(source, 1)
  let home = sim.home(cluster, source.node, bucket)
  let assert [down, stuck] = others(home)
  let cluster =
    cluster
    |> sim.send(source.node, home, BeginMove(move, entity, at, target, 1, 64))
    |> sim.send(source.node, home, BlobChunk(move, Tail, 0, 1, <<9>>))
    |> sim.crash(down)
    |> sim.drop_between([home], [stuck])
  assert core.moves_in_flight(sim.bouncer(cluster, home)) == 1
  #(cluster, home, move)
}

pub fn a_stuck_peer_holds_the_leave_until_the_bound_and_no_longer_test() {
  let #(cluster, home, _) = stuck_cluster()
  let finished = process.new_subject()
  let report =
    drain_after(
      invoke_handler,
      shutdown.claim,
      leave_in_sim(cluster, home, 200, finished),
    )
  assert report.outcome == shutdown.Expired
  assert report.elapsed >= bound_ms
  assert report.elapsed < bound_ms + 200
  let assert Ok(cluster) = process.receive(finished, 5000)
  let leaving = sim.bouncer(cluster, home)
  assert core.status(leaving).leaving
  assert !core.handed_over(leaving)
}

pub fn a_second_sigterm_stops_the_wait_at_once_test() {
  let #(cluster, home, _) = stuck_cluster()
  let finished = process.new_subject()
  let leave = leave_in_sim(cluster, home, 200, finished)
  let again = fn() {
    leave()
    let _ =
      process.spawn_unlinked(fn() {
        process.sleep(50)
        invoke_handler()
      })
    Nil
  }
  let report = drain_after(invoke_handler, shutdown.claim, again)
  assert report.outcome == shutdown.Interrupted
  assert report.elapsed >= 50
  assert report.elapsed < bound_ms
  let assert Ok(_) = process.receive(finished, 5000)
}

pub fn the_leave_bound_defaults_to_five_seconds_and_reads_its_setting_test() {
  let assert Ok(built) =
    config.build(hostname: "b-0", count: "3", basename: "b", dns_name: "d")
  assert built.timings.leave_bound_ms == 5000
  let assert Ok(set) = config.with_leave_bound(built, " 2500 ")
  assert set.timings.leave_bound_ms == 2500
  assert config.with_leave_bound(built, "-1")
    == Error(config.Invalid("FLUXER_BOUNCER_LEAVE_BOUND_MS", "-1"))
  assert config.with_leave_bound(built, "soon")
    == Error(config.Invalid("FLUXER_BOUNCER_LEAVE_BOUND_MS", "soon"))
}
