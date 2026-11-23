// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/handoff/inbound
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/ports
import fluxer_gateway/telemetry/rollout_metrics
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import support/handoff_toy

pub fn engine_moves_a_live_entity_without_losing_or_reordering_test() {
  let source_worker = WorkerId(atom.create("engine_source@test"), 1)
  let target_worker = WorkerId(atom.create("engine_target@test"), 1)
  let assert Ok(at) = epoch.from_int(7)
  let dir = handoff_toy.directory()
  let done = process.new_subject()
  let total = 400
  let #(peer_inbox, peer_go) = handoff_toy.start_peer(total, done)
  let entity = handoff_toy.spawn_entity(dir, [], False, peer_inbox)
  let installed = process.new_subject()
  let toy = handoff_toy.adapter(dir, entity, installed, peer_inbox)
  let home = handoff_toy.home()
  let base = ports.unwired()
  let source_ports =
    ports.Ports(
      ..base,
      me: source_worker,
      to_home: fn(_, message) {
        Ok(process.send(home, handoff_toy.FromWorker(message)))
      },
      holding: fn(_) { Ok(at) },
      target_codecs: fn(_) { [handoff_toy.range()] },
    )
  let target_ports =
    ports.Ports(..base, me: target_worker, to_home: fn(_, message) {
      Ok(process.send(home, handoff_toy.FromWorker(message)))
    })
  let assert Ok(engine) =
    move.start_with([toy], move.default_limits(), source_ports)
  let assert Ok(target) = inbound.start_with([toy], target_ports, fn(_) { Nil })
  process.send(
    home,
    handoff_toy.Routes(move.from_home(engine, _), fn(message) {
      process.send(target.data, inbound.Incoming(message))
    }),
  )
  process.send(peer_go, entity)
  process.sleep(50)
  let before = moves_seen()
  let assert Ok(move.Moved(_, moved_to, freeze_ms)) =
    move.move_entity(engine, Key(ns: handoff_toy.ns, id: 1), at, target_worker)
  assert moved_to == epoch.next(at)
  assert freeze_ms >= 0
  let after = moves_seen()
  assert after.started == before.started + 1
  assert after.committed == before.committed + 1
  assert after.aborted == before.aborted
  assert after.frozen == before.frozen + 1
  assert after.timed == before.timed + 1
  let assert Ok(sent) = process.receive(done, 10_000)
  process.sleep(50)
  let assert Ok(arrived) = process.receive(installed, 0)
  let ops = handoff_toy.ops(arrived)
  assert sent == total
  assert ops == int.range(from: total, to: 0, with: [], run: list.prepend)
  assert inbound.arriving_in(target.data) == []
  let engine_pid = unwrap_pid(process.subject_owner(engine.inbox))
  process.unlink(engine_pid)
  process.kill(engine_pid)
  process.unlink(target.pid)
  process.kill(target.pid)
}

type MovesSeen {
  MovesSeen(started: Int, committed: Int, aborted: Int, frozen: Int, timed: Int)
}

fn moves_seen() -> MovesSeen {
  let kind = [#("kind", "other")]
  MovesSeen(
    started: rollout_metrics.total(rollout_metrics.moves_started, kind),
    committed: rollout_metrics.total(rollout_metrics.moves_committed, kind),
    aborted: rollout_metrics.total(rollout_metrics.moves_aborted, kind),
    frozen: rollout_metrics.observed(rollout_metrics.move_freeze, [
      #("kind", "other"),
      #("size_class", "under_64k"),
    ]),
    timed: rollout_metrics.observed(rollout_metrics.move_duration, kind),
  )
}

fn unwrap_pid(found: Result(process.Pid, Nil)) -> process.Pid {
  let assert Ok(pid) = found
  pid
}

pub fn an_entity_that_cannot_move_keeps_serving_and_is_retried_until_it_moves_test() {
  let source_worker = WorkerId(atom.create("retry_source@test"), 1)
  let target_worker = WorkerId(atom.create("retry_target@test"), 1)
  let assert Ok(at) = epoch.from_int(9)
  let dir = handoff_toy.directory()
  let done = process.new_subject()
  let total = 1500
  let #(peer_inbox, peer_go) = handoff_toy.start_peer(total, done)
  let entity = handoff_toy.spawn_entity(dir, [], False, peer_inbox)
  let installed = process.new_subject()
  let toy =
    handoff_toy.adapter(dir, entity, installed, peer_inbox)
    |> handoff_toy.refusing(4)
  let home = handoff_toy.home()
  let emptied = process.new_subject()
  let base = ports.unwired()
  let source_ports =
    ports.Ports(
      ..base,
      me: source_worker,
      to_home: fn(_, message) {
        Ok(process.send(home, handoff_toy.FromWorker(message)))
      },
      holding: fn(_) { Ok(at) },
      target_codecs: fn(_) { [handoff_toy.range()] },
      close_bucket: fn(_, held, _, _) { Ok(epoch.next(held)) },
      bucket_emptied: fn(bucket, _) { process.send(emptied, bucket) },
    )
  let target_ports =
    ports.Ports(..base, me: target_worker, to_home: fn(_, message) {
      Ok(process.send(home, handoff_toy.FromWorker(message)))
    })
  let assert Ok(engine) =
    move.start_with(
      [toy],
      move.Limits(..move.default_limits(), retry_budget_ms: 30_000),
      source_ports,
    )
  let assert Ok(target) = inbound.start_with([toy], target_ports, fn(_) { Nil })
  process.send(
    home,
    handoff_toy.Routes(move.from_home(engine, _), fn(message) {
      process.send(target.data, inbound.Incoming(message))
    }),
  )
  process.send(peer_go, entity)
  process.sleep(50)
  let bucket = key.bucket_of(Key(ns: handoff_toy.ns, id: 1))
  let before = moves_seen()
  let assert Ok(outcome) =
    move.move_bucket(engine, bucket, target_worker, 60_000)
  let assert [move.Moved(_, moved_to, _)] = outcome.entities
  assert moved_to == epoch.next(at)
  let after = moves_seen()
  assert after.committed == before.committed + 1
  assert after.aborted >= before.aborted + 4
  assert after.started - before.started
    == after.committed - before.committed + after.aborted - before.aborted
  assert process.receive(emptied, 1000) == Ok(bucket)
  let assert Ok(sent) = process.receive(done, 20_000)
  process.sleep(50)
  let assert Ok(arrived) = process.receive(installed, 0)
  assert sent == total
  assert handoff_toy.ops(arrived)
    == int.range(from: total, to: 0, with: [], run: list.prepend)
  let engine_pid = unwrap_pid(process.subject_owner(engine.inbox))
  process.unlink(engine_pid)
  process.kill(engine_pid)
  process.unlink(target.pid)
  process.kill(target.pid)
}

pub fn a_thawed_stub_keeps_what_it_forwarded_after_its_mover_finished_test() {
  let dir = handoff_toy.directory()
  let entity =
    handoff_toy.spawn_entity(dir, [1, 2], False, process.new_subject())
  let reply = process.new_subject()
  let move =
    MoveId(source: WorkerId(atom.create("thaw_source@test"), 1), serial: 1)
  process.send(entity, handoff_toy.Freeze(move, reply))
  let assert Ok(_) = process.receive(reply, 1000)
  process.send(entity, handoff_toy.Op(3))
  process.send(entity, handoff_toy.Op(4))
  process.send(entity, handoff_toy.Thaw([]))
  process.send(entity, handoff_toy.Op(5))
  assert handoff_toy.ops(entity) == [1, 2, 3, 4, 5]
}
