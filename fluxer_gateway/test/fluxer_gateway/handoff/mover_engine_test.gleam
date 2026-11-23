// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key, Key}
import bouncer_proto/message.{type MoveId, type ToBouncer, MoveId}
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_gateway/handoff/adapter.{type Adapter, Adapter, Precopy}
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/ports.{type Ports}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{None, Some}
import support/handoff_toy
import support/wait

fn entity() -> Key {
  Key(ns: handoff_toy.ns, id: 1)
}

fn slow_adapter(copy_ms: Int, frozen: Subject(Nil)) -> Adapter {
  let missing = adapter.EntityMissing(entity())
  Adapter(
    ns: handoff_toy.ns,
    codecs: handoff_toy.range(),
    entities_in: fn(_) { [1] },
    freeze: fn(_, _, _, _) {
      process.send(frozen, Nil)
      Error(adapter.EntityBusy(entity()))
    },
    install: fn(_, _, _, _) { Error(adapter.InstallFailed("not a target")) },
    release: fn(_, _, _) { Error(missing) },
    seal: fn(_, _) { Error(missing) },
    abort_arrival: fn(_) { Nil },
    committed: fn(_, _, _) { Error(missing) },
    thaw: fn(_, _) { Error(missing) },
    abandon: fn(_) { Nil },
    precopy: Some(
      Precopy(
        from_version: 0,
        snapshot: fn(_, _, _) {
          process.sleep(copy_ms)
          Error(adapter.EntityBusy(entity()))
        },
        changes: fn(_, _, _, _) { Error(missing) },
        cancel: fn(_, _) { Nil },
        stage: fn(_, _, _) { Error(missing) },
        restage: fn(_, _, _) { Error(missing) },
        unstage: fn(_) { Nil },
      ),
    ),
  )
}

fn source_ports(
  name: String,
  sent: Subject(ToBouncer),
  holding: fn() -> Bool,
) -> #(Ports, Epoch) {
  let assert Ok(at) = epoch.from_int(5)
  let base = ports.unwired()
  #(
    ports.Ports(
      ..base,
      me: WorkerId(atom.create(name <> "_source@test"), 1),
      to_home: fn(_, message) { Ok(process.send(sent, message)) },
      holding: fn(_) {
        case holding() {
          True -> Ok(at)
          False -> Error(Nil)
        }
      },
      target_codecs: fn(_) { [handoff_toy.range()] },
      close_bucket: fn(_, held, _, _) { Ok(epoch.next(held)) },
    ),
    at,
  )
}

fn target(name: String) -> WorkerId {
  WorkerId(atom.create(name <> "_target@test"), 1)
}

fn begun(sent: Subject(ToBouncer)) -> MoveId {
  case process.receive(sent, 2000) {
    Ok(message.BeginMove(move:, ..)) -> move
    Ok(_) -> begun(sent)
    Error(Nil) -> MoveId(source: target("none"), serial: 0)
  }
}

fn stop(engine: move.Engine) -> Nil {
  let assert Ok(pid) = process.subject_owner(engine.inbox)
  kill(pid)
}

fn kill(pid: Pid) -> Nil {
  process.unlink(pid)
  process.kill(pid)
}

pub fn an_abort_during_a_slow_precopy_never_freezes_the_entity_test() {
  let sent = process.new_subject()
  let frozen = process.new_subject()
  let #(ports, at) = source_ports("copy_abort", sent, fn() { True })
  let assert Ok(engine) =
    move.start_with([slow_adapter(1500, frozen)], move.default_limits(), ports)
  let result = process.new_subject()
  process.spawn(fn() {
    process.send(
      result,
      move.move_entity(engine, entity(), at, target("copy_abort")),
    )
  })
  let move_id = begun(sent)
  move.from_home(engine, message.MoveAccepted(move_id))
  process.sleep(100)
  move.from_home(engine, message.Aborted(move_id))
  let assert Ok(outcome) = process.receive(result, 5000)
  assert outcome == Error(move.Refused(message.RetryAfter(200)))
  assert process.receive(frozen, 2000) == Error(Nil)
  stop(engine)
}

pub fn a_lease_lost_during_a_slow_precopy_never_freezes_the_entity_test() {
  let sent = process.new_subject()
  let frozen = process.new_subject()
  let lease = process.spawn_unlinked(fn() { process.sleep_forever() })
  let #(ports, at) =
    source_ports("copy_lease", sent, fn() { process.is_alive(lease) })
  let assert Ok(engine) =
    move.start_with([slow_adapter(1500, frozen)], move.default_limits(), ports)
  let result = process.new_subject()
  process.spawn(fn() {
    process.send(
      result,
      move.move_entity(engine, entity(), at, target("copy_lease")),
    )
  })
  let move_id = begun(sent)
  move.from_home(engine, message.MoveAccepted(move_id))
  process.sleep(100)
  process.kill(lease)
  let assert Ok(outcome) = process.receive(result, 5000)
  assert outcome == Error(move.NotHolder(key.bucket_of(entity())))
  assert process.receive(frozen, 2000) == Error(Nil)
  stop(engine)
}

pub fn a_second_move_of_an_entity_already_moving_starts_no_second_mover_test() {
  let sent = process.new_subject()
  let frozen = process.new_subject()
  let #(ports, at) = source_ports("copy_twice", sent, fn() { True })
  let assert Ok(engine) =
    move.start_with([slow_adapter(1500, frozen)], move.default_limits(), ports)
  let first = process.new_subject()
  process.spawn(fn() {
    process.send(
      first,
      move.move_entity(engine, entity(), at, target("copy_twice")),
    )
  })
  let move_id = begun(sent)
  move.from_home(engine, message.MoveAccepted(move_id))
  let started =
    rollout_metrics.total(rollout_metrics.moves_started, [#("kind", "other")])
  let assert Error(move.InDoubt(busy)) =
    move.move_entity(engine, entity(), at, target("copy_twice"))
  assert busy == move_id
  assert rollout_metrics.total(rollout_metrics.moves_started, [
      #("kind", "other"),
    ])
    == started
  move.from_home(engine, message.Aborted(move_id))
  let assert Ok(_) = process.receive(first, 5000)
  stop(engine)
}

pub fn a_permanent_refusal_ends_the_bucket_attempt_without_retrying_test() {
  let sent = process.new_subject()
  let frozen = process.new_subject()
  let #(ports, _) = source_ports("no_codec", sent, fn() { True })
  let ports = ports.Ports(..ports, target_codecs: fn(_) { [] })
  let assert Ok(engine) =
    move.start_with(
      [slow_adapter(0, frozen)],
      move.Limits(..move.default_limits(), retry_budget_ms: 30_000),
      ports,
    )
  let kind = [#("kind", "other")]
  let started = rollout_metrics.total(rollout_metrics.moves_started, kind)
  let assert Ok(outcome) =
    move.move_bucket(engine, key.bucket_of(entity()), target("no_codec"), 5000)
  let assert [move.Thawed(..)] = outcome.entities
  assert rollout_metrics.total(rollout_metrics.moves_started, kind)
    == started + 1
  stop(engine)
}

fn dying_stub_adapter(stub: Pid) -> Adapter {
  let missing = adapter.EntityMissing(entity())
  Adapter(
    ns: handoff_toy.ns,
    codecs: handoff_toy.range(),
    entities_in: fn(_) { [1] },
    freeze: fn(_, _, _, _) { Ok(adapter.Frozen(tail: <<1, 2, 3>>, stub:)) },
    install: fn(_, _, _, _) { Error(adapter.InstallFailed("not a target")) },
    release: fn(_, _, _) { Error(missing) },
    seal: fn(_, _) { Error(missing) },
    abort_arrival: fn(_) { Nil },
    committed: fn(pid, _, _) {
      kill(pid)
      Ok(Nil)
    },
    thaw: fn(_, _) { Error(missing) },
    abandon: fn(_) { Nil },
    precopy: None,
  )
}

fn sealed(sent: Subject(ToBouncer), within_ms: Int) -> Result(Int, Nil) {
  case process.receive(sent, within_ms) {
    Ok(message.SealSource(last_seq:, ..)) -> Ok(last_seq)
    Ok(_) -> sealed(sent, within_ms)
    Error(Nil) -> Error(Nil)
  }
}

pub fn a_stub_that_dies_before_it_repoints_still_seals_the_move_test() {
  let sent = process.new_subject()
  let stub = process.spawn_unlinked(fn() { process.sleep_forever() })
  let #(ports, at) = source_ports("stub_dies", sent, fn() { True })
  let assert Ok(engine) =
    move.start_with([dying_stub_adapter(stub)], move.default_limits(), ports)
  let result = process.new_subject()
  process.spawn(fn() {
    process.send(
      result,
      move.move_entity(engine, entity(), at, target("stub_dies")),
    )
  })
  let move_id = begun(sent)
  move.from_home(engine, message.MoveAccepted(move_id))
  move.from_home(engine, message.ChunkAck(move_id, blob.Tail, 0))
  move.from_home(engine, message.Committed(move_id, <<"ref">>, epoch.next(at)))
  assert sealed(sent, 2000) == Ok(0)
  move.from_home(engine, message.AppliedThrough(move_id, 0))
  let assert Ok(Ok(move.Moved(..))) = process.receive(result, 5000)
  stop(engine)
}

pub fn a_stub_that_died_while_in_doubt_still_seals_once_committed_test() {
  let sent = process.new_subject()
  let stub = process.spawn_unlinked(fn() { process.sleep_forever() })
  let #(ports, at) = source_ports("stub_gone", sent, fn() { True })
  let assert Ok(engine) =
    move.start_with([dying_stub_adapter(stub)], move.default_limits(), ports)
  let result = process.new_subject()
  process.spawn(fn() {
    process.send(
      result,
      move.move_entity(engine, entity(), at, target("stub_gone")),
    )
  })
  let move_id = begun(sent)
  move.from_home(engine, message.MoveAccepted(move_id))
  move.from_home(engine, message.ChunkAck(move_id, blob.Tail, 0))
  let watch = process.monitor(stub)
  kill(stub)
  let assert Ok(Nil) =
    process.new_selector()
    |> process.select_specific_monitor(watch, fn(_) { Nil })
    |> process.selector_receive(1000)
  move.from_home(engine, message.Committed(move_id, <<"ref">>, epoch.next(at)))
  assert sealed(sent, 2000) == Ok(0)
  move.from_home(engine, message.AppliedThrough(move_id, 0))
  let assert Ok(Ok(move.Moved(..))) = process.receive(result, 5000)
  stop(engine)
}

fn precopies(outcome: String) -> Int {
  metrics.histograms()
  |> list.filter(fn(series) {
    series.name == move.precopy_duration
    && series.labels == [#("kind", "other"), #("outcome", outcome)]
  })
  |> list.fold(0, fn(total, series) { total + series.count })
}

pub fn a_failed_precopy_is_timed_by_kind_and_outcome_test() {
  let sent = process.new_subject()
  let frozen = process.new_subject()
  let #(ports, at) = source_ports("copy_timed", sent, fn() { True })
  let assert Ok(engine) =
    move.start_with([slow_adapter(30, frozen)], move.default_limits(), ports)
  let failed = precopies("failed")
  let staged = precopies("staged")
  let result = process.new_subject()
  process.spawn(fn() {
    process.send(
      result,
      move.move_entity(engine, entity(), at, target("copy_timed")),
    )
  })
  let move_id = begun(sent)
  move.from_home(engine, message.MoveAccepted(move_id))
  assert wait.until(3000, fn() { precopies("failed") == failed + 1 })
  assert precopies("staged") == staged
  move.from_home(engine, message.Aborted(move_id))
  let _ = process.receive(result, 5000)
  stop(engine)
}
