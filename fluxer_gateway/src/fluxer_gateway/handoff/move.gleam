// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId, type Key, Key}
import bouncer_proto/message.{type MoveId, type Refusal, type ToMover, MoveId}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/app/drain_report
import fluxer_gateway/cluster/remote
import fluxer_gateway/config/draining
import fluxer_gateway/config/env
import fluxer_gateway/handoff/adapter.{type Adapter, type AdapterError}
import fluxer_gateway/handoff/ports.{type Ports}
import fluxer_gateway/handoff/precopy
import fluxer_gateway/handoff/source.{type Source}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import logging

pub type MoveError {
  NoAdapter(ns: Int)
  NotHolder(bucket: BucketId)
  Refused(reason: Refusal)
  NoTarget(bucket: BucketId)
  Adapter(error: AdapterError)
  Timeout(move: MoveId)
  InDoubt(move: MoveId)
  EngineStartFailed
}

pub type EntityOutcome {
  Moved(key: Key, epoch: Epoch, freeze_ms: Int)
  Thawed(key: Key, epoch: Epoch)
  ColdAtTarget(key: Key)
  Departed(key: Key)
}

pub type BucketOutcome {
  BucketOutcome(
    bucket: BucketId,
    to: WorkerId,
    epoch: Epoch,
    entities: List(EntityOutcome),
  )
}

pub type Limits {
  Limits(entities_in_flight: Int, buckets_in_flight: Int, retry_budget_ms: Int)
}

pub type Command {
  MoveBucket(
    bucket: BucketId,
    to: WorkerId,
    reply: Subject(Result(BucketOutcome, MoveError)),
  )
  Drain(within_ms: Int, reply: Subject(Result(List(BucketOutcome), MoveError)))
  MoveEntity(
    key: Key,
    epoch: Epoch,
    to: WorkerId,
    reply: Subject(Result(EntityOutcome, MoveError)),
  )
  Forwarded(move: MoveId, bytes: BitArray)
  Repointed(move: MoveId)
  FromHome(message: ToMover)
  HomeChanged
  MoverFinished(move: MoveId)
}

pub type Engine {
  Engine(inbox: Subject(Command))
}

type State {
  State(
    self: Subject(Command),
    adapters: Dict(Int, Adapter),
    limits: Limits,
    ports: Ports,
    movers: Dict(MoveId, InFlight),
    serial: Int,
  )
}

type InFlight {
  InFlight(key: Key, inbox: Subject(source.Event))
}

const entity_timeout_ms = 60_000

const retry_backoff_ms = 200

const retry_backoff_cap_ms = 5000

pub fn default_limits() -> Limits {
  Limits(entities_in_flight: 32, buckets_in_flight: 4, retry_budget_ms: 20_000)
}

pub fn engine_name() -> Name(Command) {
  env.process_name("fluxer_gateway_handoff_engine")
}

pub fn start(
  adapters: List(Adapter),
  limits: Limits,
) -> Result(Engine, MoveError) {
  start_with(adapters, limits, ports.unwired())
}

pub fn start_with(
  adapters: List(Adapter),
  limits: Limits,
  ports: Ports,
) -> Result(Engine, MoveError) {
  let table =
    adapters
    |> list.map(fn(entry) { #(entry.ns, entry) })
    |> dict.from_list
  actor.new_with_initialiser(1000, fn(self) {
    State(
      self:,
      adapters: table,
      limits:,
      ports:,
      movers: dict.new(),
      serial: 0,
    )
    |> actor.initialised
    |> actor.returning(self)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(engine_name())
  |> actor.start
  |> result.map(fn(started) { Engine(inbox: started.data) })
  |> result.replace_error(EngineStartFailed)
}

pub fn forward(move: MoveId, bytes: BitArray) -> Nil {
  let _ = remote.send_local(engine_name(), Forwarded(move, bytes))
  Nil
}

pub fn repointed(move: MoveId) -> Nil {
  let _ = remote.send_local(engine_name(), Repointed(move))
  Nil
}

pub fn home_changed_local() -> Nil {
  let _ = remote.send_local(engine_name(), HomeChanged)
  Nil
}

pub fn drain_local(timeout_ms: Int) -> Result(List(BucketOutcome), MoveError) {
  drain(Engine(process.named_subject(engine_name())), timeout_ms)
}

pub fn from_home_local(message: ToMover) -> Nil {
  let _ = remote.send_local(engine_name(), FromHome(message))
  Nil
}

pub fn from_home(engine: Engine, message: ToMover) -> Nil {
  process.send(engine.inbox, FromHome(message))
}

pub fn home_changed(engine: Engine) -> Nil {
  process.send(engine.inbox, HomeChanged)
}

pub fn move_bucket(
  engine: Engine,
  bucket: BucketId,
  to: WorkerId,
  timeout_ms: Int,
) -> Result(BucketOutcome, MoveError) {
  call(engine, timeout_ms, MoveBucket(bucket, to, _))
}

pub fn move_entity(
  engine: Engine,
  key: Key,
  epoch: Epoch,
  to: WorkerId,
) -> Result(EntityOutcome, MoveError) {
  call(engine, entity_timeout_ms, MoveEntity(key, epoch, to, _))
}

pub fn drain(
  engine: Engine,
  timeout_ms: Int,
) -> Result(List(BucketOutcome), MoveError) {
  call(engine, timeout_ms, Drain(timeout_ms, _))
}

fn call(
  engine: Engine,
  timeout_ms: Int,
  make: fn(Subject(Result(value, MoveError))) -> Command,
) -> Result(value, MoveError) {
  let reply = process.new_subject()
  process.send(engine.inbox, make(reply))
  process.receive(reply, timeout_ms)
  |> result.replace_error(
    Timeout(MoveId(source: ports.unwired().me, serial: 0)),
  )
  |> result.flatten
}

fn handle(state: State, command: Command) -> actor.Next(State, Command) {
  case command {
    MoveEntity(key, epoch, to, reply) ->
      actor.continue(start_mover(state, key, epoch, to, reply))
    MoveBucket(bucket, to, reply) -> {
      let engine = Engine(state.self)
      let ports = state.ports
      let limits = state.limits
      let adapters = state.adapters
      process.spawn(fn() {
        process.send(
          reply,
          run_bucket(engine, adapters, ports, limits, bucket, to),
        )
      })
      actor.continue(state)
    }
    Drain(within_ms, reply) -> {
      let engine = Engine(state.self)
      let ports = state.ports
      let limits = state.limits
      let adapters = state.adapters
      let until = ports.now_ms() + within_ms
      process.spawn(fn() {
        process.send(reply, run_drain(engine, adapters, ports, limits, until))
      })
      actor.continue(state)
    }
    Forwarded(move, bytes) -> {
      case dict.has_key(state.movers, move) {
        True -> Nil
        False ->
          rollout_metrics.count(
            rollout_metrics.stub_hits,
            [#("kind", "other"), #("outcome", "dropped")],
            1,
          )
      }
      to_mover(state, move, source.Forwarded(bytes))
      actor.continue(state)
    }
    Repointed(move) -> {
      to_mover(state, move, source.Repointed)
      actor.continue(state)
    }
    FromHome(message) -> {
      to_mover(state, mover_of(message), source.FromHome(message))
      actor.continue(state)
    }
    HomeChanged -> {
      dict.each(state.movers, fn(_, mover) {
        process.send(mover.inbox, source.HomeChanged)
      })
      actor.continue(state)
    }
    MoverFinished(move) ->
      actor.continue(with_movers(state, dict.delete(state.movers, move)))
  }
}

pub const moves_in_flight = "fluxer_gateway_handoff_moves_in_flight"

fn with_movers(state: State, movers: Dict(MoveId, InFlight)) -> State {
  metrics.gauge(moves_in_flight, [], dict.size(movers))
  State(..state, movers:)
}

fn to_mover(state: State, move: MoveId, event: source.Event) -> Nil {
  case dict.get(state.movers, move) {
    Ok(mover) -> process.send(mover.inbox, event)
    Error(Nil) -> Nil
  }
}

pub fn mover_of(message: ToMover) -> MoveId {
  case message {
    message.MoveAccepted(move)
    | message.MoveRefused(move, ..)
    | message.ChunkAck(move, ..)
    | message.Install(move, ..)
    | message.Committed(move, ..)
    | message.Release(move, ..)
    | message.Seal(move, ..)
    | message.Abort(move)
    | message.Aborted(move)
    | message.ResendFrom(move, ..)
    | message.MoveStatus(move, ..)
    | message.AppliedThrough(move, ..) -> move
  }
}

fn start_mover(
  state: State,
  key: Key,
  epoch: Epoch,
  to: WorkerId,
  reply: Subject(Result(EntityOutcome, MoveError)),
) -> State {
  case moving(state, key) {
    Ok(move) -> {
      process.send(reply, Error(InDoubt(move)))
      state
    }
    Error(Nil) -> start_fresh_mover(state, key, epoch, to, reply)
  }
}

fn moving(state: State, key: Key) -> Result(MoveId, Nil) {
  state.movers
  |> dict.to_list
  |> list.find(fn(entry) { { entry.1 }.key == key })
  |> result.map(fn(entry) { entry.0 })
}

fn start_fresh_mover(
  state: State,
  key: Key,
  epoch: Epoch,
  to: WorkerId,
  reply: Subject(Result(EntityOutcome, MoveError)),
) -> State {
  let kind = [#("kind", rollout_metrics.kind_of_ns(key.ns))]
  rollout_metrics.count(rollout_metrics.moves_started, kind, 1)
  case dict.get(state.adapters, key.ns) {
    Error(Nil) -> {
      aborted(key, "no_adapter")
      process.send(reply, Error(NoAdapter(key.ns)))
      state
    }
    Ok(entry) ->
      case
        codec_range.can_move(
          [entry.codecs],
          state.ports.target_codecs(to),
          key.ns,
        )
      {
        Error(Nil) -> {
          aborted(key, "no_codec")
          process.send(reply, Error(Refused(message.NoCommonCodec)))
          state
        }
        Ok(version) -> {
          let serial = state.serial + 1
          let move = MoveId(source: state.ports.me, serial:)
          let hooks = case entry.precopy {
            Some(found) if version >= found.from_version -> Some(found)
            _ -> None
          }
          let machine =
            source.new(
              move:,
              key:,
              epoch:,
              to:,
              version:,
              estimate: 0,
              timings: state.ports.timings,
              precopy: option.is_some(hooks),
            )
          let ready = process.new_subject()
          let engine = state.self
          let ports = state.ports
          let started = ports.now_ms()
          process.spawn(fn() {
            let inbox = process.new_subject()
            process.send(ready, inbox)
            let mover =
              Mover(
                inbox:,
                machine:,
                adapter: entry,
                ports:,
                stub: None,
                epoch:,
                version:,
                tail_bytes: 0,
                precopy: hooks,
                copied: False,
                copier: None,
                watch: process.new_selector() |> process.select(inbox),
              )
            let outcome = run_mover(mover, [])
            rollout_metrics.observe(
              rollout_metrics.move_duration,
              kind,
              ports.now_ms() - started,
            )
            process.send(engine, MoverFinished(move))
            process.send(reply, entity_result(key, move, outcome))
          })
          let inbox = process.receive_forever(ready)
          process.send(inbox, source.Begin)
          with_movers(
            State(..state, serial:),
            dict.insert(state.movers, move, InFlight(key:, inbox:)),
          )
        }
      }
  }
}

fn aborted(entity: Key, reason: String) -> Nil {
  let kind = rollout_metrics.kind_of_ns(entity.ns)
  rollout_metrics.count(
    rollout_metrics.moves_aborted,
    [#("kind", kind), #("reason", reason)],
    1,
  )
  drain_report.entity_failed(kind, entity.id)
}

fn drain_entity(kind: String, outcome: String) -> Nil {
  case draining.is_draining() {
    True ->
      rollout_metrics.count(
        rollout_metrics.drain_entities,
        [#("kind", kind), #("outcome", outcome)],
        1,
      )
    False -> Nil
  }
}

pub fn abort_reason(outcome: source.Outcome) -> Result(String, Nil) {
  case outcome {
    source.Moved(..) -> Error(Nil)
    source.Thawed(_) -> Ok("thawed")
    source.Fenced -> Ok("fenced")
    source.NotMoved(source.Refused(_)) -> Ok("refused")
    source.NotMoved(source.AdmissionTimeout) -> Ok("admission_timeout")
    source.NotMoved(source.FreezeFailed(_)) -> Ok("freeze_failed")
    source.NotMoved(source.AbortedEarly) -> Ok("aborted_early")
    source.NotMoved(source.LeaseLost) -> Ok("lease_lost")
  }
}

fn record_rollout(
  entity: Key,
  tail_bytes: Int,
  precopied: Bool,
  outcome: source.Outcome,
) -> Nil {
  let kind = rollout_metrics.kind_of_ns(entity.ns)
  case abort_reason(outcome) {
    Error(Nil) -> {
      rollout_metrics.count(
        rollout_metrics.moves_committed,
        [#("kind", kind)],
        1,
      )
      drain_entity(kind, "moved")
      drain_report.entity_moved(kind, entity.id)
      case precopied {
        True ->
          rollout_metrics.count(
            rollout_metrics.precopy_delta_bytes,
            [#("kind", kind)],
            tail_bytes,
          )
        False -> Nil
      }
    }
    Ok(reason) -> aborted(entity, reason)
  }
  case outcome, kind {
    source.Moved(freeze_ms:, ..), _ ->
      rollout_metrics.observe(
        rollout_metrics.move_freeze,
        [
          #("kind", kind),
          #("size_class", rollout_metrics.size_class(tail_bytes)),
        ],
        freeze_ms,
      )
    _, _ -> Nil
  }
  case outcome, kind {
    source.Moved(..), "call" ->
      rollout_metrics.count(
        rollout_metrics.call_moves,
        [#("outcome", "survived")],
        1,
      )
    source.Fenced, "call" ->
      rollout_metrics.count(
        rollout_metrics.call_moves,
        [#("outcome", "ended")],
        1,
      )
    _, _ -> Nil
  }
}

fn record_outcome(
  entity: Key,
  version: Int,
  tail_bytes: Int,
  outcome: source.Outcome,
) -> Nil {
  let kind = [#("ns", int.to_string(entity.ns))]
  let label = case outcome {
    source.Moved(freeze_ms:, ..) -> {
      metrics.observe_ms(
        "fluxer_gateway_handoff_freeze_ms",
        [#("size", size_class(tail_bytes)), ..kind],
        freeze_ms,
      )
      logging.log(
        logging.Info,
        "handoff moved ns="
          <> int.to_string(entity.ns)
          <> " id="
          <> int.to_string(entity.id)
          <> " codec="
          <> int.to_string(version)
          <> " tail_bytes="
          <> int.to_string(tail_bytes)
          <> " freeze_ms="
          <> int.to_string(freeze_ms),
      )
      "moved"
    }
    source.Thawed(_) -> "thawed"
    source.Fenced -> "fenced"
    source.NotMoved(_) -> "not_moved"
  }
  case label {
    "moved" -> Nil
    _ ->
      logging.log(
        logging.Warning,
        "handoff "
          <> label
          <> " ns="
          <> int.to_string(entity.ns)
          <> " id="
          <> int.to_string(entity.id)
          <> " "
          <> string.inspect(outcome),
      )
  }
  metrics.counter(
    "fluxer_gateway_handoff_moves_total",
    [#("outcome", label), ..kind],
    1,
  )
}

fn size_class(bytes: Int) -> String {
  case bytes {
    _ if bytes < 65_536 -> "under_64k"
    _ if bytes < 1_048_576 -> "under_1m"
    _ if bytes < 8_388_608 -> "under_8m"
    _ -> "over_8m"
  }
}

pub const precopy_duration = "fluxer_gateway_handoff_precopy_duration_ms"

pub fn phase(name: String) -> Nil {
  metrics.counter("fluxer_gateway_handoff_phase_total", [#("phase", name)], 1)
}

fn entity_result(
  key: Key,
  move: MoveId,
  outcome: source.Outcome,
) -> Result(EntityOutcome, MoveError) {
  case outcome {
    source.Moved(_, to_epoch, freeze_ms) -> Ok(Moved(key, to_epoch, freeze_ms))
    source.Thawed(at) -> Ok(Thawed(key, at))
    source.Fenced -> Ok(ColdAtTarget(key))
    source.NotMoved(source.Refused(reason)) -> Error(Refused(reason))
    source.NotMoved(source.AdmissionTimeout) -> Error(Timeout(move))
    source.NotMoved(source.FreezeFailed(reason)) ->
      Error(Adapter(adapter.EncodeFailed(reason)))
    source.NotMoved(source.AbortedEarly) ->
      Error(Refused(message.RetryAfter(retry_backoff_ms)))
    source.NotMoved(source.LeaseLost) -> Error(NotHolder(key.bucket_of(key)))
  }
}

type Mover {
  Mover(
    inbox: Subject(source.Event),
    machine: Source,
    adapter: Adapter,
    ports: Ports,
    stub: Option(Pid),
    epoch: Epoch,
    version: Int,
    tail_bytes: Int,
    precopy: Option(adapter.Precopy),
    copied: Bool,
    copier: Option(Pid),
    watch: process.Selector(source.Event),
  )
}

fn run_mover(mover: Mover, pending: List(source.Event)) -> source.Outcome {
  case pending, source.phase(mover.machine) {
    _, source.Finished(outcome) -> outcome
    [], _ -> {
      let event = process.selector_receive_forever(mover.watch)
      run_mover(mover, [checked(mover, event)])
    }
    [event, ..rest], before -> {
      let #(machine, effects) =
        source.step(mover.machine, event, mover.ports.now_ms())
      trace(mover, before, source.phase(machine))
      let #(mover, follow) =
        list.fold(effects, #(Mover(..mover, machine:), []), fn(acc, effect) {
          let #(next, events) = perform(acc.0, effect)
          #(next, list.append(acc.1, events))
        })
      run_mover(mover, list.append(rest, follow))
    }
  }
}

fn trace(mover: Mover, before: source.Phase, after: source.Phase) -> Nil {
  case phase_name(before) == phase_name(after) {
    True -> Nil
    False ->
      logging.log(
        logging.Info,
        "handoff source id="
          <> int.to_string(source.key(mover.machine).id)
          <> " "
          <> phase_name(before)
          <> " -> "
          <> phase_name(after),
      )
  }
}

fn phase_name(phase: source.Phase) -> String {
  case phase {
    source.Admitting(attempt) -> "admitting/" <> int.to_string(attempt)
    source.Copying -> "copying"
    source.Freezing(aborted) ->
      case aborted {
        True -> "freezing/aborted"
        False -> "freezing"
      }
    source.Streaming -> "streaming"
    source.InDoubt(_) -> "in_doubt"
    source.Releasing(..) -> "releasing"
    source.Sealed(..) -> "sealed"
    source.Finished(_) -> "finished"
  }
}

fn checked(mover: Mover, event: source.Event) -> source.Event {
  case event {
    source.Forwarded(_) -> {
      rollout_metrics.count(
        rollout_metrics.stub_hits,
        [
          #("kind", rollout_metrics.kind_of_ns(source.key(mover.machine).ns)),
          #("outcome", "forwarded"),
        ],
        1,
      )
      event
    }
    source.Fired(_) ->
      case mover.ports.holding(key.bucket_of(source.key(mover.machine))) {
        Ok(_) -> event
        Error(Nil) ->
          case source.phase(mover.machine) {
            source.Copying
            | source.Streaming
            | source.InDoubt(_)
            | source.Freezing(_) -> source.LeaseLapsed
            _ -> event
          }
      }
    _ -> event
  }
}

fn perform(
  mover: Mover,
  effect: source.Effect,
) -> #(Mover, List(source.Event)) {
  let entity = source.key(mover.machine)
  case effect {
    source.ToHome(message) -> {
      let _ = mover.ports.to_home(key.bucket_of(entity), message)
      case message {
        message.BlobChunk(part: blob.Tail, index:, count:, ..)
          if index + 1 == count
        -> phase("blob_sent")
        _ -> Nil
      }
      #(mover, [])
    }
    source.RunPrecopy -> start_precopy(mover, entity)
    source.RunFreeze ->
      case mover.ports.holding(key.bucket_of(entity)) {
        Error(Nil) -> #(mover, [source.FreezeReady(Error("lease lost"))])
        Ok(_) -> freeze(mover, entity)
      }
    source.Commit(target_ref, to_epoch) ->
      case mover.stub {
        Some(stub) -> {
          let _ = mover.adapter.committed(stub, target_ref, to_epoch)
          case process.is_alive(stub) {
            True -> #(mover, [])
            False -> #(mover, [source.Repointed])
          }
        }
        None -> #(mover, [source.Repointed])
      }
    source.Thaw(entries) -> {
      with_stub(mover, fn(stub) {
        let _ = mover.adapter.thaw(stub, entries)
        Nil
      })
      #(mover, [])
    }
    source.Abandon -> {
      mover.adapter.abandon(entity)
      #(mover, [])
    }
    source.Fence -> {
      with_stub(mover, mover.adapter.abort_arrival)
      #(mover, [])
    }
    source.After(timer, ms) -> {
      process.send_after(mover.inbox, ms, source.Fired(timer))
      #(mover, [])
    }
    source.Done(outcome) -> {
      stop_copier(mover)
      record_outcome(entity, mover.version, mover.tail_bytes, outcome)
      record_rollout(entity, mover.tail_bytes, mover.copied, outcome)
      case outcome, mover.precopy {
        source.Moved(..), _ | _, None -> Nil
        _, Some(_) if !mover.copied -> Nil
        _, Some(hooks) -> {
          let move = source.move_id(mover.machine)
          hooks.cancel(entity, move)
          precopy.drop(source.target(mover.machine), move)
        }
      }
      #(mover, [])
    }
  }
}

fn freeze(mover: Mover, entity: Key) -> #(Mover, List(source.Event)) {
  case
    mover.adapter.freeze(
      entity,
      source.move_id(mover.machine),
      mover.version,
      mover.epoch,
    )
  {
    Ok(frozen) -> #(
      Mover(
        ..mover,
        stub: Some(frozen.stub),
        tail_bytes: bit_array.byte_size(frozen.tail),
        watch: process.select_specific_monitor(
          mover.watch,
          process.monitor(frozen.stub),
          fn(_) { source.Repointed },
        ),
      ),
      [source.FreezeReady(Ok(frozen.tail))],
    )
    Error(error) -> #(mover, [
      source.FreezeReady(Error(adapter.describe(error))),
    ])
  }
}

fn stop_copier(mover: Mover) -> Nil {
  case mover.copier {
    Some(copier) -> {
      process.unlink(copier)
      process.kill(copier)
    }
    None -> Nil
  }
}

fn start_precopy(mover: Mover, entity: Key) -> #(Mover, List(source.Event)) {
  case mover.precopy {
    None -> #(mover, [source.PrecopyDone(Error("no precopy"))])
    Some(hooks) -> {
      let inbox = mover.inbox
      let copier =
        process.spawn(fn() {
          process.send(inbox, run_precopy(mover, hooks, entity))
        })
      #(Mover(..mover, copied: True, copier: Some(copier)), [])
    }
  }
}

fn run_precopy(
  mover: Mover,
  hooks: adapter.Precopy,
  entity: Key,
) -> source.Event {
  let started = mover.ports.now_ms()
  let outcome =
    precopy.run(
      hooks,
      entity,
      source.move_id(mover.machine),
      mover.version,
      source.target(mover.machine),
      precopy.default_plan(),
      mover.ports.now_ms,
    )
  let elapsed = mover.ports.now_ms() - started
  let kind = [#("ns", int.to_string(entity.ns))]
  metrics.observe_ms(
    precopy_duration,
    [
      #("kind", rollout_metrics.kind_of_ns(entity.ns)),
      #("outcome", case outcome {
        Ok(_) -> "staged"
        Error(_) -> "failed"
      }),
    ],
    elapsed,
  )
  case outcome {
    Ok(copied) -> {
      metrics.observe_ms(
        "fluxer_gateway_handoff_precopy_ms",
        [#("size", size_class(copied.bulk_bytes)), ..kind],
        elapsed,
      )
      metrics.counter(
        "fluxer_gateway_handoff_precopy_total",
        [#("outcome", "staged"), ..kind],
        1,
      )
      record_precopy(entity, "staged", copied.rounds, copied.bulk_bytes)
      logging.log(
        logging.Info,
        "handoff precopy staged ns="
          <> int.to_string(entity.ns)
          <> " id="
          <> int.to_string(entity.id)
          <> " bulk_bytes="
          <> int.to_string(copied.bulk_bytes)
          <> " rounds="
          <> int.to_string(copied.rounds)
          <> " last_changes="
          <> int.to_string(copied.last_changes)
          <> " precopy_ms="
          <> int.to_string(elapsed),
      )
      source.PrecopyDone(Ok(copied.rounds))
    }
    Error(reason) -> {
      metrics.counter(
        "fluxer_gateway_handoff_precopy_total",
        [#("outcome", "failed"), ..kind],
        1,
      )
      record_precopy(entity, "failed", 0, 0)
      logging.log(
        logging.Warning,
        "handoff precopy failed ns="
          <> int.to_string(entity.ns)
          <> " id="
          <> int.to_string(entity.id)
          <> " "
          <> reason,
      )
      source.PrecopyDone(Error(reason))
    }
  }
}

fn record_precopy(
  entity: Key,
  outcome: String,
  rounds: Int,
  bytes: Int,
) -> Nil {
  let kind = [#("kind", rollout_metrics.kind_of_ns(entity.ns))]
  rollout_metrics.count(
    rollout_metrics.precopy_outcomes,
    [#("outcome", outcome), ..kind],
    1,
  )
  rollout_metrics.count(rollout_metrics.precopy_rounds, kind, rounds)
  rollout_metrics.count(rollout_metrics.precopy_bytes, kind, bytes)
}

fn with_stub(mover: Mover, run: fn(Pid) -> Nil) -> Nil {
  case mover.stub {
    Some(stub) -> run(stub)
    None -> Nil
  }
}

type Closed {
  Closed(
    entry: Adapter,
    bucket: BucketId,
    at: Epoch,
    to: WorkerId,
    to_epoch: Epoch,
    ids: List(Int),
  )
}

type Attempted {
  Settled(outcome: EntityOutcome)
  Retry(outcome: EntityOutcome, attempt: Int)
}

fn run_bucket(
  engine: Engine,
  adapters: Dict(Int, Adapter),
  ports: Ports,
  limits: Limits,
  bucket: BucketId,
  to: WorkerId,
) -> Result(BucketOutcome, MoveError) {
  use closed <- result.map(close(adapters, ports, bucket, to))
  let deadline = ports.now_ms() + limits.retry_budget_ms
  closed.ids
  |> list.map(fn(id) { Retry(Thawed(Key(ns: bucket.ns, id:), closed.at), 0) })
  |> attempt_all(engine, ports, limits, closed, _, deadline)
  |> settle(ports, closed, _)
}

fn close(
  adapters: Dict(Int, Adapter),
  ports: Ports,
  bucket: BucketId,
  to: WorkerId,
) -> Result(Closed, MoveError) {
  use entry <- result.try(
    dict.get(adapters, bucket.ns) |> result.replace_error(NoAdapter(bucket.ns)),
  )
  use at <- result.try(
    ports.holding(bucket) |> result.replace_error(NotHolder(bucket)),
  )
  let ids = entry.entities_in(bucket.bucket)
  use to_epoch <- result.map(
    ports.close_bucket(bucket, at, to, ids) |> result.map_error(Refused),
  )
  Closed(entry:, bucket:, at:, to:, to_epoch:, ids:)
}

fn attempt_all(
  engine: Engine,
  ports: Ports,
  limits: Limits,
  closed: Closed,
  attempts: List(Attempted),
  deadline: Int,
) -> List(Attempted) {
  let batch =
    int.max(limits.entities_in_flight / int.max(limits.buckets_in_flight, 1), 1)
  let still_here = fn(id) {
    list.contains(closed.entry.entities_in(closed.bucket.bucket), id)
  }
  window(attempts, batch, fn(found) {
    case found {
      Settled(_) -> found
      Retry(outcome, attempt) ->
        move_with_retry(
          engine,
          ports,
          outcome.key,
          closed.at,
          closed.to,
          fn() { still_here(outcome.key.id) },
          deadline,
          attempt,
        )
    }
  })
}

fn settle(
  ports: Ports,
  closed: Closed,
  attempts: List(Attempted),
) -> BucketOutcome {
  let outcomes = list.map(attempts, fn(found) { found.outcome })
  case list.any(outcomes, stayed) {
    True ->
      logging.log(
        logging.Warning,
        "handoff bucket "
          <> int.to_string(closed.bucket.bucket)
          <> " kept entities that did not move, it stays open and is retried",
      )
    False -> ports.bucket_emptied(closed.bucket, closed.at)
  }
  BucketOutcome(
    bucket: closed.bucket,
    to: closed.to,
    epoch: closed.to_epoch,
    entities: outcomes,
  )
}

fn stayed(outcome: EntityOutcome) -> Bool {
  case outcome {
    Thawed(..) -> True
    Moved(..) | ColdAtTarget(..) | Departed(..) -> False
  }
}

fn move_with_retry(
  engine: Engine,
  ports: Ports,
  entity: Key,
  at: Epoch,
  to: WorkerId,
  still_here: fn() -> Bool,
  deadline: Int,
  attempt: Int,
) -> Attempted {
  let #(outcome, retry) = case move_entity(engine, entity, at, to) {
    Ok(found) -> #(found, True)
    Error(error) -> #(Thawed(entity, at), retryable(error))
  }
  case outcome, retry {
    Thawed(..), False -> Settled(outcome)
    Thawed(..), True ->
      case still_here(), ports.holding(key.bucket_of(entity)) {
        False, _ -> {
          drain_report.entity_moved(
            rollout_metrics.kind_of_ns(entity.ns),
            entity.id,
          )
          Settled(Departed(entity))
        }
        _, Error(Nil) -> Settled(outcome)
        True, Ok(_) -> {
          let wait = backoff(attempt)
          case ports.now_ms() + wait >= deadline {
            True -> Retry(outcome, attempt + 1)
            False -> {
              metrics.counter(
                "fluxer_gateway_handoff_retries_total",
                [#("ns", int.to_string(entity.ns))],
                1,
              )
              process.sleep(wait)
              move_with_retry(
                engine,
                ports,
                entity,
                at,
                to,
                still_here,
                deadline,
                attempt + 1,
              )
            }
          }
        }
      }
    _, _ -> Settled(outcome)
  }
}

fn retryable(error: MoveError) -> Bool {
  case error {
    Refused(message.TargetNotEligible)
    | Refused(message.NoCommonCodec)
    | Refused(message.EpochMismatch)
    | Refused(message.WrongEpoch(_))
    | NoAdapter(_)
    | EngineStartFailed -> False
    Refused(_)
    | NotHolder(_)
    | NoTarget(_)
    | Adapter(_)
    | Timeout(_)
    | InDoubt(_) -> True
  }
}

fn backoff(attempt: Int) -> Int {
  let base =
    int.min(
      retry_backoff_ms * int.bitwise_shift_left(1, int.min(attempt, 5)),
      retry_backoff_cap_ms,
    )
  base + int.random(retry_backoff_ms)
}

type FirstPass {
  Done(outcome: BucketOutcome)
  Pending(closed: Closed, attempts: List(Attempted))
}

fn run_drain(
  engine: Engine,
  adapters: Dict(Int, Adapter),
  ports: Ports,
  limits: Limits,
  until: Int,
) -> Result(List(BucketOutcome), MoveError) {
  let first =
    window(ports.held(), limits.buckets_in_flight, fn(bucket) {
      use to <- result.try(
        ports.place(bucket) |> result.replace_error(NoTarget(bucket)),
      )
      use closed <- result.map(close(adapters, ports, bucket, to))
      let attempts =
        closed.ids
        |> list.map(fn(id) {
          Retry(Thawed(Key(ns: bucket.ns, id:), closed.at), 0)
        })
        |> attempt_all(engine, ports, limits, closed, _, ports.now_ms())
      case list.any(attempts, is_retry) {
        True -> Pending(closed, attempts)
        False -> Done(settle(ports, closed, attempts))
      }
    })
  let deadline = int.min(ports.now_ms() + limits.retry_budget_ms, until)
  let settled =
    window(first, limits.buckets_in_flight, fn(found) {
      use pass <- result.map(found)
      case pass {
        Done(outcome) -> outcome
        Pending(closed, attempts) ->
          attempt_all(engine, ports, limits, closed, attempts, deadline)
          |> settle(ports, closed, _)
      }
    })
  case result.partition(settled) {
    #(done, []) -> Ok(list.reverse(done))
    #(_, [error, ..]) -> Error(error)
  }
}

fn is_retry(found: Attempted) -> Bool {
  case found {
    Retry(..) -> True
    Settled(_) -> False
  }
}

fn window(items: List(a), size: Int, run: fn(a) -> b) -> List(b) {
  let reply = process.new_subject()
  let indexed = list.index_map(items, fn(item, index) { #(index, item) })
  let #(first, waiting) = list.split(indexed, int.max(size, 1))
  list.each(first, start_in_window(reply, run, _))
  collect(reply, run, waiting, list.length(items), [])
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.map(fn(pair) { pair.1 })
}

fn start_in_window(
  reply: Subject(#(Int, b)),
  run: fn(a) -> b,
  item: #(Int, a),
) -> Nil {
  process.spawn(fn() { process.send(reply, #(item.0, run(item.1))) })
  Nil
}

fn collect(
  reply: Subject(#(Int, b)),
  run: fn(a) -> b,
  waiting: List(#(Int, a)),
  remaining: Int,
  acc: List(#(Int, b)),
) -> List(#(Int, b)) {
  case remaining {
    0 -> acc
    _ -> {
      let done = process.receive_forever(reply)
      let waiting = case waiting {
        [next, ..rest] -> {
          start_in_window(reply, run, next)
          rest
        }
        [] -> []
      }
      collect(reply, run, waiting, remaining - 1, [done, ..acc])
    }
  }
}
