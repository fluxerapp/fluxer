// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId, BucketId}
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_gateway/core/node
import fluxer_gateway/handoff/adapter.{type Adapter}
import fluxer_gateway/handoff/inbound
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/ports
import fluxer_gateway/handoff/service
import gleam/erlang/atom
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import support/handoff_toy

pub type Rig {
  Rig(engine: move.Engine, target: Pid)
}

pub type Drained {
  Drained(
    result: Result(List(move.BucketOutcome), move.MoveError),
    elapsed_ms: Int,
  )
}

pub fn buckets(ns: Int, ids: List(Int)) -> List(BucketId) {
  ids
  |> list.map(key.bucket)
  |> list.unique
  |> list.map(fn(bucket) { BucketId(ns:, bucket:) })
}

fn at() -> Epoch {
  let assert Ok(found) = epoch.from_int(5)
  found
}

fn source_worker() -> WorkerId {
  WorkerId(node.to_atom(node.self()), 1)
}

fn target_worker() -> WorkerId {
  WorkerId(node.to_atom(node.self()), 2)
}

pub fn start(entry: Adapter, held: List(BucketId)) -> Rig {
  let home = handoff_toy.homes()
  let to_home = fn(_, found) {
    Ok(process.send(home, handoff_toy.FromWorker(found)))
  }
  let source_ports =
    ports.Ports(
      ..ports.unwired(),
      me: source_worker(),
      to_home:,
      holding: fn(_) { Ok(at()) },
      held: fn() { held },
      place: fn(_) { Ok(target_worker()) },
      target_codecs: fn(_) { [entry.codecs] },
      close_bucket: fn(_, held_at, _, _) { Ok(epoch.next(held_at)) },
    )
  let target_ports =
    ports.Ports(..ports.unwired(), me: target_worker(), to_home:)
  let engine = claim(entry, source_ports)
  let assert Ok(target) =
    inbound.start_with([entry], target_ports, fn(_) { Nil })
  process.send(
    home,
    handoff_toy.Routes(move.from_home(engine, _), fn(found) {
      process.send(target.data, inbound.Incoming(found))
    }),
  )
  Rig(engine:, target: target.pid)
}

fn claim(entry: Adapter, source_ports: ports.Ports) -> move.Engine {
  case move.start_with([entry], service.limits, source_ports) {
    Ok(engine) -> engine
    Error(_) -> {
      process.sleep(5)
      claim(entry, source_ports)
    }
  }
}

pub fn drain(rig: Rig, within_ms: Int) -> Drained {
  let started = now_ms()
  let result = move.drain(rig.engine, within_ms)
  Drained(result:, elapsed_ms: now_ms() - started)
}

pub fn moved_all(drained: Drained, ids: List(Int)) -> Bool {
  case drained.result {
    Error(_) -> False
    Ok(outcomes) -> {
      let moved =
        list.flat_map(outcomes, fn(outcome) { outcome.entities })
        |> list.filter_map(fn(found) {
          case found {
            move.Moved(key:, ..) -> Ok(key.id)
            _ -> Error(Nil)
          }
        })
      list.sort(moved, int.compare) == list.sort(ids, int.compare)
    }
  }
}

pub fn stop(rig: Rig) -> Nil {
  case process.subject_owner(rig.engine.inbox) {
    Ok(pid) -> kill(pid)
    Error(Nil) -> Nil
  }
  kill(rig.target)
}

fn kill(pid: Pid) -> Nil {
  process.unlink(pid)
  process.kill(pid)
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: atom.Atom) -> Int

fn now_ms() -> Int {
  monotonic(atom.create("millisecond"))
}
