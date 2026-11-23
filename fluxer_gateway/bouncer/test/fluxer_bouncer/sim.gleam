// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/codec_range.{CodecRange}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease.{type BucketRecord, Claim}
import bouncer_proto/message.{
  type PeerMessage, type ToBouncer, type ToMover, type ToWorker, Beat, BeatAck,
  BucketDelta, FromPeer, Granted, Hello, WorkerBeat, WorkerClaim, WorkerHello,
}
import bouncer_proto/rendezvous
import bouncer_proto/worker.{type Profile, type WorkerId, Profile, WorkerId}
import fluxer_bouncer/config.{type Config, type Timings}
import fluxer_bouncer/core
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/set.{type Set}

pub type Worker {
  Worker(
    id: WorkerId,
    profile: Profile,
    seq: Int,
    alive: Bool,
    roster_hash: Int,
    held: Dict(BucketId, Epoch),
  )
}

pub type Sim {
  Sim(
    count: Int,
    configs: Dict(Int, Config),
    bouncers: Dict(Int, core.State),
    incarnations: Dict(Int, Int),
    now: Int,
    blocked: Set(#(Int, Int)),
    losing: fn(Int, Int, PeerMessage) -> Bool,
    deaf: Set(#(Atom, Int)),
    workers: Dict(Atom, Worker),
    log: List(#(Atom, Int, ToWorker)),
    movers: List(#(Atom, Int, ToMover)),
    last_beat: Int,
  )
}

type Delivery {
  Delivery(to: Int, input: core.Input)
}

pub fn timings() -> Timings {
  config.default_timings()
}

pub fn profile() -> Profile {
  Profile(
    role: "guilds",
    pool: "default",
    build: "a",
    template_hash: "t1",
    wire_epoch: 1,
    codecs: [CodecRange(ns: 0, oldest_read: 1, max_read: 1, max_write: 1)],
  )
}

fn config_for(
  count: Int,
  ordinal: Int,
  quorum: Int,
  timings: Timings,
) -> Config {
  let assert Ok(built) =
    config.build(
      hostname: "b-" <> int.to_string(ordinal),
      count: int.to_string(count),
      basename: "b",
      dns_name: "sim",
    )
  config.Config(..built, quorum: quorum, timings: timings)
}

pub fn node_of(sim: Sim, ordinal: Int) -> Atom {
  let assert Ok(found) = dict.get(sim.configs, ordinal)
  config.peer_node(found, ordinal)
}

pub fn new(count: Int, quorum: Int, timings: Timings) -> Sim {
  let configs =
    int.range(from: 0, to: count, with: [], run: fn(acc, ordinal) {
      [#(ordinal, config_for(count, ordinal, quorum, timings)), ..acc]
    })
    |> dict.from_list
  let sim =
    Sim(
      count: count,
      configs: configs,
      bouncers: dict.new(),
      incarnations: dict.new(),
      now: 0,
      blocked: set.new(),
      losing: keep_all,
      deaf: set.new(),
      workers: dict.new(),
      log: [],
      movers: [],
      last_beat: 0,
    )
  int.range(from: 0, to: count, with: sim, run: boot)
}

fn boot(sim: Sim, ordinal: Int) -> Sim {
  let incarnation =
    {
      dict.get(sim.incarnations, ordinal)
      |> option.from_result
      |> option.unwrap(0)
    }
    + 1
  let assert Ok(found) = dict.get(sim.configs, ordinal)
  let state = core.new(found, incarnation, sim.now, 1_000_000 + sim.now)
  let sim =
    Sim(
      ..sim,
      bouncers: dict.insert(sim.bouncers, ordinal, state),
      incarnations: dict.insert(sim.incarnations, ordinal, incarnation),
    )
  dict.keys(sim.bouncers)
  |> list.filter(fn(other) {
    other != ordinal && !set.contains(sim.blocked, #(ordinal, other))
  })
  |> list.fold(sim, fn(sim, other) {
    sim
    |> deliver([
      Delivery(ordinal, core.NodeUp(node_of(sim, other))),
      Delivery(other, core.NodeUp(node_of(sim, ordinal))),
    ])
  })
}

pub fn deliver_input(sim: Sim, ordinal: Int, input: core.Input) -> Sim {
  deliver(sim, [Delivery(ordinal, input)])
}

fn deliver(sim: Sim, batch: List(Delivery)) -> Sim {
  case batch {
    [] -> sim
    _ -> {
      let #(sim, next) =
        list.fold(batch, #(sim, []), fn(acc, delivery) {
          let #(sim, next) = acc
          case dict.get(sim.bouncers, delivery.to) {
            Error(Nil) -> acc
            Ok(state) -> {
              let #(state, effects) = core.step(state, delivery.input, sim.now)
              let sim =
                Sim(
                  ..sim,
                  bouncers: dict.insert(sim.bouncers, delivery.to, state),
                )
              list.fold(effects, #(sim, next), fn(acc, effect) {
                route(acc.0, acc.1, delivery.to, effect)
              })
            }
          }
        })
      deliver(sim, list.reverse(next))
    }
  }
}

fn route(
  sim: Sim,
  next: List(Delivery),
  from: Int,
  effect: core.Effect,
) -> #(Sim, List(Delivery)) {
  case effect {
    core.ToPeer(to, message) ->
      case
        set.contains(sim.blocked, #(from, to))
        || !dict.has_key(sim.bouncers, to)
        || sim.losing(from, to, message)
      {
        True -> #(sim, next)
        False -> #(sim, [
          Delivery(to, core.Received(FromPeer(from, message))),
          ..next
        ])
      }
    core.ToWorker(node, message) ->
      case set.contains(sim.deaf, #(node, from)) {
        True -> #(sim, next)
        False -> #(
          observe(
            Sim(..sim, log: [#(node, from, message), ..sim.log]),
            node,
            message,
          ),
          next,
        )
      }
    core.ToMover(node, message) ->
      case set.contains(sim.deaf, #(node, from)) {
        True -> #(sim, next)
        False -> #(
          Sim(..sim, movers: [#(node, from, message), ..sim.movers]),
          next,
        )
      }
  }
}

fn observe(sim: Sim, node: Atom, message: ToWorker) -> Sim {
  case dict.get(sim.workers, node) {
    Error(Nil) -> sim
    Ok(found) -> {
      let found = case message {
        BeatAck(roster: roster, ..) -> Worker(..found, roster_hash: roster.hash)
        Granted(bucket, epoch, _) ->
          Worker(..found, held: dict.insert(found.held, bucket, epoch))
        BucketDelta(records: records, ..) ->
          list.fold(records, found, fn(found, record) { learn(found, record) })
        _ -> found
      }
      Sim(..sim, workers: dict.insert(sim.workers, node, found))
    }
  }
}

fn learn(found: Worker, record: BucketRecord) -> Worker {
  case record.holder == Some(found.id), record.state {
    True, lease.Held ->
      Worker(
        ..found,
        held: dict.insert(found.held, record.bucket, record.epoch),
      )
    _, lease.Moving(to: to, to_epoch: to_epoch, ..) if to == found.id ->
      Worker(..found, held: dict.insert(found.held, record.bucket, to_epoch))
    True, _ -> found
    False, _ -> Worker(..found, held: dict.delete(found.held, record.bucket))
  }
}

pub fn reachable(sim: Sim, node: Atom) -> List(Int) {
  dict.keys(sim.bouncers)
  |> list.filter(fn(ordinal) { !set.contains(sim.deaf, #(node, ordinal)) })
  |> list.sort(int.compare)
}

pub fn send(sim: Sim, node: Atom, ordinal: Int, message: ToBouncer) -> Sim {
  case set.contains(sim.deaf, #(node, ordinal)) {
    True -> sim
    False -> deliver_input(sim, ordinal, core.Received(message))
  }
}

pub fn send_all(sim: Sim, node: Atom, message: ToBouncer) -> Sim {
  list.fold(reachable(sim, node), sim, fn(sim, ordinal) {
    send(sim, node, ordinal, message)
  })
}

pub fn home(sim: Sim, node: Atom, bucket: BucketId) -> Int {
  let assert Ok(homes) = rendezvous.homes(bucket, reachable(sim, node))
  homes.home
}

pub fn send_home(
  sim: Sim,
  node: Atom,
  bucket: BucketId,
  message: ToBouncer,
) -> Sim {
  send(sim, node, home(sim, node, bucket), message)
}

pub fn add_worker(
  sim: Sim,
  name: String,
  profile: Profile,
) -> #(Sim, WorkerId) {
  let node = atom.create(name)
  let id = WorkerId(node: node, incarnation: sim.now + 1)
  let found =
    Worker(
      id: id,
      profile: profile,
      seq: 0,
      alive: True,
      roster_hash: 0,
      held: dict.new(),
    )
  let sim = Sim(..sim, workers: dict.insert(sim.workers, node, found))
  #(beat(hello(sim, node), node), id)
}

pub fn hello(sim: Sim, node: Atom) -> Sim {
  let found = worker(sim, node)
  send_all(
    sim,
    node,
    WorkerHello(
      Hello(
        worker: found.id,
        profile: found.profile,
        protocol: bouncer_proto.own_range(),
        pinned: [],
      ),
    ),
  )
}

pub fn worker(sim: Sim, node: Atom) -> Worker {
  let assert Ok(found) = dict.get(sim.workers, node)
  found
}

pub fn digest(found: Worker) -> Int {
  found.held
  |> dict.to_list
  |> list.map(fn(pair) { Claim(pair.0, pair.1) })
  |> lease.held_digest
}

pub fn claim(sim: Sim, node: Atom) -> Sim {
  let found = worker(sim, node)
  send_all(
    sim,
    node,
    WorkerClaim(
      found.id,
      found.held |> dict.to_list |> list.map(fn(pair) { Claim(pair.0, pair.1) }),
    ),
  )
}

pub fn beat(sim: Sim, node: Atom) -> Sim {
  let found = worker(sim, node)
  case found.alive {
    False -> sim
    True -> {
      let found = Worker(..found, seq: found.seq + 1)
      let sim = Sim(..sim, workers: dict.insert(sim.workers, node, found))
      send_all(
        sim,
        node,
        WorkerBeat(
          Beat(
            worker: found.id,
            seq: found.seq,
            sent_mono: sim.now,
            held_digest: digest(found),
            roster_hash: found.roster_hash,
            draining: False,
            load: dict.size(found.held),
            unreachable: [],
          ),
        ),
      )
    }
  }
}

pub fn silence(sim: Sim, node: Atom) -> Sim {
  let found = worker(sim, node)
  Sim(
    ..sim,
    workers: dict.insert(sim.workers, node, Worker(..found, alive: False)),
  )
}

pub fn deafen(sim: Sim, node: Atom, ordinals: List(Int)) -> Sim {
  Sim(
    ..sim,
    deaf: list.fold(ordinals, sim.deaf, fn(deaf, ordinal) {
      set.insert(deaf, #(node, ordinal))
    }),
  )
}

pub fn hear(sim: Sim, node: Atom, ordinals: List(Int)) -> Sim {
  Sim(
    ..sim,
    deaf: list.fold(ordinals, sim.deaf, fn(deaf, ordinal) {
      set.delete(deaf, #(node, ordinal))
    }),
  )
}

pub fn advance(sim: Sim, ms: Int) -> Sim {
  let tick =
    { dict.values(sim.configs) |> list.first |> unwrap_config }.timings.tick_ms
  let beat_every =
    { dict.values(sim.configs) |> list.first |> unwrap_config }.timings.beat_interval_ms
  let target = sim.now + ms
  advance_to(sim, target, tick, beat_every)
}

fn unwrap_config(found: Result(Config, Nil)) -> Config {
  let assert Ok(found) = found
  found
}

fn advance_to(sim: Sim, target: Int, tick: Int, beat_every: Int) -> Sim {
  case sim.now + tick > target {
    True -> sim
    False -> {
      let sim = Sim(..sim, now: sim.now + tick)
      let sim = case sim.now - sim.last_beat >= beat_every {
        False -> sim
        True ->
          dict.keys(sim.workers)
          |> list.fold(Sim(..sim, last_beat: sim.now), beat)
      }
      let sim =
        dict.keys(sim.bouncers)
        |> list.fold(sim, fn(sim, ordinal) {
          deliver_input(sim, ordinal, core.Tick)
        })
      advance_to(sim, target, tick, beat_every)
    }
  }
}

pub fn crash(sim: Sim, ordinal: Int) -> Sim {
  let gone = node_of(sim, ordinal)
  let sim = Sim(..sim, bouncers: dict.delete(sim.bouncers, ordinal))
  dict.keys(sim.bouncers)
  |> list.fold(sim, fn(sim, other) {
    deliver_input(sim, other, core.NodeDown(gone))
  })
}

pub fn restart(sim: Sim, ordinal: Int) -> Sim {
  boot(sim, ordinal)
}

pub fn split(sim: Sim, side: List(Int), other_side: List(Int)) -> Sim {
  list.fold(side, sim, fn(sim, a) {
    list.fold(other_side, sim, fn(sim, b) {
      let sim =
        Sim(
          ..sim,
          blocked: sim.blocked |> set.insert(#(a, b)) |> set.insert(#(b, a)),
        )
      sim
      |> deliver_input(a, core.NodeDown(node_of(sim, b)))
      |> deliver_input(b, core.NodeDown(node_of(sim, a)))
    })
  })
}

fn keep_all(_from: Int, _to: Int, _message: PeerMessage) -> Bool {
  False
}

pub fn lose(sim: Sim, losing: fn(Int, Int, PeerMessage) -> Bool) -> Sim {
  Sim(..sim, losing:)
}

pub fn stop_losing(sim: Sim) -> Sim {
  Sim(..sim, losing: keep_all)
}

pub fn drop_between(sim: Sim, side: List(Int), other_side: List(Int)) -> Sim {
  list.fold(side, sim, fn(sim, a) {
    list.fold(other_side, sim, fn(sim, b) {
      Sim(
        ..sim,
        blocked: sim.blocked |> set.insert(#(a, b)) |> set.insert(#(b, a)),
      )
    })
  })
}

pub fn pass_between(sim: Sim, side: List(Int), other_side: List(Int)) -> Sim {
  list.fold(side, sim, fn(sim, a) {
    list.fold(other_side, sim, fn(sim, b) {
      Sim(
        ..sim,
        blocked: sim.blocked |> set.delete(#(a, b)) |> set.delete(#(b, a)),
      )
    })
  })
}

pub fn heal(sim: Sim, side: List(Int), other_side: List(Int)) -> Sim {
  list.fold(side, sim, fn(sim, a) {
    list.fold(other_side, sim, fn(sim, b) {
      let sim =
        Sim(
          ..sim,
          blocked: sim.blocked |> set.delete(#(a, b)) |> set.delete(#(b, a)),
        )
      deliver(sim, [
        Delivery(a, core.NodeUp(node_of(sim, b))),
        Delivery(b, core.NodeUp(node_of(sim, a))),
      ])
    })
  })
}

pub fn synced(sim: Sim, ordinal: Int) -> Bool {
  case dict.get(sim.bouncers, ordinal) {
    Ok(state) -> core.synced(state)
    Error(Nil) -> False
  }
}

pub fn record(
  sim: Sim,
  ordinal: Int,
  bucket: BucketId,
) -> Result(BucketRecord, Nil) {
  case dict.get(sim.bouncers, ordinal) {
    Ok(state) -> core.record(state, bucket)
    Error(Nil) -> Error(Nil)
  }
}

pub fn bouncer(sim: Sim, ordinal: Int) -> core.State {
  let assert Ok(state) = dict.get(sim.bouncers, ordinal)
  state
}

pub fn to_mover(sim: Sim, node: Atom) -> List(ToMover) {
  sim.movers
  |> list.filter(fn(entry) { entry.0 == node })
  |> list.map(fn(entry) { entry.2 })
  |> list.reverse
}

pub fn to_worker(sim: Sim, node: Atom) -> List(ToWorker) {
  sim.log
  |> list.filter(fn(entry) { entry.0 == node })
  |> list.map(fn(entry) { entry.2 })
  |> list.reverse
}

pub fn clear(sim: Sim) -> Sim {
  Sim(..sim, log: [], movers: [])
}
