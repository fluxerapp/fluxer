// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/push/handoff/adapter.{type Site}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/outbox
import fluxer_gateway/push/server
import gleam/bool
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result
import logging

fn grace_ms() -> Int {
  durations.push().outbox_state_drain_grace_ms
}

pub type Peer {
  Peer(node: Node, site: Site)
}

pub type Outcome {
  Drained
  Failed(left: Int)
}

pub fn hand(from: Site, to: Peer) -> Int {
  outbox.hand_all(from.outbox, outbox.target_at(to.node, to.site.outbox))
  let waiting =
    server.busy_buckets(from.server)
    |> result.unwrap([])
    |> list.flat_map(fn(bucket) {
      server.export_pending(from.server, bucket) |> result.unwrap([])
    })
  case server.requeue_at(to.node, to.site.server, waiting) {
    Ok(Nil) -> {
      metrics.lost_or_moved("moved", list.length(waiting))
      list.length(waiting)
    }
    Error(Nil) -> {
      server.requeue_local(from.server, waiting)
      0
    }
  }
}

pub fn is_idle(at: Site) -> Bool {
  outbox.is_idle(at.outbox) && server.is_idle(at.server)
}

pub fn left(at: Site) -> Int {
  result.unwrap(outbox.pending_jobs(at.outbox), 1)
  + result.unwrap(server.pending_jobs(at.server), 1)
}

pub fn run(
  from: Site,
  peer: fn() -> Result(Peer, Nil),
  active: fn() -> Bool,
  deadline_ms: Int,
  round_ms: Int,
) -> Outcome {
  run_round(from, peer, active, deadline_ms, round_ms, False)
}

fn run_round(
  from: Site,
  peer: fn() -> Result(Peer, Nil),
  active: fn() -> Bool,
  deadline_ms: Int,
  round_ms: Int,
  warned: Bool,
) -> Outcome {
  use <- bool.guard(!active(), Failed(left(from)))
  let warned = case peer(), warned {
    Ok(to), _ -> {
      let _ = hand(from, to)
      False
    }
    Error(Nil), True -> True
    Error(Nil), False -> {
      logging.log(
        logging.Warning,
        "Push drain: no live push owner, running the jobs left here",
      )
      True
    }
  }
  let now = clock.monotonic_ms()
  case is_idle(from), now >= deadline_ms {
    True, _ -> Drained
    False, True -> close(from, peer, active, round_ms)
    False, False -> {
      process.sleep(int.min(round_ms, deadline_ms - now))
      run_round(from, peer, active, deadline_ms, round_ms, warned)
    }
  }
}

pub fn close(
  from: Site,
  peer: fn() -> Result(Peer, Nil),
  active: fn() -> Bool,
  round_ms: Int,
) -> Outcome {
  use <- bool.guard(is_idle(from), Drained)
  case peer() {
    Ok(_) ->
      linger(from, peer, active, clock.monotonic_ms() + grace_ms(), round_ms)
    Error(Nil) -> count_left(from)
  }
}

fn linger(
  from: Site,
  peer: fn() -> Result(Peer, Nil),
  active: fn() -> Bool,
  until_ms: Int,
  round_ms: Int,
) -> Outcome {
  use <- bool.guard(!active(), Failed(left(from)))
  case peer() {
    Ok(to) -> {
      let _ = hand(from, to)
      Nil
    }
    Error(Nil) -> Nil
  }
  let now = clock.monotonic_ms()
  case is_idle(from), now >= until_ms {
    True, _ -> Drained
    False, True -> count_left(from)
    False, False -> {
      process.sleep(int.min(round_ms, until_ms - now))
      linger(from, peer, active, until_ms, round_ms)
    }
  }
}

fn count_left(from: Site) -> Outcome {
  let left =
    answered(fn() { outbox.pending_jobs(from.outbox) })
    + answered(fn() { server.pending_jobs(from.server) })
  metrics.close_drain(left)
  logging.log(
    logging.Error,
    "Push drain: deadline reached with " <> int.to_string(left) <> " jobs left",
  )
  Failed(left)
}

fn answered(ask: fn() -> Result(Int, Nil)) -> Int {
  ask()
  |> result.lazy_or(ask)
  |> result.unwrap(0)
}
