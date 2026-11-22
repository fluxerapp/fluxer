// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain_report.{type Report}
import fluxer_gateway/cluster/bouncer_client
import fluxer_gateway/cluster/handoff
import fluxer_gateway/cluster/router
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/gateway/socket
import fluxer_gateway/guild/manager/registry as guild_registry
import fluxer_gateway/handoff/service
import fluxer_gateway/presence/pids
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/transfer as session_transfer
import gleam/erlang/process
import gleam/int
import gleam/result
import logging

fn frame_flush_ms() -> Int {
  durations.app().drain_frame_flush_ms
}

fn handoff_wait_ms() -> Int {
  durations.app().drain_handoff_wait_ms
}

fn socket_notice_delay_ms() -> Int {
  durations.app().drain_socket_notice_delay_ms
}

fn deadline_ms() -> Int {
  durations.app().drain_deadline_ms
}

const session_retry_ms = 1000

const outcome_poll_ms = 200

const socket_poll_ms = 20

type Key {
  FluxerGatewaySocketsNotified
}

type Stored

@external(erlang, "persistent_term", "put")
fn put_term(key: Key, value: Bool) -> Stored

@external(erlang, "persistent_term", "get")
fn get_term(key: Key, default: Bool) -> Bool

fn sockets_notified() -> Bool {
  get_term(FluxerGatewaySocketsNotified, False)
}

fn set_sockets_notified(notified: Bool) -> Nil {
  put_term(FluxerGatewaySocketsNotified, notified)
  Nil
}

pub type UndrainError {
  HandoffInFlight
  UndrainUnavailable
}

pub type Holdings {
  Holdings(
    sessions: Int,
    owned_sessions: Int,
    guilds: Int,
    presences: Int,
    buckets: Int,
    sockets: Int,
    calls: Int,
    push_jobs: Int,
  )
}

pub fn holdings() -> Holdings {
  let role = settings.get().role
  let count_if = fn(wanted, count: fn() -> Int) {
    case node.runs(role, wanted) {
      True -> count()
      False -> 0
    }
  }
  Holdings(
    sessions: count_if(node.Sessions, registry.count),
    owned_sessions: count_if(node.Sessions, registry.owned),
    guilds: count_if(node.Guilds, guild_registry.count),
    presences: count_if(node.Presence, pids.count),
    buckets: service.buckets_held(),
    sockets: case handoff.holds_sockets() {
      True -> socket.lingering()
      False -> 0
    },
    calls: service.calls_left(),
    push_jobs: service.push_jobs_left(),
  )
}

pub fn left() -> drain_report.Left {
  let held = holdings()
  drain_report.Left(
    entities: held.sessions
      + held.guilds
      + held.presences
      + held.sockets
      + held.calls
      + held.push_jobs,
    buckets: held.buckets,
  )
}

fn bouncer_done() -> Bool {
  service.drained() && sockets_notified() && !holds_sessions()
}

pub fn run() -> Nil {
  case router.bouncer_mode() {
    True -> run_bouncer()
    False -> run_membership()
  }
}

fn run_bouncer() -> Nil {
  let _ = drain_report.begin()
  service.drain()
  let deadline = clock.monotonic_ms() + handoff_wait_ms()
  await_bouncer_drained(deadline)
  move_sessions(deadline)
  case sockets_notified() {
    True -> Nil
    False -> notify_sockets()
  }
  let _ = drain_report.settle(left(), bouncer_done(), True)
  let _ = bouncer_client.bye()
  Nil
}

fn start_bouncer() -> Nil {
  let already = draining.is_draining()
  let serial = drain_report.begin()
  service.drain()
  case already {
    True -> Nil
    False -> {
      set_sockets_notified(False)
      process.spawn_unlinked(fn() {
        let started = clock.monotonic_ms()
        await_bouncer_drained(started + handoff_wait_ms())
        move_sessions(started + socket_notice_delay_ms())
        case handoff.holds_sockets() {
          True ->
            process.sleep(int.max(
              0,
              started + socket_notice_delay_ms() - clock.monotonic_ms(),
            ))
          False -> Nil
        }
        case draining.is_draining() {
          True -> {
            notify_sockets()
            move_sessions(started + deadline_ms())
          }
          False -> Nil
        }
        await_outcome(serial, started + deadline_ms())
      })
      Nil
    }
  }
}

fn await_outcome(serial: Int, deadline: Int) -> Nil {
  case draining.is_draining() && drain_report.serial() == serial {
    False -> Nil
    True -> {
      let final = clock.monotonic_ms() >= deadline
      case drain_report.settle(left(), bouncer_done(), final).phase {
        drain_report.Draining -> {
          process.sleep(outcome_poll_ms)
          await_outcome(serial, deadline)
        }
        drain_report.Idle | drain_report.Drained | drain_report.Failed -> Nil
      }
    }
  }
}

fn holds_sessions() -> Bool {
  node.runs(settings.get().role, node.Sessions) && registry.count() > 0
}

fn move_sessions(deadline: Int) -> Nil {
  case holds_sessions() {
    False -> Nil
    True -> {
      let left = registry.count()
      let moved = session_transfer.export_spread(service.session_targets())
      logging.log(
        logging.Info,
        "Gateway drain: moved "
          <> int.to_string(moved)
          <> " of "
          <> int.to_string(left)
          <> " sessions",
      )
      case
        draining.is_draining()
        && clock.monotonic_ms() + session_retry_ms < deadline
      {
        True -> {
          process.sleep(session_retry_ms)
          move_sessions(deadline)
        }
        False -> Nil
      }
    }
  }
}

fn notify_sockets() -> Nil {
  case handoff.holds_sockets() {
    True -> {
      handoff.reconnect_sockets()
      process.sleep(frame_flush_ms())
      await_sockets_closed(
        clock.monotonic_ms() + durations.gateway().socket_reconnect_linger_ms,
      )
    }
    False -> Nil
  }
  set_sockets_notified(True)
}

fn await_sockets_closed(deadline: Int) -> Nil {
  case socket.lingering() == 0 || clock.monotonic_ms() >= deadline {
    True -> Nil
    False -> {
      process.sleep(socket_poll_ms)
      await_sockets_closed(deadline)
    }
  }
}

fn await_bouncer_drained(deadline: Int) -> Nil {
  case service.drained() || clock.monotonic_ms() >= deadline {
    True -> Nil
    False -> {
      process.sleep(200)
      await_bouncer_drained(deadline)
    }
  }
}

pub fn outcome() -> Result(Report, Nil) {
  case router.bouncer_mode() {
    True -> Ok(drain_report.settle(left(), bouncer_done(), False))
    False -> Error(Nil)
  }
}

pub fn drained() -> Result(Bool, Nil) {
  outcome()
  |> result.map(fn(report) { report.phase == drain_report.Drained })
}

fn run_membership() -> Nil {
  case draining.is_draining() {
    True -> Nil
    False -> {
      announce()
      let _ = settle("start")
      Nil
    }
  }
  handoff.redrain()
  let settled = settle("shutdown")
  let _ = drain_report.settle(left(), settled, True)
  handoff.reconnect_sockets()
  logging.log(logging.Info, "Gateway shutdown drain: notified clients")
  process.sleep(frame_flush_ms())
}

pub fn start() -> Nil {
  case router.bouncer_mode() {
    True -> start_bouncer()
    False -> start_membership()
  }
}

fn start_membership() -> Nil {
  announce()
  process.spawn_unlinked(fn() {
    let _ = settle("start")
    case draining.is_draining() {
      True -> handoff.reconnect_sockets()
      False -> Nil
    }
  })
  Nil
}

fn announce() -> Nil {
  let _ = drain_report.begin()
  draining.set_draining()
  handoff.drain_dispatch()
}

fn settle(phase: String) -> Bool {
  let settled = handoff.await_drained(handoff_wait_ms())
  case settled {
    True -> Nil
    False ->
      logging.log(
        logging.Warning,
        "Gateway drain: handoff still running at " <> phase <> " deadline",
      )
  }
  settled
}

pub fn undrain() -> Result(Nil, UndrainError) {
  case handoff.undrain() {
    Ok(True) -> {
      set_sockets_notified(False)
      drain_report.reset()
      Ok(Nil)
    }
    Ok(False) -> Error(HandoffInFlight)
    Error(Nil) -> Error(UndrainUnavailable)
  }
}
