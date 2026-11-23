// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/wire
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/session.{
  type SessionInbox, type SessionRef, type SocketRef, GuildRebound,
  PresenceRebound, SocketDown,
}
import fluxer_gateway/presence/client
import fluxer_gateway/session/control.{type Control}
import fluxer_gateway/session/links
import fluxer_gateway/session/state.{type State, Attached, Detached, PresenceUp}
import gleam/bool
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}

fn socket_wait_ms() -> Int {
  durations.session().server_rebind_ack_wait_ms
}

fn relay_ms() -> Int {
  durations.session().server_move_relay_ms
}

type PresenceWait {
  NoPresence
  AwaitingPresence
  PresenceDone(kept: Bool)
}

type SocketWait {
  NoSocket
  AwaitingSocket(socket: SocketRef, monitor: Monitor, deadline: Int)
}

type Departure {
  Departure(
    target: SessionRef,
    speaks_moves: Bool,
    watch: Monitor,
    seq: Int,
    pending: Set(Int),
    rebound: List(GuildId),
    presence: PresenceWait,
    socket: SocketWait,
    deadline: Int,
  )
}

type Incoming {
  Inbox(SessionInbox)
  Ctl(Control)
  Down(Monitor)
}

pub fn run(state: State, target: SessionRef, speaks_moves: Bool) -> Nil {
  let now = clock.monotonic_ms()
  let rebinding = speaks_moves && option.is_none(state.ready)
  let guilds = case rebinding {
    True ->
      links.linked(state.links)
      |> list.filter(fn(guild_ref) {
        speaks(process.subject_owner(guild_ref.inbox))
      })
    False -> []
  }
  list.each(guilds, fn(guild_ref) {
    core_guild.send(guild_ref, core_guild.SessionMoved(target, state.me))
  })
  let presence = case rebinding, state.presence {
    True, PresenceUp(link, _) ->
      case speaks(client.link_pid(link)) {
        True -> {
          client.repoint(link, target, state.me)
          AwaitingPresence
        }
        False -> PresenceDone(kept: False)
      }
    _, _ -> NoPresence
  }
  let socket = case state.socket {
    Attached(socket, monitor) -> {
      process.send(
        socket.outbox,
        session.Moved(state.id, state.me, target, state.seq),
      )
      AwaitingSocket(socket, monitor, now + socket_wait_ms())
    }
    Detached(_) -> NoSocket
  }
  let watch = case process.subject_owner(target.inbox) {
    Ok(pid) -> process.monitor(pid)
    Error(Nil) -> process.monitor(process.self())
  }
  let departure =
    Departure(
      target:,
      speaks_moves:,
      watch:,
      seq: state.seq,
      pending: guilds
        |> list.map(fn(guild_ref) { id.guild_id_to_int(guild_ref.guild) })
        |> set.from_list,
      rebound: [],
      presence:,
      socket:,
      deadline: now + relay_ms(),
    )
  wait(state, departure)
  process.demonitor_process(watch)
}

fn speaks(owner: Result(Pid, Nil)) -> Bool {
  case owner {
    Ok(pid) -> wire.pid_speaks(pid, wire.move_forms_epoch)
    Error(Nil) -> False
  }
}

fn selector(state: State) -> process.Selector(Incoming) {
  process.new_selector()
  |> process.select_map(state.me.inbox, Inbox)
  |> process.select_map(control.subject(process.self()), Ctl)
  |> process.select_monitors(fn(down) {
    case down {
      process.ProcessDown(monitor:, ..) -> Down(monitor)
      process.PortDown(monitor:, ..) -> Down(monitor)
    }
  })
}

fn settled(departure: Departure) -> Bool {
  set.is_empty(departure.pending)
  && departure.presence != AwaitingPresence
  && departure.socket == NoSocket
}

fn wait(state: State, departure: Departure) -> Nil {
  let now = clock.monotonic_ms()
  case settled(departure), now >= departure.deadline {
    True, _ -> finish(state, departure)
    False, True -> finish(state, release_socket(departure))
    False, False -> {
      let until = case departure.socket {
        AwaitingSocket(deadline:, ..) -> int.min(deadline, departure.deadline)
        NoSocket -> departure.deadline
      }
      case process.selector_receive(selector(state), int.max(0, until - now)) {
        Error(Nil) -> wait(state, expire_socket(departure))
        Ok(Down(monitor)) if monitor == departure.watch -> Nil
        Ok(incoming) -> wait(state, take(state, departure, incoming))
      }
    }
  }
}

fn expire_socket(departure: Departure) -> Departure {
  let now = clock.monotonic_ms()
  case departure.socket {
    AwaitingSocket(deadline:, ..) if deadline <= now -> release_socket(departure)
    _ -> departure
  }
}

fn release_socket(departure: Departure) -> Departure {
  case departure.socket {
    AwaitingSocket(socket:, monitor:, ..) -> {
      process.demonitor_process(monitor)
      control.send(departure.target, control.Reattach(socket, departure.seq))
      Departure(..departure, socket: NoSocket)
    }
    NoSocket -> departure
  }
}

fn take(state: State, departure: Departure, incoming: Incoming) -> Departure {
  case incoming, departure.socket {
    Down(monitor), AwaitingSocket(monitor: socket_monitor, ..)
      if monitor == socket_monitor
    -> Departure(..departure, socket: NoSocket)
    Inbox(SocketDown(pid)), AwaitingSocket(socket:, ..) if pid == socket.pid ->
      release_socket(departure)
    other, _ -> pass(state, departure, other)
  }
}

fn pass(state: State, departure: Departure, incoming: Incoming) -> Departure {
  case incoming {
    Down(monitor) ->
      case links.by_monitor(state.links, monitor) {
        Some(guild) ->
          Departure(
            ..departure,
            pending: set.delete(departure.pending, id.guild_id_to_int(guild)),
          )
        None -> departure
      }
    Inbox(SocketDown(_)) -> departure
    Inbox(GuildRebound(guild, kept)) -> {
      let key = id.guild_id_to_int(guild)
      case set.contains(departure.pending, key) {
        True ->
          Departure(
            ..departure,
            pending: set.delete(departure.pending, key),
            rebound: case kept {
              True -> [guild, ..departure.rebound]
              False -> departure.rebound
            },
          )
        False -> departure
      }
    }
    Inbox(PresenceRebound(kept)) ->
      case departure.presence {
        AwaitingPresence -> Departure(..departure, presence: PresenceDone(kept))
        _ -> departure
      }
    Inbox(message) -> {
      case departure.speaks_moves {
        True -> control.send(departure.target, control.Relayed(message))
        False -> session.send(departure.target, message)
      }
      departure
    }
    Ctl(control.HandOff(_, reply)) -> {
      process.send(reply, False)
      departure
    }
    Ctl(message) -> {
      case departure.speaks_moves {
        True -> control.send(departure.target, control.RelayedControl(message))
        False -> control.send(departure.target, message)
      }
      departure
    }
  }
}

fn finish(state: State, departure: Departure) -> Nil {
  let departure = drain(state, selector(state), departure)
  use <- bool.guard(!departure.speaks_moves, Nil)
  control.send(
    departure.target,
    control.MoveDone(
      guilds: departure.rebound,
      presence: case departure.presence {
        NoPresence -> True
        PresenceDone(kept) -> kept
        AwaitingPresence -> False
      },
    ),
  )
}

fn drain(
  state: State,
  selector: process.Selector(Incoming),
  departure: Departure,
) -> Departure {
  case process.selector_receive(selector, 0) {
    Error(Nil) -> departure
    Ok(incoming) -> drain(state, selector, pass(state, departure, incoming))
  }
}
