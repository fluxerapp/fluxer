// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/message.{type MoveId}
import fluxer_gateway/api/client as api
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type SessionId, type SessionRef, PresenceFrame,
}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move as handoff_move
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/custom_status.{type CustomStatus}
import fluxer_gateway/presence/event.{type PresenceEvent}
import fluxer_gateway/presence/fanout
import fluxer_gateway/presence/gate
import fluxer_gateway/presence/handoff/codec as presence_codec
import fluxer_gateway/presence/handoff/signal.{type Signal}
import fluxer_gateway/presence/id_set.{type IdSet}
import fluxer_gateway/presence/lanes
import fluxer_gateway/presence/leftovers
import fluxer_gateway/presence/pids
import fluxer_gateway/presence/protocol.{
  type Input, type StartRequest, Foreign, PresenceSubscriber,
}
import fluxer_gateway/presence/push_buffer.{type PushBuffer}
import fluxer_gateway/presence/push_hook
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/state.{type Effect, type State}
import fluxer_gateway/presence/version
import fluxer_gateway/session/control
import fluxer_gateway/telemetry/rollout_metrics
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Subject, type Timer}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result

fn init_timeout_ms() -> Int {
  durations.presence().server_init_timeout_ms
}

type Shedding {
  Flowing
  Coalescing(pending: Dict(Int, PresenceEvent), order: List(Int))
}

type Mode {
  Serving
  Frozen(move: MoveId, version: Int, epoch: Int, held: List(Input(Monitor)))
  Handing(
    move: MoveId,
    version: Int,
    epoch: Int,
    target: Subject(Input(Monitor)),
    waiting: List(SessionId),
    sealed: Bool,
  )
  Arriving(
    pending: List(Effect(Monitor)),
    active: Bool,
    applied: Int,
    early: Dict(Int, BitArray),
    stash: List(Input(Monitor)),
  )
}

type Message {
  Inbound(Input(Monitor))
  Signalled(Signal)
}

type Server {
  Server(
    state: State(Monitor),
    mode: Mode,
    inbox: Subject(Input(Monitor)),
    user: UserId,
    expiry: Option(Timer),
    received: Int,
    shedding: Shedding,
    shed_armed: Bool,
    leftovers: #(IdSet, PushBuffer),
    published_stamp: Int,
  )
}

type FlagValue {
  OffHeap
}

type Flag {
  MessageQueueData
  FullsweepAfter
}

@external(erlang, "erlang", "process_flag")
fn queue_flag(flag: Flag, value: FlagValue) -> FlagValue

@external(erlang, "erlang", "process_flag")
fn int_flag(flag: Flag, value: Int) -> Int

const check_every = 64

const coalesce_above = 1000

const flush_below = 100

fn shed_tick_ms() -> Int {
  durations.presence().server_shed_tick_ms
}

fn validation_timeout_ms() -> Int {
  durations.presence().server_validation_timeout_ms
}

fn validation_grace_ms() -> Int {
  durations.presence().server_validation_grace_ms
}

fn terminate_timeout_ms() -> Int {
  durations.presence().server_terminate_timeout_ms
}

fn seal_wait_ms() -> Int {
  durations.presence().server_seal_wait_ms
}

fn forward_grace_ms() -> Int {
  durations.presence().server_forward_grace_ms
}

fn handoff_timeout_ms() -> Int {
  durations.presence().server_handoff_timeout_ms
}

const handoff_confirm_attempts = 3

const max_adopt_hops = 3

fn kick_timeout_ms() -> Int {
  durations.presence().server_kick_timeout_ms
}

fn expiry_jitter_ms() -> Int {
  durations.presence().server_expiry_jitter_ms
}

pub fn start(
  request: StartRequest,
  user: UserId,
) -> Result(actor.Started(Subject(Input(Monitor))), actor.StartError) {
  boot(request, user, False)
}

pub fn start_arriving(
  request: StartRequest,
  user: UserId,
) -> Result(actor.Started(Subject(Input(Monitor))), actor.StartError) {
  boot(request, user, True)
}

fn boot(
  request: StartRequest,
  user: UserId,
  arriving: Bool,
) -> Result(actor.Started(Subject(Input(Monitor))), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(_) {
    let _ = queue_flag(MessageQueueData, OffHeap)
    let _ = int_flag(FullsweepAfter, 20)
    let inbox = process.new_subject()
    let carried_sessions = case request.carried {
      Some(carried) -> carried.sessions
      None -> []
    }
    let monitors =
      list.map(carried_sessions, fn(carried) {
        monitor_session(carried.session)
      })
    let epoch = case request.carried {
      Some(carried) -> version.epoch(carried.version)
      None -> version.new_epoch()
    }
    let #(machine, effects) =
      state.init(request, epoch, monitors, clock.system_ms())
    let server =
      Server(
        state: machine,
        mode: Serving,
        inbox:,
        user:,
        expiry: None,
        received: 0,
        shedding: Flowing,
        shed_armed: False,
        leftovers: #(id_set.empty(), push_buffer.new()),
        published_stamp: 0,
      )
    let server = case arriving {
      True ->
        Server(
          ..server,
          mode: Arriving(
            pending: effects,
            active: False,
            applied: 0,
            early: dict.new(),
            stash: [],
          ),
        )
      False -> {
        let effects =
          list.filter(effects, fn(effect) {
            case effect {
              state.Subscribe(users) -> {
                process.send(inbox, protocol.SubscribeAfterStart(users))
                False
              }
              _ -> True
            }
          })
        register(request.user, inbox)
        perform_all(server, effects)
      }
    }
    let selector =
      process.new_selector()
      |> process.select_map(inbox, Inbound)
      |> process.select_map(signal.subject(process.self()), Signalled)
      |> process.select_monitors(fn(down) {
        case down {
          process.ProcessDown(monitor:, reason:, ..) ->
            Inbound(protocol.SessionDown(monitor, ending: session_end(reason)))
          process.PortDown(monitor:, ..) ->
            Inbound(protocol.SessionDown(monitor, ending: protocol.Closed))
        }
      })
    Ok(
      actor.initialised(server)
      |> actor.selecting(selector)
      |> actor.returning(inbox),
    )
  })
  |> actor.on_message(handle)
  |> actor.start
}

fn register(user: Int, inbox: Subject(Input(Monitor))) -> Nil {
  pids.put(user, inbox)
  gate.open_all(user)
  registry.revive(user)
}

fn session_end(reason: process.ExitReason) -> protocol.SessionEnd {
  case reason {
    process.Abnormal(detail) ->
      case detail == protocol.handoff_exit() {
        True -> protocol.HandedOff
        False ->
          case
            detail == atom.to_dynamic(atom.create("noconnection"))
            || detail == atom.to_dynamic(atom.create("noproc"))
          {
            True -> protocol.Unreachable
            False -> protocol.Closed
          }
      }
    process.Normal | process.Killed -> protocol.Closed
  }
}

fn handle(server: Server, message: Message) -> actor.Next(Server, Message) {
  case message {
    Signalled(found) -> on_signal(server, found)
    Inbound(input) -> {
      counters.input(input_kind(input))
      case server.mode {
        Serving -> next(serve(server, input))
        Frozen(..) -> actor.continue(hold(server, input))
        Handing(..) -> actor.continue(pass_on(server, input))
        Arriving(..) -> actor.continue(stash(server, input))
      }
    }
  }
}

fn input_kind(input: Input(Monitor)) -> counters.InputKind {
  case input {
    protocol.ConnectSession(..) | protocol.Connect(..) -> counters.ConnectInput
    protocol.RepointSession(..) | protocol.Repoint(..) -> counters.RepointInput
    protocol.SessionDown(..) | protocol.Disconnect(..) ->
      counters.DisconnectInput
    protocol.Update(..) -> counters.UpdateInput
    protocol.SubscribeTargets(..)
    | protocol.UnsubscribeTargets(..)
    | protocol.SubscribeAfterStart(..) -> counters.SubscriptionInput
    Foreign(..) -> counters.ForeignInput
    protocol.Resend(..)
    | protocol.ResendNode(..)
    | protocol.ResendSince(..)
    | protocol.Joined(..)
    | protocol.SnapshotRead(..) -> counters.ResendInput
    protocol.UserDispatch(..) -> counters.UserDispatchInput
    protocol.Guild(..) | protocol.VisitStart(..) | protocol.VisitEnd(..) ->
      counters.GuildInput
    protocol.TerminateSessions(..)
    | protocol.TerminateReply(..)
    | protocol.TerminateAll -> counters.TerminateInput
    protocol.Rejoin
    | protocol.ExpiryTick
    | protocol.GraceExpired(..)
    | protocol.IdleCheck
    | protocol.ShedTick -> counters.TimerInput
    protocol.MoveTo(..)
    | protocol.MoveRetry(..)
    | protocol.MoveFinished(..)
    | protocol.AdoptHandoff(..)
    | protocol.Adopt(..) -> counters.HandoffInput
    protocol.Validated(..) -> counters.OtherInput
  }
}

fn next(served: Result(Server, Nil)) -> actor.Next(Server, Message) {
  case served {
    Ok(server) -> actor.continue(server)
    Error(Nil) -> actor.stop()
  }
}

fn serve(server: Server, input: Input(Monitor)) -> Result(Server, Nil) {
  case input {
    protocol.SubscribeAfterStart(users) ->
      Ok(perform(server, state.Subscribe(users)))
    _ -> handle_input(server, input)
  }
}

fn serve_all(
  server: Server,
  inputs: List(Input(Monitor)),
) -> Result(Server, Nil) {
  list.try_fold(inputs, server, serve)
}

fn handle_input(server: Server, input: Input(Monitor)) -> Result(Server, Nil) {
  let server = case input {
    protocol.ShedTick ->
      Server(..server, received: server.received + 1, shed_armed: False)
    _ -> Server(..server, received: server.received + 1)
  }
  let server = case server.received % check_every == 0 {
    True -> adjust(server)
    False -> server
  }
  case input, server.shedding {
    Foreign(incoming), Flowing -> {
      let #(machine, effects) = state.foreign(server.state, incoming)
      Ok(list.fold(effects, Server(..server, state: machine), perform))
    }
    Foreign(incoming), Coalescing(pending, order) -> {
      let #(kept, order) = case dict.get(pending, incoming.user) {
        Ok(older) -> {
          counters.incr(counters.Coalesced)
          #(event.merge(older, incoming), order)
        }
        Error(Nil) -> #(incoming, [incoming.user, ..order])
      }
      Ok(
        Server(
          ..server,
          shedding: Coalescing(dict.insert(pending, incoming.user, kept), order),
        ),
      )
    }
    protocol.ShedTick, _ -> {
      let server = adjust(server)
      case server.shedding {
        Coalescing(..) -> Ok(arm_shed(server))
        Flowing -> Ok(server)
      }
    }
    _, _ -> run(server, input)
  }
}

fn on_signal(server: Server, found: Signal) -> actor.Next(Server, Message) {
  case found, server.mode {
    signal.Freeze(move:, version:, epoch:, reply:), Serving ->
      actor.continue(freeze(server, move, version, epoch, reply))
    signal.Freeze(reply:, ..), _ -> {
      process.send(reply, Error(signal.Busy))
      actor.continue(server)
    }
    signal.Commit(target:, reply:, ..), Frozen(move:, version:, epoch:, held:)
    -> {
      process.send(reply, True)
      actor.continue(commit(server, move, version, epoch, held, target))
    }
    signal.Commit(reply:, ..), _ -> {
      process.send(reply, False)
      actor.continue(server)
    }
    signal.Thaw(reply:), Frozen(held:, ..) -> {
      process.send(reply, True)
      next(serve_all(Server(..server, mode: Serving), list.reverse(held)))
    }
    signal.Thaw(reply:), _ -> {
      process.send(reply, False)
      actor.continue(server)
    }
    signal.Repointed(session), Handing(waiting:, sealed:, ..) as mode -> {
      let waiting = list.filter(waiting, fn(found) { found != session })
      let server = Server(..server, mode: Handing(..mode, waiting:))
      case waiting, sealed {
        [], False -> actor.continue(seal(server))
        _, _ -> actor.continue(server)
      }
    }
    signal.SealDue, Handing(sealed: False, ..) -> actor.continue(seal(server))
    signal.ForwardGraceOver, Handing(..) -> actor.stop()
    signal.Release(seq:, bytes:), Arriving(applied:, early:, ..) as mode ->
      case seq > applied {
        True ->
          next(
            drain_releases(activate(
              Server(
                ..server,
                mode: Arriving(..mode, early: dict.insert(early, seq, bytes)),
              ),
            )),
          )
        False -> actor.continue(server)
      }
    signal.Seal(last_seq:, reply:), Arriving(..) ->
      case drain_releases(activate(server)) {
        Error(Nil) -> {
          process.send(reply, False)
          actor.stop()
        }
        Ok(server) -> {
          let complete = case server.mode {
            Arriving(applied:, ..) -> applied >= last_seq
            _ -> False
          }
          let stashed = case server.mode {
            Arriving(stash:, ..) -> list.reverse(stash)
            _ -> []
          }
          process.send(reply, complete)
          next(serve_all(Server(..server, mode: Serving), stashed))
        }
      }
    signal.AbortArrival, Arriving(active: False, ..)
    | signal.AbortArrival, Frozen(..)
    | signal.AbortArrival, Handing(..)
    -> actor.stop()
    signal.Probe(reply), mode -> {
      process.send(reply, case mode {
        Frozen(..) | Handing(..) -> True
        Serving | Arriving(..) -> False
      })
      actor.continue(server)
    }
    signal.Release(..), _
    | signal.Seal(..), _
    | signal.AbortArrival, _
    | signal.Repointed(..), _
    | signal.SealDue, _
    | signal.ForwardGraceOver, _
    -> actor.continue(server)
  }
}

fn freeze(
  server: Server,
  move: MoveId,
  version: Int,
  epoch: Epoch,
  reply: Subject(Result(BitArray, signal.FreezeRefusal)),
) -> Server {
  let server = flush_coalesced(server)
  case state.can_freeze(server.state) {
    False -> {
      process.send(reply, Error(signal.Busy))
      server
    }
    True ->
      case
        handoff_codec.encode(
          presence_codec.tail(),
          state.export(server.state),
          version,
          epoch.to_int(epoch),
        )
      {
        Ok(tail) -> {
          process.send(reply, Ok(tail))
          Server(
            ..server,
            mode: Frozen(move:, version:, epoch: epoch.to_int(epoch), held: []),
          )
        }
        Error(_) -> {
          process.send(reply, Error(signal.Unencodable("presence tail")))
          server
        }
      }
  }
}

fn flush_coalesced(server: Server) -> Server {
  case server.shedding {
    Flowing -> server
    Coalescing(pending, order) ->
      order
      |> list.reverse
      |> list.fold(Server(..server, shedding: Flowing), fn(server, user) {
        case dict.get(pending, user) {
          Ok(found) -> {
            let #(machine, effects) = state.foreign(server.state, found)
            perform_all(Server(..server, state: machine), effects)
          }
          Error(Nil) -> server
        }
      })
  }
}

fn forward(
  server: Server,
  move: MoveId,
  version: Int,
  epoch: Int,
  input: Input(Monitor),
) -> Nil {
  case state.crossing(server.state, input) {
    Error(Nil) -> Nil
    Ok(portable) ->
      case
        handoff_codec.encode(presence_codec.inbox(), portable, version, epoch)
      {
        Ok(bytes) -> handoff_move.forward(move, bytes)
        Error(_) -> {
          counters.incr(counters.ForwardDropped)
          rollout_metrics.count(
            rollout_metrics.stub_hits,
            [#("kind", "presence"), #("outcome", "dropped")],
            1,
          )
        }
      }
  }
}

fn hold(server: Server, input: Input(Monitor)) -> Server {
  case server.mode {
    Frozen(move:, version:, epoch:, held:) -> {
      forward(server, move, version, epoch, input)
      Server(
        ..server,
        mode: Frozen(move:, version:, epoch:, held: [input, ..held]),
      )
    }
    _ -> server
  }
}

fn commit(
  server: Server,
  move: MoveId,
  version: Int,
  epoch: Int,
  held: List(Input(Monitor)),
  target: Subject(Input(Monitor)),
) -> Server {
  let user = state.user(server.state)
  let #(machine, effects) = state.hand_over(server.state)
  let server = perform_all(Server(..server, state: machine), effects)
  list.each(list.reverse(held), redirect(_, target))
  case pids.lookup(user) == Ok(server.inbox) {
    True -> pids.delete(user)
    False -> Nil
  }
  gate.open_all(user)
  let refs = state.session_refs(machine)
  list.each(refs, control.send(
    _,
    control.PresenceRebind(target, process.self()),
  ))
  counters.incr(counters.Handoff)
  let server =
    Server(
      ..server,
      mode: Handing(
        move:,
        version:,
        epoch:,
        target:,
        waiting: list.map(refs, fn(found) { found.id }),
        sealed: False,
      ),
    )
  case refs {
    [] -> seal(server)
    _ -> {
      let _ =
        process.send_after(
          signal.subject(process.self()),
          seal_wait_ms(),
          signal.SealDue,
        )
      server
    }
  }
}

fn redirect(input: Input(Monitor), target: Subject(Input(Monitor))) -> Nil {
  case input {
    protocol.AdoptHandoff(reply:, ..) ->
      process.send(reply, protocol.AdoptMoved(target))
    _ -> Nil
  }
}

fn pass_on(server: Server, input: Input(Monitor)) -> Server {
  case server.mode {
    Handing(sealed: False, move:, version:, epoch:, target:, ..) -> {
      redirect(input, target)
      forward(server, move, version, epoch, input)
      server
    }
    Handing(sealed: True, target:, ..) -> {
      rollout_metrics.count(
        rollout_metrics.stub_hits,
        [#("kind", "presence"), #("outcome", "forwarded")],
        1,
      )
      redirect(input, target)
      case state.crossing(server.state, input) {
        Ok(portable) -> process.send(target, portable)
        Error(Nil) -> Nil
      }
      server
    }
    _ -> server
  }
}

fn seal(server: Server) -> Server {
  case server.mode {
    Handing(move:, ..) as mode -> {
      handoff_move.repointed(move)
      let _ =
        process.send_after(
          signal.subject(process.self()),
          forward_grace_ms(),
          signal.ForwardGraceOver,
        )
      Server(..server, mode: Handing(..mode, sealed: True))
    }
    _ -> server
  }
}

fn stash(server: Server, input: Input(Monitor)) -> Server {
  case server.mode {
    Arriving(stash:, ..) as mode ->
      Server(..server, mode: Arriving(..mode, stash: [input, ..stash]))
    _ -> server
  }
}

fn activate(server: Server) -> Server {
  case server.mode {
    Arriving(active: False, pending:, ..) as mode -> {
      register(state.user(server.state), server.inbox)
      let server =
        Server(..server, mode: Arriving(..mode, active: True, pending: []))
      perform_all(server, pending)
    }
    _ -> server
  }
}

fn drain_releases(server: Server) -> Result(Server, Nil) {
  case server.mode {
    Arriving(applied:, early:, ..) as mode ->
      case dict.get(early, applied + 1) {
        Error(Nil) -> Ok(server)
        Ok(bytes) -> {
          let server =
            Server(
              ..server,
              mode: Arriving(
                ..mode,
                applied: applied + 1,
                early: dict.delete(early, applied + 1),
              ),
            )
          case released(bytes) {
            Ok(input) -> serve(server, input)
            Error(Nil) -> {
              counters.incr(counters.ForwardDropped)
              Ok(server)
            }
          }
          |> result.try(drain_releases)
        }
      }
    _ -> Ok(server)
  }
}

fn released(bytes: BitArray) -> Result(Input(Monitor), Nil) {
  case handoff_codec.decode(presence_codec.inbox(), bytes) {
    Ok(#(_, input)) -> protocol.retag(input)
    Error(_) -> Error(Nil)
  }
}

fn monitor_session(target: SessionRef) -> Monitor {
  case process.subject_owner(target.inbox) {
    Ok(pid) -> process.monitor(pid)
    Error(Nil) -> process.monitor(process.self())
  }
}

fn owner_elsewhere(server: Server) -> Option(Atom) {
  let self = remote.self_atom()
  case route.owner(state.user(server.state)) {
    Ok(node) if node != self -> Some(node)
    _ -> None
  }
}

fn admit(server: Server, input: Input(Monitor)) -> Option(Input(Monitor)) {
  case input, state.is_moving(server.state) {
    protocol.ConnectSession(target, status, afk, mobile, guilds, targets, reply),
      False
    -> {
      counters.session_visibility(afk)
      Some(protocol.Connect(
        protocol.ConnectRequest(
          session: target,
          status:,
          afk:,
          mobile:,
          guilds:,
          targets:,
          monitor: monitor_session(target),
        ),
        reply,
      ))
    }
    protocol.RepointSession(target, from), False ->
      Some(protocol.Repoint(target, from, monitor_session(target)))
    protocol.AdoptHandoff(request, reply), False -> {
      let monitors = case request.carried {
        Some(carried) ->
          list.map(carried.sessions, fn(carried) {
            monitor_session(carried.session)
          })
        None -> []
      }
      Some(protocol.Adopt(request, monitors, reply))
    }
    protocol.MoveTo(node), _ ->
      case owner_elsewhere(server) == Some(node) {
        True -> Some(input)
        False -> None
      }
    protocol.MoveRetry(_), _ ->
      Some(protocol.MoveRetry(owner_elsewhere(server)))
    _, _ -> Some(input)
  }
}

fn run(server: Server, input: Input(Monitor)) -> Result(Server, Nil) {
  case admit(server, input) {
    Some(input) -> step(server, input)
    None -> Ok(server)
  }
}

fn step(server: Server, input: Input(Monitor)) -> Result(Server, Nil) {
  let #(machine, effects) = state.handle(server.state, input, clock.system_ms())
  let server = Server(..server, state: machine)
  let replayed =
    list.flat_map(effects, fn(effect) {
      case effect {
        state.Replay(inputs) -> inputs
        _ -> []
      }
    })
  case list.contains(effects, state.Stop) {
    True -> {
      gate.open_all(state.user(machine))
      case state.has_moved(machine) {
        True -> Nil
        False -> registry.bury(state.user(machine))
      }
      let before = list.filter(effects, fn(effect) { effect != state.Stop })
      let server = perform_all(server, before)
      let _ = perform_all(server, state.terminate(server.state))
      leftovers.release(state.user(server.state), process.self())
      Error(Nil)
    }
    False -> list.try_fold(replayed, perform_all(server, effects), run)
  }
}

fn adjust(server: Server) -> Server {
  let length = fanout.queue_length(process.self())
  case server.shedding {
    Flowing ->
      case length > coalesce_above {
        True ->
          Server(..server, shedding: Coalescing(dict.new(), []))
          |> arm_shed
        False -> server
      }
    Coalescing(pending, order) ->
      case length < flush_below {
        True ->
          order
          |> list.reverse
          |> list.fold(Server(..server, shedding: Flowing), fn(server, user) {
            case dict.get(pending, user) {
              Ok(found) -> {
                let #(machine, effects) =
                  state.handle(server.state, Foreign(found), clock.system_ms())
                perform_all(Server(..server, state: machine), effects)
              }
              Error(Nil) -> server
            }
          })
        False -> server
      }
  }
}

fn arm_shed(server: Server) -> Server {
  case server.shed_armed {
    True -> server
    False -> {
      let _ =
        process.send_after(server.inbox, shed_tick_ms(), protocol.ShedTick)
      Server(..server, shed_armed: True)
    }
  }
}

fn perform_all(server: Server, effects: List(Effect(Monitor))) -> Server {
  list.fold(effects, server, perform)
  |> record_leftovers
}

fn record_leftovers(server: Server) -> Server {
  let current = state.leftovers(server.state)
  case current == server.leftovers {
    True -> server
    False -> {
      leftovers.record(state.user(server.state), current.0, current.1)
      Server(..server, leftovers: current)
    }
  }
}

pub fn recover_crashed(user: Int, pid: process.Pid) -> Nil {
  let found = leftovers.take(user, pid)
  case id.user_id(user), found.temporary_guilds, found.pending {
    _, [], [] | Error(_), _, _ -> Nil
    Ok(user), guilds, pending -> {
      let _ =
        process.spawn_unlinked(fn() {
          case guilds {
            [] -> Nil
            _ -> kick_temporary(user, guilds)
          }
          case pending {
            [] -> Nil
            _ -> push_hook.flush(user, pending)
          }
        })
      Nil
    }
  }
}

fn subscriber(server: Server) -> protocol.Subscriber {
  PresenceSubscriber(user: state.user(server.state), inbox: server.inbox)
}

fn perform(server: Server, effect: Effect(Monitor)) -> Server {
  case effect {
    state.SendSessions(refs, message) -> {
      list.each(refs, session.send(_, message))
      server
    }
    state.NotifySessions(refs, message) -> {
      list.each(refs, control.send(_, message))
      server
    }
    state.SendSnapshot(target, events) -> {
      send_snapshot(target, events)
      server
    }
    state.WriteRow(user, row) -> {
      cache.put(user, row)
      server
    }
    state.DeleteRow(user) -> {
      cache.delete(user)
      server
    }
    state.Fanout(outgoing) -> {
      let _ = fanout.publish(outgoing)
      Server(..server, published_stamp: version.stamp())
    }
    state.ResendEvent(outgoing, to) -> {
      fanout.resend_to(outgoing, to)
      server
    }
    state.ResendToNode(outgoing, target) -> {
      fanout.resend_to_node(outgoing, target)
      server
    }
    state.Admit(to, catchup, current, visible) -> {
      admit_subscriber(server, to, catchup, current, visible)
      server
    }
    state.Subscribe(users) -> {
      registry.subscribe_many(
        subscriber(server),
        users,
        resend: state.has_live_sessions(server.state),
      )
      server
    }
    state.Unsubscribe(users) -> {
      registry.unsubscribe_many(subscriber(server), users)
      server
    }
    state.ReadSnapshot(users, token) -> {
      let inbox = server.inbox
      let _ =
        process.spawn_unlinked(fn() {
          process.send(
            inbox,
            protocol.SnapshotRead(token, cache.lookup_events(users)),
          )
        })
      server
    }
    state.StartValidation(candidate, generation) -> {
      start_validation(server, candidate, generation)
      server
    }
    state.ArmExpiry(after_ms) -> {
      cancel_expiry(server)
      let timer =
        process.send_after(
          server.inbox,
          after_ms + 1 + int.random(expiry_jitter_ms()),
          protocol.ExpiryTick,
        )
      Server(..server, expiry: Some(timer))
    }
    state.CancelExpiry -> {
      cancel_expiry(server)
      Server(..server, expiry: None)
    }
    state.ArmGrace(token, after_ms) -> {
      let _ =
        process.send_after(server.inbox, after_ms, protocol.GraceExpired(token))
      server
    }
    state.ArmIdleCheck(after_ms) -> {
      let _ = process.send_after(server.inbox, after_ms, protocol.IdleCheck)
      server
    }
    state.PushFlush(pending) -> {
      push_hook.flush(server.user, pending)
      server
    }
    state.PushNow(pending) -> {
      push_hook.send_now(server.user, pending)
      server
    }
    state.PushOverflow(dropped) -> {
      push_hook.overflow(dropped)
      server
    }
    state.NoteSessionActive -> {
      push_hook.note_session_active(server.user)
      server
    }
    state.Demonitor(monitor) -> {
      process.demonitor_process(monitor)
      server
    }
    state.AskTerminate(target, hashes, token) -> {
      ask_terminate(server.inbox, target, hashes, token)
      server
    }
    state.KickTemporary(guilds) -> {
      kick_temporary(server.user, guilds)
      server
    }
    state.ReplyConnect(to, reply) -> {
      process.send(to, reply)
      server
    }
    state.ReplyTerminate(to, outcome) -> {
      process.send(to, outcome)
      server
    }
    state.ReplyAdopt(to, reply) -> {
      process.send(to, reply)
      server
    }
    state.StartMove(target, token, request) -> {
      start_move(server.inbox, target, token, request)
      server
    }
    state.RetryMove(after_ms) -> {
      let _ =
        process.send_after(server.inbox, after_ms, protocol.MoveRetry(None))
      server
    }
    state.Forward(to, inputs) -> {
      list.each(inputs, process.send(to, _))
      server
    }
    state.Replay(_) | state.Stop -> server
  }
}

fn admit_subscriber(
  server: Server,
  to: protocol.Subscriber,
  catchup: protocol.Catchup,
  current: Option(PresenceEvent),
  visible: Bool,
) -> Nil {
  let target = state.user(server.state)
  let last = option.map(current, fn(found) { found.version })
  let admitted = gate.admit(target, to, last)
  let resent = case catchup, admitted {
    protocol.Everything, _ -> visible
    protocol.ChangedSince(stamp:, ..), True -> server.published_stamp > stamp
    protocol.ChangedSince(..), False -> False
  }
  case current, resent {
    Some(found), True -> fanout.resend_to(found, to)
    _, _ -> Nil
  }
  case last, admitted {
    Some(found), True -> lanes.settle(target, to, found)
    _, _ -> Nil
  }
}

fn cancel_expiry(server: Server) -> Nil {
  case server.expiry {
    Some(timer) -> {
      let _ = process.cancel_timer(timer)
      Nil
    }
    None -> Nil
  }
}

fn send_snapshot(target: SessionRef, events: List(PresenceEvent)) -> Nil {
  list.each(events, fn(found) {
    case event.user_id(found) {
      Ok(user) ->
        session.send(
          target,
          PresenceFrame(user, event.wire_json(found), event.facts(found)),
        )
      Error(Nil) -> Nil
    }
  })
}

fn start_validation(
  server: Server,
  candidate: CustomStatus,
  generation: Int,
) -> Nil {
  let inbox = server.inbox
  let user = server.user
  let _ =
    process.spawn_unlinked(fn() {
      let reply =
        api.call_raw(
          "validate_custom_status",
          [
            #("user_id", id.json(id.user_id_to_int(user))),
            #("custom_status", custom_status.validation_payload(candidate)),
          ],
          clock.deadline_in(validation_timeout_ms()),
        )
      let outcome = case reply {
        Ok(data) -> validation_reply(raw_json.wrap(data))
        Error(api.ApiTimeout) -> {
          counters.incr(counters.ValidationTimeout)
          custom_status.Rejected
        }
        Error(_) -> custom_status.Rejected
      }
      process.send(inbox, protocol.Validated(generation, outcome))
    })
  let _ =
    process.send_after(
      inbox,
      validation_timeout_ms() + validation_grace_ms(),
      protocol.Validated(generation, custom_status.Rejected),
    )
  Nil
}

fn validation_reply(data: RawJson) -> custom_status.ValidationReply {
  case raw_json.fields(data) {
    Ok(fields) ->
      case list.key_find(fields, "custom_status") {
        Error(Nil) -> custom_status.ValidatedWithoutValue
        Ok(value) ->
          case custom_status.from_tree(json_tree.from_raw(value)) {
            Ok(status) -> custom_status.Validated(Some(status))
            Error(Nil) -> custom_status.Validated(None)
          }
      }
    Error(Nil) -> custom_status.ValidatedWithoutValue
  }
}

fn ask_terminate(
  inbox: Subject(Input(Monitor)),
  target: SessionRef,
  hashes: List(String),
  token: Int,
) -> Nil {
  let _ =
    process.spawn_unlinked(fn() {
      let reply = process.new_subject()
      control.send(target, control.TerminateMatching(hashes, reply))
      let result = case process.receive(reply, terminate_timeout_ms()) {
        Ok(True) -> protocol.Terminated
        Ok(False) -> protocol.KeptBySession
        Error(Nil) ->
          case process.subject_owner(target.inbox) {
            Ok(pid) ->
              case remote.node_of(target.inbox) == remote.self_atom() {
                True ->
                  case process.is_alive(pid) {
                    True -> protocol.TerminateFailed("timeout")
                    False -> protocol.Gone
                  }
                False -> protocol.TerminateFailed("timeout")
              }
            Error(Nil) -> protocol.Gone
          }
      }
      process.send(inbox, protocol.TerminateReply(token, target.id, result))
    })
  Nil
}

fn confirm_handoff(
  target: Atom,
  manager: process.Name(protocol.ManagerMsg),
  user: Int,
  attempts: Int,
) -> Result(Subject(Input(Monitor)), Nil) {
  case
    remote.call(target, manager, handoff_timeout_ms(), fn(reply) {
      protocol.Lookup(user, reply)
    })
  {
    Ok(found) -> found
    Error(remote.CallTimeout) if attempts > 1 ->
      confirm_handoff(target, manager, user, attempts - 1)
    Error(_) -> Error(Nil)
  }
}

fn kick_temporary(user: UserId, guilds: List(Int)) -> Nil {
  let _ =
    process.spawn_unlinked(fn() {
      api.call(
        "kick_temporary_member",
        [
          #("user_id", id.json(id.user_id_to_int(user))),
          #("guild_ids", json.array(guilds, id.json)),
        ],
        clock.deadline_in(kick_timeout_ms()),
      )
    })
  Nil
}

fn start_move(
  inbox: Subject(Input(Monitor)),
  target: Atom,
  token: Int,
  request: StartRequest,
) -> Nil {
  let _ =
    process.spawn_unlinked(fn() {
      let manager = route.name(route.Manager, route.shard(request.user))
      let result =
        remote.call(target, manager, handoff_timeout_ms(), fn(reply) {
          protocol.StartHandoff(request, reply)
        })
      let moved = case result {
        Ok(Ok(protocol.HandoffSpawned(inbox))) -> Ok(inbox)
        Ok(Ok(protocol.HandoffRunning(inbox))) ->
          adopt_at(inbox, request, max_adopt_hops)
        Ok(Error(Nil)) -> Error(Nil)
        Error(remote.CallTimeout) ->
          case
            confirm_handoff(
              target,
              manager,
              request.user,
              handoff_confirm_attempts,
            )
          {
            Ok(inbox) -> adopt_at(inbox, request, max_adopt_hops)
            Error(Nil) -> Error(Nil)
          }
        Error(remote.CallUnavailable) | Error(remote.CallBusy) -> Error(Nil)
      }
      case moved {
        Ok(_) -> counters.incr(counters.Handoff)
        Error(Nil) -> Nil
      }
      process.send(inbox, protocol.MoveFinished(token, moved))
    })
  Nil
}

fn adopt_at(
  inbox: Subject(Input(Monitor)),
  request: StartRequest,
  hops_left: Int,
) -> Result(Subject(Input(Monitor)), Nil) {
  case
    remote.call_subject(inbox, handoff_timeout_ms(), fn(reply) {
      protocol.AdoptHandoff(request, reply)
    })
  {
    Ok(protocol.Adopted) -> Ok(inbox)
    Ok(protocol.AdoptMoved(to)) if hops_left > 0 ->
      adopt_at(to, request, hops_left - 1)
    Ok(protocol.AdoptMoved(_)) | Error(_) -> Error(Nil)
  }
}
