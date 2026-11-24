// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/message.{type ToBouncer, type ToWorker, BeatAck, FromPeer}
import bouncer_proto/transport
import fluxer_bouncer/config.{type Config}
import fluxer_bouncer/core
import fluxer_bouncer/discovery
import fluxer_bouncer/shutdown
import fluxer_bouncer/status
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Subject}
import gleam/list
import gleam/otp/actor

type Message {
  Bouncer(ToBouncer)
  Ticked
  NodeEvent(up: Bool, node: Result(Atom, Nil))
  LeaveNow
}

type State {
  State(
    core: core.State,
    config: Config,
    ticks: Subject(Nil),
    peers: Dict(Int, Atom),
    published: core.Status,
    left: Bool,
  )
}

type TimeUnit {
  Millisecond
}

type MonitorOption {
  NodeType(NodeTypes)
}

type NodeTypes {
  All
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

@external(erlang, "net_kernel", "monitor_nodes")
fn monitor_nodes(flag: Bool, options: List(MonitorOption)) -> Dynamic

@external(erlang, "erlang", "send")
fn send_raw(to: Name(ToBouncer), message: #(Atom)) -> #(Atom)

fn leave_tag() -> Atom {
  atom.create("fluxer_bouncer_leave")
}

pub fn leave() -> Nil {
  case process.named(transport.bouncer_name()) {
    Ok(_) -> {
      let _ = send_raw(transport.bouncer_name(), #(leave_tag()))
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn start(
  config: Config,
) -> Result(actor.Started(Subject(ToBouncer)), actor.StartError) {
  actor.new_with_initialiser(5000, fn(_) { initialise(config) })
  |> actor.on_message(handle)
  |> actor.start
}

fn initialise(
  config: Config,
) -> Result(actor.Initialised(State, Message, Subject(ToBouncer)), String) {
  let me = process.self()
  case process.register(me, transport.bouncer_name()) {
    Ok(Nil) -> {
      let inbox = process.named_subject(transport.bouncer_name())
      let ticks = process.new_subject()
      let _ = monitor_nodes(True, [NodeType(All)])
      let _ = discovery.start(config)
      let now = monotonic_time(Millisecond)
      let fresh =
        core.new(
          config,
          system_time(Millisecond),
          now,
          system_time(Millisecond),
        )
      status.init()
      status.publish(core.status(fresh))
      let state =
        State(
          core: fresh,
          config: config,
          ticks: ticks,
          peers: dict.from_list(discovery.peers(config)),
          published: core.status(fresh),
          left: False,
        )
      process.send_after(ticks, config.timings.tick_ms, Nil)
      let node_of = fn(raw: Dynamic) {
        decode.run(raw, decode.at([1], atom.decoder()))
        |> result_nil
      }
      let selector =
        process.new_selector()
        |> process.select_map(inbox, Bouncer)
        |> process.select_record(leave_tag(), 0, fn(_) { LeaveNow })
        |> process.select_map(ticks, fn(_) { Ticked })
        |> process.select_record(atom.create("nodeup"), 2, fn(raw) {
          NodeEvent(True, node_of(raw))
        })
        |> process.select_record(atom.create("nodedown"), 2, fn(raw) {
          NodeEvent(False, node_of(raw))
        })
      let state =
        list.fold(connected(), state, fn(state, node) {
          advance(state, core.NodeUp(node))
        })
      state
      |> actor.initialised
      |> actor.selecting(selector)
      |> actor.returning(inbox)
      |> Ok
    }
    Error(Nil) -> Error("the bouncer name is already registered")
  }
}

fn result_nil(found: Result(a, b)) -> Result(a, Nil) {
  case found {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
  }
}

type NodeKind {
  Visible
  Hidden
}

@external(erlang, "erlang", "nodes")
fn nodes(kinds: List(NodeKind)) -> List(Atom)

fn connected() -> List(Atom) {
  nodes([Visible, Hidden])
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Bouncer(received) -> actor.continue(advance(state, core.Received(received)))
    Ticked -> {
      process.send_after(state.ticks, state.config.timings.tick_ms, Nil)
      actor.continue(advance(state, core.Tick))
    }
    NodeEvent(True, Ok(node)) ->
      actor.continue(advance(state, core.NodeUp(node)))
    NodeEvent(False, Ok(node)) ->
      actor.continue(advance(state, core.NodeDown(node)))
    NodeEvent(_, Error(Nil)) -> actor.continue(state)
    LeaveNow -> actor.continue(advance(state, core.Leave))
  }
}

fn advance(state: State, input: core.Input) -> State {
  let #(next, effects) =
    core.step(state.core, input, monotonic_time(Millisecond))
  list.each(effects, perform(state, _))
  let state = case !state.left && core.handed_over(next) {
    True -> {
      shutdown.announce_left()
      State(..state, left: True)
    }
    False -> state
  }
  let current = core.status(next)
  case current == state.published {
    True -> State(..state, core: next)
    False -> {
      status.publish(current)
      State(..state, core: next, published: current)
    }
  }
}

fn perform(state: State, effect: core.Effect) -> Nil {
  let _ = case effect {
    core.ToPeer(ordinal, message) ->
      case dict.get(state.peers, ordinal) {
        Ok(node) ->
          transport.deliver(
            to: node,
            name: transport.bouncer_name(),
            message: FromPeer(state.config.ordinal, message),
          )
        Error(Nil) -> Ok(Nil)
      }
    core.ToWorker(node, message) -> to_worker(node, message)
    core.ToMover(node, message) ->
      transport.deliver(
        to: node,
        name: transport.mover_name(),
        message: message,
      )
  }
  Nil
}

fn to_worker(
  node: Atom,
  message: ToWorker,
) -> Result(Nil, transport.SendFailure) {
  case message {
    BeatAck(..) ->
      transport.send(to: node, name: transport.worker_name(), message: message)
    _ ->
      transport.deliver(
        to: node,
        name: transport.worker_name(),
        message: message,
      )
  }
}
