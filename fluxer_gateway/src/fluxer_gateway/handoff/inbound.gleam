// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key
import bouncer_proto/message.{type MoveId, type ToMover}
import bouncer_proto/transport
import fluxer_gateway/handoff/adapter.{type Adapter}
import fluxer_gateway/handoff/arrival.{type Arrival}
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/ports.{type Ports}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import logging

pub type Message {
  Incoming(message: ToMover)
  HomeChanged
  ArrivalFinished(move: MoveId)
  Arriving(reply: Subject(List(MoveId)))
}

type State {
  State(
    self: Subject(Message),
    adapters: Dict(Int, Adapter),
    ports: Ports,
    to_engine: fn(ToMover) -> Nil,
    arrivals: Dict(MoveId, Subject(arrival.Event)),
  )
}

pub fn start(
  adapters: List(Adapter),
) -> Result(actor.Started(Subject(ToMover)), actor.StartError) {
  build(adapters, ports.unwired(), move.from_home_local, True, fn(_) {
    process.named_subject(transport.mover_name())
  })
}

pub fn start_registered(
  adapters: List(Adapter),
  ports: Ports,
) -> Result(actor.Started(Subject(ToMover)), actor.StartError) {
  build(adapters, ports, move.from_home_local, True, fn(_) {
    process.named_subject(transport.mover_name())
  })
}

pub fn start_with(
  adapters: List(Adapter),
  ports: Ports,
  to_engine: fn(ToMover) -> Nil,
) -> Result(actor.Started(Subject(Message)), actor.StartError) {
  build(adapters, ports, to_engine, False, fn(self) { self })
}

fn build(
  adapters: List(Adapter),
  ports: Ports,
  to_engine: fn(ToMover) -> Nil,
  registered: Bool,
  returning: fn(Subject(Message)) -> data,
) -> Result(actor.Started(data), actor.StartError) {
  actor.new_with_initialiser(1000, fn(self) {
    let selector = process.new_selector() |> process.select(self)
    use selector <- result.map(case registered {
      False -> Ok(selector)
      True -> {
        let name = transport.mover_name()
        process.register(process.self(), name)
        |> result.replace_error("handoff inbound name taken")
        |> result.replace(process.select_map(
          selector,
          process.named_subject(name),
          Incoming,
        ))
      }
    })
    initial(self, adapters, ports, to_engine)
    |> actor.initialised
    |> actor.selecting(selector)
    |> actor.returning(returning(self))
  })
  |> actor.on_message(handle)
  |> actor.start
}

fn initial(
  self: Subject(Message),
  adapters: List(Adapter),
  ports: Ports,
  to_engine: fn(ToMover) -> Nil,
) -> State {
  State(
    self:,
    adapters: adapters
      |> list.map(fn(entry) { #(entry.ns, entry) })
      |> dict.from_list,
    ports:,
    to_engine:,
    arrivals: dict.new(),
  )
}

pub fn arriving() -> List(MoveId) {
  []
}

pub fn arriving_in(inbound: Subject(Message)) -> List(MoveId) {
  process.call(inbound, 1000, Arriving)
}

pub fn home_changed(inbound: Subject(Message)) -> Nil {
  process.send(inbound, HomeChanged)
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Arriving(reply) -> {
      process.send(reply, dict.keys(state.arrivals))
      actor.continue(state)
    }
    HomeChanged -> {
      dict.each(state.arrivals, fn(_, handler) {
        process.send(handler, arrival.HomeChanged)
      })
      actor.continue(state)
    }
    ArrivalFinished(id) ->
      actor.continue(State(..state, arrivals: dict.delete(state.arrivals, id)))
    Incoming(incoming) -> {
      let id = move.mover_of(incoming)
      case id.source == state.ports.me {
        True -> {
          state.to_engine(incoming)
          actor.continue(state)
        }
        False -> actor.continue(to_arrival(state, id, incoming))
      }
    }
  }
}

fn to_arrival(state: State, id: MoveId, incoming: ToMover) -> State {
  case dict.get(state.arrivals, id), incoming {
    Ok(handler), _ -> {
      process.send(handler, arrival.FromHome(incoming))
      state
    }
    Error(Nil), message.Install(_, entity, stamp, version, ..) ->
      case dict.get(state.adapters, entity.ns) {
        Error(Nil) -> {
          let _ =
            state.ports.to_home(
              key.bucket_of(entity),
              message.InstallRefused(id, message.UnsupportedVersion(version)),
            )
          state
        }
        Ok(entry) -> {
          let machine =
            arrival.new(
              move: id,
              key: entity,
              epoch: stamp,
              version:,
              me: state.ports.me,
              status_after_ms: state.ports.arrival_status_ms,
            )
          let ready = process.new_subject()
          let owner = state.self
          let ports = state.ports
          process.spawn_unlinked(fn() {
            let inbox = process.new_subject()
            process.send(ready, inbox)
            run(
              Handler(inbox:, machine:, adapter: entry, ports:, entity: None),
              [arrival.FromHome(incoming)],
            )
            process.send(owner, ArrivalFinished(id))
          })
          case process.receive(ready, 1000) {
            Ok(inbox) ->
              State(..state, arrivals: dict.insert(state.arrivals, id, inbox))
            Error(Nil) -> state
          }
        }
      }
    Error(Nil), message.MoveStatus(..) | Error(Nil), message.Abort(_) -> state
    Error(Nil), _ -> state
  }
}

type Handler {
  Handler(
    inbox: Subject(arrival.Event),
    machine: Arrival,
    adapter: Adapter,
    ports: Ports,
    entity: Option(Pid),
  )
}

fn run(handler: Handler, pending: List(arrival.Event)) -> Nil {
  case pending, arrival.phase(handler.machine) {
    _, arrival.Finished(_) -> Nil
    [], _ -> run(handler, [process.receive_forever(handler.inbox)])
    [event, ..rest], before -> {
      let #(machine, effects) = arrival.step(handler.machine, event)
      let after = arrival.phase(machine)
      case phase_name(before) == phase_name(after) {
        True -> Nil
        False ->
          logging.log(
            logging.Info,
            "handoff arrival id="
              <> int.to_string(arrival.key(machine).id)
              <> " "
              <> phase_name(before)
              <> " -> "
              <> phase_name(after),
          )
      }
      let #(handler, follow) =
        list.fold(effects, #(Handler(..handler, machine:), []), fn(acc, effect) {
          let #(next, events) = perform(acc.0, effect)
          #(next, list.append(acc.1, events))
        })
      run(handler, list.append(rest, follow))
    }
  }
}

fn phase_name(phase: arrival.Phase) -> String {
  case phase {
    arrival.Receiving -> "receiving"
    arrival.Installing(aborted: True) -> "installing/aborted"
    arrival.Installing(aborted: False) -> "installing"
    arrival.Waiting(committed: True, ..) -> "waiting/committed"
    arrival.Waiting(committed: False, ..) -> "waiting"
    arrival.Finished(outcome) ->
      case outcome {
        arrival.Arrived(_) -> "arrived"
        arrival.Discarded -> "discarded"
        arrival.Refused(reason) -> "refused " <> reason
      }
  }
}

fn perform(
  handler: Handler,
  effect: arrival.Effect,
) -> #(Handler, List(arrival.Event)) {
  let entity = arrival.key(handler.machine)
  case effect {
    arrival.ToHome(outgoing) -> {
      let _ = handler.ports.to_home(key.bucket_of(entity), outgoing)
      #(handler, [])
    }
    arrival.RunInstall(tail) -> {
      move.phase("install_started")
      case
        handler.adapter.install(
          entity,
          arrival.move_id(handler.machine).source,
          arrival.epoch(handler.machine),
          tail,
        )
      {
        Ok(arrived) -> #(Handler(..handler, entity: Some(arrived.entity)), [
          arrival.InstallReady(Ok(arrived.target_ref)),
        ])
        Error(error) -> {
          logging.log(
            logging.Warning,
            "handoff install refused: " <> adapter.describe(error),
          )
          #(handler, [arrival.InstallReady(Error(adapter.describe(error)))])
        }
      }
    }
    arrival.Deliver(seq, bytes) -> {
      with_entity(handler, fn(pid) {
        let _ = handler.adapter.release(pid, seq, bytes)
        Nil
      })
      #(handler, [])
    }
    arrival.Close(last_seq) -> {
      with_entity(handler, fn(pid) {
        let _ = handler.adapter.seal(pid, last_seq)
        Nil
      })
      #(handler, [])
    }
    arrival.Discard -> {
      with_entity(handler, handler.adapter.abort_arrival)
      #(handler, [])
    }
    arrival.After(timer, ms) -> {
      process.send_after(handler.inbox, ms, arrival.Fired(timer))
      #(handler, [])
    }
    arrival.Done(_) -> #(handler, [])
  }
}

fn with_entity(handler: Handler, run: fn(Pid) -> Nil) -> Nil {
  case handler.entity {
    Some(pid) -> run(pid)
    None -> Nil
  }
}
