// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/rollout/subscribers.{type Subscribers}
import gleam/erlang/process.{type Name, type Subject}
import gleam/otp/actor

pub opaque type Message(event) {
  Subscribe(Subject(event))
  Unsubscribe(Subject(event))
  Notify(event)
  OwnerDown(process.Down)
}

pub fn start(
  name: Name(Message(event)),
  init_timeout_ms: Int,
) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms, fn(self) {
    let selector =
      process.new_selector()
      |> process.select(self)
      |> process.select_monitors(OwnerDown)
    subscribers.new()
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(name)
  |> actor.on_message(handle)
  |> actor.start
}

pub fn subscribe(name: Name(Message(event)), subject: Subject(event)) -> Nil {
  send(name, Subscribe(subject))
}

pub fn unsubscribe(name: Name(Message(event)), subject: Subject(event)) -> Nil {
  send(name, Unsubscribe(subject))
}

pub fn notify(name: Name(Message(event)), event: event) -> Nil {
  send(name, Notify(event))
}

fn send(name: Name(Message(event)), message: Message(event)) -> Nil {
  case process.named(name) {
    Ok(_) -> process.send(process.named_subject(name), message)
    Error(Nil) -> Nil
  }
}

fn handle(
  state: Subscribers(event),
  message: Message(event),
) -> actor.Next(Subscribers(event), Message(event)) {
  case message {
    Subscribe(subject) -> actor.continue(subscribers.add(state, subject))
    Unsubscribe(subject) -> actor.continue(subscribers.remove(state, subject))
    Notify(event) -> {
      subscribers.notify(state, event)
      actor.continue(state)
    }
    OwnerDown(process.ProcessDown(pid:, ..)) ->
      actor.continue(subscribers.owner_down(state, pid))
    OwnerDown(process.PortDown(..)) -> actor.continue(state)
  }
}
