// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/rollout/subscribers
import gleam/erlang/process

pub fn subscribe_is_idempotent_and_keyed_by_owner_test() {
  let first = process.new_subject()
  let second = process.new_subject()
  let owner = process.self()
  let state =
    subscribers.new()
    |> subscribers.add(first)
    |> subscribers.add(first)
    |> subscribers.add(second)
  assert subscribers.size(state) == 2
  assert subscribers.watches(state, owner)
  let state = subscribers.remove(state, first)
  assert subscribers.size(state) == 1
  assert subscribers.watches(state, owner)
  let state = subscribers.remove(state, second)
  assert subscribers.size(state) == 0
  assert !subscribers.watches(state, owner)
}

pub fn owner_down_drops_every_subject_of_that_owner_test() {
  let subject = process.new_subject()
  let state =
    subscribers.new()
    |> subscribers.add(subject)
    |> subscribers.add(process.new_subject())
    |> subscribers.owner_down(process.self())
  assert subscribers.size(state) == 0
  subscribers.notify(state, 1)
  assert process.receive(subject, 0) == Error(Nil)
}

pub fn notify_reaches_each_subject_once_test() {
  let subject = process.new_subject()
  let state =
    subscribers.new()
    |> subscribers.add(subject)
    |> subscribers.add(subject)
  subscribers.notify(state, 7)
  assert process.receive(subject, 0) == Ok(7)
  assert process.receive(subject, 0) == Error(Nil)
}
