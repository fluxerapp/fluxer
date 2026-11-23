// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/result
import gleam/set.{type Set}

pub opaque type Subscribers(message) {
  Subscribers(owners: Dict(Pid, Owner(message)))
}

type Owner(message) {
  Owner(monitor: process.Monitor, subjects: Set(Subject(message)))
}

pub fn new() -> Subscribers(message) {
  Subscribers(dict.new())
}

pub fn add(
  subscribers: Subscribers(message),
  subject: Subject(message),
) -> Subscribers(message) {
  case process.subject_owner(subject) {
    Error(Nil) -> subscribers
    Ok(pid) -> {
      let owner = case dict.get(subscribers.owners, pid) {
        Ok(owner) ->
          Owner(..owner, subjects: set.insert(owner.subjects, subject))
        Error(Nil) ->
          Owner(
            monitor: process.monitor(pid),
            subjects: set.from_list([subject]),
          )
      }
      Subscribers(dict.insert(subscribers.owners, pid, owner))
    }
  }
}

pub fn remove(
  subscribers: Subscribers(message),
  subject: Subject(message),
) -> Subscribers(message) {
  {
    use pid <- result.try(process.subject_owner(subject))
    use owner <- result.map(dict.get(subscribers.owners, pid))
    let subjects = set.delete(owner.subjects, subject)
    case set.is_empty(subjects) {
      True -> {
        process.demonitor_process(owner.monitor)
        dict.delete(subscribers.owners, pid)
      }
      False -> dict.insert(subscribers.owners, pid, Owner(..owner, subjects:))
    }
  }
  |> result.map(Subscribers)
  |> result.unwrap(subscribers)
}

pub fn owner_down(
  subscribers: Subscribers(message),
  pid: Pid,
) -> Subscribers(message) {
  Subscribers(dict.delete(subscribers.owners, pid))
}

pub fn notify(subscribers: Subscribers(message), message: message) -> Nil {
  dict.each(subscribers.owners, fn(_, owner) {
    set.each(owner.subjects, process.send(_, message))
  })
}

pub fn size(subscribers: Subscribers(message)) -> Int {
  dict.fold(subscribers.owners, 0, fn(total, _, owner) {
    total + set.size(owner.subjects)
  })
}

pub fn watches(subscribers: Subscribers(message), pid: Pid) -> Bool {
  dict.has_key(subscribers.owners, pid)
}
