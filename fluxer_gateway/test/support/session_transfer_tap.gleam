// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/transfer
import fluxer_gateway/session/seed.{type Seed}
import gleam/erlang/process.{type Pid, type Subject}

pub type Event {
  Received(seed: Seed)
  Committing(seed: Seed, reply: Subject(Result(SessionRef, Nil)))
  Aborting(reply: Subject(Bool))
}

pub opaque type Tap {
  Tap(events: Subject(Event), release: Subject(Subject(Nil)))
}

type Input {
  Transfer(transfer.Message)
  Release(Subject(Nil))
}

pub fn start() -> Tap {
  let ready = process.new_subject()
  let events = process.new_subject()
  process.spawn_unlinked(fn() {
    let name = names.transfer()
    let original = process.named(name)
    let _ = process.unregister(name)
    let assert Ok(Nil) = process.register(process.self(), name)
    let release = process.new_subject()
    process.send(ready, release)
    let selector =
      process.new_selector()
      |> process.select_map(process.named_subject(name), Transfer)
      |> process.select_map(release, Release)
    serve(selector, events, original, Error(Nil))
  })
  let assert Ok(release) = process.receive(ready, 1000)
  Tap(events:, release:)
}

fn serve(
  selector: process.Selector(Input),
  events: Subject(Event),
  original: Result(Pid, Nil),
  last: Result(Seed, Nil),
) -> Nil {
  case process.selector_receive_forever(selector) {
    Transfer(transfer.Receive(found, _, reply)) -> {
      process.send(reply, True)
      process.send(events, Received(found))
      serve(selector, events, original, Ok(found))
    }
    Transfer(transfer.Commit(_, _, reply)) -> {
      case last {
        Ok(found) -> process.send(events, Committing(found, reply))
        Error(Nil) -> process.send(reply, Error(Nil))
      }
      serve(selector, events, original, last)
    }
    Transfer(transfer.Abort(_, _, reply)) -> {
      process.send(events, Aborting(reply))
      serve(selector, events, original, last)
    }
    Transfer(_) -> serve(selector, events, original, last)
    Release(done) -> {
      let _ = process.unregister(names.transfer())
      case original {
        Ok(pid) -> {
          let _ = process.register(pid, names.transfer())
          Nil
        }
        Error(Nil) -> Nil
      }
      process.send(done, Nil)
    }
  }
}

pub fn next(tap: Tap, timeout_ms: Int) -> Result(Event, Nil) {
  process.receive(tap.events, timeout_ms)
}

pub fn committing(
  tap: Tap,
  timeout_ms: Int,
) -> Result(#(Seed, Subject(Result(SessionRef, Nil))), Nil) {
  case next(tap, timeout_ms) {
    Ok(Committing(found, reply)) -> Ok(#(found, reply))
    Ok(_) -> committing(tap, timeout_ms)
    Error(Nil) -> Error(Nil)
  }
}

pub fn aborting(tap: Tap, timeout_ms: Int) -> Result(Subject(Bool), Nil) {
  case next(tap, timeout_ms) {
    Ok(Aborting(reply)) -> Ok(reply)
    Ok(_) -> aborting(tap, timeout_ms)
    Error(Nil) -> Error(Nil)
  }
}

pub fn release(tap: Tap) -> Nil {
  let done = process.new_subject()
  process.send(tap.release, done)
  let _ = process.receive(done, 1000)
  Nil
}
