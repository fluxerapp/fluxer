// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/spawn
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list

pub type Lost {
  Crashed
  TimedOut
}

type Event(r) {
  Finished(pid: Pid, value: r)
  Down(pid: Pid)
}

type Running(a) {
  Running(monitor: Monitor, chunk: a)
}

const kill_wait_ms = 5000

pub fn run(
  chunks chunks: List(a),
  limit limit: Int,
  wait wait: fn() -> Int,
  work work: fn(a) -> r,
  state state: s,
  done done: fn(s, a, Result(r, Lost)) -> s,
) -> #(s, Bool) {
  loop(
    chunks,
    dict.new(),
    process.new_subject(),
    limit,
    wait,
    work,
    state,
    done,
  )
}

fn loop(
  pending: List(a),
  running: Dict(Pid, Running(a)),
  results: Subject(Event(r)),
  limit: Int,
  wait: fn() -> Int,
  work: fn(a) -> r,
  state: s,
  done: fn(s, a, Result(r, Lost)) -> s,
) -> #(s, Bool) {
  case pending, dict.size(running) < limit {
    [chunk, ..rest], True -> {
      let #(pid, monitor) =
        spawn.monitored(fn() {
          process.send(results, Finished(process.self(), work(chunk)))
        })
      let running = dict.insert(running, pid, Running(monitor, chunk))
      loop(rest, running, results, limit, wait, work, state, done)
    }
    _, _ ->
      case dict.is_empty(running) {
        True -> #(state, False)
        False ->
          case
            process.selector_receive(
              selector(running, results),
              int.max(0, wait()),
            )
          {
            Ok(event) -> {
              let #(running, state) =
                settle(running, event, Crashed, state, done)
              loop(pending, running, results, limit, wait, work, state, done)
            }
            Error(Nil) -> {
              dict.each(running, fn(pid, _) { process.kill(pid) })
              let state = reap(running, results, state, done)
              let state =
                list.fold(pending, state, fn(state, chunk) {
                  done(state, chunk, Error(TimedOut))
                })
              #(state, True)
            }
          }
      }
  }
}

fn selector(
  running: Dict(Pid, Running(a)),
  results: Subject(Event(r)),
) -> process.Selector(Event(r)) {
  dict.fold(
    running,
    process.new_selector() |> process.select(results),
    fn(selector, pid, entry) {
      process.select_specific_monitor(selector, entry.monitor, fn(_) {
        Down(pid)
      })
    },
  )
}

fn settle(
  running: Dict(Pid, Running(a)),
  event: Event(r),
  lost: Lost,
  state: s,
  done: fn(s, a, Result(r, Lost)) -> s,
) -> #(Dict(Pid, Running(a)), s) {
  case dict.get(running, event.pid) {
    Error(Nil) -> #(running, state)
    Ok(entry) -> {
      process.demonitor_process(entry.monitor)
      let outcome = case event {
        Finished(value:, ..) -> Ok(value)
        Down(_) -> Error(lost)
      }
      #(dict.delete(running, event.pid), done(state, entry.chunk, outcome))
    }
  }
}

fn reap(
  running: Dict(Pid, Running(a)),
  results: Subject(Event(r)),
  state: s,
  done: fn(s, a, Result(r, Lost)) -> s,
) -> s {
  case dict.is_empty(running) {
    True -> state
    False ->
      case process.selector_receive(selector(running, results), kill_wait_ms) {
        Ok(event) -> {
          let #(running, state) = settle(running, event, TimedOut, state, done)
          reap(running, results, state, done)
        }
        Error(Nil) ->
          dict.fold(running, state, fn(state, _, entry) {
            process.demonitor_process(entry.monitor)
            done(state, entry.chunk, Error(TimedOut))
          })
      }
  }
}
