// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Selector}
import gleam/int
import gleam/list
import gleam/result

pub type Outcome {
  HandedOver
  Interrupted
  Expired
}

type Signal {
  Term
  Left
}

type TimeUnit {
  Millisecond
}

type SignalMode {
  Handle
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "erlang", "binary_to_atom")
fn name_atom(name: String) -> Name(Signal)

@external(erlang, "erlang", "send")
fn send_raw(to: Pid, message: #(Atom)) -> #(Atom)

@external(erlang, "erlang", "halt")
fn halt(status: Int) -> Nil

@external(erlang, "init", "stop")
fn init_stop(status: Int) -> Atom

@external(erlang, "os", "set_signal")
fn set_signal(signal: Atom, mode: SignalMode) -> Atom

@external(erlang, "gen_event", "which_handlers")
fn which_handlers(manager: Atom) -> List(Dynamic)

@external(erlang, "gen_event", "add_handler")
fn add_handler(manager: Atom, handler: Atom, arguments: List(Nil)) -> Dynamic

@external(erlang, "gen_event", "swap_handler")
fn swap_handler(
  manager: Atom,
  from: #(Atom, List(Nil)),
  to: #(Atom, List(Nil)),
) -> Dynamic

fn name() -> Name(Signal) {
  name_atom("fluxer_bouncer_shutdown")
}

fn term_tag() -> Atom {
  atom.create("fluxer_bouncer_sigterm")
}

fn left_tag() -> Atom {
  atom.create("fluxer_bouncer_left")
}

fn signal_server() -> Atom {
  atom.create("erl_signal_server")
}

fn default_handler() -> Atom {
  atom.create("erl_signal_handler")
}

fn handler() -> Atom {
  atom.create("fluxer_bouncer@shutdown")
}

pub fn init(_arguments: Dynamic) -> Result(Nil, Nil) {
  Ok(Nil)
}

pub fn handle_event(event: Dynamic, state: Nil) -> Result(Nil, Nil) {
  case event == atom.to_dynamic(atom.create("sigterm")) {
    True -> on_sigterm()
    False -> Nil
  }
  Ok(state)
}

pub fn handle_call(_request: Dynamic, state: Nil) -> #(Atom, Nil, Nil) {
  #(atom.create("ok"), Nil, state)
}

fn on_sigterm() -> Nil {
  case process.named(name()) {
    Ok(pid) -> {
      let _ = send_raw(pid, #(term_tag()))
      Nil
    }
    Error(Nil) -> {
      let _ = init_stop(0)
      Nil
    }
  }
}

pub fn announce_left() -> Nil {
  case process.named(name()) {
    Ok(pid) -> {
      let _ = send_raw(pid, #(left_tag()))
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn installed() -> Bool {
  let handlers = which_handlers(signal_server())
  list.contains(handlers, atom.to_dynamic(handler()))
  && !list.contains(handlers, atom.to_dynamic(default_handler()))
}

pub fn claim() -> Result(Nil, Nil) {
  process.register(process.self(), name())
}

pub fn install() -> Result(Nil, Nil) {
  use Nil <- result.try(claim())
  let _ = set_signal(atom.create("sigterm"), Handle)
  let handlers = which_handlers(signal_server())
  let _ = case
    list.contains(handlers, atom.to_dynamic(handler())),
    list.contains(handlers, atom.to_dynamic(default_handler()))
  {
    False, True ->
      swap_handler(signal_server(), #(default_handler(), []), #(handler(), []))
    False, False -> add_handler(signal_server(), handler(), [])
    True, _ -> dynamic.nil()
  }
  case installed() {
    True -> Ok(Nil)
    False -> Error(Nil)
  }
}

fn term_selector() -> Selector(Signal) {
  process.new_selector()
  |> process.select_record(term_tag(), 0, fn(_) { Term })
}

fn drain_selector() -> Selector(Signal) {
  term_selector()
  |> process.select_record(left_tag(), 0, fn(_) { Left })
}

pub fn await_term() -> Nil {
  let _ = process.selector_receive_forever(term_selector())
  Nil
}

pub fn drain(bound_ms: Int, leave: fn() -> Nil) -> Outcome {
  leave()
  case process.selector_receive(drain_selector(), bound_ms) {
    Ok(Left) -> HandedOver
    Ok(Term) -> Interrupted
    Error(Nil) -> Expired
  }
}

pub fn run(bound_ms: Int, leave: fn() -> Nil) -> Nil {
  await_term()
  let started = monotonic_time(Millisecond)
  case drain(bound_ms, leave) {
    HandedOver -> {
      let _ = init_stop(0)
      process.sleep(int.max(0, started + bound_ms - monotonic_time(Millisecond)))
      halt(0)
    }
    Interrupted | Expired -> halt(0)
  }
}
