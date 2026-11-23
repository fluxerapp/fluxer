// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/breaker.{type Breaker}
import fluxer_gateway/api/metrics as api_metrics
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/http
import gleam/http/request.{type Request}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import gleam/uri
import logging

fn init_timeout_ms() -> Int {
  durations.api().http_init_timeout_ms
}

fn half_open_retry_ms() -> Int {
  durations.api().http_half_open_retry_ms
}

fn open_retry_floor_ms() -> Int {
  durations.api().http_open_retry_floor_ms
}

const default_max_concurrency = 512

fn default_recovery_timeout_ms() -> Int {
  durations.api().http_default_recovery_timeout_ms
}

const open_jitter_percent = 20

const mode_cell = 1

const until_cell = 2

const probes_cell = 3

const in_flight_cell = 4

const closed_mode = 0

const open_mode = 1

const half_open_mode = 2

fn keep_alive_timeout_ms() -> Int {
  durations.api().http_keep_alive_timeout_ms
}

const profile_shards = 8

const worker_grace_ms = 1000

pub type HttpFailure {
  CircuitOpen(retry_after_ms: Int)
  Overloaded
  Timeout
  Reset(reason: String)
  Transport(reason: String)
  BadUrl
}

pub type Response {
  Response(status: Int, body: BitArray)
}

pub opaque type Message {
  Held(caller: Pid, probe: Bool)
  Done(caller: Pid, outcome: breaker.Outcome)
  HalfOpenNow(until: Int)
  CallerDown(process.Down)
  ResetBreaker(reply: Subject(Nil))
}

type Holder {
  Holder(monitor: Monitor, probe: Bool)
}

type State {
  State(
    self: Subject(Message),
    breaker: Breaker,
    cells: Atomics,
    holders: Dict(Pid, Holder),
  )
}

type Gate {
  Gate(
    cells: Atomics,
    max_concurrency: Int,
    probes: Int,
    profiles: List(atom.Atom),
  )
}

type Atomics

type AtomicsOption {
  Signed(Bool)
}

type Key {
  FluxerGatewayApiGate
}

type Stored

type HttpcOption {
  MaxSessions(Int)
  MaxKeepAliveLength(Int)
  MaxPipelineLength(Int)
  KeepAliveTimeout(Int)
  Ipfamily(IpFamily)
}

type RequestOption {
  BodyFormat(BodyFormat)
}

type BodyFormat {
  Binary
}

type ServiceOption {
  Profile(atom.Atom)
}

type IpFamily {
  Inet
}

type StatusLine =
  #(Charlist, Int, Charlist)

type Reply =
  #(StatusLine, List(#(Charlist, Charlist)), BitArray)

@external(erlang, "httpc", "request")
fn httpc_request(
  method: http.Method,
  request: #(Charlist, List(#(Charlist, Charlist)), Charlist, BitArray),
  http_options: List(#(atom.Atom, Dynamic)),
  options: List(RequestOption),
  profile: atom.Atom,
) -> Result(Reply, Dynamic)

@external(erlang, "httpc", "set_options")
fn httpc_set_options(options: List(HttpcOption), profile: atom.Atom) -> Dynamic

@external(erlang, "inets", "start")
fn inets_start(
  service: atom.Atom,
  options: List(ServiceOption),
) -> Result(Pid, Dynamic)

@external(erlang, "erlang", "spawn")
fn spawn_quiet(work: fn() -> Nil) -> Pid

@external(erlang, "erlang", "phash2")
fn phash2(term: Pid, range: Int) -> Int

@external(erlang, "atomics", "new")
fn atomics_new(size: Int, options: List(AtomicsOption)) -> Atomics

@external(erlang, "atomics", "get")
fn atomics_get(cells: Atomics, index: Int) -> Int

@external(erlang, "atomics", "put")
fn atomics_put(cells: Atomics, index: Int, value: Int) -> Stored

@external(erlang, "atomics", "add_get")
fn atomics_add_get(cells: Atomics, index: Int, by: Int) -> Int

@external(erlang, "atomics", "sub")
fn atomics_sub(cells: Atomics, index: Int, by: Int) -> Stored

@external(erlang, "persistent_term", "get")
fn get_gate(key: Key, default: Option(Gate)) -> Option(Gate)

@external(erlang, "persistent_term", "put")
fn put_gate(key: Key, value: Option(Gate)) -> Stored

@external(erlang, "rand", "uniform")
fn uniform(range: Int) -> Int

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway_api_http")
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() { start(settings) })
}

fn start(settings: Settings) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    use profiles <- result.try(start_profiles())
    let cells = atomics_new(4, [Signed(True)])
    let config =
      breaker.Config(
        ..breaker.default_config(),
        min_failures: settings.gateway_http_failure_threshold,
      )
    put_gate(
      FluxerGatewayApiGate,
      Some(Gate(
        cells:,
        max_concurrency: positive_or(
          settings.gateway_http_rpc_max_concurrency,
          default_max_concurrency,
        ),
        probes: config.probes,
        profiles:,
      )),
    )
    api_metrics.breaker_closed()
    api_metrics.watch_in_flight(in_flight)
    State(self:, breaker: breaker.new(config), cells:, holders: dict.new())
    |> actor.initialised
    |> actor.selecting(
      process.new_selector()
      |> process.select(self)
      |> process.select_monitors(CallerDown),
    )
    |> Ok
  })
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

fn start_profiles() -> Result(List(atom.Atom), String) {
  list.repeat(Nil, profile_shards)
  |> list.index_map(fn(_, index) {
    atom.create("fluxer_gateway_api_http_" <> int.to_string(index))
  })
  |> list.try_map(fn(profile) {
    use Nil <- result.map(start_profile(profile))
    profile
  })
}

fn start_profile(profile: atom.Atom) -> Result(Nil, String) {
  let started = case inets_start(atom.create("httpc"), [Profile(profile)]) {
    Ok(_) -> Ok(Nil)
    Error(reason) ->
      case decode.run(reason, decode.at([0], atom.decoder())) {
        Ok(tag) ->
          case atom.to_string(tag) {
            "already_started" -> Ok(Nil)
            _ -> Error(string.inspect(reason))
          }
        Error(_) -> Error(string.inspect(reason))
      }
  }
  use Nil <- result.try(started)
  let stored =
    httpc_set_options(
      [
        MaxSessions(256),
        MaxKeepAliveLength(0),
        MaxPipelineLength(0),
        KeepAliveTimeout(keep_alive_timeout_ms()),
        Ipfamily(Inet),
      ],
      profile,
    )
  case decode.run(stored, atom.decoder()) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(string.inspect(stored))
  }
}

fn pick_profile() -> atom.Atom {
  case get_gate(FluxerGatewayApiGate, None) {
    Some(Gate(profiles:, ..)) ->
      case list.drop(profiles, phash2(process.self(), list.length(profiles))) {
        [profile, ..] -> profile
        [] -> atom.create("default")
      }
    None -> atom.create("default")
  }
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  let now = clock.monotonic_ms()
  case message {
    Held(caller:, probe:) -> {
      let holder = Holder(monitor: process.monitor(caller), probe:)
      actor.continue(
        State(..state, holders: dict.insert(state.holders, caller, holder)),
      )
    }
    Done(caller:, outcome:) ->
      case dict.get(state.holders, caller) {
        Ok(holder) -> {
          let state = release(state, caller, holder)
          case breaker.state(state.breaker), holder.probe {
            breaker.HalfOpen(_), False -> actor.continue(state)
            _, _ -> actor.continue(record(state, outcome, now))
          }
        }
        Error(Nil) -> actor.continue(state)
      }
    CallerDown(process.ProcessDown(pid:, ..)) ->
      case dict.get(state.holders, pid) {
        Ok(holder) -> {
          let state = release(state, pid, holder)
          case holder.probe {
            True -> actor.continue(record(state, breaker.Failure, now))
            False -> actor.continue(state)
          }
        }
        Error(Nil) -> actor.continue(state)
      }
    CallerDown(process.PortDown(..)) -> actor.continue(state)
    HalfOpenNow(until) ->
      case breaker.state(state.breaker) {
        breaker.Open(current) if current == until -> {
          let before = breaker.state(state.breaker)
          let next = breaker.expire(state.breaker, int.max(now, until))
          actor.continue(changed(State(..state, breaker: next), before))
        }
        _ -> actor.continue(state)
      }
    ResetBreaker(reply:) -> {
      let before = breaker.state(state.breaker)
      let next = breaker.reset(state.breaker)
      process.send(reply, Nil)
      actor.continue(changed(State(..state, breaker: next), before))
    }
  }
}

fn release(state: State, caller: Pid, holder: Holder) -> State {
  process.demonitor_process(holder.monitor)
  atomics_sub(state.cells, in_flight_cell, 1)
  State(..state, holders: dict.delete(state.holders, caller))
}

fn record(state: State, outcome: breaker.Outcome, now: Int) -> State {
  let before = breaker.state(state.breaker)
  let open_ms = recovery_timeout_ms()
  let jitter = open_ms * open_jitter_percent / 100
  let open_for = open_ms - jitter + uniform(jitter * 2 + 1) - 1
  let next = breaker.record(state.breaker, outcome, now, open_for)
  changed(State(..state, breaker: next), before)
}

fn changed(state: State, before: breaker.State) -> State {
  let after = breaker.state(state.breaker)
  case before == after {
    True -> state
    False -> {
      case after {
        breaker.Closed -> {
          atomics_put(state.cells, mode_cell, closed_mode)
          logging.log(logging.Info, "API circuit breaker closed")
          api_metrics.breaker("closed", 0)
        }
        breaker.Open(until) -> {
          atomics_put(state.cells, until_cell, until)
          atomics_put(state.cells, mode_cell, open_mode)
          process.send_after(
            state.self,
            int.max(until - clock.monotonic_ms(), 0),
            HalfOpenNow(until),
          )
          logging.log(logging.Warning, "API circuit breaker opening")
          api_metrics.breaker("open", 1)
        }
        breaker.HalfOpen(0) -> {
          atomics_put(state.cells, probes_cell, 0)
          atomics_put(state.cells, mode_cell, half_open_mode)
          api_metrics.breaker("half_open", 2)
        }
        breaker.HalfOpen(_) -> Nil
      }
      state
    }
  }
}

type Admission {
  Admitted(gate: Option(Gate))
  Refused(HttpFailure)
}

fn admit() -> Admission {
  case get_gate(FluxerGatewayApiGate, None) {
    None -> Admitted(None)
    Some(gate) ->
      case breaker_allows(gate) {
        Error(failure) -> Refused(failure)
        Ok(probe) ->
          case
            atomics_add_get(gate.cells, in_flight_cell, 1)
            > gate.max_concurrency
          {
            True -> {
              atomics_sub(gate.cells, in_flight_cell, 1)
              case probe {
                True -> {
                  atomics_sub(gate.cells, probes_cell, 1)
                  Nil
                }
                False -> Nil
              }
              Refused(Overloaded)
            }
            False ->
              case process.named(name()) {
                Ok(_) -> {
                  process.send(
                    process.named_subject(name()),
                    Held(caller: process.self(), probe:),
                  )
                  Admitted(Some(gate))
                }
                Error(Nil) -> {
                  atomics_sub(gate.cells, in_flight_cell, 1)
                  Admitted(None)
                }
              }
          }
      }
  }
}

pub fn is_open() -> Bool {
  case get_gate(FluxerGatewayApiGate, None) {
    None -> False
    Some(gate) -> atomics_get(gate.cells, mode_cell) == open_mode
  }
}

@internal
pub fn reset_breaker() -> Nil {
  case process.named(name()) {
    Ok(_) -> process.call(process.named_subject(name()), 5000, ResetBreaker)
    Error(Nil) -> Nil
  }
}

@internal
pub fn in_flight() -> Int {
  case get_gate(FluxerGatewayApiGate, None) {
    None -> 0
    Some(gate) -> atomics_get(gate.cells, in_flight_cell)
  }
}

@internal
pub fn is_closed() -> Bool {
  case get_gate(FluxerGatewayApiGate, None) {
    None -> True
    Some(gate) -> atomics_get(gate.cells, mode_cell) == closed_mode
  }
}

fn breaker_allows(gate: Gate) -> Result(Bool, HttpFailure) {
  let mode = atomics_get(gate.cells, mode_cell)
  case mode {
    _ if mode == open_mode -> {
      let left = atomics_get(gate.cells, until_cell) - clock.monotonic_ms()
      Error(CircuitOpen(int.max(left, open_retry_floor_ms())))
    }
    _ if mode == half_open_mode ->
      case atomics_add_get(gate.cells, probes_cell, 1) > gate.probes {
        True -> Error(CircuitOpen(half_open_retry_ms()))
        False -> Ok(True)
      }
    _ -> Ok(False)
  }
}

pub fn post(
  url: String,
  headers: List(#(String, String)),
  body: BitArray,
  timeout_ms: Int,
) -> Result(Response, HttpFailure) {
  use base <- result.try(request.to(url) |> result.replace_error(BadUrl))
  case admit() {
    Refused(failure) -> Error(failure)
    Admitted(gate) -> {
      let outgoing =
        list.fold(headers, base, fn(req, header) {
          request.set_header(req, header.0, header.1)
        })
        |> request.set_method(http.Post)
        |> request.set_body(body)
      let outcome = send(outgoing, int.max(timeout_ms, 1))
      finish(gate, outcome)
      outcome
    }
  }
}

fn send(
  outgoing: Request(BitArray),
  timeout_ms: Int,
) -> Result(Response, HttpFailure) {
  let content_type =
    request.get_header(outgoing, "content-type")
    |> result.unwrap("application/octet-stream")
  let headers =
    list.filter_map(outgoing.headers, fn(header) {
      case header.0 {
        "content-type" -> Error(Nil)
        _ ->
          Ok(#(charlist.from_string(header.0), charlist.from_string(header.1)))
      }
    })
  let url = outgoing |> request.to_uri |> uri.to_string |> charlist.from_string
  let profile = pick_profile()
  let replies = process.new_subject()
  let worker =
    spawn_quiet(fn() {
      process.send(
        replies,
        httpc_request(
          outgoing.method,
          #(url, headers, charlist.from_string(content_type), outgoing.body),
          [
            #(atom.create("timeout"), dynamic.int(timeout_ms)),
            #(atom.create("autoredirect"), dynamic.bool(False)),
          ],
          [BodyFormat(Binary)],
          profile,
        ),
      )
    })
  let monitor = process.monitor(worker)
  let selector =
    process.new_selector()
    |> process.select_map(replies, Replied)
    |> process.select_specific_monitor(monitor, Crashed)
  case process.selector_receive(selector, timeout_ms + worker_grace_ms) {
    Ok(Replied(reply)) -> {
      process.demonitor_process(monitor)
      case reply {
        Ok(#(#(_, status, _), _, body)) -> Ok(Response(status, body))
        Error(reason) -> Error(failure(reason))
      }
    }
    Ok(Crashed(down)) -> {
      logging.log(
        logging.Warning,
        "API HTTP request crashed: " <> crash_reason(down),
      )
      let _ = start_profiles()
      Error(Transport("request_exception"))
    }
    Error(Nil) -> {
      process.kill(worker)
      let _ = process.selector_receive(selector, worker_grace_ms)
      process.demonitor_process(monitor)
      Error(Timeout)
    }
  }
}

type Attempt {
  Replied(Result(Reply, Dynamic))
  Crashed(process.Down)
}

fn crash_reason(down: process.Down) -> String {
  case down.reason {
    process.Normal -> "normal"
    process.Killed -> "killed"
    process.Abnormal(reason) ->
      decode.run(
        reason,
        decode.one_of(atom.decoder(), [decode.at([0], atom.decoder())]),
      )
      |> result.map(atom.to_string)
      |> result.unwrap("abnormal")
  }
}

fn failure(reason: Dynamic) -> HttpFailure {
  let tag =
    decode.run(
      reason,
      decode.one_of(atom.decoder(), [decode.at([0], atom.decoder())]),
    )
    |> result.map(atom.to_string)
  case tag {
    Ok("timeout") -> Timeout
    Ok("socket_closed_remotely") | Ok("session_remotely_closed") ->
      Reset(string.inspect(reason))
    _ -> Transport(string.inspect(reason))
  }
}

fn finish(gate: Option(Gate), outcome: Result(Response, HttpFailure)) -> Nil {
  let recorded = case outcome {
    Ok(Response(status, _)) if status >= 500 -> breaker.Failure
    Ok(_) -> breaker.Success
    Error(_) -> breaker.Failure
  }
  case gate, process.named(name()) {
    Some(_), Ok(_) ->
      process.send(
        process.named_subject(name()),
        Done(caller: process.self(), outcome: recorded),
      )
    Some(found), Error(Nil) -> {
      atomics_sub(found.cells, in_flight_cell, 1)
      Nil
    }
    None, _ -> Nil
  }
}

fn recovery_timeout_ms() -> Int {
  positive_or(
    settings.get().gateway_http_recovery_timeout_ms,
    default_recovery_timeout_ms(),
  )
}

fn positive_or(value: Int, default: Int) -> Int {
  case value > 0 {
    True -> value
    False -> default
  }
}
