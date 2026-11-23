// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type SessionRef, type SocketMsg, type StartError, DispatchFrame,
}
import fluxer_gateway/guild/manager/registry as guild_registry
import fluxer_gateway/session/manager
import fluxer_gateway/session/manager/registry
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import support/session_harness as h
import support/storm_api.{type World}

pub type Outcome {
  Outcome(
    ready_ms: Int,
    all_ms: Int,
    start_retries: Int,
    unavailable_in_ready: Int,
    guild_deletes: Int,
    frames: Int,
    failure: String,
  )
}

pub type NodeReport {
  NodeReport(
    node: String,
    clients: Int,
    outcomes: List(Outcome),
    sessions: Int,
    memory_bytes: Int,
    client_bytes: Int,
    scheduler_mean: Float,
    scheduler_peak: Float,
    joins_expired: Int,
    wall_ms: Int,
    cpu_ms: Int,
    hot: List(String),
  )
}

pub opaque type Control {
  Go(at_ms: Int, deadline_ms: Int, outcomes: Subject(Outcome))
  Cut(done: Subject(Nil))
  Measure(reply: Subject(Int))
  Stop
}

type Event {
  Control(Control)
  Socket(SocketMsg)
  Started(Result(SessionRef, StartError))
  SessionDown(process.Down)
}

type Client {
  Client(
    world: World,
    n: Int,
    control: Subject(Control),
    outbox: Subject(SocketMsg),
    replies: Subject(Result(SessionRef, StartError)),
    session: Option(#(SessionRef, Monitor)),
  )
}

type Wave {
  Wave(
    started_ms: Int,
    deadline_ms: Int,
    outcomes: Subject(Outcome),
    retries: Int,
    ready_ms: Int,
    expected: Int,
    available: Set(Int),
    unavailable_in_ready: Int,
    deletes: Int,
    frames: Int,
    reported: Bool,
  )
}

@external(erlang, "persistent_term", "put")
fn put_term(key: Atom, value: a) -> Atom

@external(erlang, "persistent_term", "get")
fn get_term(key: Atom, default: a) -> a

@external(erlang, "erlang", "memory")
fn memory_total(kind: Atom) -> Int

@external(erlang, "erlang", "process_info")
fn process_memory(pid: process.Pid, item: Atom) -> #(Atom, Int)

@external(erlang, "erlang", "system_flag")
fn system_flag(flag: Atom, value: Bool) -> Bool

@external(erlang, "erlang", "statistics")
fn statistics(kind: Atom) -> List(#(Int, Int, Int))

@external(erlang, "erlang", "node")
fn node_name() -> Atom

fn fleet_key() -> Atom {
  atom.create("fluxer_gateway_storm_fleet")
}

pub fn start_fleet(world: World, from: Int, count: Int) -> Int {
  let fleet =
    int.range(from, from + count, [], fn(acc, n) {
      [spawn_client(world, n), ..acc]
    })
  put_term(fleet_key(), fleet)
  list.length(fleet)
}

fn fleet() -> List(Subject(Control)) {
  get_term(fleet_key(), [])
}

pub fn stop_fleet() -> Nil {
  list.each(fleet(), process.send(_, Stop))
  put_term(fleet_key(), [])
  Nil
}

pub fn wave(spread_ms: Int, deadline_ms: Int) -> NodeReport {
  let clients = fleet()
  collect_garbage()
  let memory_before = gateway_memory()
  let expired_before = joins_expired()
  let cpu_before = runtime_ms()
  let sampler = start_sampler()
  let hot = process.new_subject()
  process.spawn_unlinked(fn() {
    process.sleep(
      envoy.get("FLUXER_STORM_PROFILE_AT_MS")
      |> result.try(int.parse)
      |> result.unwrap(spread_ms / 2),
    )
    profile_once()
    process.send(hot, hot_processes(2000))
  })
  let outcomes = process.new_subject()
  let started = h.now_ms()
  let deadline = started + spread_ms + deadline_ms
  start_trace(started, deadline)
  list.each(clients, fn(client) {
    process.send(
      client,
      Go(started + int.random(spread_ms + 1), deadline, outcomes),
    )
  })
  let collected = gather(outcomes, list.length(clients), deadline + 1000, [])
  let wall = h.now_ms() - started
  let #(mean, peak) = stop_sampler(sampler)
  collect_garbage()
  let client_bytes = client_memory(clients)
  NodeReport(
    node: atom.to_string(node_name()),
    clients: list.length(clients),
    outcomes: collected,
    sessions: registry.count(),
    memory_bytes: gateway_memory() - memory_before,
    client_bytes:,
    scheduler_mean: mean,
    scheduler_peak: peak,
    joins_expired: joins_expired() - expired_before,
    wall_ms: wall,
    cpu_ms: runtime_ms() - cpu_before,
    hot: process.receive(hot, 60_000) |> result.unwrap([]),
  )
}

pub fn cut() -> Int {
  let done = process.new_subject()
  let clients = fleet()
  list.each(clients, fn(client) { process.send(client, Cut(done)) })
  list.fold(clients, 0, fn(count, _) {
    case process.receive(done, 30_000) {
      Ok(Nil) -> count + 1
      Error(Nil) -> count
    }
  })
}

fn gather(
  outcomes: Subject(Outcome),
  left: Int,
  deadline: Int,
  acc: List(Outcome),
) -> List(Outcome) {
  case left {
    0 -> acc
    _ ->
      case process.receive(outcomes, int.max(deadline - h.now_ms(), 0)) {
        Ok(outcome) -> gather(outcomes, left - 1, deadline, [outcome, ..acc])
        Error(Nil) -> acc
      }
  }
}

fn collect_garbage() -> Nil {
  list.each(process_list(), fn(pid) { garbage_collect_pid(pid) })
}

@external(erlang, "erlang", "processes")
fn process_list() -> List(process.Pid)

@external(erlang, "erlang", "garbage_collect")
fn garbage_collect_pid(pid: process.Pid) -> Bool

fn gateway_memory() -> Int {
  memory_total(atom.create("total"))
}

fn client_memory(clients: List(Subject(Control))) -> Int {
  let reply = process.new_subject()
  list.each(clients, fn(client) { process.send(client, Measure(reply)) })
  list.fold(clients, 0, fn(sum, _) {
    case process.receive(reply, 5000) {
      Ok(bytes) -> sum + bytes
      Error(Nil) -> sum
    }
  })
}

fn joins_expired() -> Int {
  metrics.total(rollout_metrics.joins_expired, fn(_) { True })
}

type Sampler {
  Sample(reply: Subject(#(Float, Float)))
}

fn start_sampler() -> Subject(Sampler) {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let _ = system_flag(atom.create("scheduler_wall_time"), True)
    let control = process.new_subject()
    process.send(ready, control)
    let first = wall_time()
    sample_loop(control, first, first, 0.0)
  })
  let assert Ok(control) = process.receive(ready, 5000)
  control
}

fn stop_sampler(sampler: Subject(Sampler)) -> #(Float, Float) {
  process.call(sampler, 10_000, Sample)
}

fn sample_loop(
  control: Subject(Sampler),
  first: List(#(Int, Int, Int)),
  last: List(#(Int, Int, Int)),
  peak: Float,
) -> Nil {
  case process.receive(control, 500) {
    Ok(Sample(reply)) -> {
      let now = wall_time()
      process.send(reply, #(
        utilisation(first, now),
        float.max(peak, utilisation(last, now)),
      ))
    }
    Error(Nil) -> {
      let now = wall_time()
      sample_loop(control, first, now, float.max(peak, utilisation(last, now)))
    }
  }
}

fn wall_time() -> List(#(Int, Int, Int)) {
  let normal = schedulers(atom.create("schedulers"))
  statistics(atom.create("scheduler_wall_time"))
  |> list.filter(fn(row) { row.0 <= normal })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
}

fn utilisation(
  from: List(#(Int, Int, Int)),
  to: List(#(Int, Int, Int)),
) -> Float {
  let #(active, total) =
    list.zip(from, to)
    |> list.fold(#(0, 0), fn(sum, pair) {
      let #(#(_, a0, t0), #(_, a1, t1)) = pair
      #(sum.0 + a1 - a0, sum.1 + t1 - t0)
    })
  case total {
    0 -> 0.0
    _ -> int.to_float(active) /. int.to_float(total)
  }
}

fn spawn_client(world: World, n: Int) -> Subject(Control) {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let control = process.new_subject()
    process.send(ready, control)
    idle(Client(
      world:,
      n:,
      control:,
      outbox: process.new_subject(),
      replies: process.new_subject(),
      session: None,
    ))
  })
  let assert Ok(control) = process.receive(ready, 5000)
  control
}

fn selector(client: Client) -> process.Selector(Event) {
  let base =
    process.new_selector()
    |> process.select_map(client.control, Control)
    |> process.select_map(client.outbox, Socket)
    |> process.select_map(client.replies, Started)
  case client.session {
    Some(#(_, monitor)) ->
      process.select_specific_monitor(base, monitor, SessionDown)
    None -> base
  }
}

fn idle(client: Client) -> Nil {
  case process.selector_receive_forever(selector(client)) {
    Control(Go(at_ms:, deadline_ms:, outcomes:)) -> {
      process.sleep(int.max(at_ms - h.now_ms(), 0))
      let started = h.now_ms()
      identify(
        client,
        Wave(
          started_ms: started,
          deadline_ms:,
          outcomes:,
          retries: 0,
          ready_ms: -1,
          expected: list.length(storm_api.guilds_of(client.world, client.n)),
          available: set.new(),
          unavailable_in_ready: 0,
          deletes: 0,
          frames: 0,
          reported: False,
        ),
      )
    }
    Control(Cut(done)) -> {
      let client = end_session(client)
      process.send(done, Nil)
      idle(client)
    }
    Control(Measure(reply)) -> {
      measure(reply)
      idle(client)
    }
    Control(Stop) -> {
      let _ = end_session(client)
      Nil
    }
    Socket(_) | Started(_) -> idle(client)
    SessionDown(_) -> idle(Client(..client, session: None))
  }
}

fn measure(reply: Subject(Int)) -> Nil {
  let #(_, bytes) = process_memory(process.self(), atom.create("memory"))
  process.send(reply, bytes)
}

fn end_session(client: Client) -> Client {
  case client.session {
    None -> client
    Some(#(ref, monitor)) -> {
      session.send(ref, session.Terminate(session.TerminatedByManager))
      let _ =
        process.new_selector()
        |> process.select_specific_monitor(monitor, fn(_) { Nil })
        |> process.selector_receive(10_000)
      process.demonitor_process(monitor)
      flush(client.outbox)
      Client(..client, session: None)
    }
  }
}

fn flush(outbox: Subject(SocketMsg)) -> Nil {
  case process.receive(outbox, 0) {
    Ok(_) -> flush(outbox)
    Error(Nil) -> Nil
  }
}

fn identify(client: Client, wave: Wave) -> Nil {
  manager.start(
    h.identify(storm_api.token(client.n)),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    session.SocketRef(pid: process.self(), outbox: client.outbox),
    client.replies,
  )
  running(client, wave)
}

fn running(client: Client, wave: Wave) -> Nil {
  let left = wave.deadline_ms - h.now_ms()
  case wave.reported, left <= 0 {
    False, True -> {
      let wave = report(wave, "deadline")
      running(client, wave)
    }
    _, _ -> {
      let wait = case wave.reported {
        True -> 60_000
        False -> left
      }
      case process.selector_receive(selector(client), wait) {
        Error(Nil) -> running(client, wave)
        Ok(event) -> on_event(client, wave, event)
      }
    }
  }
}

fn on_event(client: Client, wave: Wave, event: Event) -> Nil {
  case event {
    Started(Ok(ref)) ->
      case process.subject_owner(ref.inbox) {
        Ok(pid) ->
          running(
            Client(..client, session: Some(#(ref, process.monitor(pid)))),
            wave,
          )
        Error(Nil) -> running(client, report(wave, "no session pid"))
      }
    Started(Error(session.StartUnavailable(..)))
    | Started(Error(session.StartRateLimited(..))) -> {
      process.sleep(1000 + int.random(1000))
      case h.now_ms() < wave.deadline_ms {
        True -> identify(client, Wave(..wave, retries: wave.retries + 1))
        False -> running(client, report(wave, "start retries ran out"))
      }
    }
    Started(Error(other)) ->
      running(client, report(wave, "start failed " <> start_error(other)))
    Socket(session.Send(_, frames)) ->
      running(client, list.fold(frames, wave, on_frame))
    Socket(session.Close(_, _, reason)) ->
      running(
        Client(..client, session: None),
        report(wave, "closed " <> reason),
      )
    Socket(session.Moved(from:, ..)) -> {
      session.send(from, session.SocketDown(process.self()))
      running(client, wave)
    }
    Socket(session.Reattached(..)) -> running(client, wave)
    SessionDown(_) ->
      running(Client(..client, session: None), report(wave, "session down"))
    Control(Cut(done)) -> {
      let _ = report(wave, "cut")
      let client = end_session(client)
      process.send(done, Nil)
      idle(client)
    }
    Control(Measure(reply)) -> {
      measure(reply)
      running(client, wave)
    }
    Control(Stop) -> {
      let _ = end_session(client)
      Nil
    }
    Control(Go(..)) -> running(client, wave)
  }
}

fn start_error(error: StartError) -> String {
  case error {
    session.StartAuthFailed -> "auth"
    session.StartShardingRequired -> "sharding"
    session.StartTooManySessions -> "too many sessions"
    session.StartUnknownError -> "unknown"
    session.StartRateLimited(..) -> "rate limited"
    session.StartUnavailable(..) -> "unavailable"
  }
}

fn on_frame(wave: Wave, frame: session.Outbound) -> Wave {
  let wave = Wave(..wave, frames: wave.frames + 1)
  case frame {
    DispatchFrame(_, "READY", payload) -> {
      let #(available, unavailable) = ready_guilds(payload)
      Wave(
        ..wave,
        ready_ms: h.now_ms() - wave.started_ms,
        available: set.union(wave.available, available),
        unavailable_in_ready: unavailable,
      )
      |> settle
    }
    DispatchFrame(_, "GUILD_CREATE", payload) ->
      case guild_id(payload) {
        Ok(guild) ->
          Wave(..wave, available: set.insert(wave.available, guild)) |> settle
        Error(Nil) -> wave
      }
    DispatchFrame(_, "GUILD_DELETE", payload) ->
      Wave(
        ..wave,
        deletes: wave.deletes + 1,
        available: case guild_id(payload) {
          Ok(guild) -> set.delete(wave.available, guild)
          Error(Nil) -> wave.available
        },
      )
    _ -> wave
  }
}

fn settle(wave: Wave) -> Wave {
  case
    !wave.reported
    && wave.ready_ms >= 0
    && set.size(wave.available) >= wave.expected
  {
    True -> report(wave, "")
    False -> wave
  }
}

fn report(wave: Wave, failure: String) -> Wave {
  case wave.reported {
    True -> wave
    False -> {
      let all_ms = case failure {
        "" -> h.now_ms() - wave.started_ms
        _ -> -1
      }
      process.send(
        wave.outcomes,
        Outcome(
          ready_ms: wave.ready_ms,
          all_ms:,
          start_retries: wave.retries,
          unavailable_in_ready: wave.unavailable_in_ready,
          guild_deletes: wave.deletes,
          frames: wave.frames,
          failure:,
        ),
      )
      Wave(..wave, reported: True)
    }
  }
}

fn ready_guilds(payload: RawJson) -> #(Set(Int), Int) {
  raw_json.field(payload, "guilds")
  |> result.try(raw_json.items)
  |> result.unwrap([])
  |> list.fold(#(set.new(), 0), fn(acc, guild) {
    case
      raw_json.field(guild, "unavailable")
      |> result.try(raw_json.read(_, decode.bool))
    {
      Ok(True) -> #(acc.0, acc.1 + 1)
      _ ->
        case guild_id(guild) {
          Ok(found) -> #(set.insert(acc.0, found), acc.1)
          Error(Nil) -> acc
        }
    }
  })
}

fn guild_id(payload: RawJson) -> Result(Int, Nil) {
  raw_json.field(payload, "id") |> result.try(raw_json.snowflake)
}

@external(erlang, "erlang", "system_info")
fn schedulers(item: Atom) -> Int

@external(erlang, "erlang", "process_info")
fn process_items(pid: process.Pid, items: List(Atom)) -> Dynamic

fn reductions_of(pid: process.Pid) -> Int {
  decode.run(
    process_items(pid, [atom.create("reductions")]),
    decode.at([0, 1], decode.int),
  )
  |> result.unwrap(0)
}

fn hot_processes(window_ms: Int) -> List(String) {
  let before =
    process_list() |> list.map(fn(pid) { #(pid, reductions_of(pid)) })
  process.sleep(window_ms)
  before
  |> list.map(fn(row) { #(row.0, reductions_of(row.0) - row.1) })
  |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
  |> list.take(12)
  |> list.map(fn(row) {
    int.to_string(row.1)
    <> " "
    <> string.inspect(
      process_items(row.0, [
        atom.create("registered_name"),
        atom.create("initial_call"),
        atom.create("current_function"),
        atom.create("message_queue_len"),
        atom.create("dictionary"),
      ]),
    )
    |> string.slice(0, 400)
  })
}

type Device

@external(erlang, "tprof", "start")
fn tprof_start(options: dict.Dict(Atom, Atom)) -> Dynamic

@external(erlang, "tprof", "stop")
fn tprof_stop() -> Dynamic

@external(erlang, "tprof", "set_pattern")
fn tprof_set_pattern(module: Atom, function: Atom, arity: Atom) -> Dynamic

@external(erlang, "tprof", "enable_trace")
fn tprof_enable_pids(which: List(process.Pid)) -> Dynamic

@external(erlang, "tprof", "disable_trace")
fn tprof_disable_pids(which: List(process.Pid)) -> Dynamic

fn hottest(window_ms: Int) -> List(process.Pid) {
  let me = process.self()
  let before =
    process_list() |> list.map(fn(pid) { #(pid, reductions_of(pid)) })
  process.sleep(window_ms)
  before
  |> list.filter(fn(row) { row.0 != me })
  |> list.map(fn(row) { #(row.0, reductions_of(row.0) - row.1) })
  |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
  |> list.take(1)
  |> list.map(fn(row) { row.0 })
}

@external(erlang, "tprof", "collect")
fn tprof_collect() -> Dynamic

@external(erlang, "tprof", "inspect")
fn tprof_inspect(collected: Dynamic, mode: Atom, sort: Atom) -> Dynamic

@external(erlang, "tprof", "format")
fn tprof_format(device: Device, inspected: Dynamic) -> Dynamic

@external(erlang, "file", "open")
fn file_open(path: String, modes: List(Atom)) -> Result(Device, Dynamic)

@external(erlang, "file", "close")
fn file_close(device: Device) -> Dynamic

fn profile_once() -> Nil {
  case envoy.get("FLUXER_STORM_PROFILE") {
    Error(Nil) | Ok("") -> Nil
    Ok(dir) -> {
      let any = atom.create("_")
      let _ =
        tprof_start(
          dict.from_list([#(atom.create("type"), atom.create("call_time"))]),
        )
      let _ = tprof_set_pattern(any, any, any)
      let target = hottest(500)
      let _ = tprof_enable_pids(target)
      process.sleep(1000)
      let _ = tprof_disable_pids(target)
      let collected = tprof_collect()
      let inspected =
        tprof_inspect(
          collected,
          atom.create("total"),
          atom.create("measurement"),
        )
      let path = dir <> "/" <> atom.to_string(node_name()) <> ".txt"
      case file_open(path, [atom.create("write")]) {
        Ok(device) -> {
          let _ = tprof_format(device, dynamic_map_values(inspected))
          let _ = file_close(device)
          Nil
        }
        Error(_) -> Nil
      }
      let _ = tprof_stop()
      Nil
    }
  }
}

@external(erlang, "maps", "get")
fn map_get(key: Atom, map: Dynamic) -> Dynamic

fn dynamic_map_values(inspected: Dynamic) -> Dynamic {
  map_get(atom.create("all"), inspected)
}

@external(erlang, "erlang", "statistics")
fn runtime(item: Atom) -> #(Int, Int)

fn runtime_ms() -> Int {
  runtime(atom.create("runtime")).0
}

@external(erlang, "ets", "tab2list")
fn hold_rows(table: Atom) -> List(#(#(process.Pid, Int)))

@external(erlang, "ets", "tab2list")
fn ets_rows(table: Atom) -> List(#(Int, Int))

@external(erlang, "ets", "info")
fn ets_info(table: Atom, item: Atom) -> Int

@external(erlang, "erlang", "statistics")
fn run_queue(item: Atom) -> Int

@external(erlang, "file", "write_file")
fn append_file(path: String, data: String, modes: List(Atom)) -> Dynamic

fn start_trace(started: Int, deadline: Int) -> Nil {
  case envoy.get("FLUXER_STORM_TRACE") {
    Error(Nil) | Ok("") -> Nil
    Ok(dir) -> {
      let path = dir <> "/" <> atom.to_string(node_name()) <> ".txt"
      process.spawn_unlinked(fn() { trace_loop(path, started, deadline) })
      Nil
    }
  }
}

fn trace_loop(path: String, started: Int, deadline: Int) -> Nil {
  let now = h.now_ms()
  case now > deadline {
    True -> Nil
    False -> {
      let in_flight =
        ets_rows(atom.create("fluxer_gateway_session_join_quota"))
        |> list.fold(0, fn(sum, row) { sum + row.1 })
      let waiters =
        ets_info(
          atom.create("fluxer_gateway_session_join_queue"),
          atom.create("size"),
        )
        - 1
      let queues =
        process_list()
        |> list.map(fn(pid) {
          #(
            pid,
            decode.run(
              process_items(pid, [atom.create("message_queue_len")]),
              decode.at([0, 1], decode.int),
            )
              |> result.unwrap(0),
          )
        })
        |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
        |> list.take(3)
        |> list.map(fn(row) {
          int.to_string(row.1)
          <> "@"
          <> string.inspect(
            process_items(row.0, [
              atom.create("registered_name"),
              atom.create("current_stacktrace"),
            ]),
          )
          |> string.slice(0, 700)
        })
      let holds =
        hold_rows(atom.create("fluxer_gateway_session_join_holds"))
        |> list.fold(dict.new(), fn(acc, row) {
          let #(#(_, guild)) = row
          dict.upsert(acc, guild, fn(found) { option.unwrap(found, 0) + 1 })
        })
        |> dict.to_list
        |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
        |> list.take(4)
        |> list.map(fn(row) {
          let where = case
            id.guild_id(row.0)
            |> result.replace_error(Nil)
            |> result.try(guild_registry.lookup)
          {
            Ok(handle) ->
              "local "
              <> string.inspect(
                process_items(handle.pid, [
                  atom.create("message_queue_len"),
                  atom.create("current_function"),
                ]),
              )
            Error(Nil) -> "remote"
          }
          int.to_string(row.0) <> ":" <> int.to_string(row.1) <> " " <> where
        })
      let line =
        int.to_string(now - started)
        <> " holds="
        <> string.join(holds, " ; ")
        <> " rq="
        <> int.to_string(run_queue(atom.create("total_run_queue_lengths")))
        <> " inflight="
        <> int.to_string(in_flight)
        <> " waiters="
        <> int.to_string(waiters)
        <> " sessions="
        <> int.to_string(registry.count())
        <> " mq="
        <> string.join(queues, " | ")
        <> "\n"
      let _ = append_file(path, line, [atom.create("append")])
      process.sleep(500)
      trace_loop(path, started, deadline)
    }
  }
}
