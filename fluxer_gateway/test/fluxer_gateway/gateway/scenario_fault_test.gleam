// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/api/http
import fluxer_gateway/config/settings.{
  type Durations, Durations, GatewayDurations, SessionDurations,
}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session as core_session
import fluxer_gateway/gateway/ip_connections
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/state
import fluxer_gateway/util/table
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/string
import support/api_breaker
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/isolation
import support/presence_scenario
import support/session_harness as sh

type Atomics

@external(erlang, "atomics", "new")
fn atomics_new(size: Int, options: List(Nil)) -> Atomics

@external(erlang, "atomics", "add_get")
fn atomics_add_get(cells: Atomics, index: Int, by: Int) -> Int

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: String) -> List(#(String, Int))

@external(erlang, "ets", "lookup")
fn ets_lookup_user(table: Atom, key: Int) -> List(#(Int, Int))

fn open_connections() -> Int {
  case ets_lookup(atom.create("gateway_ip_connections"), "127.0.0.1") {
    [#(_, count)] -> count
    _ -> 0
  }
}

fn user_sessions(user: Int) -> Int {
  case ets_lookup_user(atom.create("session_user_counts"), user) {
    [#(_, count)] -> count
    _ -> 0
  }
}

fn sessions_of(user: Int) -> List(core_session.SessionRef) {
  list.filter(registry.all(), fn(ref) { id.user_id_to_int(ref.user) == user })
}

fn socket_pid(ref: core_session.SessionRef) -> Pid {
  let assert state.Attached(socket, _) = sh.state_of(ref).socket
  socket.pid
}

fn ready(port: Int, token: String) -> #(ws.Client, ws.Payload) {
  let client = h.hello(port)
  ws.send_json(client, h.identify_payload(token))
  ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
}

fn gateway_knobs(
  durations: Durations,
  change: fn(settings.GatewayDurations) -> settings.GatewayDurations,
) -> Durations {
  Durations(..durations, gateway: change(durations.gateway))
}

fn session_knobs(
  durations: Durations,
  change: fn(settings.SessionDurations) -> settings.SessionDurations,
) -> Durations {
  Durations(..durations, session: change(durations.session))
}

pub fn killed_socket_leaves_the_session_resumable_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000401"))
  ip_connections.reclaim_dead_owners()
  let before = open_connections()
  let #(client, ready) = ready(scenario.gateway.port, "token-kill-socket")
  assert open_connections() == before + 1
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let pid = socket_pid(ref)
  process.kill(pid)
  assert ws.expect_eof(client, 3000)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  ip_connections.reclaim_dead_owners()
  assert open_connections() == before
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-kill-socket", h.session_id(ready), seq),
  )
  let #(_again, resumed) = ws.expect_payload(again, 5000)
  assert resumed.t == Some("RESUMED")
}

pub fn session_killed_during_a_resume_gives_invalid_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000402"))
  let #(client, ready) = ready(scenario.gateway.port, "token-kill-session")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let pid = sh.session_pid(ref)
  ws.close_socket(client)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  sh.suspend(pid)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-kill-session", h.session_id(ready), seq),
  )
  process.sleep(200)
  process.kill(pid)
  let started = sh.now_ms()
  let #(again, invalid) = ws.expect_payload(again, 5000)
  assert invalid.op == 9
  assert sh.now_ms() - started < 2000
  ws.send_json(again, h.identify_payload("token-kill-session"))
  let #(_again, fresh) =
    ws.wait_for(again, 8000, fn(p) { p.t == Some("READY") })
  assert h.session_id(fresh) != h.session_id(ready)
}

pub fn resume_reply_timeout_closes_and_the_session_stays_resumable_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(..g, socket_resume_reply_timeout_ms: 300)
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000403"))
  let #(client, ready) = ready(scenario.gateway.port, "token-resume-timeout")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let pid = sh.session_pid(ref)
  ws.close_socket(client)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  sh.suspend(pid)
  let stuck = h.hello(scenario.gateway.port)
  ws.send_json(
    stuck,
    h.resume_payload("token-resume-timeout", h.session_id(ready), seq),
  )
  let #(code, reason) = ws.expect_close(stuck, 3000)
  assert code == Some(4000)
  assert reason == "Session unavailable"
  sh.resume_process(pid)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-resume-timeout", h.session_id(ready), seq),
  )
  let #(_again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
}

pub fn start_timeout_holds_retries_and_keeps_one_session_test_() {
  use <- h.within(30.0)
  isolation.set_durations(fn(d) {
    d
    |> session_knobs(fn(s) { SessionDurations(..s, manager_start_timeout: 300) })
    |> gateway_knobs(fn(g) {
      GatewayDurations(
        ..g,
        socket_pending_identify_retry_base_ms: 50,
        socket_pending_identify_retry_jitter_ms: 1,
      )
    })
  })
  let calls = atomics_new(1, [])
  let user = 1_000_000_000_000_404
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> {
        case atomics_add_get(calls, 1, 1) {
          1 -> process.sleep(900)
          _ -> Nil
        }
        #(200, stub.session_reply(int.to_string(user)))
      }
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let #(client, first) = ready(scenario.gateway.port, "token-start-timeout")
  let frames = drain(client, 1500, [])
  assert list.all(frames, fn(p) { p.t != Some("READY") })
  assert sh.wait_until(fn() { list.length(sessions_of(user)) == 1 }, 3000)
  assert sh.wait_until(fn() { user_sessions(user) == 1 }, 3000)
  let assert [only] = sessions_of(user)
  assert core_session.session_id_to_string(only.id) == h.session_id(first)
}

fn drain(
  client: ws.Client,
  timeout: Int,
  seen: List(ws.Payload),
) -> List(ws.Payload) {
  case ws.next(client, timeout) {
    #(client, ws.Text(text)) ->
      drain(client, timeout, [ws.decode_payload(text), ..seen])
    #(_, ws.TimedOut) -> list.reverse(seen)
    #(client, ws.Ping(_)) | #(client, ws.Pong(_)) ->
      drain(client, timeout, seen)
    #(_, event) -> {
      assert event == ws.TimedOut
      list.reverse(seen)
    }
  }
}

pub fn answered_heartbeat_requests_keep_the_connection_open_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(
        ..g,
        limits_heartbeat_interval_ms: 600,
        limits_heartbeat_timeout_ms: 400,
      )
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000405"))
  let #(client, _) = ready(scenario.gateway.port, "token-heartbeat-request")
  let requests = answer_requests(client, sh.now_ms() + 3000, 0)
  assert requests >= 2
}

fn answer_requests(client: ws.Client, until: Int, requests: Int) -> Int {
  let left = until - sh.now_ms()
  case left <= 0 {
    True -> requests
    False ->
      case ws.next(client, left) {
        #(client, ws.Text(text)) ->
          case ws.decode_payload(text).op {
            1 -> {
              ws.send_json(client, h.heartbeat_payload(json.null()))
              answer_requests(client, until, requests + 1)
            }
            _ -> answer_requests(client, until, requests)
          }
        #(_, event) -> {
          assert event == ws.TimedOut
          requests
        }
      }
  }
}

pub fn resume_and_identify_on_two_connections_both_succeed_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000406"))
  let #(client, ready) = ready(scenario.gateway.port, "token-race")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  ws.close_socket(client)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  let resuming = h.hello(scenario.gateway.port)
  let identifying = h.hello(scenario.gateway.port)
  ws.send_json(
    resuming,
    h.resume_payload("token-race", h.session_id(ready), seq),
  )
  ws.send_json(identifying, h.identify_payload("token-race"))
  let #(resuming, resumed) =
    ws.wait_for(resuming, 5000, fn(p) { p.t == Some("RESUMED") })
  let #(_identifying, fresh) =
    ws.wait_for(identifying, 8000, fn(p) { p.t == Some("READY") })
  assert resumed.op == 0
  assert h.session_id(fresh) != h.session_id(ready)
  assert list.all(drain(resuming, 500, []), fn(p) { p.op == 0 })
}

pub fn identify_pipelined_behind_a_resume_closes_4005_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000407"))
  let #(client, ready) = ready(scenario.gateway.port, "token-pipelined")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  ws.close_socket(client)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-pipelined", h.session_id(ready), seq),
  )
  ws.send_json(again, h.identify_payload("token-pipelined"))
  let #(again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  let #(code, _) = ws.expect_close(again, 3000)
  assert code == Some(4005)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
}

pub fn identify_waits_out_an_open_api_breaker_test_() {
  use <- h.within(60.0)
  isolation.set_durations(fn(d) {
    d
    |> session_knobs(fn(s) {
      SessionDurations(..s, shard_fetch_timeout_ms: 300)
    })
    |> gateway_knobs(fn(g) {
      GatewayDurations(
        ..g,
        socket_pending_identify_retry_base_ms: 100,
        socket_pending_identify_retry_jitter_ms: 1,
      )
    })
  })
  settings.store(
    settings.Settings(..settings.get(), gateway_http_recovery_timeout_ms: 500),
  )
  let healthy = atomics_new(1, [])
  let user = "1000000000000408"
  use scenario <- h.run(fn(kind, _body) {
    case atomics_add_get(healthy, 1, 0), kind {
      0, _ -> #(500, "down")
      _, "session" -> #(200, stub.session_reply(user))
      _, _ -> #(200, "{\"data\":{}}")
    }
  })
  list.each(list.repeat(Nil, 25), fn(_) {
    let _ = probe()
    Nil
  })
  assert sh.wait_until(api_is_open, 2000)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-breaker"))
  let #(client, quiet) = ws.silent(client, 1200)
  assert quiet
  let _ = atomics_add_get(healthy, 1, 1)
  let #(_client, ready) =
    ws.wait_for(client, 10_000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
}

pub fn slow_reader_is_disconnected_and_the_session_detaches_test_() {
  use <- h.within(60.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(..g, server_send_timeout_ms: 500)
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000409"))
  let #(_client, ready) = ready(scenario.gateway.port, "token-slow-reader")
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let pid = socket_pid(ref)
  let blob = string.repeat("x", 12_000)
  let _ =
    list.index_map(list.repeat(Nil, 3000), fn(_, n) {
      core_session.send(
        ref,
        core_session.Dispatch(
          "SCENARIO_FLOOD",
          raw_json.wrap(bit_array.from_string(
            "{\"n\":" <> int.to_string(n) <> ",\"blob\":\"" <> blob <> "\"}",
          )),
        ),
      )
    })
  assert sh.await_exit(pid, 10_000)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
}

fn probe() -> Result(Dynamic, api.ApiError) {
  api.call("probe", [], clock.deadline_in(2000))
}

fn api_is_open() -> Bool {
  api.is_open()
}

@external(erlang, "inets", "stop")
fn inets_stop(service: Atom, profile: Atom) -> Dynamic

fn stop_http_profiles() -> Nil {
  list.repeat(Nil, 8)
  |> list.index_map(fn(_, index) {
    inets_stop(
      atom.create("httpc"),
      atom.create("fluxer_gateway_api_http_" <> int.to_string(index)),
    )
  })
  Nil
}

pub fn lost_http_client_profiles_fail_the_call_and_come_back_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(fn(_kind, _body) { #(200, "{\"data\":{\"ok\":1}}") })
  stop_http_profiles()
  let caller = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      process.send(caller, api.call("probe", [], clock.deadline_in(2000)))
    })
  assert process.receive(caller, 5000) == Ok(Error(api.ApiUnavailable))
  assert sh.await_exit(pid, 1000)
  let assert Ok(_) = api.call("probe", [], clock.deadline_in(2000))
  let assert Ok(_) = probe()
  let pids =
    list.repeat(Nil, 16)
    |> list.map(fn(_) {
      process.spawn_unlinked(fn() {
        process.send(caller, api.call("probe", [], clock.deadline_in(2000)))
      })
    })
  let replies =
    list.map(pids, fn(_) {
      let assert Ok(reply) = process.receive(caller, 5000)
      reply
    })
  assert list.all(replies, fn(reply) { reply != Error(api.ApiUnavailable) })
  assert list.length(stub.calls(scenario.api, "probe")) >= 18
}

pub fn late_failure_from_before_the_outage_does_not_reopen_the_breaker_test_() {
  use <- h.within(30.0)
  settings.store(
    settings.Settings(..settings.get(), gateway_http_recovery_timeout_ms: 300),
  )
  let healthy = atomics_new(1, [])
  use _scenario <- h.run(fn(kind, _body) {
    case kind, atomics_add_get(healthy, 1, 0) {
      "straggler", _ -> {
        process.sleep(1500)
        #(500, "late")
      }
      _, 0 -> #(500, "down")
      _, _ -> #(200, "{\"data\":{}}")
    }
  })
  api_breaker.hold_background_callers()
  let straggler = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(straggler, api.call("straggler", [], clock.deadline_in(5000)))
  })
  process.sleep(100)
  list.each(list.repeat(Nil, 25), fn(_) {
    let _ = probe()
    Nil
  })
  assert sh.wait_until(api_is_open, 2000)
  let _ = atomics_add_get(healthy, 1, 1)
  assert sh.wait_until(fn() { !api_is_open() }, 3000)
  let assert Ok(_) = probe()
  assert process.receive(straggler, 5000) == Ok(Error(api.ApiHttp(500)))
  process.sleep(50)
  assert !api_is_open()
  list.each(list.repeat(Nil, 3), fn(_) {
    let assert Ok(_) = probe()
    Nil
  })
  process.sleep(50)
  assert api_closed()
}

fn api_closed() -> Bool {
  http.is_closed()
}

fn linked_presence(ref: core_session.SessionRef) -> Result(Pid, Nil) {
  case sh.state_of(ref).presence {
    state.PresenceUp(link:, ..) -> presence_client.link_pid(link)
    state.PresenceDown | state.PresenceConnecting(..) -> Error(Nil)
  }
}

fn heartbeat_is_acked(client: ws.Client) -> Bool {
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_, ack) = ws.wait_for(client, 3000, fn(p) { p.op == 11 })
  ack.op == 11
}

pub fn presence_killed_after_ready_is_relinked_and_the_socket_stays_test_() {
  use <- h.within(30.0)
  let user = 1_000_000_000_000_410
  use scenario <- h.run(h.sessions_for(int.to_string(user)))
  let #(client, ready) = ready(scenario.gateway.port, "token-kill-presence")
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  assert sh.wait_until(fn() { linked_presence(ref) != Error(Nil) }, 5000)
  let assert Ok(old) = linked_presence(ref)
  process.kill(old)
  assert sh.wait_until(
    fn() {
      case linked_presence(ref) {
        Ok(pid) -> pid != old && process.is_alive(pid)
        Error(Nil) -> False
      }
    },
    10_000,
  )
  assert presence_scenario.presence_pid(user) == linked_presence(ref)
  assert heartbeat_is_acked(client)
  assert !sh.is_detached(ref)
}

pub fn presence_killed_during_identify_still_gives_ready_test_() {
  use <- h.within(30.0)
  let user = 1_000_000_000_000_411
  use scenario <- h.run(h.sessions_for(int.to_string(user)))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-kill-presence-early"))
  assert sh.wait_until(
    fn() {
      case presence_scenario.presence_pid(user) {
        Ok(pid) -> {
          process.kill(pid)
          True
        }
        Error(Nil) -> False
      }
    },
    5000,
  )
  let #(client, ready) =
    ws.wait_for(client, 10_000, fn(p) { p.t == Some("READY") })
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  assert sh.wait_until(
    fn() {
      case linked_presence(ref) {
        Ok(pid) -> process.is_alive(pid)
        Error(Nil) -> False
      }
    },
    10_000,
  )
  assert heartbeat_is_acked(client)
}

fn count_acks(client: ws.Client, until: Int, acks: Int) -> Int {
  let left = until - sh.now_ms()
  case left <= 0 {
    True -> acks
    False ->
      case ws.next(client, left) {
        #(client, ws.Text(text)) ->
          case ws.decode_payload(text).op {
            11 -> count_acks(client, until, acks + 1)
            _ -> count_acks(client, until, acks)
          }
        #(_, event) -> {
          assert event == ws.TimedOut
          acks
        }
      }
  }
}

pub fn busy_session_still_gets_heartbeats_acked_once_each_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(
        ..g,
        limits_heartbeat_interval_ms: 1500,
        limits_heartbeat_timeout_ms: 1000,
        socket_heartbeat_ack_fallback_ms: 300,
      )
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000412"))
  let #(client, ready) = ready(scenario.gateway.port, "token-busy-session")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let pid = sh.session_pid(ref)
  sh.suspend(pid)
  ws.send_json(client, h.heartbeat_payload(json.int(seq)))
  let started = sh.now_ms()
  let #(client, ack) = ws.wait_for(client, 3000, fn(p) { p.op == 11 })
  assert ack.op == 11
  assert sh.now_ms() - started < 1500
  ws.send_json(client, h.heartbeat_payload(json.int(seq)))
  let #(client, _) = ws.wait_for(client, 3000, fn(p) { p.op == 11 })
  ws.send_json(client, h.heartbeat_payload(json.int(seq)))
  let #(client, _) = ws.wait_for(client, 3000, fn(p) { p.op == 11 })
  sh.resume_process(pid)
  ws.send_json(client, h.heartbeat_payload(json.int(seq)))
  assert count_acks(client, sh.now_ms() + 1200, 0) == 1
  assert !sh.is_detached(ref)
}

fn socket_pids() -> List(Pid) {
  table.to_list(table.named("fluxer_gateway_ws_sockets"))
  |> list.map(fn(row: #(Pid, Dynamic)) { row.0 })
  |> list.filter(process.is_alive)
}

fn new_socket(before: List(Pid)) -> Pid {
  let assert [pid] =
    list.filter(socket_pids(), fn(pid) { !list.contains(before, pid) })
  pid
}

pub fn socket_killed_while_its_session_starts_leaves_nothing_behind_test_() {
  use <- h.within(30.0)
  let user = 1_000_000_000_000_413
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> {
        process.sleep(500)
        #(200, stub.session_reply(int.to_string(user)))
      }
      _ -> #(200, "{\"data\":{}}")
    }
  })
  ip_connections.reclaim_dead_owners()
  let before_connections = open_connections()
  let before = socket_pids()
  let client = h.hello(scenario.gateway.port)
  let pid = new_socket(before)
  ws.send_json(client, h.identify_payload("token-kill-starting"))
  process.sleep(100)
  process.kill(pid)
  assert ws.expect_eof(client, 3000)
  process.sleep(900)
  assert sessions_of(user) == []
  assert user_sessions(user) == 0
  ip_connections.reclaim_dead_owners()
  assert open_connections() == before_connections
}

pub fn socket_killed_while_resuming_keeps_the_session_resumable_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000414"))
  let #(client, ready) = ready(scenario.gateway.port, "token-kill-resuming")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let session = sh.session_pid(ref)
  ws.close_socket(client)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  sh.suspend(session)
  let before = socket_pids()
  let resuming = h.hello(scenario.gateway.port)
  let pid = new_socket(before)
  ws.send_json(
    resuming,
    h.resume_payload("token-kill-resuming", h.session_id(ready), seq),
  )
  process.sleep(100)
  process.kill(pid)
  assert ws.expect_eof(resuming, 3000)
  sh.resume_process(session)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-kill-resuming", h.session_id(ready), seq),
  )
  let #(_again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
}

fn trickle(socket: ws.Socket, bytes: List(String)) -> Nil {
  case bytes {
    [] -> Nil
    [byte, ..rest] -> {
      ws.send_raw(socket, bit_array.from_string(byte))
      process.sleep(50)
      trickle(socket, rest)
    }
  }
}

pub fn trickled_request_head_is_cut_off_at_the_request_timeout_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(..g, server_request_timeout_ms: 300)
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000415"))
  let socket = ws.raw_connect(scenario.gateway.port)
  let started = sh.now_ms()
  process.spawn_unlinked(fn() {
    trickle(socket, string.to_graphemes("GET /?v=1 HTTP/1.1\r\nHost: x\r\n"))
  })
  assert ws.recv_raw(socket, 5000) == Error(Nil)
  assert sh.now_ms() - started < 2000
}

pub fn idle_keep_alive_connection_is_closed_at_the_request_timeout_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(..g, server_request_timeout_ms: 300)
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000416"))
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<"GET /_health HTTP/1.1\r\nHost: x\r\n\r\n":utf8>>)
  let assert Ok(reply) = ws.recv_raw(socket, 2000)
  let assert Ok(text) = bit_array.to_string(reply)
  assert string.starts_with(text, "HTTP/1.1 ")
  let started = sh.now_ms()
  assert ws.recv_raw(socket, 5000) == Error(Nil)
  assert sh.now_ms() - started < 2000
}

pub fn upgraded_socket_outlives_the_request_timeout_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(..g, server_request_timeout_ms: 200)
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000417"))
  let #(client, _) = ready(scenario.gateway.port, "token-request-timeout")
  process.sleep(600)
  assert heartbeat_is_acked(client)
}

pub fn token_rate_limited_identify_is_held_and_retried_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(
        ..g,
        abuse_protection_identify_window_secs: 1,
        socket_pending_identify_retry_base_ms: 100,
        socket_pending_identify_retry_jitter_ms: 50,
      )
    }),
  )
  settings.store(
    settings.Settings(..settings.get(), identify_token_limit_per_minute: 1),
  )
  use scenario <- h.run(h.sessions_for("1000000000000418"))
  let #(_first, first) = ready(scenario.gateway.port, "token-throttled")
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-throttled"))
  let #(_client, second) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert h.session_id(second) != h.session_id(first)
}

fn outcome(client: ws.Client, resumed: Bool) -> String {
  let wait = case resumed {
    True -> 500
    False -> 3000
  }
  case ws.next(client, wait) {
    #(client, ws.Text(text)) ->
      outcome(client, resumed || ws.decode_payload(text).t == Some("RESUMED"))
    #(_, ws.TimedOut) if resumed -> "resumed"
    #(_, ws.Close(code: Some(code), ..)) -> "closed " <> int.to_string(code)
    #(_, event) -> string.inspect(event)
  }
}

pub fn three_resumes_at_once_leave_exactly_one_connection_attached_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000419"))
  let #(client, ready) = ready(scenario.gateway.port, "token-triple")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  ws.close_socket(client)
  assert sh.wait_until(fn() { sh.is_detached(ref) }, 3000)
  let clients =
    list.repeat(Nil, 3) |> list.map(fn(_) { h.hello(scenario.gateway.port) })
  list.each(clients, fn(c) {
    ws.send_json(c, h.resume_payload("token-triple", h.session_id(ready), seq))
  })
  let outcomes = list.map(clients, outcome(_, False))
  assert list.count(outcomes, fn(o) { o == "resumed" }) == 1
  assert list.count(outcomes, fn(o) { o == "closed 4000" }) == 2
  assert sh.wait_until(fn() { !sh.is_detached(ref) }, 2000)
  let attached = socket_pid(ref)
  assert process.is_alive(attached)
}

pub fn resume_after_the_resume_window_is_invalid_session_test_() {
  use <- h.within(30.0)
  isolation.set_durations(
    gateway_knobs(_, fn(g) {
      GatewayDurations(..g, limits_resume_timeout_ms: 300)
    }),
  )
  use scenario <- h.run(h.sessions_for("1000000000000420"))
  let #(client, ready) = ready(scenario.gateway.port, "token-expired")
  let assert Some(seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let session = sh.session_pid(ref)
  ws.close_socket(client)
  assert sh.await_exit(session, 3000)
  assert registry.lookup(h.session_id(ready)) == Error(Nil)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-expired", h.session_id(ready), seq),
  )
  let #(again, invalid) = ws.expect_payload(again, 5000)
  assert invalid.op == 9
  ws.send_json(again, h.identify_payload("token-expired"))
  let #(_again, fresh) =
    ws.wait_for(again, 8000, fn(p) { p.t == Some("READY") })
  assert h.session_id(fresh) != h.session_id(ready)
}
