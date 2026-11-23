// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/draining
import fluxer_gateway/core/id
import fluxer_gateway/core/session as core_session
import fluxer_gateway/session/manager/registry
import gleam/bit_array
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/rollout_guard
import support/wait

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: String) -> List(#(String, Int))

@external(erlang, "ets", "lookup")
fn ets_lookup_user(table: Atom, key: Int) -> List(#(Int, Int))

fn user_sessions(user: Int) -> Int {
  case ets_lookup_user(atom.create("session_user_counts"), user) {
    [#(_, count)] -> count
    _ -> 0
  }
}

fn user_settles_at(user: Int, expected: Int, attempts: Int) -> Bool {
  wait.until(int.max(attempts * 20, 5000), fn() {
    user_sessions(user) == expected
  })
}

pub fn closing_releases_the_user_session_count_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000111"))
  let user = 1_000_000_000_000_111
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-count"))
  let #(client, _) = ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert user_settles_at(user, 1, 50)
  ws.send_close(client, 1000)
  let _ = ws.expect_eof(client, 2000)
  assert user_settles_at(user, 0, 100)
}

pub fn closing_during_start_leaves_no_session_count_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> {
        process.sleep(400)
        #(200, stub.session_reply("1000000000000112"))
      }
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let user = 1_000_000_000_000_112
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-count-2"))
  process.sleep(100)
  ws.send_close(client, 1000)
  let _ = ws.expect_eof(client, 2000)
  process.sleep(800)
  assert user_settles_at(user, 0, 100)
  assert list.all(registry.all(), fn(ref) {
    id.user_id_to_int(ref.user) != user
  })
}

fn open_connections() -> Int {
  case ets_lookup(atom.create("gateway_ip_connections"), "127.0.0.1") {
    [#(_, count)] -> count
    _ -> 0
  }
}

fn settles_at(expected: Int, attempts: Int) -> Bool {
  wait.until(int.max(attempts * 20, 5000), fn() {
    open_connections() == expected
  })
}

fn no_api(_kind: String, _body: String) -> #(Int, String) {
  #(200, "{\"data\":{}}")
}

pub fn every_close_path_releases_the_ip_slot_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(no_api)
  let port = scenario.gateway.port
  let baseline = open_connections()
  let closers = [
    fn(client: ws.Client) {
      ws.send_close(client, 1000)
      let _ = ws.expect_eof(client, 2000)
      Nil
    },
    fn(client: ws.Client) {
      ws.send_close(client, 4000)
      let _ = ws.expect_eof(client, 2000)
      Nil
    },
    fn(client: ws.Client) { ws.close_socket(client) },
    fn(client: ws.Client) {
      ws.send_text(client, "nope")
      let _ = ws.expect_eof(client, 2000)
      Nil
    },
    fn(client: ws.Client) {
      ws.send_frame(client, ws.frame(1, <<0:size(20_000)-unit(8)>>, True))
      let _ = ws.expect_eof(client, 2000)
      Nil
    },
    fn(client: ws.Client) {
      ws.send_close(client, 999)
      let _ = ws.expect_eof(client, 2000)
      Nil
    },
    fn(client: ws.Client) {
      ws.send_frame(client, ws.frame(0, <<"x":utf8>>, True))
      let _ = ws.expect_eof(client, 2000)
      Nil
    },
  ]
  list.each(closers, fn(closer) {
    let client = h.hello(port)
    assert settles_at(baseline + 1, 50)
    closer(client)
    assert settles_at(baseline, 100)
  })
}

type Atomics

@external(erlang, "atomics", "new")
fn atomics_new(size: Int, options: List(Nil)) -> Atomics

@external(erlang, "atomics", "add_get")
fn atomics_add_get(cells: Atomics, index: Int, by: Int) -> Int

pub fn rate_limited_session_start_is_held_and_retried_test_() {
  use <- h.within(30.0)
  let attempts = atomics_new(1, [])
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" ->
        case atomics_add_get(attempts, 1, 1) {
          1 -> #(429, "{}")
          _ -> #(200, stub.session_reply("1000000000000101"))
        }
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-429"))
  let #(_client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
  assert list.length(stub.token_calls(scenario.api, "session", "token-429"))
    == 2
}

pub fn payloads_sent_while_starting_survive_a_held_retry_test_() {
  use <- h.within(30.0)
  let attempts = atomics_new(1, [])
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" ->
        case atomics_add_get(attempts, 1, 1) {
          1 -> {
            process.sleep(300)
            #(429, "{}")
          }
          _ -> #(200, stub.session_reply("1000000000000105"))
        }
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-held"))
  ws.send_text(client, "{\"op\":3,\"d\":{\"status\":\"idle\"}}")
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_client, ack) = ws.wait_for(client, 2000, fn(p) { p.op == 11 })
  assert ack.op == 11
}

pub fn payloads_sent_while_held_wait_for_the_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000106"))
  let client = h.hello(scenario.gateway.port)
  let paused = rollout_guard.pause_sessions()
  ws.send_json(client, h.identify_payload("token-held-2"))
  process.sleep(100)
  ws.send_text(client, "{\"op\":3,\"d\":{\"status\":\"idle\"}}")
  let #(client, quiet) = ws.silent(client, 300)
  rollout_guard.resume_sessions(paused)
  assert quiet
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_client, ack) = ws.wait_for(client, 2000, fn(p) { p.op == 11 })
  assert ack.op == 11
}

pub fn heartbeats_are_read_while_many_payloads_are_held_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000107"))
  let client = h.hello(scenario.gateway.port)
  let paused = rollout_guard.pause_sessions()
  ws.send_json(client, h.identify_payload("token-held-3"))
  process.sleep(100)
  let filler = ws.frame(1, <<"{\"op\":13,\"d\":1}":utf8>>, True)
  ws.send_frame(client, bit_array.concat(list.repeat(filler, 30)))
  process.sleep(100)
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 1000)
  rollout_guard.resume_sessions(paused)
  assert ack.op == 11
  let #(_client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
}

pub fn held_payload_cap_closes_4008_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000108"))
  let client = h.hello(scenario.gateway.port)
  let paused = rollout_guard.pause_sessions()
  ws.send_json(client, h.identify_payload("token-held-4"))
  process.sleep(100)
  let filler = ws.frame(1, <<"{\"op\":13,\"d\":1}":utf8>>, True)
  ws.send_frame(client, bit_array.concat(list.repeat(filler, 65)))
  let #(code, reason) = ws.expect_close(client, 2000)
  rollout_guard.resume_sessions(paused)
  assert code == Some(4008)
  assert reason == "Rate limited"
}

pub fn session_crash_sends_invalid_session_and_allows_identify_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000109"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-crash"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let assert Ok(pid) = process.subject_owner(ref.inbox)
  process.kill(pid)
  let #(client, invalid) = ws.wait_for(client, 2000, fn(p) { p.op == 9 })
  assert decode.run(invalid.d, decode.bool) == Ok(False)
  ws.send_json(client, h.heartbeat_payload(json.int(1)))
  let #(client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  ws.send_json(client, h.identify_payload("token-crash"))
  let #(_client, again) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert h.session_id(again) != h.session_id(ready)
}

pub fn session_terminate_sends_invalid_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000110"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-term"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  core_session.send(ref, core_session.Terminate(core_session.Replaced))
  let #(_client, invalid) = ws.wait_for(client, 2000, fn(p) { p.op == 9 })
  assert decode.run(invalid.d, decode.bool) == Ok(False)
}

pub fn identify_while_draining_is_sent_to_another_node_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000102"))
  let client = h.hello(scenario.gateway.port)
  draining.set_draining()
  ws.send_json(client, h.identify_payload("token-drain"))
  let #(client, reconnect) = ws.expect_payload(client, 2000)
  let #(code, _reason) = ws.expect_close(client, 2000)
  draining.clear_draining()
  assert reconnect.op == 7
  assert code == Some(4000)
  assert stub.token_calls(scenario.api, "session", "token-drain") == []
}

pub fn resume_while_draining_is_told_to_reconnect_after_resumed_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000104"))
  let #(first, ready) =
    h.hello(scenario.gateway.port)
    |> fn(client) {
      ws.send_json(client, h.identify_payload("token-resume-drain"))
      ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
    }
  let assert Some(seq) = ready.s
  let id = h.session_id(ready)
  ws.close_socket(first)
  let late = h.hello(scenario.gateway.port)
  draining.set_draining()
  ws.send_json(late, h.resume_payload("token-resume-drain", id, seq))
  let #(late, resumed_late) =
    ws.wait_for(late, 5000, fn(p) { p.t == Some("RESUMED") })
  let #(late, reconnect) = ws.wait_for(late, 5000, fn(p) { p.op == 7 })
  let #(code, _reason) = ws.expect_close(late, 2000)
  draining.clear_draining()
  assert resumed_late.op == 0
  assert reconnect.op == 7
  assert code == Some(4000)
  let elsewhere = h.hello(scenario.gateway.port)
  ws.send_json(elsewhere, h.resume_payload("token-resume-drain", id, seq))
  let #(_elsewhere, resumed) =
    ws.wait_for(elsewhere, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
}

pub fn identify_while_sessions_are_paused_is_held_until_they_resume_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000121"))
  let client = h.hello(scenario.gateway.port)
  let paused = rollout_guard.pause_sessions()
  ws.send_json(client, h.identify_payload("token-paused"))
  let #(client, quiet) = ws.silent(client, 1500)
  rollout_guard.resume_sessions(paused)
  assert quiet
  assert stub.token_calls(scenario.api, "session", "token-paused") == []
  let #(_client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
}

pub fn resume_while_held_releases_the_held_identify_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000103"))
  let #(first, ready) =
    h.hello(scenario.gateway.port)
    |> fn(client) {
      ws.send_json(client, h.identify_payload("token-hold"))
      ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
    }
  let assert Some(seq) = ready.s
  let id = h.session_id(ready)
  ws.close_socket(first)
  let second = h.hello(scenario.gateway.port)
  let paused = rollout_guard.pause_sessions()
  ws.send_json(second, h.identify_payload("token-hold"))
  let #(second, quiet) = ws.silent(second, 300)
  rollout_guard.resume_sessions(paused)
  assert quiet
  ws.send_json(second, h.resume_payload("token-hold", id, seq))
  let #(second, resumed) =
    ws.wait_for(second, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  let #(_second, quiet) = ws.silent(second, 2500)
  assert quiet
  assert list.length(stub.token_calls(scenario.api, "session", "token-hold"))
    == 1
}

pub fn pipelined_frame_after_upgrade_is_processed_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<
    "GET /?v=1 HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n":utf8,
    ws.frame(1, <<"{\"op\":1,\"d\":null}":utf8>>, True):bits,
  >>)
  let assert Ok(client) = ws.read_upgrade(socket)
  let #(client, hello) = ws.expect_payload(client, 2000)
  assert hello.op == 10
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn health_route_answers_on_the_same_listener_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let socket = ws.raw_connect(scenario.gateway.port)
  ws.send_raw(socket, <<
    "GET /_health HTTP/1.1\r\nHost: localhost\r\n\r\nGET /nope HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n":utf8,
  >>)
  let replies = ws.read_all(socket, <<>>)
  assert ws.count(replies, "HTTP/1.1 ") == 2
  assert ws.count(replies, "HTTP/1.1 404") == 1
}

pub fn heartbeat_seq_is_forwarded_to_the_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000104"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-seq"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Some(seq) = ready.s
  ws.send_json(client, h.heartbeat_payload(json.int(seq + 50)))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}
