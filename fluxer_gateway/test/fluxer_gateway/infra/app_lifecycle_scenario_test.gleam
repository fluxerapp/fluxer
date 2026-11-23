// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import fluxer_gateway/app/application
import fluxer_gateway/app/http as app_http
import fluxer_gateway/app/supervisor.{
  type Child, ClusterHandoff, ClusterMembership, GuildEtsOwner, GuildManager,
  HttpListener, NatsPool, PushPermit, PushServer, Rollout, RolloutChanges,
  RpcServer, SessionManager, SessionTransfer, StartGate,
}
import fluxer_gateway/config/build_info
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import fluxer_gateway/infra/harness
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/application as erlang_application
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/int
import gleam/list
import gleam/result
import support/cluster_peers as peers
import support/isolation
import support/wait

@external(erlang, "application", "stop")
fn stop_application(app: Atom) -> a

@external(erlang, "application", "ensure_all_started")
fn ensure_all_started(app: Atom) -> a

fn get(path: String) -> #(Int, String) {
  let #(status, body, _) = get_versioned(path)
  #(status, body)
}

fn get_versioned(path: String) -> #(Int, String, Result(String, Nil)) {
  let assert Ok(port) = app_http.bound_port()
  let assert Ok(req) =
    request.to("http://127.0.0.1:" <> int.to_string(port) <> path)
  let assert Ok(response) = httpc.send(req)
  let #(header, _) = build_info.version_header()
  #(response.status, response.body, response.get_header(response, header))
}

fn fast_drain() -> Nil {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      app: settings.AppDurations(
        drain_deadline_ms: 600_000,
        drain_frame_flush_ms: 20,
        drain_handoff_wait_ms: 2000,
        drain_socket_notice_delay_ms: 20,
      ),
    )
  })
}

fn index(children: List(Child), child: Child) -> Int {
  children
  |> list.index_map(fn(planned, at) { #(planned, at) })
  |> list.key_find(child)
  |> result.unwrap(-1)
}

fn before(children: List(Child), first: Child, second: Child) -> Bool {
  case index(children, first), index(children, second) {
    -1, _ | _, -1 -> True
    a, b -> a < b
  }
}

const roles = [
  node.AllRoles,
  node.Websocket,
  node.Sessions,
  node.Presence,
  node.Guilds,
  node.Calls,
  node.Push,
]

pub fn every_role_boots_dependencies_before_their_users_test() {
  list.each([True, False], fn(cluster) {
    list.each(roles, fn(role) {
      let children =
        supervisor.plan(
          settings.Settings(
            ..settings.defaults(),
            role:,
            cluster_enabled: cluster,
          ),
        )
      assert list.last(children) == Ok(HttpListener)
      assert list.unique(children) == children
      assert before(children, RpcServer, Rollout)
      assert before(children, NatsPool, Rollout)
      assert before(children, Rollout, StartGate)
      assert before(children, RolloutChanges, Rollout)
      assert before(children, RolloutChanges, StartGate)
      assert before(children, SessionTransfer, SessionManager)
      assert before(children, GuildEtsOwner, GuildManager)
      assert before(children, PushPermit, PushServer)
      assert before(children, ClusterMembership, ClusterHandoff)
    })
  })
}

pub fn every_planned_child_is_running_test() {
  let planned = supervisor.plan(isolation.pristine_settings())
  let running = harness.running()
  assert list.length(running) == list.length(planned)
  list.index_map(planned, fn(_, at) { at })
  |> list.each(fn(at) {
    let assert Ok(pid) = list.key_find(running, at)
    assert process.is_alive(pid)
  })
}

pub fn invalid_settings_refuse_to_boot_without_touching_the_running_app_test() {
  let before = settings.get()
  envoy.set("FLUXER_GATEWAY_PINNED_GUILD_IDS", "not-a-guild")
  let started = application.start(erlang_application.Normal, [])
  envoy.unset("FLUXER_GATEWAY_PINNED_GUILD_IDS")
  let assert Error(application.InvalidSettings(_)) = started
  assert settings.get() == before
}

pub fn health_routes_follow_drain_and_undrain_test() {
  assert get("/_health") == #(200, "OK")
  assert get("/_health/./") == #(200, "OK")
  let #(status, _) = get("/nope/../_metrics")
  assert status == 200
  assert get("/_health/drain") == #(200, "DRAINING")
  assert draining.is_draining()
  assert get("/_health/ready") == #(503, "DRAINING")
  assert get("/_health/undrain") == #(200, "READY")
  assert !draining.is_draining()
  let #(status, _) = get("/missing")
  assert status == 404
}

pub fn every_http_answer_carries_the_build_version_test() {
  let #(_, version) = build_info.version_header()
  let #(status, _, health) = get_versioned("/_health")
  assert status == 200
  assert health == Ok(version)
  let #(status, _, metrics) = get_versioned("/_metrics")
  assert status == 200
  assert metrics == Ok(version)
  let #(status, _, missing) = get_versioned("/missing")
  assert status == 404
  assert missing == Ok(version)
}

pub fn prep_stop_drains_once_and_flags_readiness_test() {
  fast_drain()
  assert application.prep_stop(Nil) == Nil
  assert draining.is_draining()
  assert get("/_health/ready") == #(503, "DRAINING")
  assert application.prep_stop(Nil) == Nil
  assert draining.is_draining()
}

type Event {
  Down(Child, Bool)
}

fn watch_shutdown(children: List(#(Child, Pid))) -> process.Subject(Event) {
  let events = process.new_subject()
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let selector =
      list.fold(children, process.new_selector(), fn(selector, entry) {
        let #(child, pid) = entry
        let monitor = process.monitor(pid)
        process.select_specific_monitor(selector, monitor, fn(_) { child })
      })
    process.send(ready, Nil)
    relay(selector, events, list.length(children))
  })
  let assert Ok(Nil) = process.receive(ready, 1000)
  events
}

fn relay(
  selector: process.Selector(Child),
  events: process.Subject(Event),
  remaining: Int,
) -> Nil {
  case remaining {
    0 -> Nil
    _ ->
      case process.selector_receive(selector, 30_000) {
        Ok(child) -> {
          process.send(events, Down(child, draining.is_draining()))
          relay(selector, events, remaining - 1)
        }
        Error(Nil) -> Nil
      }
  }
}

fn downs(events: process.Subject(Event), acc: List(Event)) -> List(Event) {
  case process.receive(events, 5000) {
    Ok(event) -> downs(events, [event, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

pub fn application_restart_stops_in_reverse_and_comes_back_ready_test() {
  fast_drain()
  let planned = supervisor.plan(isolation.pristine_settings())
  let running = harness.running()
  let children =
    list.index_map(planned, fn(child, at) {
      let assert Ok(pid) = list.key_find(running, at)
      #(child, pid)
    })
  let events = watch_shutdown(children)
  let app = atom.create("fluxer_gateway")
  let _ = stop_application(app)
  let order = downs(events, [])
  let _ = ensure_all_started(app)
  isolation.refresh_after_restart()
  assert list.length(order) == list.length(planned)
  assert list.map(order, fn(event) {
      let Down(child, _) = event
      child
    })
    == list.reverse(planned)
  assert list.all(order, fn(event) {
    let Down(_, drained) = event
    drained
  })
  assert wait.until(5000, fn() {
    list.length(harness.running()) == list.length(planned)
  })
  assert !draining.is_draining()
  let #(status, body) = get("/_health/ready")
  assert body != "DRAINING"
  assert status == 200 || body == "NATS_UNAVAILABLE"
}

fn started(peer: peers.Peer) -> Bool {
  let found = peers.call(peer, "application", "which_applications", [])
  decode.run(found, decode.list(decode.at([0], atom.decoder())))
  |> result.map(list.contains(_, atom.create("fluxer_gateway")))
  == Ok(True)
}

fn top_supervisor(peer: peers.Peer) -> Result(Dynamic, Nil) {
  let found =
    peers.call(peer, "application", "get_supervisor", [
      atom.to_dynamic(atom.create("fluxer_gateway")),
    ])
  decode.run(found, decode.at([1], decode.dynamic))
  |> result.replace_error(Nil)
}

pub fn a_dead_top_supervisor_halts_the_node_test() {
  let peer =
    peers.start(peers.unique_name("lifecycle"), [
      #("FLUXER_GATEWAY_PORT", "0"),
      #("FLUXER_GATEWAY_STATUS_PORT", "0"),
      #("FLUXER_NATS_URL", "nats://127.0.0.1:1"),
      #("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:1"),
      #("FLUXER_GATEWAY_LOGGER_LEVEL", "emergency"),
      #("ERL_CRASH_DUMP_SECONDS", "0"),
    ])
  let monitor = process.monitor(peer.pid)
  peers.cast(peer, "fluxer_gateway", "main", [])
  assert wait.until(15_000, fn() { started(peer) })
  let assert Ok(top) = top_supervisor(peer)
  peers.cast(peer, "erlang", "exit", [top, atom.to_dynamic(atom.create("kill"))])
  let halted =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { True })
    |> process.selector_receive(15_000)
  case halted {
    Ok(True) -> Nil
    _ -> peers.stop(peer)
  }
  assert halted == Ok(True)
}
