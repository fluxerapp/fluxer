// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/metrics as api_metrics
import fluxer_gateway/config/settings.{Durations, VoiceDurations}
import fluxer_gateway/core/id
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/resident
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/state as call_state
import fluxer_gateway/voice/metrics as voice_metrics
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import support/isolation
import support/voice_api as api
import support/voice_guild as g

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

fn observed(name: String, labels: List(#(String, String))) -> Int {
  metrics.histograms()
  |> list.find(fn(series) {
    series.name == name && series.labels == metrics.normalise(labels)
  })
  |> result.map(fn(series) { series.count })
  |> result.unwrap(0)
}

fn sample_labels(name: String) -> List(List(#(String, String))) {
  metrics.read_all()
  |> list.filter(fn(sample) { sample.name == name })
  |> list.map(fn(sample) { sample.labels })
}

fn transition(scope: String, kind: String) -> Int {
  value(voice_metrics.transitions_metric, [
    #("scope", scope),
    #("kind", kind),
  ])
}

fn request(kind: String) -> Int {
  value(voice_metrics.guild_requests_metric, [#("request", kind)])
}

fn server_updates(host: g.Host, ref: SessionRef) -> List(Dynamic) {
  g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE")
}

fn nonce_for(stub: api.Api) -> Option(String) {
  api.calls_of(stub, "voice_get_token")
  |> list.first
  |> result.map(fn(call) { api.field(call, "token_nonce") })
  |> result.unwrap(None)
}

pub fn guild_join_and_leave_are_counted_and_timed_test() {
  let stub = api.start()
  let host = g.start(g.world())
  let token_call = [#("request_type", "voice_get_token")]
  let joins = transition("guild", "join")
  let leaves = transition("guild", "leave")
  let updates = request("client_update")
  let readies = request("token_ready")
  let rpcs = request("rpc")
  let sent = value(voice_metrics.server_updates_metric, [#("scope", "guild")])
  let tokens =
    value(voice_metrics.token_results_metric, [
      #("job", "join"),
      #("outcome", "ok"),
    ])
  let turnarounds =
    observed(voice_metrics.token_turnaround_ms, [#("job", "join")])
  let api_ok =
    value(api_metrics.requests, [
      #("request_type", "voice_get_token"),
      #("outcome", "ok"),
    ])
  let api_timed = observed(api_metrics.request_duration, token_call)
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  assert api.eventually(fn() { list.length(server_updates(host, s1)) == 1 })
  let assert Ok(update) = list.first(server_updates(host, s1))
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.confirm(host, conn, nonce_for(stub)) == "{\"success\":true}"
  assert transition("guild", "join") == joins + 1
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() { g.states(host) == [] })
  assert transition("guild", "leave") == leaves + 1
  assert request("client_update") == updates + 2
  assert request("token_ready") == readies + 1
  assert request("rpc") == rpcs + 1
  assert value(voice_metrics.server_updates_metric, [#("scope", "guild")])
    == sent + 1
  assert value(voice_metrics.token_results_metric, [
      #("job", "join"),
      #("outcome", "ok"),
    ])
    == tokens + 1
  assert observed(voice_metrics.token_turnaround_ms, [#("job", "join")])
    == turnarounds + 1
  assert value(api_metrics.requests, [
      #("request_type", "voice_get_token"),
      #("outcome", "ok"),
    ])
    == api_ok + 1
  assert observed(api_metrics.request_duration, token_call) == api_timed + 1
  assert api.eventually(fn() {
    value(api_metrics.requests, [
      #("request_type", "voice_state_remove"),
      #("outcome", "ok"),
    ])
    > 0
  })
  assert sample_labels(voice_metrics.transitions_metric)
    == list.flat_map(voice_metrics.scopes, fn(scope) {
      list.map(voice_metrics.transition_kinds, fn(kind) {
        [#("scope", scope), #("kind", kind)]
      })
    })
  g.stop(host)
  api.stop(stub)
}

pub fn refused_tokens_count_as_failed_results_test() {
  let stub = api.start()
  let host = g.start(g.world())
  api.set(
    stub,
    "voice_get_token",
    api.Reply(api.Status(
      403,
      "{\"code\":\"UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS\"}",
    )),
  )
  let failed = fn() {
    value(voice_metrics.token_results_metric, [
      #("job", "join"),
      #("outcome", "error"),
    ])
  }
  let rejected = fn() {
    value(api_metrics.requests, [
      #("request_type", "voice_get_token"),
      #("outcome", "rejected"),
    ])
  }
  let failed_before = failed()
  let rejected_before = rejected()
  g.client(host, g.session(1), g.join(10))
  assert api.eventually(fn() { failed() == failed_before + 1 })
  assert rejected() == rejected_before + 1
  g.stop(host)
  api.stop(stub)
}

pub fn rejections_sent_to_sessions_count_as_guild_errors_test() {
  let stub = api.start()
  let world =
    g.World(..g.world(), channels: [g.VoiceChannel(10, 1, False, [], True)])
  let host = g.start(world)
  let errors = value(voice_metrics.guild_errors_metric, [])
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  assert api.eventually(fn() { list.length(server_updates(host, s1)) == 1 })
  let assert Ok(update) = list.first(server_updates(host, s1))
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.confirm(host, conn, nonce_for(stub)) == "{\"success\":true}"
  let s2 = g.session(2)
  g.client(host, s2, g.join(10))
  assert api.eventually(fn() { g.errors_to(g.effects(host), s2) != [] })
  assert value(voice_metrics.guild_errors_metric, []) == errors + 1
  g.stop(host)
  api.stop(stub)
}

fn init(channel: id.ChannelId, recipients: List(id.UserId)) -> call_state.Init {
  let assert Ok(message) = id.message_id(id.channel_id_to_int(channel) + 1)
  call_state.Init(
    channel:,
    message:,
    region: None,
    ringing: [],
    recipients:,
    caller: None,
  )
}

pub fn call_create_lookup_and_end_are_counted_test() {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      voice: VoiceDurations(..d.voice, server_idle_timeout_ms: 300),
    )
  })
  let assert Ok(channel) = id.channel_id(7_300_000_000_000_001)
  let assert Ok(missing) = id.channel_id(7_300_000_000_000_002)
  let assert Ok(user) = id.user_id(7_300_000_000_000_003)
  let outcome = fn(label) { [#("outcome", label)] }
  let created = observed(voice_metrics.call_create_ms, outcome("created"))
  let exists = observed(voice_metrics.call_create_ms, outcome("exists"))
  let found = observed(voice_metrics.call_lookup_ms, outcome("found"))
  let not_found = observed(voice_metrics.call_lookup_ms, outcome("not_found"))
  let started = value(voice_metrics.calls_started_metric, [])
  let ended = value(voice_metrics.calls_ended_metric, [])
  let assert Ok(call) = manager.create(channel, init(channel, [user]))
  let assert Error(manager.AlreadyExists) =
    manager.create(channel, init(channel, [user]))
  let assert Ok(_) = manager.lookup(channel)
  let assert Error(manager.NotFound) = manager.lookup(missing)
  assert observed(voice_metrics.call_create_ms, outcome("created"))
    == created + 1
  assert observed(voice_metrics.call_create_ms, outcome("exists")) == exists + 1
  assert observed(voice_metrics.call_lookup_ms, outcome("found")) == found + 1
  assert observed(voice_metrics.call_lookup_ms, outcome("not_found"))
    == not_found + 1
  assert value(voice_metrics.calls_started_metric, []) == started + 1
  let assert Ok(pid) = process.subject_owner(call)
  let watch = process.monitor(pid)
  let assert Ok(_) =
    process.new_selector()
    |> process.select_specific_monitor(watch, fn(down) { down })
    |> process.selector_receive(3000)
  assert value(voice_metrics.calls_ended_metric, []) == ended + 1
}

pub fn voice_tables_and_mailboxes_reach_the_resident_families_test() {
  let tables = list.map(resident.tables(), fn(held) { held.name })
  list.each(
    [
      "fluxer_voice_guilds", "fluxer_voice_dm_sessions",
      "fluxer_voice_count_connections", "fluxer_voice_count_owners",
    ],
    fn(name) {
      assert list.contains(tables, name)
    },
  )
  let processes = list.map(resident.singletons(), fn(held) { held.name })
  list.each(
    ["fluxer_gateway_voice_counts", "fluxer_voice_call_manager"],
    fn(name) {
      assert list.contains(processes, name)
    },
  )
}
