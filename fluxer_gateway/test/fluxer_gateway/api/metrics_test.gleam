// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client
import fluxer_gateway/api/http
import fluxer_gateway/api/metrics as api_metrics
import fluxer_gateway/core/clock
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/result
import support/voice_api as api
import support/wait

fn requests(kind: String, outcome: String) -> Int {
  metrics.value(metrics.read_all(), api_metrics.requests, [
    #("request_type", kind),
    #("outcome", outcome),
  ])
}

fn timed(kind: String) -> Int {
  metrics.histograms()
  |> list.find(fn(series) {
    series.name == api_metrics.request_duration
    && series.labels == [#("request_type", kind)]
  })
  |> result.map(fn(series) { series.count })
  |> result.unwrap(0)
}

fn call(kind: String) -> Result(Nil, client.ApiError) {
  client.call(kind, [], clock.deadline_in(2000))
  |> result.replace(Nil)
}

pub fn every_api_attempt_is_counted_and_timed_by_request_type_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", api.Reply(api.Data(json.object([]))))
  api.set(stub, "voice_get_token", api.Reply(api.Status(404, "missing")))
  api.set(stub, "session", api.Reply(api.Status(503, "busy")))
  let ok = requests("get_dm_channel", "ok")
  let ok_timed = timed("get_dm_channel")
  let rejected = requests("voice_get_token", "rejected")
  let failed = requests("session", "server_error")
  let unknown = requests("other", "rejected")
  let assert Ok(Nil) = call("get_dm_channel")
  let assert Error(client.ApiRejected(404, _)) = call("voice_get_token")
  let assert Error(client.ApiHttp(503)) = call("session")
  api.set(stub, "not_a_known_request", api.Reply(api.Status(400, "no")))
  let assert Error(client.ApiRejected(400, _)) = call("not_a_known_request")
  api.stop(stub)
  assert requests("get_dm_channel", "ok") == ok + 1
  assert timed("get_dm_channel") == ok_timed + 1
  assert requests("voice_get_token", "rejected") == rejected + 1
  assert requests("session", "server_error") == failed + 1
  assert requests("other", "rejected") == unknown + 1
  assert requests("not_a_known_request", "rejected") == 0
}

pub fn a_retried_api_request_counts_each_retry_test() {
  let stub = api.start()
  api.set(stub, "guild_collection", api.Reply(api.Status(503, "busy")))
  let before =
    metrics.value(metrics.read_all(), api_metrics.retries, [
      #("request_type", "guild_collection"),
    ])
  let attempts = requests("guild_collection", "server_error")
  let assert Error(_) =
    client.call_with_retry("guild_collection", [], clock.deadline_in(3000))
  api.stop(stub)
  let retried =
    metrics.value(metrics.read_all(), api_metrics.retries, [
      #("request_type", "guild_collection"),
    ])
    - before
  assert retried >= 1
  assert requests("guild_collection", "server_error") == attempts + retried + 1
}

pub fn api_requests_in_flight_are_sampled_at_scrape_test() {
  let stub = api.start()
  api.set(stub, "forum_unreads", api.Hold)
  let caller =
    process.spawn_unlinked(fn() {
      let _ = call("forum_unreads")
      Nil
    })
  assert api.await_held(stub, "forum_unreads", 1)
  assert metrics.value(metrics.read_all(), api_metrics.in_flight, []) >= 1
  assert metrics.value(metrics.read_all(), api_metrics.in_flight, [])
    == http.in_flight()
  process.kill(caller)
  api.stop(stub)
  assert wait.until(2000, fn() { http.in_flight() == 0 })
}

pub fn breaker_transitions_are_counted_by_state_test() {
  let stub = api.start()
  api.set(stub, "get_read_state", api.Reply(api.Status(500, "down")))
  let opened =
    metrics.value(metrics.read_all(), api_metrics.breaker_transitions, [
      #("state", "open"),
    ])
  let closed =
    metrics.value(metrics.read_all(), api_metrics.breaker_transitions, [
      #("state", "closed"),
    ])
  list.each(list.repeat(Nil, 40), fn(_) {
    let _ = call("get_read_state")
    Nil
  })
  assert wait.until(2000, http.is_open)
  assert requests("get_read_state", "circuit_open") >= 1
  assert metrics.value(metrics.read_all(), api_metrics.breaker_state, []) == 1
  http.reset_breaker()
  api.stop(stub)
  assert metrics.value(metrics.read_all(), api_metrics.breaker_transitions, [
      #("state", "open"),
    ])
    >= opened + 1
  assert metrics.value(metrics.read_all(), api_metrics.breaker_transitions, [
      #("state", "closed"),
    ])
    >= closed + 1
  assert metrics.value(metrics.read_all(), api_metrics.breaker_state, []) == 0
}

pub fn request_types_fold_into_a_closed_set_test() {
  assert api_metrics.request_type("session") == "session"
  assert api_metrics.request_type("guild_collection") == "guild_collection"
  assert api_metrics.request_type("123456789") == "other"
  assert list.length(api_metrics.request_types)
    == list.length(list.unique(api_metrics.request_types))
}
