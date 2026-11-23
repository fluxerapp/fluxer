// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/telemetry/rpc.{
  Available, ClusterSnapshot, NodeStats, Unavailable, VoiceCounts,
}
import fluxer_gateway/telemetry/watchdog
import gleam/erlang/atom
import gleam/json
import gleam/string

fn stats(node_id: String, sessions: Int, uptime: Int) -> rpc.NodeStats {
  NodeStats(
    node_id:,
    sessions:,
    session_resumes_total: 1,
    websocket_dispatches_total: 2,
    websocket_dispatch_drops_total: 0,
    guilds: 3,
    presences: 4,
    calls: 0,
    memory_total: 100,
    memory_processes: 60,
    memory_system: 40,
    process_count: 10,
    process_limit: 1000,
    uptime_seconds: uptime,
    cluster: ClusterSnapshot(1, 1, 2, 0, 0, 1, 0, 5, 6),
  )
}

pub fn aggregate_node_stats_healthy_test() {
  let text =
    rpc.aggregate_node_stats([
      Available(stats("b", 5, 300)),
      Available(stats("a", 7, 200)),
    ])
    |> json.to_string
  assert string.contains(text, "\"sessions\":12")
  assert string.contains(text, "\"status\":\"healthy\"")
  assert string.contains(text, "\"uptime_seconds\":200")
  assert string.contains(text, "\"node_count\":2")
  assert string.contains(text, "\"total\":\"200\"")
}

pub fn aggregate_node_stats_degraded_test() {
  let text =
    rpc.aggregate_node_stats([Available(stats("a", 7, 50)), Unavailable("z")])
    |> json.to_string
  assert string.contains(text, "\"status\":\"degraded\"")
  assert string.contains(text, "\"status\":\"unavailable\"")
  assert string.contains(text, "\"sessions\":7")
}

pub fn aggregate_voice_counts_test() {
  let merged =
    rpc.aggregate_voice_state_counts([
      VoiceCounts(3, [#("eu", 2), #("us", 1)], [#("s1", 3)]),
      VoiceCounts(2, [#("us", 2), #("ap", 0)], [#("s2", 2)]),
    ])
  assert merged.total_voice_states == 5
  assert merged.regions == [#("us", 3), #("eu", 2)]
  assert merged.servers == [#("s1", 3), #("s2", 2)]
}

pub fn unknown_method_test() {
  assert rpc.handle("process.nope", raw_json.null())
    == Error(error.UnknownMethod("process.nope"))
}

pub fn watchdog_actions_test() {
  assert watchdog.action(10, []) == watchdog.Quiet
  assert watchdog.action(1001, [1001]) == watchdog.Warn
  assert watchdog.action(10_001, [10_001]) == watchdog.Collect
  assert watchdog.action(60_000, [60_000, 55_000])
    == watchdog.CollectPendingGrowth
  assert watchdog.action(70_000, [70_000, 60_000, 55_000]) == watchdog.Kill
  assert watchdog.action(70_000, [70_000, 80_000, 55_000])
    == watchdog.CollectPendingGrowth
}

pub fn watchdog_growth_test() {
  assert watchdog.is_growing([3000, 2000, 1500])
  assert !watchdog.is_growing([3000, 2000, 900])
  assert !watchdog.is_growing([3000, 2000])
  assert watchdog.record_sample(5, [4, 3, 2]) == [5, 4, 3]
}

pub fn node_reports_marks_unreachable_peer_by_node_name_test() {
  let ghost = node.from_atom(atom.create("ghost@unreachable.invalid"))
  let assert [Available(local), Unavailable(missing)] =
    rpc.node_reports([node.self(), ghost])
  assert local.node_id != ""
  assert missing == "ghost@unreachable.invalid"
}
