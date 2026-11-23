// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{BucketId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/cluster/metrics as cluster_metrics
import fluxer_gateway/handoff/service
import fluxer_gateway/telemetry/metrics
import gleam/erlang/atom
import gleam/list
import gleam/result

fn closes() -> List(#(List(#(String, String)), Int)) {
  metrics.histograms()
  |> list.filter(fn(series) { series.name == cluster_metrics.close_duration })
  |> list.map(fn(series) { #(series.labels, series.count) })
}

fn total(found: List(#(List(#(String, String)), Int))) -> Int {
  list.fold(found, 0, fn(sum, entry) { sum + entry.1 })
}

pub fn a_bucket_close_is_timed_by_outcome_test() {
  let before = total(closes())
  let assert Ok(at) = epoch.from_int(3)
  let to = WorkerId(atom.create("close_metrics_target@test"), 1)
  let ports = service.live_ports([])
  assert result.is_error(
    ports.close_bucket(BucketId(key.guild, 4093), at, to, []),
  )
  let after = closes()
  assert total(after) == before + 1
  assert list.all(after, fn(entry) {
    case entry.0 {
      [#("outcome", outcome)] ->
        list.contains(cluster_metrics.close_outcomes, outcome)
      _ -> False
    }
  })
}
