// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics
import gleam/list

pub fn recoveries() -> Int {
  metrics.histograms()
  |> list.filter(fn(series) {
    series.name == "fluxer_gateway_cluster_down_duration_ms"
  })
  |> list.fold(0, fn(sum, series) { sum + series.count })
}
