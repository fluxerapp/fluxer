// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/timings
import gleam/json
import gleam/string

pub fn finish_renders_payload_test() {
  let text =
    timings.start("identify")
    |> timings.step("validate")
    |> timings.step("start_session")
    |> timings.finish
    |> json.to_string
  assert string.contains(text, "\"unit\":\"microseconds\"")
  assert string.contains(text, "\"name\":\"validate\"")
  assert string.contains(text, "\"name\":\"start_session\"")
  assert string.contains(text, "\"total_us\":")
  assert string.contains(text, "\"pod_name\":")
}

pub fn trace_keeps_order_test() {
  let text =
    timings.start("resume")
    |> timings.step("first")
    |> timings.step("second")
    |> timings.finish
    |> json.to_string
  let assert Ok(#(before, _)) = string.split_once(text, "second")
  assert string.contains(before, "first")
}
