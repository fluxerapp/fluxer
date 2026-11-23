// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session as core_session
import fluxer_gateway/session/manager/registry
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{Some}
import support/gateway_harness as h
import support/gateway_ws_client as ws

fn push(ref: core_session.SessionRef, from: Int, to: Int) -> Nil {
  case from > to {
    True -> Nil
    False -> {
      core_session.send(
        ref,
        core_session.Dispatch(
          "SCENARIO_EVENT",
          raw_json.wrap(bit_array.from_string(
            "{\"n\":" <> int.to_string(from) <> "}",
          )),
        ),
      )
      push(ref, from + 1, to)
    }
  }
}

fn collect(
  client: ws.Client,
  seen: List(ws.Payload),
  until: fn(List(ws.Payload)) -> Bool,
) -> List(ws.Payload) {
  case until(seen) {
    True -> list.reverse(seen)
    False -> {
      let #(client, payload) = ws.expect_payload(client, 5000)
      collect(client, [payload, ..seen], until)
    }
  }
}

fn sequences(payloads: List(ws.Payload)) -> List(Int) {
  list.filter_map(payloads, fn(p) {
    case p.op, p.s {
      0, Some(seq) -> Ok(seq)
      _, _ -> Error(Nil)
    }
  })
}

fn ascending(values: List(Int)) -> Bool {
  case values {
    [a, b, ..rest] -> a < b && ascending([b, ..rest])
    _ -> True
  }
}

pub fn resume_with_many_missed_events_replays_them_in_order_test_() {
  use <- h.within(60.0)
  use scenario <- h.run(h.sessions_for("1000000000000301"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-replay"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Some(ready_seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  ws.close_socket(client)
  assert h.await_detached(h.session_id(ready))
  push(ref, 1, 300)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-replay", h.session_id(ready), ready_seq),
  )
  push(ref, 301, 310)
  let frames =
    collect(again, [], fn(seen) {
      list.count(seen, fn(p) { p.t == Some("SCENARIO_EVENT") }) == 310
      && list.any(seen, fn(p) { p.t == Some("RESUMED") })
    })
  let seqs = sequences(list.filter(frames, fn(p) { p.t != Some("RESUMED") }))
  assert ascending(seqs)
  assert list.first(seqs) == Ok(ready_seq + 1)
  assert list.length(seqs) == 310
  let assert [resumed] = list.filter(frames, fn(p) { p.t == Some("RESUMED") })
  let assert Some(resumed_seq) = resumed.s
  let before_resumed =
    list.take_while(frames, fn(p) { p.t != Some("RESUMED") })
    |> sequences
  assert list.all(before_resumed, fn(seq) { seq <= resumed_seq })
  let after_resumed =
    list.drop_while(frames, fn(p) { p.t != Some("RESUMED") })
    |> list.drop(1)
    |> sequences
  assert list.all(after_resumed, fn(seq) { seq > resumed_seq })
}

pub fn resume_at_the_current_sequence_replays_nothing_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000302"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-replay-2"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  push(ref, 1, 3)
  let frames =
    collect(client, [], fn(seen) {
      list.count(seen, fn(p) { p.t == Some("SCENARIO_EVENT") }) == 3
    })
  let assert Ok(last) = list.last(sequences(frames))
  ws.close_socket(client)
  assert h.await_detached(h.session_id(ready))
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-replay-2", h.session_id(ready), last),
  )
  let #(again, first) = ws.expect_payload(again, 5000)
  assert first.t == Some("RESUMED")
  assert first.s == Some(last)
  let #(_again, quiet) = ws.silent(again, 300)
  assert quiet
}

pub fn resume_from_an_older_sequence_replays_the_gap_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000303"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-replay-3"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Some(ready_seq) = ready.s
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  push(ref, 1, 5)
  let _ =
    collect(client, [], fn(seen) {
      list.count(seen, fn(p) { p.t == Some("SCENARIO_EVENT") }) == 5
    })
  ws.close_socket(client)
  assert h.await_detached(h.session_id(ready))
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-replay-3", h.session_id(ready), ready_seq + 2),
  )
  let frames =
    collect(again, [], fn(seen) {
      list.any(seen, fn(p) { p.t == Some("RESUMED") })
    })
  assert sequences(frames)
    == [ready_seq + 3, ready_seq + 4, ready_seq + 5, ready_seq + 5]
  Nil
}
