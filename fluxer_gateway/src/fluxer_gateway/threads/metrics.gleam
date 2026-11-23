// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics.{type Sample, Sample}
import fluxer_gateway/util/table.{type Table}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/string

type TotalKey {
  Holder(Pid)
  CollectorRegistered
}

const held_metric = "fluxer_gateway_thread_store_threads"

fn totals() -> Table {
  table.named("fluxer_gateway_thread_store_totals")
}

pub fn threads_held(count: Int) -> Nil {
  case table.ensure_public(totals()) {
    False -> Nil
    True -> {
      let key = Holder(process.self())
      case count {
        0 -> table.delete(totals(), key)
        _ -> table.insert(totals(), #(key, count))
      }
      case table.insert_new(totals(), #(CollectorRegistered, 1)) {
        True -> metrics.register_collector(held_metric, held_samples)
        False -> Nil
      }
    }
  }
}

pub fn held() -> Int {
  let rows: List(#(TotalKey, Int)) = table.to_list(totals())
  list.fold(rows, 0, fn(total, row) {
    case row {
      #(Holder(owner) as key, count) ->
        case process.is_alive(owner) {
          True -> total + count
          False -> {
            table.delete(totals(), key)
            total
          }
        }
      #(CollectorRegistered, _) -> total
    }
  })
}

fn held_samples() -> List(Sample) {
  [Sample(held_metric, [], held())]
}

pub fn dispatched(event: String, variant: String, count: Int) -> Nil {
  case count {
    0 -> Nil
    _ ->
      metrics.counter(
        "fluxer_gateway_thread_dispatch_total",
        [#("event", string.lowercase(event)), #("variant", variant)],
        count,
      )
  }
}

pub fn delivered_in_thread(event: String, count: Int) -> Nil {
  dispatched(thread_event(event), "thread", count)
}

fn thread_event(event: String) -> String {
  case event {
    "MESSAGE_CREATE"
    | "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK"
    | "MESSAGE_REACTION_ADD"
    | "MESSAGE_REACTION_REMOVE"
    | "MESSAGE_REACTION_REMOVE_ALL"
    | "MESSAGE_REACTION_REMOVE_EMOJI"
    | "TYPING_START"
    | "CHANNEL_PINS_UPDATE" -> event
    _ -> "other"
  }
}

pub fn list_sync(reason: String) -> Nil {
  metrics.counter(
    "fluxer_gateway_thread_list_sync_total",
    [#("reason", reason)],
    1,
  )
}

pub fn fetch(result: String) -> Nil {
  metrics.counter(
    "fluxer_gateway_thread_collection_fetch_total",
    [#("result", result)],
    1,
  )
}

pub fn unknown_parent() -> Nil {
  metrics.counter(
    "fluxer_gateway_thread_permission_unknown_parent_total",
    [],
    1,
  )
}
