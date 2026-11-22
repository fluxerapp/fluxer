// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/draining
import fluxer_gateway/push/job.{type Kind}
import fluxer_gateway/push/rule.{type Class}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics

pub fn count(name: String, by: Int) -> Nil {
  case by {
    0 -> Nil
    _ -> metrics.counter("fluxer_gateway_" <> name, [], by)
  }
}

pub fn count_labelled(
  name: String,
  labels: List(#(String, String)),
  by: Int,
) -> Nil {
  case by {
    0 -> Nil
    _ -> metrics.counter("fluxer_gateway_" <> name, labels, by)
  }
}

pub fn gauge(name: String, labels: List(#(String, String)), value: Int) -> Nil {
  metrics.gauge("fluxer_gateway_" <> name, labels, value)
}

fn class_label(class: Class) -> String {
  case class {
    rule.Direct -> "direct"
    rule.Bulk -> "bulk"
  }
}

pub fn outbox(name: String, class: Class, by: Int) -> Nil {
  count_labelled("push_outbox_" <> name, [#("class", class_label(class))], by)
  case name {
    "delivered_total" -> drain_jobs("finished", by)
    _ -> Nil
  }
}

type Key {
  FluxerGatewayPushDrainClosed
}

type Stored

@external(erlang, "persistent_term", "put")
fn put_term(key: Key, value: Bool) -> Stored

@external(erlang, "persistent_term", "get")
fn get_term(key: Key, default: Bool) -> Bool

fn drain_closed() -> Bool {
  draining.is_draining() && get_term(FluxerGatewayPushDrainClosed, False)
}

pub fn open_drain() -> Nil {
  put_term(FluxerGatewayPushDrainClosed, False)
  Nil
}

pub fn close_drain(left: Int) -> Nil {
  case drain_closed() {
    True -> Nil
    False -> {
      lost_or_moved("lost", left)
      put_term(FluxerGatewayPushDrainClosed, draining.is_draining())
      Nil
    }
  }
}

pub fn drain_jobs(outcome: String, by: Int) -> Nil {
  case draining.is_draining() {
    True -> lost_or_moved(outcome, by)
    False -> Nil
  }
}

pub fn lost_or_moved(outcome: String, by: Int) -> Nil {
  case drain_closed() {
    True -> Nil
    False ->
      rollout_metrics.count(
        rollout_metrics.push_jobs_drain,
        [#("outcome", outcome)],
        by,
      )
  }
}

pub fn outbox_dropped(kind: Kind, class: Class, reason: String) -> Nil {
  drain_jobs("lost", 1)
  count_labelled(
    "push_outbox_dropped_total",
    [
      #("kind", job.kind_label(kind)),
      #("reason", reason),
      #("class", class_label(class)),
    ],
    1,
  )
}

pub fn skipped(reason: String, by: Int) -> Nil {
  count_labelled("push_recipients_skipped_total", [#("reason", reason)], by)
}

pub fn guild(key: String) -> Nil {
  count_labelled("guild_push_counters_total", [#("key", key)], 1)
}
