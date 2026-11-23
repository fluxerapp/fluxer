// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics
import gleam/list
import gleam/option.{type Option, None, Some}

pub const requests = "fluxer_gateway_nats_rpc_requests_total"

pub const duration = "fluxer_gateway_nats_rpc_duration_ms"

pub const handlers = "fluxer_gateway_nats_rpc_handlers"

pub const methods = [
  "call.confirm_connection", "call.create", "call.disconnect_user_if_in_channel",
  "call.get", "call.ring", "call.stop_ringing", "call.update_region",
  "guild.can_manage_role", "guild.can_manage_roles", "guild.check_permission",
  "guild.check_target_member", "guild.confirm_voice_connection_from_livekit",
  "guild.disconnect_voice_user_if_in_channel", "guild.dispatch",
  "guild.dispatch_many", "guild.evict_visitors", "guild.get_auth_context",
  "guild.get_category_channel_count", "guild.get_channel_count",
  "guild.get_counts", "guild.get_data", "guild.get_first_viewable_text_channel",
  "guild.get_member", "guild.get_members_with_role",
  "guild.get_online_counts_batch", "guild.get_user_permissions",
  "guild.get_user_permissions_batch", "guild.get_viewable_channels",
  "guild.get_voice_state", "guild.get_voice_states_for_channel",
  "guild.has_member", "guild.list_members", "guild.list_members_cursor",
  "guild.move_member", "guild.reload", "guild.reload_all",
  "guild.resolve_channel_mentions", "guild.resolve_mention_sources_page",
  "guild.shutdown", "guild.start", "guild.stop", "guild.switch_voice_region",
  "guild.update_member_voice", "presence.add_temporary_guild",
  "presence.dispatch", "presence.end_visit", "presence.has_active",
  "presence.join_guild", "presence.leave_guild",
  "presence.remove_temporary_guild", "presence.start_visit",
  "presence.sync_group_dm_recipients", "presence.terminate_all_sessions",
  "presence.terminate_sessions", "process.memory_stats", "process.node_stats",
  "process.voice_state_counts", "push.clear_channel_notifications",
  "push.sync_user_guild_settings", "voice.confirm_connection",
  "voice.disconnect_user_if_in_channel", "voice.get_voice_states_for_channel",
]

type Counters

type CountersOption {
  WriteConcurrency
}

type Key {
  FluxerGatewayNatsRpcHandlers
}

type Stored

@external(erlang, "counters", "new")
fn counters_new(size: Int, options: List(CountersOption)) -> Counters

@external(erlang, "counters", "add")
fn counters_add(counters: Counters, index: Int, by: Int) -> Stored

@external(erlang, "counters", "put")
fn counters_put(counters: Counters, index: Int, value: Int) -> Stored

@external(erlang, "counters", "get")
fn counters_get(counters: Counters, index: Int) -> Int

@external(erlang, "persistent_term", "get")
fn get_counters(key: Key, default: Option(Counters)) -> Option(Counters)

@external(erlang, "persistent_term", "put")
fn put_counters(key: Key, value: Option(Counters)) -> Stored

fn live() -> Counters {
  case get_counters(FluxerGatewayNatsRpcHandlers, None) {
    Some(found) -> found
    None -> {
      let created = counters_new(1, [WriteConcurrency])
      put_counters(FluxerGatewayNatsRpcHandlers, Some(created))
      metrics.register_collector(handlers, fn() {
        [metrics.Sample(handlers, [], counters_get(created, 1))]
      })
      created
    }
  }
}

pub fn method(name: String) -> String {
  case list.contains(methods, name) {
    True -> name
    False -> "other"
  }
}

pub fn family(name: String) -> String {
  case name {
    "guild." <> _ -> "guild"
    "presence." <> _ -> "presence"
    "call." <> _ -> "call"
    "voice." <> _ -> "voice"
    "process." <> _ -> "process"
    "push." <> _ -> "push"
    _ -> "other"
  }
}

pub fn handled(name: String, outcome: String) -> Nil {
  metrics.counter(
    requests,
    [#("method", method(name)), #("outcome", outcome)],
    1,
  )
}

pub fn answered(name: String, succeeded: Bool, elapsed_ms: Int) -> Nil {
  let outcome = case succeeded {
    True -> "ok"
    False -> "error"
  }
  handled(name, outcome)
  metrics.observe_ms(duration, [#("family", family(name))], elapsed_ms)
}

pub fn handler_started() -> Nil {
  counters_add(live(), 1, 1)
  Nil
}

pub fn handler_finished() -> Nil {
  counters_add(live(), 1, -1)
  Nil
}

pub fn watch() -> Nil {
  counters_put(live(), 1, 0)
  Nil
}
