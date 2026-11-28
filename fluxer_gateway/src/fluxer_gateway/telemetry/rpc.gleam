// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/guild/manager as guild_manager
import fluxer_gateway/guild/manager/shard as guild_shard
import fluxer_gateway/telemetry/memory_stats.{type GuildMemory, GuildMemory}
import fluxer_gateway/telemetry/metrics.{type Sample}
import fluxer_gateway/telemetry/multicall
import fluxer_gateway/telemetry/prometheus
import fluxer_gateway/telemetry/vm
import fluxer_gateway/util/decode as id_decode
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/order
import gleam/result
import gleam/string

fn node_rpc_timeout_ms() -> Int {
  durations.telemetry().rpc_node_rpc_timeout_ms
}

fn memory_rpc_timeout_ms() -> Int {
  durations.telemetry().rpc_memory_rpc_timeout_ms
}

const default_memory_stats_limit = 100

const max_memory_stats_limit = 500

pub type ClusterSnapshot {
  ClusterSnapshot(
    member_count: Int,
    session_resumes_total: Int,
    websocket_dispatches_total: Int,
    websocket_dispatch_drops_total: Int,
    discovery_resolve_failures_total: Int,
    membership_up: Int,
    membership_down: Int,
    owner_self: Int,
    owner_peer: Int,
  )
}

pub type NodeStats {
  NodeStats(
    node_id: String,
    sessions: Int,
    session_resumes_total: Int,
    websocket_dispatches_total: Int,
    websocket_dispatch_drops_total: Int,
    guilds: Int,
    presences: Int,
    calls: Int,
    memory_total: Int,
    memory_processes: Int,
    memory_system: Int,
    process_count: Int,
    process_limit: Int,
    uptime_seconds: Int,
    cluster: ClusterSnapshot,
  )
}

pub type NodeReport {
  Available(stats: NodeStats)
  Unavailable(node_id: String)
}

pub type VoiceCounts {
  VoiceCounts(
    total_voice_states: Int,
    regions: List(#(String, Int)),
    servers: List(#(String, Int)),
  )
}

type PeerReply(a) =
  #(Node, Result(a, Nil))

pub fn handle(method: String, params: RawJson) -> Result(Json, RpcError) {
  case method {
    "process.memory_stats" -> {
      use limit <- result.try(memory_stats_limit(params))
      Ok(memory_stats_reply(limit))
    }
    "process.node_stats" -> Ok(node_stats())
    "process.voice_state_counts" -> Ok(voice_state_counts())
    "process.reconcile_unowned" -> {
      use dry_run <- result.try(bool_param(params, "dry_run"))
      Ok(reconcile_unowned(dry_run))
    }
    _ -> Error(error.UnknownMethod(method))
  }
}

fn memory_stats_limit(params: RawJson) -> Result(Int, RpcError) {
  case raw_json.fields(params) {
    Error(Nil) -> Error(error.InvalidParams)
    Ok(fields) ->
      case list.key_find(list.reverse(fields), "limit") {
        Error(Nil) -> Ok(default_memory_stats_limit)
        Ok(value) ->
          raw_json.read(value, id_decode.snowflake())
          |> result.map(int.min(_, max_memory_stats_limit))
          |> result.replace_error(error.InvalidParams)
      }
  }
}

fn bool_param(params: RawJson, name: String) -> Result(Bool, RpcError) {
  case raw_json.fields(params) {
    Error(Nil) -> Error(error.InvalidParams)
    Ok(fields) ->
      case list.key_find(list.reverse(fields), name) {
        Error(Nil) -> Error(error.InvalidParams)
        Ok(value) ->
          raw_json.read(value, decode.bool)
          |> result.replace_error(error.InvalidParams)
      }
  }
}

fn reconcile_unowned(dry_run: Bool) -> Json {
  let flag = case dry_run {
    True -> 1
    False -> 0
  }
  let reports =
    gather(
      sorted_nodes(router.nodes_with(node.Guilds)),
      "local_reconcile_unowned_json",
      [flag],
      node_rpc_timeout_ms(),
      sweep_decoder(),
      fn() { local_sweep(flag) },
    )
  json.object([
    #("dry_run", json.bool(dry_run)),
    #(
      "nodes",
      json.array(reports, fn(reply) {
        case reply.1 {
          Ok(found) -> sweep_to_json(found)
          Error(Nil) ->
            json.object([
              #("node", json.string(node.to_string(reply.0))),
              #("error", json.string("unreachable")),
            ])
        }
      }),
    ),
  ])
}

pub fn local_reconcile_unowned_json(flag: Int) -> String {
  local_sweep(flag) |> sweep_to_json |> json.to_string
}

fn local_sweep(flag: Int) -> #(String, guild_manager.LocalSweep) {
  #(
    local_node_id(),
    guild_manager.sweep_ownership_local(True, flag == 1, local_sweep_ms()),
  )
}

fn local_sweep_ms() -> Int {
  int.max(node_rpc_timeout_ms() - sweep_margin_ms, node_rpc_timeout_ms() / 2)
}

const sweep_margin_ms = 3000

fn sweep_to_json(found: #(String, guild_manager.LocalSweep)) -> Json {
  let #(id, swept) = found
  json.object([
    #("node", json.string(id)),
    #("running", json.int(swept.report.running)),
    #("unowned", json.int(swept.report.unowned)),
    #("disowned", json.int(swept.report.disowned)),
    #("lapsed", json.int(swept.report.lapsed)),
    #("shards", json.int(swept.shards)),
    #("shards_replied", json.int(swept.replied)),
    #("partial", json.bool(swept.replied < swept.shards)),
  ])
}

fn sweep_decoder() -> Decoder(#(String, guild_manager.LocalSweep)) {
  use id <- decode.field("node", decode.string)
  use running <- decode.field("running", decode.int)
  use unowned <- decode.field("unowned", decode.int)
  use disowned <- decode.field("disowned", decode.int)
  use lapsed <- decode.field("lapsed", decode.int)
  use shards <- decode.field("shards", decode.int)
  use replied <- decode.field("shards_replied", decode.int)
  decode.success(#(
    id,
    guild_manager.LocalSweep(
      report: guild_shard.SweepReport(running:, unowned:, disowned:, lapsed:),
      shards:,
      replied:,
    ),
  ))
}

fn target_nodes() -> List(Node) {
  sorted_nodes([node.self(), ..router.members()])
}

fn voice_count_nodes() -> List(Node) {
  sorted_nodes(list.append(
    router.nodes_with(node.Guilds),
    router.nodes_with(node.Calls),
  ))
}

fn sorted_nodes(nodes: List(Node)) -> List(Node) {
  nodes
  |> list.unique
  |> list.sort(fn(a, b) { string.compare(node.to_string(a), node.to_string(b)) })
}

fn gather(
  targets: List(Node),
  function: String,
  args: List(Int),
  timeout_ms: Int,
  decoder: Decoder(a),
  local: fn() -> a,
) -> List(PeerReply(a)) {
  let self = node.self()
  let peers = list.filter(targets, fn(target) { target != self })
  let replies =
    multicall.call(
      list.map(peers, node.to_atom),
      atom.create("fluxer_gateway@telemetry@rpc"),
      atom.create(function),
      args,
      timeout_ms,
    )
    |> list.map(decode_reply(_, decoder))
    |> list.zip(peers, _)
  list.map(targets, fn(target) {
    case target == self {
      True -> #(target, Ok(local()))
      False -> #(target, list.key_find(replies, target) |> result.flatten)
    }
  })
}

fn decode_reply(reply: multicall.Reply, decoder: Decoder(a)) -> Result(a, Nil) {
  use text <- result.try(multicall.text(reply))
  raw_json.read(raw_json.wrap_string(text), decoder)
}

fn local_node_id() -> String {
  case envoy.get("HOSTNAME") {
    Ok(hostname) ->
      case string.trim(hostname) {
        "" -> node.to_string(node.self())
        trimmed -> trimmed
      }
    Error(Nil) -> node.to_string(node.self())
  }
}

fn memory_stats_reply(limit: Int) -> Json {
  let guilds =
    gather(
      target_nodes(),
      "local_memory_stats_json",
      [limit],
      memory_rpc_timeout_ms(),
      node_memory_decoder(),
      fn() { #(local_node_id(), memory_stats.local(limit)) },
    )
    |> list.flat_map(fn(reply) {
      case reply.1 {
        Ok(#(id, guilds)) -> list.map(guilds, fn(guild) { #(guild, id) })
        Error(Nil) -> []
      }
    })
    |> list.sort(fn(a, b) { memory_stats.compare(a.0, b.0) })
    |> list.take(limit)
  json.object([
    #(
      "guilds",
      json.array(guilds, fn(entry) { memory_stats.to_json(entry.0, entry.1) }),
    ),
  ])
}

pub fn local_memory_stats_json(limit: Int) -> String {
  json.object([
    #("node_id", json.string(local_node_id())),
    #("guilds", json.array(memory_stats.local(limit), guild_memory_to_json)),
  ])
  |> json.to_string
}

fn node_memory_decoder() -> Decoder(#(String, List(GuildMemory))) {
  use node_id <- decode.field("node_id", decode.string)
  use guilds <- decode.field("guilds", decode.list(guild_memory_decoder()))
  decode.success(#(node_id, guilds))
}

fn guild_memory_to_json(guild: GuildMemory) -> Json {
  json.object([
    #("guild_id", json.nullable(guild.guild_id, json.string)),
    #("guild_name", json.string(guild.name)),
    #("guild_icon", json.nullable(guild.icon, json.string)),
    #("memory", json.int(guild.memory)),
    #("member_count", json.int(guild.member_count)),
    #("session_count", json.int(guild.session_count)),
    #("presence_count", json.int(guild.presence_count)),
  ])
}

fn guild_memory_decoder() -> Decoder(GuildMemory) {
  use guild_id <- decode.field("guild_id", decode.optional(decode.string))
  use name <- decode.field("guild_name", decode.string)
  use icon <- decode.field("guild_icon", decode.optional(decode.string))
  use memory <- decode.field("memory", decode.int)
  use member_count <- decode.field("member_count", decode.int)
  use session_count <- decode.field("session_count", decode.int)
  use presence_count <- decode.field("presence_count", decode.int)
  decode.success(GuildMemory(
    guild_id:,
    name:,
    icon:,
    memory:,
    member_count:,
    session_count:,
    presence_count:,
  ))
}

fn node_stats() -> Json {
  target_nodes()
  |> node_reports
  |> aggregate_node_stats
}

pub fn node_reports(targets: List(Node)) -> List(NodeReport) {
  gather(
    targets,
    "local_node_stats_json",
    [],
    node_rpc_timeout_ms(),
    node_stats_decoder(),
    local_node_stats,
  )
  |> list.map(fn(reply) {
    case reply.1 {
      Ok(stats) -> Available(stats)
      Error(Nil) -> Unavailable(node.to_string(reply.0))
    }
  })
}

fn local_node_stats() -> NodeStats {
  let samples = prometheus.current_samples()
  let value = fn(name) { metrics.value(samples, name, []) }
  NodeStats(
    node_id: local_node_id(),
    sessions: value("fluxer_gateway_sessions_total"),
    session_resumes_total: value("fluxer_gateway_session_resumes_total"),
    websocket_dispatches_total: value(
      "fluxer_gateway_websocket_dispatches_total",
    ),
    websocket_dispatch_drops_total: value(
      "fluxer_gateway_websocket_dispatch_drops_total",
    ),
    guilds: value("fluxer_gateway_guilds_total"),
    presences: metrics.value(samples, "fluxer_gateway_processes", [
      #("type", "presence"),
    ]),
    calls: value("fluxer_gateway_calls_total"),
    memory_total: vm.memory(of: "total"),
    memory_processes: vm.memory(of: "processes"),
    memory_system: vm.memory(of: "system"),
    process_count: vm.process_count(),
    process_limit: vm.process_limit(),
    uptime_seconds: vm.uptime_seconds(),
    cluster: cluster_snapshot(samples),
  )
}

fn cluster_snapshot(samples: List(Sample)) -> ClusterSnapshot {
  let value = fn(name, labels) { metrics.value(samples, name, labels) }
  ClusterSnapshot(
    member_count: value("fluxer_gateway_cluster_member_count", []),
    session_resumes_total: value("fluxer_gateway_session_resumes_total", []),
    websocket_dispatches_total: value(
      "fluxer_gateway_websocket_dispatches_total",
      [],
    ),
    websocket_dispatch_drops_total: value(
      "fluxer_gateway_websocket_dispatch_drops_total",
      [],
    ),
    discovery_resolve_failures_total: value(
      "fluxer_gateway_cluster_discovery_resolve_failures_total",
      [],
    ),
    membership_up: value("fluxer_gateway_cluster_membership_transitions_total", [
      #("direction", "up"),
    ]),
    membership_down: value(
      "fluxer_gateway_cluster_membership_transitions_total",
      [#("direction", "down")],
    ),
    owner_self: value("fluxer_gateway_cluster_owner_resolutions_total", [
      #("result", "self"),
    ]),
    owner_peer: value("fluxer_gateway_cluster_owner_resolutions_total", [
      #("result", "peer"),
    ]),
  )
}

pub fn local_node_stats_json() -> String {
  local_node_stats() |> Available |> report_to_json |> json.to_string
}

fn report_to_json(report: NodeReport) -> Json {
  case report {
    Available(stats) ->
      json.object([
        #("calls", json.int(stats.calls)),
        #("cluster_metrics", cluster_to_json(stats.cluster)),
        #("guilds", json.int(stats.guilds)),
        #(
          "memory",
          memory_json(
            json.string(int.to_string(stats.memory_processes)),
            json.string(int.to_string(stats.memory_system)),
            json.string(int.to_string(stats.memory_total)),
          ),
        ),
        #("node_id", json.string(stats.node_id)),
        #("presences", json.int(stats.presences)),
        #("process_count", json.int(stats.process_count)),
        #("process_limit", json.int(stats.process_limit)),
        #("session_resumes_total", json.int(stats.session_resumes_total)),
        #("sessions", json.int(stats.sessions)),
        #("status", json.string("healthy")),
        #("uptime_seconds", json.int(stats.uptime_seconds)),
        #(
          "websocket_dispatch_drops_total",
          json.int(stats.websocket_dispatch_drops_total),
        ),
        #(
          "websocket_dispatches_total",
          json.int(stats.websocket_dispatches_total),
        ),
      ])
    Unavailable(node_id) ->
      json.object([
        #("calls", json.null()),
        #("guilds", json.null()),
        #("memory", memory_json(json.null(), json.null(), json.null())),
        #("node_id", json.string(node_id)),
        #("presences", json.null()),
        #("process_count", json.null()),
        #("process_limit", json.null()),
        #("session_resumes_total", json.null()),
        #("sessions", json.null()),
        #("status", json.string("unavailable")),
        #("uptime_seconds", json.null()),
        #("websocket_dispatch_drops_total", json.null()),
        #("websocket_dispatches_total", json.null()),
      ])
  }
}

fn memory_json(processes: Json, system: Json, total: Json) -> Json {
  json.object([
    #("processes", processes),
    #("system", system),
    #("total", total),
  ])
}

fn cluster_to_json(cluster: ClusterSnapshot) -> Json {
  json.object([
    #(
      "gateway_cluster_discovery_resolve_failures_total",
      json.int(cluster.discovery_resolve_failures_total),
    ),
    #("gateway_cluster_member_count", json.int(cluster.member_count)),
    #(
      "gateway_cluster_membership_transitions_total",
      json.object([
        #("down", json.int(cluster.membership_down)),
        #("up", json.int(cluster.membership_up)),
      ]),
    ),
    #(
      "gateway_node_router_owner_resolutions_total",
      json.object([
        #("peer", json.int(cluster.owner_peer)),
        #("self", json.int(cluster.owner_self)),
      ]),
    ),
    #("session_resumes_total", json.int(cluster.session_resumes_total)),
    #(
      "websocket_dispatch_drops_total",
      json.int(cluster.websocket_dispatch_drops_total),
    ),
    #(
      "websocket_dispatches_total",
      json.int(cluster.websocket_dispatches_total),
    ),
  ])
}

fn node_stats_decoder() -> Decoder(NodeStats) {
  use node_id <- decode.field("node_id", decode.string)
  use sessions <- decode.field("sessions", decode.int)
  use session_resumes_total <- decode.field("session_resumes_total", decode.int)
  use websocket_dispatches_total <- decode.field(
    "websocket_dispatches_total",
    decode.int,
  )
  use websocket_dispatch_drops_total <- decode.field(
    "websocket_dispatch_drops_total",
    decode.int,
  )
  use guilds <- decode.field("guilds", decode.int)
  use presences <- decode.field("presences", decode.int)
  use calls <- decode.field("calls", decode.int)
  use memory_total <- decode.subfield(["memory", "total"], decimal_string())
  use memory_processes <- decode.subfield(
    ["memory", "processes"],
    decimal_string(),
  )
  use memory_system <- decode.subfield(["memory", "system"], decimal_string())
  use process_count <- decode.field("process_count", decode.int)
  use process_limit <- decode.field("process_limit", decode.int)
  use uptime_seconds <- decode.field("uptime_seconds", decode.int)
  use cluster <- decode.field("cluster_metrics", cluster_decoder())
  decode.success(NodeStats(
    node_id:,
    sessions:,
    session_resumes_total:,
    websocket_dispatches_total:,
    websocket_dispatch_drops_total:,
    guilds:,
    presences:,
    calls:,
    memory_total:,
    memory_processes:,
    memory_system:,
    process_count:,
    process_limit:,
    uptime_seconds:,
    cluster:,
  ))
}

fn cluster_decoder() -> Decoder(ClusterSnapshot) {
  use member_count <- decode.field("gateway_cluster_member_count", decode.int)
  use session_resumes_total <- decode.field("session_resumes_total", decode.int)
  use websocket_dispatches_total <- decode.field(
    "websocket_dispatches_total",
    decode.int,
  )
  use websocket_dispatch_drops_total <- decode.field(
    "websocket_dispatch_drops_total",
    decode.int,
  )
  use discovery_resolve_failures_total <- decode.field(
    "gateway_cluster_discovery_resolve_failures_total",
    decode.int,
  )
  use membership_up <- decode.subfield(
    ["gateway_cluster_membership_transitions_total", "up"],
    decode.int,
  )
  use membership_down <- decode.subfield(
    ["gateway_cluster_membership_transitions_total", "down"],
    decode.int,
  )
  use owner_self <- decode.subfield(
    ["gateway_node_router_owner_resolutions_total", "self"],
    decode.int,
  )
  use owner_peer <- decode.subfield(
    ["gateway_node_router_owner_resolutions_total", "peer"],
    decode.int,
  )
  decode.success(ClusterSnapshot(
    member_count:,
    session_resumes_total:,
    websocket_dispatches_total:,
    websocket_dispatch_drops_total:,
    discovery_resolve_failures_total:,
    membership_up:,
    membership_down:,
    owner_self:,
    owner_peer:,
  ))
}

fn decimal_string() -> Decoder(Int) {
  use text <- decode.then(decode.string)
  case id.parse_decimal(text) {
    Ok(value) if value >= 0 -> decode.success(value)
    _ -> decode.failure(0, "DecimalString")
  }
}

pub fn aggregate_node_stats(reports: List(NodeReport)) -> Json {
  let reports =
    list.sort(reports, fn(a, b) { string.compare(report_id(a), report_id(b)) })
  let stats =
    list.filter_map(reports, fn(report) {
      case report {
        Available(stats) -> Ok(stats)
        Unavailable(_) -> Error(Nil)
      }
    })
  let sum = fn(read: fn(NodeStats) -> Int) {
    list.fold(stats, 0, fn(total, entry) { total + int.max(0, read(entry)) })
  }
  let status = case list.length(stats) == list.length(reports) {
    True -> "healthy"
    False -> "degraded"
  }
  let uptime =
    stats
    |> list.map(fn(entry) { entry.uptime_seconds })
    |> list.filter(fn(seconds) { seconds >= 0 })
    |> list.reduce(int.min)
    |> result.unwrap(0)
  json.object([
    #("calls", json.int(sum(fn(entry) { entry.calls }))),
    #("guilds", json.int(sum(fn(entry) { entry.guilds }))),
    #(
      "memory",
      memory_json(
        json.string(int.to_string(sum(fn(entry) { entry.memory_processes }))),
        json.string(int.to_string(sum(fn(entry) { entry.memory_system }))),
        json.string(int.to_string(sum(fn(entry) { entry.memory_total }))),
      ),
    ),
    #("node_count", json.int(list.length(reports))),
    #("nodes", json.array(reports, report_to_json)),
    #("presences", json.int(sum(fn(entry) { entry.presences }))),
    #("process_count", json.int(sum(fn(entry) { entry.process_count }))),
    #("process_limit", json.int(sum(fn(entry) { entry.process_limit }))),
    #(
      "session_resumes_total",
      json.int(sum(fn(entry) { entry.session_resumes_total })),
    ),
    #("sessions", json.int(sum(fn(entry) { entry.sessions }))),
    #("status", json.string(status)),
    #("uptime_seconds", json.int(uptime)),
    #(
      "websocket_dispatch_drops_total",
      json.int(sum(fn(entry) { entry.websocket_dispatch_drops_total })),
    ),
    #(
      "websocket_dispatches_total",
      json.int(sum(fn(entry) { entry.websocket_dispatches_total })),
    ),
  ])
}

fn report_id(report: NodeReport) -> String {
  case report {
    Available(stats) -> stats.node_id
    Unavailable(node_id) -> node_id
  }
}

fn voice_state_counts() -> Json {
  voice_count_nodes()
  |> gather(
    "local_voice_state_counts_json",
    [],
    node_rpc_timeout_ms(),
    voice_counts_decoder(),
    local_voice_state_counts,
  )
  |> list.map(fn(reply) { result.unwrap(reply.1, VoiceCounts(0, [], [])) })
  |> aggregate_voice_state_counts
  |> voice_counts_to_json
}

fn local_voice_state_counts() -> VoiceCounts {
  let samples = prometheus.current_samples()
  let by_label = fn(label) {
    samples
    |> list.filter(fn(sample) {
      sample.name == "fluxer_gateway_voice_connections"
    })
    |> list.filter_map(fn(sample) {
      case sample.labels {
        [#(key, id)] if key == label -> Ok(#(id, sample.value))
        _ -> Error(Nil)
      }
    })
  }
  VoiceCounts(
    total_voice_states: metrics.value(
      samples,
      "fluxer_gateway_voice_connections_total",
      [],
    ),
    regions: by_label("region"),
    servers: by_label("server"),
  )
}

pub fn local_voice_state_counts_json() -> String {
  local_voice_state_counts() |> voice_counts_to_json |> json.to_string
}

pub fn aggregate_voice_state_counts(counts: List(VoiceCounts)) -> VoiceCounts {
  VoiceCounts(
    total_voice_states: list.fold(counts, 0, fn(total, entry) {
      total + int.max(0, entry.total_voice_states)
    }),
    regions: merge_entries(list.flat_map(counts, fn(entry) { entry.regions })),
    servers: merge_entries(list.flat_map(counts, fn(entry) { entry.servers })),
  )
}

fn merge_entries(entries: List(#(String, Int))) -> List(#(String, Int)) {
  entries
  |> list.fold(dict.new(), fn(merged: Dict(String, Int), entry) {
    let #(id, count) = entry
    case count > 0 {
      True ->
        dict.insert(merged, id, result.unwrap(dict.get(merged, id), 0) + count)
      False -> merged
    }
  })
  |> dict.to_list
  |> list.sort(fn(a, b) {
    case int.compare(b.1, a.1) {
      order.Eq -> string.compare(a.0, b.0)
      other -> other
    }
  })
}

fn voice_counts_to_json(counts: VoiceCounts) -> Json {
  json.object([
    #("regions", entries_json(counts.regions, "region_id")),
    #("servers", entries_json(counts.servers, "server_id")),
    #("total_voice_states", json.int(counts.total_voice_states)),
  ])
}

fn entries_json(entries: List(#(String, Int)), id_key: String) -> Json {
  json.array(entries, fn(entry) {
    let #(id, count) = entry
    json.object([
      #(id_key, json.string(id)),
      #("voice_state_count", json.int(count)),
    ])
  })
}

fn voice_counts_decoder() -> Decoder(VoiceCounts) {
  use total <- decode.field("total_voice_states", decode.int)
  use regions <- decode.field(
    "regions",
    decode.list(entry_decoder("region_id")),
  )
  use servers <- decode.field(
    "servers",
    decode.list(entry_decoder("server_id")),
  )
  decode.success(VoiceCounts(total, regions, servers))
}

fn entry_decoder(id_key: String) -> Decoder(#(String, Int)) {
  use id <- decode.field(id_key, decode.string)
  use count <- decode.field("voice_state_count", decode.int)
  decode.success(#(id, count))
}
