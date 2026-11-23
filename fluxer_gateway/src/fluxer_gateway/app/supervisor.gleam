// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/codec_range.{type CodecRange, CodecRange}
import bouncer_proto/key
import fluxer_gateway/api/http as api_http
import fluxer_gateway/app/http as app_http
import fluxer_gateway/app/status
import fluxer_gateway/cluster/bouncer_client
import fluxer_gateway/cluster/discovery
import fluxer_gateway/cluster/handoff
import fluxer_gateway/cluster/lease_probe
import fluxer_gateway/cluster/membership
import fluxer_gateway/cluster/relay
import fluxer_gateway/config/rollout
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/node
import fluxer_gateway/gateway/start_gate
import fluxer_gateway/guild/counts_cache
import fluxer_gateway/guild/ets_owner
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/health as guild_health
import fluxer_gateway/guild/manager as guild_manager
import fluxer_gateway/handoff/service as handoff_service
import fluxer_gateway/nats/pool as nats_pool
import fluxer_gateway/presence/cache as presence_cache
import fluxer_gateway/presence/fanout as presence_fanout
import fluxer_gateway/presence/handoff/codec as presence_codec
import fluxer_gateway/presence/manager as presence_manager
import fluxer_gateway/push/handoff/codec as push_codec
import fluxer_gateway/push/outbox as push_outbox
import fluxer_gateway/push/permit as push_permit
import fluxer_gateway/push/server as push_server
import fluxer_gateway/rpc/server as rpc_server
import fluxer_gateway/session/manager as session_manager
import fluxer_gateway/session/manager/transfer as session_transfer
import fluxer_gateway/telemetry/periodic_gc
import fluxer_gateway/telemetry/stall_monitor
import fluxer_gateway/telemetry/watchdog
import fluxer_gateway/voice/call/handoff/codec as call_codec
import fluxer_gateway/voice/call/manager as call_manager
import fluxer_gateway/voice/counts as voice_counts
import fluxer_gateway/voice/permission_sync
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/otp/static_supervisor.{type Supervisor}
import gleam/otp/supervision.{type ChildSpecification}

pub type Child {
  ApiHttp
  GuildEtsOwner
  RpcServer
  NatsPool
  StartGate
  RolloutChanges
  Rollout
  VoicePermissionSync
  ClusterDiscovery
  ClusterMembership
  ClusterLeaseProbe
  ClusterBouncer
  ClusterRelay
  PeriodicGc
  Watchdog
  StallMonitor
  PresenceFanout
  PushOutbox
  SessionTransfer
  SessionManager
  PresenceCache
  PresenceManager
  GuildHealth
  GuildCountsCache
  GuildManager
  VoiceCounts
  CallManager
  PushServer
  PushPermit
  ClusterHandoff
  HandoffEngine
  StatusListener
  HttpListener
}

pub fn start(
  settings: Settings,
) -> Result(actor.Started(Supervisor), actor.StartError) {
  plan(settings)
  |> list.fold(
    static_supervisor.new(static_supervisor.OneForOne)
      |> static_supervisor.restart_tolerance(intensity: 5, period: 10),
    fn(builder, child) { static_supervisor.add(builder, spec(child, settings)) },
  )
  |> static_supervisor.start
}

pub fn plan(settings: Settings) -> List(Child) {
  let runs = fn(role) { node.runs(settings.role, role) }
  let bouncer = option.is_some(settings.bouncer)
  let cluster = settings.cluster_enabled && !bouncer
  list.flatten([
    [ApiHttp, GuildEtsOwner],
    only_if(runs(node.Guilds) || runs(node.Push), [PushPermit]),
    [
      RpcServer,
      NatsPool,
      RolloutChanges,
      Rollout,
      StartGate,
      VoicePermissionSync,
    ],
    only_if(cluster, [ClusterDiscovery, ClusterMembership]),
    only_if(bouncer, [ClusterLeaseProbe, ClusterBouncer]),
    [ClusterRelay, PeriodicGc, Watchdog, StallMonitor],
    only_if(runs(node.Presence) || runs(node.Guilds), [PresenceFanout]),
    only_if(runs(node.Push) || runs(node.Calls), [PushOutbox]),
    only_if(runs(node.Sessions), [SessionTransfer, SessionManager]),
    only_if(runs(node.Presence), [PresenceCache, PresenceManager]),
    only_if(runs(node.Guilds), [
      GuildHealth,
      GuildCountsCache,
      GuildManager,
      VoiceCounts,
    ]),
    only_if(runs(node.Calls), [
      CallManager,
      ..only_if(!runs(node.Guilds), [VoiceCounts])
    ]),
    only_if(runs(node.Push), [PushServer]),
    only_if(cluster && settings.role != node.Websocket, [ClusterHandoff]),
    only_if(bouncer && handoff_service.movable(settings), [HandoffEngine]),
    only_if(settings.status_port > 0, [StatusListener]),
    [HttpListener],
  ])
}

fn only_if(condition: Bool, children: List(Child)) -> List(Child) {
  case condition {
    True -> children
    False -> []
  }
}

fn spec(child: Child, settings: Settings) -> ChildSpecification(Nil) {
  case child {
    ApiHttp -> api_http.supervised(settings)
    GuildEtsOwner -> ets_owner.supervised(settings)
    RpcServer -> rpc_server.supervised(settings)
    NatsPool -> nats_pool.supervised(settings)
    StartGate -> start_gate.supervised(settings)
    RolloutChanges -> rollout.supervised_changes(settings)
    Rollout -> rollout.supervised(settings)
    VoicePermissionSync -> permission_sync.supervised(settings)
    ClusterDiscovery -> discovery.supervised(settings)
    ClusterMembership -> membership.supervised(settings)
    ClusterLeaseProbe -> lease_probe.supervised(settings)
    ClusterBouncer -> bouncer_client.supervised(settings, codecs(settings))
    ClusterRelay -> relay.supervised(settings)
    PeriodicGc -> periodic_gc.supervised(settings)
    Watchdog -> watchdog.supervised(settings)
    StallMonitor -> stall_monitor.supervised(settings)
    PresenceFanout -> presence_fanout.supervised(settings)
    PushOutbox -> push_outbox.supervised(settings)
    SessionTransfer -> session_transfer.supervised(settings)
    SessionManager -> session_manager.supervised(settings)
    PresenceCache -> presence_cache.supervised(settings)
    PresenceManager -> presence_manager.supervised(settings)
    GuildHealth -> guild_health.supervised(settings)
    GuildCountsCache -> counts_cache.supervised(settings)
    GuildManager -> guild_manager.supervised(settings)
    VoiceCounts -> voice_counts.supervised(settings)
    CallManager -> call_manager.supervised(settings)
    PushServer -> push_server.supervised(settings)
    PushPermit -> push_permit.supervised(settings)
    ClusterHandoff -> handoff.supervised(settings)
    HandoffEngine -> handoff_service.supervised(settings)
    HttpListener -> app_http.supervised(settings)
    StatusListener -> status.supervised(settings)
  }
}

pub fn codecs(settings: Settings) -> List(CodecRange) {
  let guilds = guild_codec.range()
  let pinned = case settings.bouncer {
    option.Some(bouncer) if bouncer.pool != "default" -> [
      #(node.Guilds, CodecRange(..guilds, ns: key.first_pinned)),
    ]
    _ -> []
  }
  [
    #(node.Guilds, guilds),
    #(node.Sessions, codec_range.unmovable(key.session)),
    #(node.Presence, presence_codec.range()),
    #(node.Calls, call_codec.range()),
    #(node.Push, push_codec.range()),
    ..pinned
  ]
  |> list.filter(fn(pair) { node.runs(settings.role, pair.0) })
  |> list.map(fn(pair) { pair.1 })
}
