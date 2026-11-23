// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/codec_range
import bouncer_proto/key
import fluxer_gateway/app/health
import fluxer_gateway/app/http
import fluxer_gateway/app/supervisor.{
  ApiHttp, CallManager, ClusterBouncer, ClusterDiscovery, ClusterHandoff,
  ClusterLeaseProbe, ClusterMembership, ClusterRelay, GuildCountsCache,
  GuildEtsOwner, GuildHealth, GuildManager, HandoffEngine, HttpListener,
  NatsPool, PeriodicGc, PresenceCache, PresenceFanout, PresenceManager,
  PushOutbox, PushPermit, PushServer, Rollout, RolloutChanges, RpcServer,
  SessionManager, SessionTransfer, StallMonitor, StartGate, StatusListener,
  VoiceCounts, VoicePermissionSync, Watchdog,
}
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import gleam/list
import gleam/option
import glisten

const common = [
  ApiHttp,
  GuildEtsOwner,
  RpcServer,
  NatsPool,
  RolloutChanges,
  Rollout,
  StartGate,
  VoicePermissionSync,
]

const common_with_permit = [
  ApiHttp,
  GuildEtsOwner,
  PushPermit,
  RpcServer,
  NatsPool,
  RolloutChanges,
  Rollout,
  StartGate,
  VoicePermissionSync,
]

const always_after_cluster = [ClusterRelay, PeriodicGc, Watchdog, StallMonitor]

fn plan(role: node.Role, cluster: Bool) -> List(supervisor.Child) {
  supervisor.plan(
    settings.Settings(..settings.defaults(), role:, cluster_enabled: cluster),
  )
}

pub fn websocket_role_test() {
  assert plan(node.Websocket, False)
    == list.flatten([
      common,
      always_after_cluster,
      [StatusListener, HttpListener],
    ])
}

pub fn websocket_role_in_cluster_has_no_handoff_test() {
  assert plan(node.Websocket, True)
    == list.flatten([
      common,
      [ClusterDiscovery, ClusterMembership],
      always_after_cluster,
      [StatusListener, HttpListener],
    ])
}

pub fn websocket_sessions_role_runs_sockets_and_sessions_only_test() {
  assert plan(node.WebsocketSessions, True)
    == list.flatten([
      common,
      [ClusterDiscovery, ClusterMembership],
      always_after_cluster,
      [
        SessionTransfer,
        SessionManager,
        ClusterHandoff,
        StatusListener,
        HttpListener,
      ],
    ])
  assert node.runs(node.WebsocketSessions, node.Websocket)
  assert node.runs(node.WebsocketSessions, node.Sessions)
  assert !node.runs(node.WebsocketSessions, node.Guilds)
  assert !node.runs(node.WebsocketSessions, node.Presence)
  assert !node.runs(node.Sessions, node.WebsocketSessions)
}

pub fn all_roles_test() {
  assert plan(node.AllRoles, True)
    == list.flatten([
      common_with_permit,
      [ClusterDiscovery, ClusterMembership],
      always_after_cluster,
      [
        PresenceFanout,
        PushOutbox,
        SessionTransfer,
        SessionManager,
        PresenceCache,
        PresenceManager,
        GuildHealth,
        GuildCountsCache,
        GuildManager,
        VoiceCounts,
        CallManager,
        PushServer,
        ClusterHandoff,
        StatusListener,
        HttpListener,
      ],
    ])
}

pub fn calls_role_runs_voice_counts_test() {
  assert plan(node.Calls, False)
    == list.flatten([
      common,
      always_after_cluster,
      [PushOutbox, CallManager, VoiceCounts, StatusListener, HttpListener],
    ])
}

pub fn guilds_role_test() {
  assert plan(node.Guilds, True)
    == list.flatten([
      common_with_permit,
      [ClusterDiscovery, ClusterMembership],
      always_after_cluster,
      [
        PresenceFanout,
        GuildHealth,
        GuildCountsCache,
        GuildManager,
        VoiceCounts,
        ClusterHandoff,
        StatusListener,
        HttpListener,
      ],
    ])
}

pub fn push_and_sessions_roles_test() {
  assert plan(node.Push, False)
    == list.flatten([
      common_with_permit,
      always_after_cluster,
      [PushOutbox, PushServer, StatusListener, HttpListener],
    ])
  assert plan(node.Sessions, False)
    == list.flatten([
      common,
      always_after_cluster,
      [SessionTransfer, SessionManager, StatusListener, HttpListener],
    ])
  assert plan(node.Presence, False)
    == list.flatten([
      common,
      always_after_cluster,
      [
        PresenceFanout,
        PresenceCache,
        PresenceManager,
        StatusListener,
        HttpListener,
      ],
    ])
}

pub fn routes_test() {
  assert http.route("/_health") == http.HealthRoute
  assert http.route("/_health/undrain") == http.HealthRoute
  assert http.route("/_metrics") == http.MetricsRoute
  assert http.route("/") == http.SocketRoute
  assert http.route("/other") == http.NotFound
}

pub fn health_modes_test() {
  assert health.mode("/_health") == health.Liveness
  assert health.mode("/_health/ready") == health.Readiness
  assert health.mode("/_health/drain") == health.Drain
  assert health.mode("/_health/drained") == health.Drained
  assert health.mode("/_health/undrain") == health.Undrain
}

pub fn readiness_status_test() {
  let accepted = wire.Accepted
  assert health.readiness_status(False, True, accepted, True)
    == #(503, "DRAINING")
  assert health.readiness_status(True, False, accepted, True)
    == #(503, "JOINING")
  assert health.readiness_status(True, True, wire.Pending, True)
    == #(503, "JOINING")
  assert health.readiness_status(True, True, accepted, False)
    == #(503, "NATS_UNAVAILABLE")
  assert health.readiness_status(True, True, accepted, True) == #(200, "OK")
}

pub fn readiness_names_an_epoch_mismatch_with_both_epochs_test() {
  let mismatch = wire.Mismatch(local: 7, peer: 5)
  let expected = #(503, "EPOCH_MISMATCH local=7 peer=5")
  assert health.readiness_status(True, True, mismatch, True) == expected
  assert health.readiness_status(True, False, mismatch, False) == expected
  assert health.readiness_status(False, True, mismatch, True)
    == #(503, "DRAINING")
}

pub fn loopback_test() {
  assert health.is_loopback(glisten.IpV4(127, 0, 0, 1))
  assert health.is_loopback(glisten.IpV6(0, 0, 0, 0, 0, 0, 0, 1))
  assert !health.is_loopback(glisten.IpV4(10, 0, 0, 1))
}

pub fn bouncer_hello_advertises_every_namespace_the_role_holds_test() {
  let with = fn(role, pool) {
    supervisor.codecs(
      settings.Settings(
        ..settings.defaults(),
        role:,
        bouncer: option.Some(settings.BouncerSettings(
          dns_name: "bouncers",
          basename: "gateway-bouncer",
          count: 3,
          pool:,
          template_hash: "",
        )),
      ),
    )
    |> list.map(fn(range) { range.ns })
  }
  assert with(node.AllRoles, "default")
    == [key.guild, key.session, key.presence, key.call, key.push]
  assert with(node.Websocket, "default") == []
  assert with(node.WebsocketSessions, "default") == [key.session]
  assert with(node.Guilds, "large") == [key.guild, key.first_pinned]
  assert codec_range.can_move(
      [codec_range.unmovable(key.session)],
      [codec_range.unmovable(key.session)],
      key.session,
    )
    == Error(Nil)
}

fn bouncer_plan(role: node.Role) -> List(supervisor.Child) {
  supervisor.plan(
    settings.Settings(
      ..settings.defaults(),
      role:,
      cluster_enabled: True,
      bouncer: option.Some(settings.BouncerSettings(
        dns_name: "bouncers",
        basename: "gateway-bouncer",
        count: 3,
        pool: "default",
        template_hash: "",
      )),
    ),
  )
}

pub fn bouncer_mode_starts_the_move_engine_only_for_movable_roles_test() {
  let bouncer = [ClusterLeaseProbe, ClusterBouncer]
  assert bouncer_plan(node.Guilds)
    == list.flatten([
      common_with_permit,
      bouncer,
      always_after_cluster,
      [
        PresenceFanout,
        GuildHealth,
        GuildCountsCache,
        GuildManager,
        VoiceCounts,
        HandoffEngine,
        StatusListener,
        HttpListener,
      ],
    ])
  assert bouncer_plan(node.Presence)
    == list.flatten([
      common,
      bouncer,
      always_after_cluster,
      [
        PresenceFanout,
        PresenceCache,
        PresenceManager,
        HandoffEngine,
        StatusListener,
        HttpListener,
      ],
    ])
  assert !list.contains(bouncer_plan(node.WebsocketSessions), HandoffEngine)
  assert list.contains(bouncer_plan(node.Calls), HandoffEngine)
  assert list.contains(bouncer_plan(node.Push), HandoffEngine)
}

pub fn call_and_push_buckets_advertise_a_movable_codec_test() {
  let ranges = fn(role) {
    supervisor.codecs(
      settings.Settings(
        ..settings.defaults(),
        role:,
        bouncer: option.Some(settings.BouncerSettings(
          dns_name: "bouncers",
          basename: "gateway-bouncer",
          count: 3,
          pool: "default",
          template_hash: "",
        )),
      ),
    )
  }
  assert codec_range.can_move(ranges(node.Calls), ranges(node.Calls), key.call)
    == Ok(1)
  assert codec_range.can_move(ranges(node.Push), ranges(node.Push), key.push)
    == Ok(1)
}
