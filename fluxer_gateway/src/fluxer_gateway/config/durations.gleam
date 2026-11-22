// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type ApiDurations, type AppDurations, type BouncerDurations,
  type ClusterDurations, type ConfigDurations, type GatewayDurations,
  type GuildDurations, type NatsDurations, type PresenceDurations,
  type PushDurations, type RpcDurations, type SessionDurations,
  type TelemetryDurations, type ThreadsDurations, type UtilDurations,
  type VoiceDurations,
}

pub fn api() -> ApiDurations {
  settings.get().durations.api
}

pub fn app() -> AppDurations {
  settings.get().durations.app
}

pub fn bouncer() -> BouncerDurations {
  settings.get().durations.bouncer
}

pub fn cluster() -> ClusterDurations {
  settings.get().durations.cluster
}

pub fn config() -> ConfigDurations {
  settings.get().durations.config
}

pub fn gateway() -> GatewayDurations {
  settings.get().durations.gateway
}

pub fn guild() -> GuildDurations {
  settings.get().durations.guild
}

pub fn nats() -> NatsDurations {
  settings.get().durations.nats
}

pub fn presence() -> PresenceDurations {
  settings.get().durations.presence
}

pub fn push() -> PushDurations {
  settings.get().durations.push
}

pub fn rpc() -> RpcDurations {
  settings.get().durations.rpc
}

pub fn session() -> SessionDurations {
  settings.get().durations.session
}

pub fn telemetry() -> TelemetryDurations {
  settings.get().durations.telemetry
}

pub fn threads() -> ThreadsDurations {
  settings.get().durations.threads
}

pub fn util() -> UtilDurations {
  settings.get().durations.util
}

pub fn voice() -> VoiceDurations {
  settings.get().durations.voice
}
