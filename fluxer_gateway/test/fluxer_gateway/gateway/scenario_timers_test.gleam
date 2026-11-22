// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, Durations, GatewayDurations, SessionDurations,
}
import gleam/option.{Some}
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/isolation

fn short_heartbeat(durations: Durations) -> Durations {
  Durations(
    ..durations,
    gateway: GatewayDurations(
      ..durations.gateway,
      limits_heartbeat_interval_ms: 300,
      limits_heartbeat_timeout_ms: 200,
    ),
  )
}

pub fn silent_client_is_closed_with_session_timeout_test_() {
  use <- h.within(30.0)
  isolation.set_durations(short_heartbeat)
  use scenario <- h.run(h.sessions_for("1000000000000141"))
  let client = h.hello(scenario.gateway.port)
  let #(code, _) = ws.expect_close(client, 5000)
  assert code == Some(4009)
}

pub fn durations_are_back_to_defaults_after_a_test_test() {
  assert settings.get().durations == settings.default_durations()
}

pub fn set_durations_changes_only_the_named_field_test() {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      session: SessionDurations(..durations.session, server_offline_grace_ms: 1),
    )
  })
  let changed = settings.get().durations
  assert changed.session.server_offline_grace_ms == 1
  assert changed.gateway == settings.default_durations().gateway
}
