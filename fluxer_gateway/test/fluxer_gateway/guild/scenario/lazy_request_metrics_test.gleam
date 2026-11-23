// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{AsMember, Background}
import fluxer_gateway/core/session
import fluxer_gateway/telemetry/metrics
import gleam/json
import gleam/list
import gleam/option
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

fn observed(name: String) -> Int {
  metrics.histograms()
  |> list.filter(fn(series) { series.name == name && series.labels == [] })
  |> list.fold(0, fn(total, series) { total + series.count })
}

pub fn a_lazy_request_is_timed_by_the_guild_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join_with(
    member,
    handle,
    fake.request(member, Background, AsMember),
  ))
  let _ = fake.drain(member, 50)
  let before = observed("fluxer_gateway_guild_lazy_request_ms")
  fake.op(
    member,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(json.object([#("active", json.bool(True))])),
    ),
  )
  let assert option.Some(_) = fake.await_event(member, "GUILD_SYNC", 1000)
  assert observed("fluxer_gateway_guild_lazy_request_ms") - before == 1
}
