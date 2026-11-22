// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, Durations, SessionDurations,
}
import fluxer_gateway/core/guild
import fluxer_gateway/session/links
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{None}
import gleeunit/should
import support/isolation
import support/session_fake_guild as fake_guild
import support/session_harness.{Frame} as h

fn fast_retries() -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      session: SessionDurations(
        ..d.session,
        server_ready_deadline_ms: 300,
        guilds_first_join_floor_ms: 300,
        guilds_join_grace_ms: 200,
        guilds_after_ready_join_ms: 300,
        links_backoff_base_ms: 150,
        links_backoff_cap_ms: 1000,
      ),
    )
  })
}

fn joins_until_quiet(fake: fake_guild.FakeGuild, seen: List(Int)) -> List(Int) {
  case fake_guild.joins(fake, 1500) {
    Ok(request) -> joins_until_quiet(fake, [request.attempt, ..seen])
    Error(Nil) -> list.reverse(seen)
  }
}

pub fn gone_guild_retries_then_is_deleted_from_the_session_test() {
  fast_retries()
  let guild = h.unique_user()
  let fake =
    fake_guild.start(
      guild,
      fake_guild.Answer(fn(request) { guild.NotMember(request.attempt) }),
    )
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let #(ref, socket, _) = h.ready_session("tok-gone-1")
  let assert Ok(Frame(_, "GUILD_DELETE", deleted)) =
    h.await_event(socket, "GUILD_DELETE", 5000)
  decode.run(deleted, decode.at(["id"], decode.string))
  |> should.equal(Ok(int.to_string(guild)))
  decode.run(
    deleted,
    decode.optional_field("unavailable", False, decode.bool, decode.success),
  )
  |> should.equal(Ok(False))
  links.get(h.state_of(ref).links, h.guild_id(guild)) |> should.equal(None)
  list.length(joins_until_quiet(fake, []))
  |> should.equal(links.not_member_retries + 1)
  h.terminate(ref)
  fake_guild.crash(fake)
}
