// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, GuildDurations}
import fluxer_gateway/core/guild
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/isolation

@external(erlang, "sys", "suspend")
fn suspend(pid: Pid) -> Atom

@external(erlang, "sys", "resume")
fn resume(pid: Pid) -> Atom

fn empty_grace(empty_ms: Int) -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      guild: GuildDurations(..d.guild, server_auto_stop_grace_ms: empty_ms),
    )
  })
}

pub fn a_join_queued_behind_the_auto_stop_is_served_test() {
  empty_grace(50)
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let first = fake.start(1)
  assert fake.is_joined(fake.join(first, handle))
  fake.leave(first, handle)
  fixture.sync(handle)
  let _ = suspend(handle.pid)
  process.sleep(200)
  let second = fake.start(1)
  let reply = process.new_subject()
  guild.send(
    handle.ref,
    guild.Join(fake.request(second, guild.Focused, guild.AsMember), reply),
  )
  let _ = resume(handle.pid)
  let assert Ok(answer) = process.receive(reply, 3000)
  assert fake.is_joined(answer)
  assert process.is_alive(handle.pid)
  fixture.stop(found)
}
