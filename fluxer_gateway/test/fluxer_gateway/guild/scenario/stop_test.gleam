// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager/registry
import gleam/erlang/process
import support/guild_api.{type FakeApi}
import support/guild_fixture as fixture
import support/guild_session as fake

const rounds = 20

pub fn stop_returns_only_after_the_guild_is_gone_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
  restart(api, found, fixture.started(api, found), rounds)
}

fn restart(
  api: FakeApi,
  found: fixture.Guild,
  handle: Handle,
  left: Int,
) -> Nil {
  let session = fake.start(2)
  assert fake.is_joined(fake.join(session, handle))
  fixture.stop(found)
  assert !process.is_alive(handle.pid)
  assert registry.lookup(fixture.guild_id(found)) == Error(Nil)
  case left > 1 {
    True -> {
      let next = fixture.started(api, found)
      assert next.pid != handle.pid
      restart(api, found, next, left - 1)
    }
    False -> Nil
  }
}
