// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, GuildDurations}
import fluxer_gateway/core/id
import fluxer_gateway/voice/guild_state
import gleam/erlang/process
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/guild_voice as voice
import support/isolation
import support/voice_ids
import support/wait

const voice_channel = 40

fn empty_grace(empty_ms: Int) -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      guild: GuildDurations(..d.guild, server_auto_stop_grace_ms: empty_ms),
    )
  })
}

fn voice_guild() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_channel(voice_channel, 2, None, [])
  |> fixture.with_member(2, [])
}

pub fn guild_with_a_voice_user_and_no_sessions_stays_up_test() {
  empty_grace(50)
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-idle-2")
  fake.leave(member, handle)
  fixture.sync(handle)
  process.sleep(300)
  assert process.is_alive(handle.pid)
  assert voice.in_voice(handle, 2)
  assert !voice.forced(api, 2)
  fixture.stop(found)
}

pub fn guild_stops_once_the_last_voice_user_leaves_test() {
  empty_grace(50)
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-idle-left-2")
  fake.leave(member, handle)
  fixture.sync(handle)
  process.sleep(200)
  assert process.is_alive(handle.pid)
  let assert Ok(user) = id.user_id(2)
  let assert Ok(channel) = id.channel_id(voice_channel)
  let assert Ok(_) =
    voice.rpc(
      handle,
      guild_state.DisconnectIfInChannel(
        user,
        channel,
        Some(voice_ids.conn("conn-idle-left-2")),
      ),
    )
  assert wait.until(2000, fn() { !process.is_alive(handle.pid) })
}
