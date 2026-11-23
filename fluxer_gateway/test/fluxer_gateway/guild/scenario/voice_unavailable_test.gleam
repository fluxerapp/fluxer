// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{AsMember, Focused, JoinRequest}
import fluxer_gateway/guild/manager
import gleam/json.{type Json}
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/guild_voice as voice
import support/wait

const voice_channel = 40

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn voice_guild() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_channel(voice_channel, 2, None, [])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
}

fn features(found: fixture.Guild, names: List(String)) -> Json {
  json.object([
    #("id", json.string(int_text(found.id))),
    #("features", json.array(names, json.string)),
  ])
}

fn staff(found: fake.FakeSession) -> guild.JoinRequest {
  JoinRequest(..fake.request(found, Focused, AsMember), staff: True)
}

pub fn unavailable_guild_disconnects_its_voice_users_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-member")
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    features(found, ["UNAVAILABLE_FOR_EVERYONE"]),
  )
  fixture.sync(handle)
  assert !voice.in_voice(handle, 2)
  assert wait.until(3000, fn() { voice.forced(api, 2) })
  fixture.stop(found)
}

pub fn staff_keep_voice_when_only_staff_may_see_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let employee = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join_with(employee, handle, staff(employee)))
  voice.connect(api, handle, member, voice_channel, "conn-member")
  voice.connect(api, handle, employee, voice_channel, "conn-staff")
  let _ = fake.drain(employee, 50)
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    features(found, ["UNAVAILABLE_FOR_EVERYONE_BUT_STAFF"]),
  )
  fixture.sync(handle)
  assert !voice.in_voice(handle, 2)
  assert voice.in_voice(handle, 3)
  assert wait.until(3000, fn() { voice.forced(api, 2) })
  assert wait.until(3000, fn() { voice.forced_connection(api, "conn-member") })
  assert !voice.forced(api, 3)
  let assert Some(left) = fake.await_event(employee, "VOICE_STATE_UPDATE", 1000)
  assert fake.text_at(left, ["user_id"]) == Some("2")
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    features(found, ["UNAVAILABLE_FOR_EVERYONE"]),
  )
  fixture.sync(handle)
  assert !voice.in_voice(handle, 3)
  assert wait.until(3000, fn() { voice.forced(api, 3) })
  fixture.stop(found)
}

pub fn reload_into_unavailable_disconnects_voice_users_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-member")
  fixture.publish(
    api,
    fixture.with_property(
      found,
      "features",
      json.array(["UNAVAILABLE_FOR_EVERYONE"], json.string),
    ),
  )
  assert manager.reload_local([found.id], 5000) == 1
  assert !voice.in_voice(handle, 2)
  assert wait.until(3000, fn() { voice.forced(api, 2) })
  fixture.stop(found)
}
