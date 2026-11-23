// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/guild
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/push/throttle
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/result
import support/guild_api.{type FakeApi}
import support/guild_fixture as fixture
import support/guild_session as fake
import support/push_throttle
import support/wait

const author = 1

const channel = 10

const recheck_ms = 1500

fn with_short_grace(body: fn() -> Nil) -> Nil {
  let previous = settings.get()
  let _ =
    settings.store(
      settings.Settings(
        ..previous,
        durations: settings.Durations(
          ..previous.durations,
          guild: settings.GuildDurations(
            ..previous.durations.guild,
            grace_recheck_ms: recheck_ms,
            grace_hold_call_timeout_ms: 1500,
          ),
        ),
      ),
    )
  body()
  let _ = settings.store(previous)
  Nil
}

fn unique_user() -> Int {
  100_000_000_000_000_000 + int.random(800_000_000_000_000_000)
}

fn mention(user: Int, message_id: Int) -> Json {
  let text = int.to_string(user)
  json.object([
    #("id", json.string(int.to_string(message_id))),
    #("channel_id", json.string(int.to_string(channel))),
    #("content", json.string("hey <@" <> text <> ">")),
    #("author", fixture.user(author)),
    #("mentions", json.preprocessed_array([fixture.user(user)])),
  ])
}

fn pushed_to(api: FakeApi, user: Int) -> Bool {
  let text = int.to_string(user)
  guild_api.calls(api)
  |> list.any(fn(call) {
    decode.run(call.body, decode.at(["user_ids"], decode.list(decode.string)))
    |> result.unwrap([])
    |> list.contains(text)
  })
}

fn hold(found: fake.FakeSession, handle: Handle, engaged: Bool) -> Nil {
  guild.send(handle.ref, guild.SessionPushHold(found.ref.id, engaged))
}

fn twin(of: fake.FakeSession) -> fake.FakeSession {
  let found = fake.start(1)
  fake.FakeSession(
    ..found,
    ref: session.SessionRef(..found.ref, id: of.ref.id, user: of.ref.user),
  )
}

fn calm() -> Nil {
  push_throttle.calm(throttle.UserGuildSettings)
  push_throttle.calm(throttle.BlockedIds)
}

fn engaged_member(
  api: FakeApi,
) -> #(Int, fixture.Guild, Handle, fake.FakeSession) {
  let user = unique_user()
  let found =
    fixture.new(author)
    |> fixture.with_text(channel)
    |> fixture.with_member(user, [])
  let handle = fixture.started(api, found)
  let member = fake.start(user)
  assert fake.is_joined(fake.join(member, handle))
  hold(member, handle, True)
  fixture.sync(handle)
  #(user, found, handle, member)
}

pub fn a_released_hold_is_pushed_after_the_grace_window_test() {
  calm()
  use <- with_short_grace
  use api <- guild_api.with_api_for(20_000)
  let #(user, found, handle, member) = engaged_member(api)
  fixture.dispatch(handle, "MESSAGE_CREATE", mention(user, 900_001))
  fixture.sync(handle)
  process.sleep(recheck_ms / 3)
  assert !pushed_to(api, user)
  hold(member, handle, False)
  fixture.sync(handle)
  assert wait.until(recheck_ms + 3000, fn() { pushed_to(api, user) })
  fake.stop(member)
  fixture.stop(found)
}

pub fn a_hold_released_on_the_rebuilt_guild_is_pushed_test() {
  calm()
  use <- with_short_grace
  use api <- guild_api.with_api_for(20_000)
  let #(user, found, handle, member) = engaged_member(api)
  fixture.dispatch(handle, "MESSAGE_CREATE", mention(user, 900_002))
  fixture.sync(handle)
  fixture.stop(found)
  let rebuilt = fixture.started(api, found)
  assert rebuilt.pid != handle.pid
  assert fake.is_joined(fake.join(member, rebuilt))
  hold(member, rebuilt, False)
  fixture.sync(rebuilt)
  assert !pushed_to(api, user)
  assert wait.until(recheck_ms + 3000, fn() { pushed_to(api, user) })
  fake.stop(member)
  fixture.stop(found)
}

pub fn a_session_moved_during_the_grace_window_still_holds_test() {
  calm()
  use <- with_short_grace
  use api <- guild_api.with_api_for(20_000)
  let #(user, found, handle, member) = engaged_member(api)
  fixture.dispatch(handle, "MESSAGE_CREATE", mention(user, 900_003))
  fixture.sync(handle)
  let target = twin(member)
  guild.send(handle.ref, guild.SessionMoved(target.ref, member.ref))
  fixture.sync(handle)
  fake.stop(member)
  fixture.sync(handle)
  process.sleep(recheck_ms + 1500)
  assert !pushed_to(api, user)
  fake.stop(target)
  fixture.stop(found)
}

pub fn a_session_that_left_during_the_grace_window_is_pushed_test() {
  calm()
  use <- with_short_grace
  use api <- guild_api.with_api_for(20_000)
  let #(user, found, handle, member) = engaged_member(api)
  fixture.dispatch(handle, "MESSAGE_CREATE", mention(user, 900_004))
  fixture.sync(handle)
  fake.stop(member)
  fixture.sync(handle)
  assert wait.until(recheck_ms + 3000, fn() { pushed_to(api, user) })
  fixture.stop(found)
}
