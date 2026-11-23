// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/guild/handoff/freeze
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/remains
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/registry as presence_registry
import fluxer_gateway/threads/metrics as thread_metrics
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/guild_voice as voice
import support/voice_ids
import support/wait

const voice_channel = 40

fn voice_guild() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_channel(voice_channel, 2, None, [])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
}

fn counted(conn: String) -> Bool {
  list.contains(voice_ids.counted(), conn)
}

fn guild_interest(guild: Int, user: Int) -> Bool {
  presence_registry.local_subscribers(user)
  |> list.any(fn(subscriber) {
    case subscriber {
      protocol.GuildSubscriber(guild: found, ..) -> found == guild
      protocol.PresenceSubscriber(..) -> False
    }
  })
}

pub fn killed_guild_disconnects_its_voice_users_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let other = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(other, handle))
  voice.connect(api, handle, member, voice_channel, "conn-kill-2")
  voice.connect(api, handle, other, voice_channel, "conn-kill-3")
  assert wait.until(2000, fn() {
    counted("conn-kill-2") && counted("conn-kill-3")
  })
  assert !voice.forced(api, 2)
  process.kill(handle.pid)
  assert wait.until(3000, fn() {
    voice.forced_connection(api, "conn-kill-2")
    && voice.forced_connection(api, "conn-kill-3")
  })
  assert wait.until(1000, fn() {
    !counted("conn-kill-2") && !counted("conn-kill-3")
  })
  fixture.stop(found)
}

pub fn crashed_guild_releases_presence_interest_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  assert wait.until(2000, fn() { guild_interest(found.id, 2) })
  process.send_abnormal_exit(handle.pid, "injected")
  assert wait.until(2000, fn() { !guild_interest(found.id, 2) })
  fixture.stop(found)
}

pub fn crashed_guild_voice_users_are_disconnected_once_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-crash-2")
  process.send_abnormal_exit(handle.pid, "injected")
  assert wait.until(3000, fn() { voice.forced_connection(api, "conn-crash-2") })
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  assert fake.is_joined(fake.join(member, again))
  fixture.sync(again)
  process.sleep(200)
  let forced =
    guild_api.calls(api)
    |> list.filter(fn(call) {
      call.kind == "voice_force_disconnect_participant"
    })
  assert list.length(forced) == 1
  assert !voice.in_voice(again, 2)
  fixture.stop(found)
}

pub fn stopped_guild_leaves_voice_users_alone_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-stop-2")
  fixture.stop(found)
  assert wait.until(2000, fn() { !process.is_alive(handle.pid) })
  process.sleep(300)
  assert !voice.forced(api, 2)
  assert !counted("conn-stop-2")
}

pub fn left_voice_user_is_not_disconnected_after_a_crash_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let other = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(other, handle))
  voice.connect(api, handle, member, voice_channel, "conn-left-2")
  voice.connect(api, handle, other, voice_channel, "conn-left-3")
  fixture.dispatch(handle, "GUILD_MEMBER_REMOVE", removal(found, 2))
  fixture.sync(handle)
  assert wait.until(2000, fn() { voice.forced(api, 2) })
  let before = forced_count(api, "conn-left-2")
  process.kill(handle.pid)
  assert wait.until(3000, fn() { voice.forced_connection(api, "conn-left-3") })
  process.sleep(200)
  assert forced_count(api, "conn-left-2") == before
  fixture.stop(found)
}

fn forced_count(api: guild_api.FakeApi, conn: String) -> Int {
  guild_api.calls(api)
  |> list.filter(fn(call) {
    call.kind == "voice_force_disconnect_participant"
    && decode.run(call.body, decode.at(["connection_id"], decode.string))
    == Ok(conn)
  })
  |> list.length
}

fn removal(found: fixture.Guild, user: Int) -> json.Json {
  json.object([
    #("guild_id", json.string(int.to_string(found.id))),
    #("user", fixture.user(user)),
  ])
}

fn thread_json(guild: Int, thread_id: Int, parent: Int) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(thread_id))),
    #("guild_id", json.string(int.to_string(guild))),
    #("parent_id", json.string(int.to_string(parent))),
    #("type", json.int(11)),
    #("name", json.string("t" <> int.to_string(thread_id))),
    #("owner_id", json.string("1")),
    #("last_message_id", json.null()),
    #(
      "thread_metadata",
      json.object([
        #("archived", json.bool(False)),
        #("locked", json.bool(False)),
        #("auto_archive_duration", json.int(1440)),
      ]),
    ),
  ])
}

pub fn killed_guild_gives_back_its_held_threads_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Data(
      json.object([
        #(
          "threads",
          json.preprocessed_array([
            thread_json(found.id, 100, 10),
            thread_json(found.id, 101, 10),
          ]),
        ),
        #("thread_members", json.preprocessed_array([])),
      ]),
    ),
  )
  let before = thread_metrics.held()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  assert wait.until(2000, fn() { thread_metrics.held() == before + 2 })
  process.kill(handle.pid)
  assert wait.until(2000, fn() { thread_metrics.held() == before })
  let again = fixture.started(api, found)
  assert wait.until(2000, fn() { thread_metrics.held() == before + 2 })
  fixture.stop(found)
  assert wait.until(2000, fn() { !process.is_alive(again.pid) })
  assert wait.until(2000, fn() { thread_metrics.held() == before })
}

fn freeze_with_idle_owner(handle: inbox.Handle) -> process.Pid {
  let owner = process.spawn_unlinked(fn() { process.sleep_forever() })
  let reply = process.new_subject()
  process.send(handle.control, inbox.Freeze([owner], reply))
  let assert Ok(Ok(_)) = process.receive(reply, 2000)
  owner
}

pub fn frozen_guild_that_dies_keeps_voice_until_the_handoff_fails_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-frozen-2")
  assert wait.until(2000, fn() { counted("conn-frozen-2") })
  let owner = freeze_with_idle_owner(handle)
  process.kill(handle.pid)
  assert wait.until(2000, fn() { !process.is_alive(handle.pid) })
  process.sleep(300)
  assert !voice.forced(api, 2)
  remains.release(fixture.guild_id(found), handle.pid)
  assert wait.until(3000, fn() { voice.forced_connection(api, "conn-frozen-2") })
  assert wait.until(1000, fn() { !counted("conn-frozen-2") })
  remains.release(fixture.guild_id(found), handle.pid)
  process.sleep(200)
  assert forced_count(api, "conn-frozen-2") == 1
  process.kill(owner)
  fixture.stop(found)
}

pub fn frozen_guild_that_dies_after_the_target_took_over_leaves_voice_alone_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-adopted-2")
  assert wait.until(2000, fn() { counted("conn-adopted-2") })
  let owner = freeze_with_idle_owner(handle)
  process.kill(handle.pid)
  assert wait.until(2000, fn() { !process.is_alive(handle.pid) })
  remains.forget(fixture.guild_id(found), handle.pid)
  remains.release(fixture.guild_id(found), handle.pid)
  process.sleep(300)
  assert !voice.forced(api, 2)
  process.kill(owner)
  fixture.stop(found)
}

pub fn failed_move_thaws_and_a_later_crash_still_disconnects_voice_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-move-2")
  assert wait.until(2000, fn() { counted("conn-move-2") })
  let nowhere = node.from_atom(atom.create("nowhere@127.0.0.1"))
  assert freeze.move(fixture.guild_id(found), nowhere)
    == Error(freeze.TargetUnreachable)
  assert voice.in_voice(handle, 2)
  assert !voice.forced(api, 2)
  process.kill(handle.pid)
  assert wait.until(3000, fn() { voice.forced_connection(api, "conn-move-2") })
  fixture.stop(found)
}
