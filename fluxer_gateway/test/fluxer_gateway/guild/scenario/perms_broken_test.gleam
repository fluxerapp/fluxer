// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/node
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/json
import gleam/option.{Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/wait

@external(erlang, "sys", "get_state")
fn sys_get_state(pid: Pid) -> Dynamic

@external(erlang, "erlang", "element")
fn server_state(index: Int, server: Dynamic) -> GuildState

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn message(channel: Int, message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn join_through_shard(found: fixture.Guild, session: fake.FakeSession) -> Bool {
  let reply = process.new_subject()
  let assert Ok(Nil) =
    manager.join(
      node.self(),
      fixture.guild_id(found),
      fake.request(session, guild.Focused, guild.AsMember),
      reply,
    )
  let assert Ok(answer) = process.receive(reply, 5000)
  fake.is_joined(answer)
}

fn break_perms(pid: Pid) -> Nil {
  sys_get_state(pid)
  |> server_state(2, _)
  |> guild_state.perms_handle
  |> perms.release
}

pub fn guild_with_a_dead_perm_handle_restarts_instead_of_going_silent_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  break_perms(handle.pid)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
  let _ = fake.drain(member, 20)
  assert join_through_shard(found, member)
  let assert Ok(again) = registry.lookup(fixture.guild_id(found))
  assert again.pid != handle.pid
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 3))
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  fixture.stop(found)
}
