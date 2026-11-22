// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager
import fluxer_gateway/rpc/server as rpc_server
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture.{type Overwrite, RoleAllow, RoleDeny} as fixture
import support/guild_session as fake
import support/wait

const role = 50

const text_channel = 10

const secret_channel = 20

fn int_text(value: Int) -> String {
  int.to_string(value)
}

fn guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(text_channel)
  |> fixture.with_channel(secret_channel, 0, None, guarded(found))
  |> fixture.with_member(2, [role])
  |> fixture.with_member(3, [])
}

fn guarded(found: fixture.Guild) -> List(Overwrite) {
  [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ]
}

fn channel_update(found: fixture.Guild, overwrites: List(Overwrite)) -> Json {
  fixture.channel(found.id, secret_channel, 0, None, overwrites)
}

fn role_update(found: fixture.Guild, permissions: Int) -> Json {
  json.object([
    #("guild_id", json.string(int_text(found.id))),
    #("role", fixture.role(role, permissions, 1)),
  ])
}

fn member_update(found: fixture.Guild, user: Int, roles: List(Int)) -> Json {
  json.object([
    #("guild_id", json.string(int_text(found.id))),
    #("user", fixture.user(user)),
    #("nick", json.null()),
    #("roles", json.array(roles, fn(found) { json.string(int_text(found)) })),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

fn message(channel: Int, message_id: Int) -> Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn noise(found: fixture.Guild, step: Int) -> #(String, Json) {
  case step % 6 {
    0 -> #("CHANNEL_UPDATE", channel_update(found, []))
    1 -> #(
      "CHANNEL_UPDATE",
      channel_update(found, [RoleDeny(found.id, fixture.view_channel)]),
    )
    2 -> #("GUILD_ROLE_UPDATE", role_update(found, fixture.administrator))
    3 -> #("GUILD_MEMBER_UPDATE", member_update(found, 3, [role]))
    4 -> #("GUILD_ROLE_UPDATE", role_update(found, 0))
    _ -> #("GUILD_MEMBER_UPDATE", member_update(found, 3, []))
  }
}

fn storm(handle: inbox.Handle, found: fixture.Guild, writers: Int, steps: Int) {
  let done = process.new_subject()
  list.each(upto(writers), fn(writer) {
    process.spawn_unlinked(fn() {
      list.each(upto(steps), fn(step) {
        let #(event, payload) = noise(found, step * writer + writer)
        case step % 4 {
          0 ->
            process.send(
              handle.control,
              inbox.DispatchMany([
                #(event, fixture.to_dynamic(payload)),
                #(
                  "MESSAGE_CREATE",
                  fixture.to_dynamic(message(secret_channel, 900_000 + step)),
                ),
              ]),
            )
          _ -> fixture.dispatch(handle, event, payload)
        }
      })
      process.send(done, Nil)
    })
  })
  list.each(upto(writers), fn(_) {
    let assert Ok(Nil) = process.receive(done, 10_000)
  })
}

fn settle(handle: inbox.Handle, found: fixture.Guild) -> Nil {
  process.send(
    handle.control,
    inbox.DispatchMany([
      #(
        "CHANNEL_UPDATE",
        fixture.to_dynamic(channel_update(found, guarded(found))),
      ),
      #("GUILD_ROLE_UPDATE", fixture.to_dynamic(role_update(found, 0))),
      #("GUILD_MEMBER_UPDATE", fixture.to_dynamic(member_update(found, 3, []))),
    ]),
  )
  fixture.sync(handle)
}

fn rpc(method: String, fields: List(#(String, Json))) -> Dynamic {
  let body =
    rpc_server.execute(method, <<json.to_string(json.object(fields)):utf8>>)
  let assert Ok(value) = id_decode.decode_json(<<body:utf8>>)
  value
}

fn permissions(
  found: fixture.Guild,
  user: Int,
  channel: Int,
) -> Result(Int, Nil) {
  let reply =
    rpc("guild.get_user_permissions", [
      #("guild_id", json.string(int_text(found.id))),
      #("user_id", json.string(int_text(user))),
      #("channel_id", json.string(int_text(channel))),
    ])
  case decode.run(reply, decode.at(["result", "permissions"], decode.string)) {
    Ok(text) -> int.parse(text)
    Error(_) -> Error(Nil)
  }
}

fn can_view(found: fixture.Guild, user: Int, channel: Int) -> Bool {
  case permissions(found, user, channel) {
    Ok(bits) ->
      int.bitwise_and(bits, fixture.view_channel) == fixture.view_channel
    Error(Nil) -> False
  }
}

fn got_message(found: fake.FakeSession, message_id: Int) -> Bool {
  fake.dispatches(found, 200)
  |> list.any(fn(pair) {
    pair.0 == "MESSAGE_CREATE"
    && fake.text_at(pair.1, ["id"]) == Some(int_text(message_id))
  })
}

pub fn edit_storm_settles_on_the_last_write_test() {
  use api <- guild_api.with_api
  let found = guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let outsider = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(outsider, handle))
  storm(handle, found, 4, 40)
  settle(handle, found)
  let _ = fake.drain(member, 100)
  let _ = fake.drain(outsider, 0)
  assert wait.until(1000, fn() {
    can_view(found, 2, secret_channel) && !can_view(found, 3, secret_channel)
  })
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_channel, 777))
  fixture.sync(handle)
  assert got_message(member, 777)
  assert !got_message(outsider, 777)
  fake.leave(outsider, handle)
  fixture.sync(handle)
  let rejoin = fake.join(outsider, handle)
  assert !list.contains(fake.channel_ids(rejoin), int_text(secret_channel))
  assert list.contains(fake.channel_ids(rejoin), int_text(text_channel))
  fixture.stop(found)
}

pub fn deleted_channel_is_not_revived_by_late_updates_test() {
  use api <- guild_api.with_api
  let found = guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let delete = fixture.channel(found.id, secret_channel, 0, None, [])
  process.send(
    handle.control,
    inbox.DispatchMany([
      #("CHANNEL_UPDATE", fixture.to_dynamic(channel_update(found, []))),
      #("CHANNEL_DELETE", fixture.to_dynamic(delete)),
      #("CHANNEL_UPDATE", fixture.to_dynamic(channel_update(found, []))),
      #(
        "CHANNEL_UPDATE_BULK",
        fixture.to_dynamic(
          json.object([
            #("channels", json.preprocessed_array([channel_update(found, [])])),
          ]),
        ),
      ),
    ]),
  )
  fixture.sync(handle)
  let _ = fake.drain(member, 100)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_channel, 501))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(text_channel, 502))
  fixture.sync(handle)
  let seen = fake.dispatches(member, 200)
  let ids =
    list.filter_map(seen, fn(pair) {
      case pair.0 {
        "MESSAGE_CREATE" -> option.to_result(fake.text_at(pair.1, ["id"]), Nil)
        _ -> Error(Nil)
      }
    })
  assert ids == ["502"]
  fake.leave(member, handle)
  fixture.sync(handle)
  let rejoin = fake.join(member, handle)
  assert !list.contains(fake.channel_ids(rejoin), int_text(secret_channel))
  fixture.stop(found)
}

pub fn readers_never_fail_while_permissions_churn_test() {
  use api <- guild_api.with_api
  let found = guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let results = process.new_subject()
  list.each([1, 2, 3], fn(_) {
    process.spawn_unlinked(fn() {
      let failures =
        upto(60)
        |> list.filter(fn(_) {
          case permissions(found, 2, secret_channel) {
            Ok(_) -> False
            Error(Nil) -> True
          }
        })
        |> list.length
      process.send(results, failures)
    })
  })
  storm(handle, found, 2, 60)
  list.each([1, 2, 3], fn(_) {
    let assert Ok(failures) = process.receive(results, 20_000)
    assert failures == 0
  })
  fixture.stop(found)
}

fn subscribe_list(found: fake.FakeSession, handle, channel: Int) -> Nil {
  fake.op(
    found,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(
        json.object([
          #(
            "member_list_channels",
            json.object([
              #(
                int_text(channel),
                json.preprocessed_array([json.array([0, 99], json.int)]),
              ),
            ]),
          ),
        ]),
      ),
    ),
  )
}

fn member_count(update: Dynamic) -> Result(Int, Nil) {
  decode.run(update, decode.at(["member_count"], decode.int))
  |> option.from_result
  |> option.to_result(Nil)
}

fn last_count(found: fake.FakeSession) -> Result(Int, Nil) {
  fake.dispatches(found, 200)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "GUILD_MEMBER_LIST_UPDATE" -> member_count(pair.1)
      _ -> Error(Nil)
    }
  })
  |> list.last
}

pub fn member_list_follows_access_flip_flops_test() {
  use api <- guild_api.with_api
  let found = guild()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  subscribe_list(owner, handle, secret_channel)
  let assert Some(first) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert member_count(first) == Ok(2)
  storm(handle, found, 3, 30)
  settle(handle, found)
  let _ = fake.drain(owner, 200)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    member_update(found, 3, [role]),
  )
  fixture.sync(handle)
  assert last_count(owner) == Ok(3)
  fixture.dispatch(handle, "GUILD_MEMBER_UPDATE", member_update(found, 3, []))
  fixture.sync(handle)
  assert last_count(owner) == Ok(2)
  fixture.stop(found)
}

fn upto(count: Int) -> List(Int) {
  int.range(count, 0, [], fn(acc, value) { [value, ..acc] })
}

pub fn readers_never_fail_across_reloads_test() {
  use api <- guild_api.with_api
  let found = guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let results = process.new_subject()
  let deadline = clock.monotonic_ms() + 1500
  list.each([1, 2, 3], fn(_) {
    process.spawn_unlinked(fn() {
      process.send(results, read_until(found, deadline, 0, 0))
    })
  })
  list.each(upto(8), fn(round) {
    let overwrites = case round % 2 {
      0 -> guarded(found)
      _ -> []
    }
    fixture.publish(
      api,
      fixture.Guild(..found, channels: [
        fixture.channel(found.id, text_channel, 0, None, []),
        fixture.channel(found.id, secret_channel, 0, None, overwrites),
      ]),
    )
    assert manager.reload_local([found.id], 5000) == 1
  })
  list.each([1, 2, 3], fn(_) {
    let assert Ok(#(reads, failures)) = process.receive(results, 10_000)
    assert reads > 0
    assert failures == 0
  })
  assert process.is_alive(handle.pid)
  fixture.stop(found)
}

fn read_until(
  found: fixture.Guild,
  deadline: Int,
  reads: Int,
  failures: Int,
) -> #(Int, Int) {
  case clock.monotonic_ms() > deadline {
    True -> #(reads, failures)
    False -> {
      let failed = case
        permissions(found, 2, secret_channel),
        viewable(found, 2)
      {
        Ok(_), Ok(_) -> 0
        _, _ -> 1
      }
      read_until(found, deadline, reads + 1, failures + failed)
    }
  }
}

fn viewable(found: fixture.Guild, user: Int) -> Result(Nil, Nil) {
  let reply =
    rpc("guild.get_viewable_channels", [
      #("guild_id", json.string(int_text(found.id))),
      #("user_id", json.string(int_text(user))),
    ])
  case decode.run(reply, decode.at(["ok"], decode.bool)) {
    Ok(True) -> Ok(Nil)
    _ -> Error(Nil)
  }
}
