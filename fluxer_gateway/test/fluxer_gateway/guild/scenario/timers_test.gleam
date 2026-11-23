// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/scenario/passive_test
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/option.{Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

pub fn empty_guilds_stop_and_passive_sessions_catch_up_test_() {
  use <- fixture.slow(60)
  use api <- guild_api.with_api
  let left = fixture.new(1) |> fixture.with_text(10)
  let returned = fixture.new(1) |> fixture.with_text(10)
  let crashed = fixture.new(1) |> fixture.with_text(10)
  let busy = passive_test.big_guild()
  let left_handle = fixture.started(api, left)
  let returned_handle = fixture.started(api, returned)
  let crashed_handle = fixture.started(api, crashed)
  let busy_handle = fixture.started(api, busy)
  let first = fake.start(1)
  let second = fake.start(1)
  let third = fake.start(1)
  let watcher = fake.start(2)
  assert fake.is_joined(fake.join(first, left_handle))
  assert fake.is_joined(fake.join(second, returned_handle))
  assert fake.is_joined(fake.join(third, crashed_handle))
  assert fake.is_joined(passive_test.passive_join(watcher, busy_handle))
  fake.leave(first, left_handle)
  fake.leave(second, returned_handle)
  fake.stop(third)
  let _ = fake.drain(watcher, 50)
  fixture.dispatch(
    busy_handle,
    "MESSAGE_CREATE",
    passive_test.message(10, 77, []),
  )
  fixture.dispatch(
    busy_handle,
    "MESSAGE_CREATE",
    passive_test.message(20, 78, []),
  )
  process.sleep(20_000)
  let again = fake.start(1)
  assert fake.is_joined(fake.join(again, returned_handle))
  let assert Some(update) = fake.await_event(watcher, "PASSIVE_UPDATES", 12_000)
  assert decode.run(
      update,
      decode.at(["channels"], decode.dict(decode.string, decode.string)),
    )
    |> option.from_result
    |> option.map(dict.to_list)
    == Some([#("10", "77")])
  process.sleep(12_000)
  assert !process.is_alive(left_handle.pid)
  assert !process.is_alive(crashed_handle.pid)
  assert process.is_alive(returned_handle.pid)
  assert process.is_alive(busy_handle.pid)
  assert registry.lookup(fixture.guild_id(left)) == Error(Nil)
  fake.leave(again, returned_handle)
  fixture.stop(returned)
  fixture.stop(busy)
}
