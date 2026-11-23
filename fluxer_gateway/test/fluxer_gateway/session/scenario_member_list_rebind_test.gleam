// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/session
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/session/control
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import support/session_fake_guild as fake_guild
import support/session_harness as h

fn lazy_request(guild: Int) -> session.ClientOp {
  let assert Ok(data) =
    json.parse(
      json.to_string(
        json.object([
          #(
            "subscriptions",
            json.object([
              #(
                int.to_string(guild),
                json.object([
                  #("active", json.bool(True)),
                  #("typing", json.bool(True)),
                  #(
                    "member_list_channels",
                    json.object([
                      #(
                        "10",
                        json.preprocessed_array([json.array([0, 99], json.int)]),
                      ),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
      decode.dynamic,
    )
  session.LazyRequest(data)
}

fn await_lazy(fake: fake_guild.FakeGuild) -> Result(Dynamic, Nil) {
  case process.receive(fake.seen, 2000) {
    Ok(guild.SessionOp(_, session.LazyRequest(data))) -> Ok(data)
    Ok(_) -> await_lazy(fake)
    Error(Nil) -> Error(Nil)
  }
}

fn at_guild(data: Dynamic, guild: Int) -> dict.Dict(String, Dynamic) {
  decode.run(
    data,
    decode.at(
      ["subscriptions", int.to_string(guild)],
      decode.dict(decode.string, decode.dynamic),
    ),
  )
  |> result.unwrap(dict.new())
}

fn ranges(data: Dynamic, guild: Int) -> Result(List(List(Int)), Nil) {
  at_guild(data, guild)
  |> dict.get("member_list_channels")
  |> result.try(fn(found) {
    decode.run(found, decode.at(["10"], decode.list(decode.list(decode.int))))
    |> result.replace_error(Nil)
  })
}

pub fn a_guild_rebind_replays_member_list_ranges_to_the_new_owner_test() {
  let guild_id = h.unique_user()
  let first = fake_guild.start(guild_id, fake_guild.Accept)
  use <- h.with_api(
    h.session_data(h.unique_user(), False, [guild_id]) |> h.session_api,
  )
  let #(ref, _, _) = h.ready_session("tok-member-list-rebind")
  session.send(ref, session.SocketOp(lazy_request(guild_id)))
  let assert Ok(_) = await_lazy(first)
  let second = fake_guild.start(guild_id, fake_guild.Accept)
  let assert Ok(target) = registry.lookup(h.guild_id(guild_id))
  control.send(ref, control.GuildRebind(target.ref, first.pid))
  let assert Ok(replayed) = await_lazy(second)
  assert ranges(replayed, guild_id) == Ok([[0, 99]])
  assert at_guild(replayed, guild_id) |> dict.keys |> list.sort(string.compare)
    == ["member_list_channels"]
  assert fake_guild.joins(second, 200) == Error(Nil)
  h.terminate(ref)
  fake_guild.crash(first)
  fake_guild.crash(second)
}

pub fn a_guild_rebind_without_member_lists_replays_nothing_test() {
  let guild_id = h.unique_user()
  let first = fake_guild.start(guild_id, fake_guild.Accept)
  use <- h.with_api(
    h.session_data(h.unique_user(), False, [guild_id]) |> h.session_api,
  )
  let #(ref, _, _) = h.ready_session("tok-member-list-rebind-none")
  let second = fake_guild.start(guild_id, fake_guild.Accept)
  let assert Ok(target) = registry.lookup(h.guild_id(guild_id))
  control.send(ref, control.GuildRebind(target.ref, first.pid))
  assert await_lazy(second) == Error(Nil)
  h.terminate(ref)
  fake_guild.crash(first)
  fake_guild.crash(second)
}
