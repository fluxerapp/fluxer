// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/query
import fluxer_gateway/guild/read_model
import fluxer_gateway/rpc/server
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import support/guild_api
import support/guild_fixture as fixture
import support/wait

const workers = 8

const cycles = 150

fn member_guild() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
}

pub fn stale_view_never_answers_a_member_as_missing_test() {
  use api <- guild_api.with_api
  let found = member_guild()
  let handle = fixture.started(api, found)
  let guild = fixture.guild_id(found)
  let assert Ok(user) = id.user_id(3)
  let assert Ok(view) = read_model.get(guild)
  assert query.answer(view, inbox.HasMember(user))
    == Ok(json.object([#("has_member", json.bool(True))]))
  fixture.stop(found)
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
  assert result.is_error(query.answer(view, inbox.HasMember(user)))
  assert result.is_error(query.answer(view, inbox.GetMember(user)))
  assert result.is_error(query.answer(view, inbox.ListMembers(10, 0)))
}

pub fn member_rpc_racing_guild_restarts_never_denies_a_member_test() {
  use api <- guild_api.with_api
  let found = member_guild()
  let _ = fixture.started(api, found)
  let replies = process.new_subject()
  let pids =
    list.repeat(Nil, workers)
    |> list.map(fn(_) {
      process.spawn_unlinked(fn() { ask_loop(found, replies) })
    })
  restart_loop(found, cycles)
  list.each(pids, fn(pid) { process.kill(pid) })
  let denied = collect_denied(replies, 0)
  assert denied == 0
  fixture.stop(found)
}

fn ask_loop(found: fixture.Guild, replies: process.Subject(Int)) -> Nil {
  case denied_count(found) {
    0 -> Nil
    count -> process.send(replies, count)
  }
  ask_loop(found, replies)
}

fn denied_count(found: fixture.Guild) -> Int {
  let guild = #("guild_id", json.string(int.to_string(found.id)))
  let has =
    rpc("guild.has_member", [guild, #("user_id", json.string("3"))])
    |> field(["result", "has_member"], decode.bool)
  let listed =
    rpc("guild.list_members", [
      guild,
      #("limit", json.int(10)),
      #("offset", json.int(0)),
    ])
    |> field(["result", "total"], decode.int)
  case has, listed {
    Ok(False), _ -> 1
    _, Ok(0) -> 1
    _, _ -> 0
  }
}

fn rpc(method: String, fields: List(#(String, json.Json))) -> String {
  server.execute(method, <<json.to_string(json.object(fields)):utf8>>)
}

fn field(
  body: String,
  path: List(String),
  decoder: decode.Decoder(a),
) -> Result(a, Nil) {
  use value <- result.try(id_decode.decode_json(<<body:utf8>>))
  decode.run(value, decode.at(path, decoder)) |> result.replace_error(Nil)
}

fn restart_loop(found: fixture.Guild, left: Int) -> Nil {
  case left {
    0 -> Nil
    _ -> {
      case registry.lookup(fixture.guild_id(found)) {
        Ok(handle) ->
          case left % 2 {
            0 -> process.kill(handle.pid)
            _ -> fixture.stop(found)
          }
        Error(Nil) -> Nil
      }
      process.sleep(2)
      restart_loop(found, left - 1)
    }
  }
}

fn collect_denied(replies: process.Subject(Int), total: Int) -> Int {
  case process.receive(replies, 200) {
    Ok(count) -> collect_denied(replies, total + count)
    Error(Nil) -> total
  }
}
