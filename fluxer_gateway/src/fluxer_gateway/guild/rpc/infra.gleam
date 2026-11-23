// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type RpcError, RpcFailed, RpcTimeout}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/health
import fluxer_gateway/guild/inbox.{type Handle, type Query}
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/query
import fluxer_gateway/guild/read_model.{type View}
import fluxer_gateway/guild/remote
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import fluxer_gateway/util/decode as edge
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import logging

fn reload_all_slack_ms() -> Int {
  durations.guild().infra_reload_all_slack_ms
}

fn reload_timeout_ms() -> Int {
  durations.guild().infra_reload_timeout_ms
}

fn start_backoff_jitter_ms() -> Int {
  durations.guild().infra_start_backoff_jitter_ms
}

fn start_backoff_base_ms() -> Int {
  durations.guild().infra_start_backoff_base_ms
}

fn start_backoff_max_ms() -> Int {
  durations.guild().infra_start_backoff_max_ms
}

pub fn guild_call_timeout_ms() -> Int {
  durations.guild().infra_guild_call_timeout_ms
}

fn start_lookup_timeout_ms() -> Int {
  durations.guild().infra_start_lookup_timeout_ms
}

const start_attempts = 3

const answer_attempts = 3

fn reload_all_timeout_ms() -> Int {
  durations.guild().infra_reload_all_timeout_ms
}

type Group {
  Group(owner: node.Node, guilds: List(Int))
}

pub fn handle(guild: GuildId) -> Result(Handle, RpcError) {
  case registry.lookup(guild) {
    Ok(found) -> Ok(found)
    Error(Nil) ->
      manager.start_or_lookup(node.self(), guild, start_lookup_timeout_ms())
      |> result.replace_error(RpcFailed("guild_not_found"))
  }
}

pub fn view(guild: GuildId) -> Result(View, RpcError) {
  case read_model.get(guild) {
    Ok(found) -> Ok(found)
    Error(Nil) -> {
      use _ <- result.try(handle(guild))
      read_model.get(guild)
      |> result.replace_error(RpcFailed("guild_not_found"))
    }
  }
}

pub fn answer(guild: GuildId, asked: Query) -> Result(Json, RpcError) {
  answer_live(guild, asked, answer_attempts)
}

fn answer_live(
  guild: GuildId,
  asked: Query,
  left: Int,
) -> Result(Json, RpcError) {
  use found <- result.try(view(guild))
  let reply = query.answer(found, asked)
  case read_model.is_live(found) || left <= 1 {
    True -> reply |> result.map_error(query_error)
    False -> answer_live(guild, asked, left - 1)
  }
}

pub fn ask(guild: GuildId, asked: Query) -> Result(Json, RpcError) {
  use deadline_ms, reply <- ask_with(guild)
  inbox.Ask(asked, deadline_ms, reply)
}

pub fn ask_thread_bits(
  guild: GuildId,
  user: UserId,
  thread: ChannelId,
) -> Result(Int, RpcError) {
  use deadline_ms, reply <- ask_with(guild)
  inbox.AskThreadBits(user, thread, deadline_ms, reply)
}

fn ask_with(
  guild: GuildId,
  build: fn(Int, process.Subject(Result(a, inbox.QueryError))) -> inbox.Control,
) -> Result(a, RpcError) {
  use found <- result.try(handle(guild))
  case health.is_overloaded(guild) {
    True -> Error(RpcFailed("guild_overloaded"))
    False -> {
      let reply = process.new_subject()
      process.send(
        found.control,
        build(clock.monotonic_ms() + guild_call_timeout_ms(), reply),
      )
      case process.receive(reply, guild_call_timeout_ms()) {
        Ok(answer) -> answer |> result.map_error(query_error)
        Error(Nil) -> Error(RpcTimeout)
      }
    }
  }
}

fn query_error(error: inbox.QueryError) -> RpcError {
  case error {
    inbox.Forbidden -> RpcFailed("forbidden")
    inbox.QueryFailed(code) -> RpcFailed(code)
  }
}

pub fn dispatch(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use event <- result.try(rpc_params.field(params, "event"))
  use data <- result.try(rpc_params.field(params, "data"))
  use event_name <- result.try(
    rpc_params.string(event)
    |> result.replace_error(error.RpcInternalError),
  )
  use payload <- result.try(
    edge.decode_json(raw_json.to_bits(data))
    |> result.replace_error(error.RpcInternalError),
  )
  use found <- result.try(handle(guild))
  process.send(found.control, inbox.Dispatch(event_name, payload))
  Ok(json.bool(True))
}

pub fn dispatch_many(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use events <- result.try(
    rpc_params.field(params, "events")
    |> result.try(fn(value) {
      edge.decode_json(raw_json.to_bits(value))
      |> result.try(edge.events)
      |> result.replace_error(RpcFailed("validation_invalid_params"))
    }),
  )
  use found <- result.try(handle(guild))
  process.send(found.control, inbox.DispatchMany(events))
  Ok(json.bool(True))
}

pub fn start(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  start_with_retry(guild, 1)
}

fn start_with_retry(guild: GuildId, attempt: Int) -> Result(Json, RpcError) {
  case handle(guild) {
    Ok(_) -> Ok(json.bool(True))
    Error(_) if attempt < start_attempts -> {
      let backoff =
        int.min(
          start_backoff_base_ms() * int.bitwise_shift_left(1, attempt - 1),
          start_backoff_max_ms(),
        )
      process.sleep(backoff + int.random(start_backoff_jitter_ms()) + 1)
      start_with_retry(guild, attempt + 1)
    }
    Error(_) -> Error(RpcFailed("guild_start_error"))
  }
}

pub fn stop(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  stop_guild(guild, "guild_stop_error")
}

fn stop_guild(guild: GuildId, failure: String) -> Result(Json, RpcError) {
  case
    manager.ask_shard(
      node.self(),
      guild,
      fn(reply) { shard.StopGuild(guild, reply) },
      guild_call_timeout_ms(),
    )
  {
    Ok(True) -> Ok(json.bool(True))
    _ -> Error(RpcFailed(failure))
  }
}

pub fn shutdown(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  stop_guild(guild, "guild_shutdown_error")
}

pub fn reload(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  case
    manager.ask_shard(
      node.self(),
      guild,
      fn(reply) { shard.ReloadGuild(guild, reply) },
      reload_timeout_ms(),
    )
  {
    Ok(shard.Reloaded) -> Ok(json.bool(True))
    Ok(shard.NotRunning) ->
      case manager.start_or_lookup(node.self(), guild, reload_timeout_ms()) {
        Ok(_) -> Ok(json.bool(True))
        Error(_) -> Error(RpcFailed("guild_reload_error"))
      }
    _ -> Error(RpcFailed("guild_reload_error"))
  }
}

pub fn reload_all(params: Params) -> Result(Json, RpcError) {
  use guilds <- result.try(rpc_params.guild_list(params, "guild_ids"))
  let groups = case guilds {
    [] -> list.map(router.nodes_with(node.Guilds), Group(_, []))
    _ ->
      guilds
      |> list.fold(dict.new(), fn(acc, guild) {
        case owner_node(guild) {
          Some(owner) ->
            dict.upsert(acc, node.to_string(owner), add_to_group(
              _,
              owner,
              guild,
            ))
          None -> acc
        }
      })
      |> dict.values
      |> list.map(fn(group) {
        Group(..group, guilds: list.reverse(group.guilds))
      })
  }
  let replies = process.new_subject()
  list.each(groups, fn(group) {
    process.spawn(fn() { process.send(replies, reload_group(group)) })
  })
  let total =
    collect(
      replies,
      list.length(groups),
      clock.deadline_in(reload_all_timeout_ms()),
      0,
    )
  Ok(json.object([#("count", json.int(total))]))
}

fn owner_node(guild: GuildId) -> Option(node.Node) {
  case router.owner(router.GuildKey(guild), node.Guilds) {
    router.Local -> Some(node.self())
    router.Remote(owner) -> Some(owner)
    router.OwnerUnavailable(_) -> None
  }
}

fn add_to_group(
  existing: Option(Group),
  owner: node.Node,
  guild: GuildId,
) -> Group {
  case existing {
    Some(group) ->
      Group(..group, guilds: [id.guild_id_to_int(guild), ..group.guilds])
    None -> Group(owner:, guilds: [id.guild_id_to_int(guild)])
  }
}

fn reload_group(group: Group) -> Int {
  let Group(owner:, guilds:) = group
  case owner == node.self() {
    True -> manager.reload_local(guilds, reload_all_timeout_ms())
    False ->
      remote.call(
        owner,
        "fluxer_gateway@guild@manager",
        "reload_local",
        [remote.arg(guilds), remote.arg(reload_all_timeout_ms())],
        reload_all_timeout_ms() + reload_all_slack_ms(),
        expecting: decode.int,
      )
      |> result.unwrap(0)
  }
}

fn collect(
  replies: process.Subject(Int),
  left: Int,
  deadline: clock.Deadline,
  total: Int,
) -> Int {
  case left {
    0 -> total
    _ ->
      case process.receive(replies, clock.remaining_ms(deadline)) {
        Ok(count) ->
          collect(replies, left - 1, deadline, total + int.max(count, 0))
        Error(Nil) -> total
      }
  }
}

pub fn forward_timeout_ms() -> Int {
  durations.guild().infra_forward_timeout_ms
}

pub fn forward_payload(
  owner: node.Node,
  method: String,
  payload: BitArray,
) -> Result(String, RpcError) {
  forward_to(owner, "fluxer_gateway@rpc@server", "execute_owned", [
    remote.arg(method),
    remote.arg(payload),
  ])
}

pub fn forward_call(
  owner: node.Node,
  function: String,
  args: List(remote.Arg),
) -> Result(String, RpcError) {
  forward_to(owner, "fluxer_gateway@guild@rpc", function, args)
}

fn forward_to(
  owner: node.Node,
  module: String,
  function: String,
  args: List(remote.Arg),
) -> Result(String, RpcError) {
  let reply =
    remote.call(
      owner,
      module,
      function,
      args,
      forward_timeout_ms(),
      expecting: {
        use ok <- decode.field(0, decode.bool)
        use body <- decode.field(1, decode.string)
        decode.success(#(ok, body))
      },
    )
  case reply {
    Ok(#(True, body)) -> Ok(body)
    Ok(#(False, "invalid_params")) -> Error(error.InvalidParams)
    Ok(#(False, "timeout")) -> Error(RpcTimeout)
    Ok(#(False, "internal_error")) -> Error(error.RpcInternalError)
    Ok(#(False, message)) -> Error(RpcFailed(message))
    Error(reason) -> {
      logging.log(
        logging.Warning,
        "guild rpc forward to "
          <> node.to_string(owner)
          <> " failed: "
          <> string.inspect(reason),
      )
      Error(RpcTimeout)
    }
  }
}
