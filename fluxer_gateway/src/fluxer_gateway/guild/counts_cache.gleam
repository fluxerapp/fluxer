// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import fluxer_gateway/guild/counts_cache/shard
import fluxer_gateway/guild/names
import fluxer_gateway/guild/read_model
import fluxer_gateway/guild/remote
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn init_timeout_ms() -> Int {
  durations.guild().counts_cache_init_timeout_ms
}

fn sweep_interval_ms() -> Int {
  durations.guild().counts_cache_sweep_interval_ms
}

fn remote_timeout_ms() -> Int {
  durations.guild().counts_cache_remote_timeout_ms
}

type Sweep {
  Sweep
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(named) {
      let timers = process.new_subject()
      process.send_after(timers, sweep_interval_ms(), Sweep)
      actor.initialised(timers)
      |> actor.selecting(
        process.new_selector()
        |> process.select(named)
        |> process.select(timers),
      )
      |> actor.returning(Nil)
      |> Ok
    })
    |> actor.named(names.counts_cache())
    |> actor.on_message(fn(timers, _message) {
      sweep()
      process.send_after(timers, sweep_interval_ms(), Sweep)
      actor.continue(timers)
    })
    |> actor.start
  })
}

fn sweep() -> Nil {
  shard.guild_ints()
  |> list.each(fn(guild_int) {
    case id.guild_id(guild_int) {
      Ok(guild) ->
        case read_model.get(guild) {
          Ok(_) -> Nil
          Error(Nil) -> shard.delete_int(guild_int)
        }
      Error(_) -> shard.delete_int(guild_int)
    }
  })
}

pub fn counts(guild: GuildId) -> Result(#(Int, Int), Nil) {
  case shard.get(id.guild_id_to_int(guild)) {
    Ok(found) -> Ok(found)
    Error(Nil) ->
      case router.owner(router.GuildKey(guild), node.Guilds) {
        router.Remote(owner) -> remote_counts(owner, [guild]) |> first
        _ -> Error(Nil)
      }
  }
}

fn first(found: List(#(Int, Int, Int))) -> Result(#(Int, Int), Nil) {
  case found {
    [#(_, member_count, online_count), ..] -> Ok(#(member_count, online_count))
    [] -> Error(Nil)
  }
}

pub fn bulk(guilds: List(GuildId)) -> List(#(Int, Int, Int)) {
  let #(local, remote_groups) =
    list.fold(guilds, #([], dict.new()), fn(acc, guild) {
      let #(found, groups) = acc
      case shard.get(id.guild_id_to_int(guild)) {
        Ok(#(member_count, online_count)) -> #(
          [#(id.guild_id_to_int(guild), member_count, online_count), ..found],
          groups,
        )
        Error(Nil) ->
          case router.owner(router.GuildKey(guild), node.Guilds) {
            router.Remote(owner) -> #(
              found,
              dict.upsert(groups, node.to_string(owner), fn(existing) {
                case existing {
                  option.Some(#(target, ids)) -> #(target, [guild, ..ids])
                  option.None -> #(owner, [guild])
                }
              }),
            )
            _ -> acc
          }
      }
    })
  remote_groups
  |> dict.values
  |> list.flat_map(fn(group) { remote_counts(group.0, group.1) })
  |> list.append(list.reverse(local), _)
}

fn remote_counts(
  owner: node.Node,
  guilds: List(GuildId),
) -> List(#(Int, Int, Int)) {
  remote.call(
    owner,
    "fluxer_gateway@guild@counts_cache",
    "local_counts",
    [remote.arg(list.map(guilds, id.guild_id_to_int))],
    remote_timeout_ms(),
    expecting: decode.list({
      use guild <- decode.field(0, decode.int)
      use member_count <- decode.field(1, decode.int)
      use online_count <- decode.field(2, decode.int)
      decode.success(#(guild, member_count, online_count))
    }),
  )
  |> result.unwrap([])
}

pub fn local_counts(guilds: List(Int)) -> List(#(Int, Int, Int)) {
  list.filter_map(guilds, fn(guild) {
    shard.get(guild)
    |> result.map(fn(found) { #(guild, found.0, found.1) })
  })
}
