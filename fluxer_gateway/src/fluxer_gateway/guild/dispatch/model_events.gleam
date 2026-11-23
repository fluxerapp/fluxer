// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/members
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/state/channels as channel_events
import fluxer_gateway/guild/state/members as member_events
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/perms/cutoff
import fluxer_gateway/perms/event as perm_event
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/result

pub opaque type Prior {
  NoPrior
  DeletedRole(holders: List(UserId), channels: List(ChannelId))
}

pub fn prior(state: GuildState, event_name: String, raw: Dynamic) -> Prior {
  case
    event_name,
    decode.run(raw, decode.at(["role_id"], id_decode.role_id()))
  {
    "GUILD_ROLE_DELETE", Ok(role) ->
      DeletedRole(
        holders: members.with_role(guild_state.members_table(state), role)
          |> list.filter_map(fn(user) {
            id.user_id(user) |> result.replace_error(Nil)
          }),
        channels: guild_state.guild_data(state).channels
          |> list.filter(fn(row) {
            list.any(channel.overwrites(row), fn(overwrite) {
              channel.overwrite_target(overwrite) == channel.RoleTarget(role)
            })
          })
          |> list.map(channel.id),
      )
    _, _ -> NoPrior
  }
}

pub fn perm_events(
  state: GuildState,
  event_name: String,
  raw: Dynamic,
  change: member_events.Change,
  prior: Prior,
) -> List(perm_event.Event) {
  let guild_data = guild_state.guild_data(state)
  case event_name, change {
    _, member_events.Added(user) | _, member_events.Updated(user) ->
      case guild_state.member(state, user) {
        Some(found) -> [perm_event.member_upsert(found)]
        None -> []
      }
    _, member_events.Removed(user) -> [perm_event.member_remove(user)]
    "GUILD_ROLE_CREATE", _
    | "GUILD_ROLE_UPDATE", _
    | "GUILD_ROLE_UPDATE_BULK", _
    -> [perm_event.roles(guild_data.roles)]
    "GUILD_ROLE_DELETE", _ -> [
      perm_event.roles(guild_data.roles),
      ..stripped_role_events(state, prior)
    ]
    "CHANNEL_CREATE", _ ->
      channel_upserts(state, event_name, raw, perm_event.Created)
    "CHANNEL_UPDATE", _ | "CHANNEL_UPDATE_BULK", _ ->
      channel_upserts(state, event_name, raw, perm_event.Updated)
    "CHANNEL_DELETE", _ ->
      channel_events.channel_ids(event_name, raw)
      |> list.map(perm_event.channel_delete)
      |> list.append([order(state)])
    "GUILD_UPDATE", _ -> {
      let facts = guild_state.facts(state)
      let nsfw = data.decoded_property(guild_data, "nsfw", decode.bool)
      let history =
        data.decoded_property(
          guild_data,
          "message_history_cutoff",
          cutoff.decoder(),
        )
        |> option.unwrap(cutoff.Missing)
      [
        perm_event.owner(core_guild.owner(facts)),
        perm_event.guild_nsfw(nsfw, nsfw_level(core_guild.nsfw_level(facts))),
        perm_event.history_cutoff(history),
      ]
    }
    _, _ -> []
  }
}

fn stripped_role_events(
  state: GuildState,
  prior: Prior,
) -> List(perm_event.Event) {
  case prior {
    NoPrior -> []
    DeletedRole(holders:, channels:) -> {
      let guild_data = guild_state.guild_data(state)
      list.append(
        list.filter_map(holders, fn(user) {
          guild_state.member(state, user)
          |> option.map(perm_event.member_upsert)
          |> option.to_result(Nil)
        }),
        list.filter_map(channels, fn(channel_id) {
          data.find_channel(guild_data, channel_id)
          |> option.map(perm_event.channel_upsert(_, perm_event.Updated))
          |> option.to_result(Nil)
        }),
      )
    }
  }
}

fn channel_upserts(
  state: GuildState,
  event_name: String,
  raw: Dynamic,
  cause: perm_event.Cause,
) -> List(perm_event.Event) {
  let guild_data = guild_state.guild_data(state)
  channel_events.channel_ids(event_name, raw)
  |> list.filter_map(fn(channel_id) {
    data.find_channel(guild_data, channel_id)
    |> option.map(perm_event.channel_upsert(_, cause))
    |> option.to_result(Nil)
  })
  |> list.append([order(state)])
}

fn order(state: GuildState) -> perm_event.Event {
  guild_state.guild_data(state).channels
  |> list.map(channel.id)
  |> perm_event.channel_order
}

fn nsfw_level(level: core_guild.NsfwLevel) -> Int {
  case level {
    core_guild.NsfwDefault -> 0
    core_guild.NsfwExplicit -> 1
    core_guild.NsfwSafe -> 2
    core_guild.NsfwAgeRestricted -> 3
  }
}

pub fn list_events(
  state: GuildState,
  change: member_events.Change,
) -> List(list_event.Event) {
  case change {
    member_events.Added(user) | member_events.Updated(user) ->
      case guild_state.member(state, user) {
        Some(found) -> [list_event.member_upsert(found, list_event.Changed)]
        None -> []
      }
    member_events.Removed(user) -> [list_event.member_remove(user)]
    member_events.Unchanged -> []
  }
}
