// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/role
import fluxer_gateway/guild/data
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/visitors/table.{type Visitor}
import fluxer_gateway/perms
import fluxer_gateway/threads/payload as thread_payload
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/store
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/value as thread_value
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}

const max_snapshot_threads = 200

pub fn view(state: GuildState, adult: Bool, thread_viewer: Bool) -> Set(Int) {
  let full =
    perms.visitor_channels(guild_state.perms_reader(state), adult)
    |> result.map(fn(grants) {
      grants
      |> list.map(fn(grant) { id.channel_id_to_int(grant.0) })
      |> set.from_list
    })
    |> result.unwrap(set.new())
  case thread_viewer {
    True -> full
    False -> {
      let guild_data = guild_state.guild_data(state)
      set.filter(full, fn(channel_int) {
        id.channel_id(channel_int)
        |> option.from_result
        |> option.then(data.find_channel(guild_data, _))
        |> option.map(fn(row) {
          !channel.is_thread_only_kind(channel.kind(row))
        })
        |> option.unwrap(True)
      })
    }
  }
}

pub fn in_view(visitor: Visitor, channel: Int) -> Bool {
  set.contains(visitor.view, channel)
}

pub fn channel_visible(
  state: GuildState,
  visitor: Visitor,
  channel_int: Int,
) -> Bool {
  in_view(visitor, channel_int)
  || {
    id.channel_id(channel_int)
    |> option.from_result
    |> option.then(store.thread(guild_state.threads(state), _))
    |> option.map(fn(found) {
      thread_visible(
        visitor,
        Some(channel.kind_to_int(thread.kind(found))),
        option.map(thread.parent(found), id.channel_id_to_int),
      )
    })
    |> option.unwrap(False)
  }
}

pub fn thread_visible(
  visitor: Visitor,
  kind: Option(Int),
  parent: Option(Int),
) -> Bool {
  visitor.thread_viewer
  && is_public_thread(kind)
  && { option.map(parent, in_view(visitor, _)) |> option.unwrap(False) }
}

fn is_public_thread(kind: Option(Int)) -> Bool {
  case kind {
    Some(10) | Some(11) -> True
    _ -> False
  }
}

pub fn build(state: GuildState, visitor: Visitor) -> RawJson {
  let guild_data = guild_state.guild_data(state)
  let guild = guild_state.guild_id(state)
  let all = guild_data.channels
  let visible =
    list.filter(all, fn(row) {
      set.contains(visitor.view, id.channel_id_to_int(channel.id(row)))
    })
  let channels =
    with_parents(visible, all)
    |> list.map(fn(row) {
      strip_overwrites(
        json_tree.Raw(data.channel_wire(guild_data, row)),
        id.guild_id_to_int(guild),
      )
      |> mask_channel(visitor, _)
      |> json_tree.to_json
    })
  let base = [
    #("id", id.guild_json(guild)),
    #("properties", data.properties_json(guild_data, [])),
    #("roles", roles_json(visitor, guild_data)),
    #("channels", json.preprocessed_array(channels)),
    #("emojis", raw_json.to_json(guild_data.emojis)),
    #("stickers", raw_json.to_json(guild_data.stickers)),
    #("members", json.preprocessed_array([])),
    #("member_count", json.int(guild_state.member_count(state))),
    #("online_count", json.int(guild_state.online_count(state))),
    #("presences", json.preprocessed_array([])),
    #("voice_states", json.preprocessed_array([])),
    #("joined_at", json.null()),
    #("visiting", json.bool(True)),
  ]
  case visitor.thread_viewer {
    True -> list.append(base, [#("threads", visitor_threads(state, visitor))])
    False -> base
  }
  |> json.object
  |> raw_json.from_json
}

fn roles_json(visitor: Visitor, guild_data: data.GuildData) -> Json {
  case visitor.thread_viewer {
    True -> data.roles_json(guild_data)
    False ->
      json.array(guild_data.roles, fn(row) {
        json_tree.Raw(role.raw(row))
        |> thread_payload.mask_role_tree
        |> json_tree.to_json
      })
  }
}

fn mask_channel(visitor: Visitor, wire: JsonTree) -> JsonTree {
  case visitor.thread_viewer {
    True -> wire
    False -> thread_payload.mask_channel_tree(wire)
  }
}

fn visitor_threads(state: GuildState, visitor: Visitor) -> Json {
  rows.threads(store.rows(guild_state.threads(state)))
  |> list.filter(fn(found) {
    is_public_thread(Some(channel.kind_to_int(thread.kind(found))))
    && case thread.parent(found) {
      Some(parent) -> in_view(visitor, id.channel_id_to_int(parent))
      None -> False
    }
  })
  |> list.sort(fn(a, b) {
    int.compare(thread.id_int(a), thread.id_int(b)) |> order.negate
  })
  |> list.take(max_snapshot_threads)
  |> list.map(fn(found) { thread_value.to_json(thread.payload(found)) })
  |> json.preprocessed_array
}

fn with_parents(visible: List(Channel), all: List(Channel)) -> List(Channel) {
  let ids = set.from_list(list.map(visible, fn(row) { channel.id(row) }))
  let missing =
    visible
    |> list.filter_map(fn(row) { option.to_result(channel.parent(row), Nil) })
    |> list.filter(fn(parent) { !set.contains(ids, parent) })
    |> set.from_list
  list.append(
    visible,
    list.filter(all, fn(row) { set.contains(missing, channel.id(row)) }),
  )
}

fn strip_overwrites(wire: JsonTree, guild: Int) -> JsonTree {
  case
    json_tree.field(wire, "permission_overwrites")
    |> result.try(json_tree.items)
  {
    Ok(overwrites) ->
      json_tree.set(
        wire,
        "permission_overwrites",
        json_tree.Array(
          list.filter(overwrites, fn(overwrite) {
            json_tree.snowflake_field(overwrite, "id") == Ok(guild)
          }),
        ),
      )
      |> result.unwrap(wire)
    Error(Nil) -> wire
  }
}

pub fn strip_channel(wire: JsonTree) -> JsonTree {
  case json_tree.snowflake_field(wire, "guild_id") {
    Ok(channel_guild) -> strip_overwrites(wire, channel_guild)
    Error(Nil) -> {
      let empty = json_tree.Array([])
      json_tree.set(wire, "permission_overwrites", empty)
      |> result.unwrap(json_tree.Object([#("permission_overwrites", empty)]))
    }
  }
}
