// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id.{type ChannelId, type GuildId}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/guild/data.{type GuildData}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}

pub fn sort_for_ordering(channels: List(Channel)) -> List(Channel) {
  let ids =
    channels
    |> list.map(fn(row) { id.channel_id_to_int(channel.id(row)) })
    |> set.from_list
  let Buckets(roots:, categories:, children:) =
    list.fold(channels, Buckets([], [], dict.new()), fn(acc, row) {
      classify(row, ids, acc)
    })
  let root_block = sort_group(roots)
  let #(ordered, seen) =
    categories
    |> sort_by_position
    |> list.fold(#(root_block, mark_seen(root_block, set.new())), fn(acc, row) {
      let block = [row, ..children_of(row, children)]
      #(list.append(acc.0, block), mark_seen(block, acc.1))
    })
  let remaining =
    channels
    |> sort_by_position
    |> list.filter(fn(row) {
      !set.contains(seen, id.channel_id_to_int(channel.id(row)))
    })
  list.append(ordered, remaining)
}

type Buckets {
  Buckets(
    roots: List(Channel),
    categories: List(Channel),
    children: Dict(Int, List(Channel)),
  )
}

fn classify(row: Channel, ids: Set(Int), acc: Buckets) -> Buckets {
  case channel.kind(row), channel.parent(row) {
    channel.Category, _ -> Buckets(..acc, categories: [row, ..acc.categories])
    _, None -> Buckets(..acc, roots: [row, ..acc.roots])
    _, Some(parent) -> {
      let parent_int = id.channel_id_to_int(parent)
      case set.contains(ids, parent_int) {
        True ->
          Buckets(
            ..acc,
            children: dict.upsert(acc.children, parent_int, fn(existing) {
              [row, ..option.unwrap(existing, [])]
            }),
          )
        False -> acc
      }
    }
  }
}

fn children_of(
  row: Channel,
  children: Dict(Int, List(Channel)),
) -> List(Channel) {
  dict.get(children, id.channel_id_to_int(channel.id(row)))
  |> result.unwrap([])
  |> sort_group
}

fn mark_seen(rows: List(Channel), seen: Set(Int)) -> Set(Int) {
  list.fold(rows, seen, fn(acc, row) {
    set.insert(acc, id.channel_id_to_int(channel.id(row)))
  })
}

fn sort_group(rows: List(Channel)) -> List(Channel) {
  let sorted = sort_by_position(rows)
  list.flatten([
    list.filter(sorted, is_layout_text),
    list.filter(sorted, is_layout_voice),
    list.filter(sorted, fn(row) {
      !is_layout_text(row) && !is_layout_voice(row)
    }),
  ])
}

fn sort_by_position(rows: List(Channel)) -> List(Channel) {
  list.sort(rows, fn(a, b) {
    case int.compare(channel.position(a), channel.position(b)) {
      order.Eq -> id.compare_channel_id(channel.id(a), channel.id(b))
      other -> other
    }
  })
}

fn is_layout_text(row: Channel) -> Bool {
  case channel.kind_to_int(channel.kind(row)) {
    0 | 5 | 998 | 15 | 16 -> True
    _ -> False
  }
}

fn is_layout_voice(row: Channel) -> Bool {
  channel.kind(row) == channel.Voice
}

fn everyone_can_view(
  row: Channel,
  guild: GuildId,
  everyone: Permissions,
) -> Bool {
  case permissions.has(everyone, permissions.Administrator) {
    True -> True
    False ->
      row
      |> apply_everyone_overwrites(guild, everyone)
      |> permissions.has(permissions.ViewChannel)
  }
}

fn apply_everyone_overwrites(
  row: Channel,
  guild: GuildId,
  base: Permissions,
) -> Permissions {
  let everyone = channel.RoleTarget(id.everyone_role(guild))
  list.fold(channel.overwrites(row), base, fn(acc, overwrite) {
    case channel.overwrite_target(overwrite) == everyone {
      True ->
        permissions.apply(
          acc,
          allow: channel.overwrite_allow(overwrite),
          deny: channel.overwrite_deny(overwrite),
        )
      False -> acc
    }
  })
}

pub fn first_everyone_viewable(guild_data: GuildData) -> Option(ChannelId) {
  let guild = data.guild_id(guild_data)
  let everyone =
    data.everyone_permissions(guild_data) |> option.unwrap(permissions.none())
  guild_data.channels
  |> sort_for_ordering
  |> list.find(fn(row) {
    case channel.kind(row) {
      channel.Text | channel.Voice | channel.Announcement ->
        everyone_can_view(row, guild, everyone)
      _ -> False
    }
  })
  |> option.from_result
  |> option.map(channel.id)
}

pub type Mention {
  Mention(id: ChannelId, name: String, kind: Int)
}

pub fn mentions(
  guild_data: GuildData,
  wanted: List(ChannelId),
  name_of: fn(Channel) -> Option(String),
) -> List(Mention) {
  let guild = data.guild_id(guild_data)
  let everyone =
    data.everyone_permissions(guild_data) |> option.unwrap(permissions.none())
  wanted
  |> list.unique
  |> list.filter_map(fn(channel_id) {
    use row <- result.try(
      data.find_channel(guild_data, channel_id) |> option.to_result(Nil),
    )
    let viewable =
      !channel.is_thread_only_kind(channel.kind(row))
      && everyone_can_view(row, guild, everyone)
    use <- bool.guard(!viewable, Error(Nil))
    use name <- result.try(name_of(row) |> option.to_result(Nil))
    Ok(Mention(
      id: channel_id,
      name:,
      kind: channel.kind_to_int(channel.kind(row)),
    ))
  })
}

pub fn category_count(guild_data: GuildData, category: ChannelId) -> Int {
  list.count(guild_data.channels, fn(row) {
    channel.parent(row) == Some(category)
  })
}
