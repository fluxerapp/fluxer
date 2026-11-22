// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/perms
import fluxer_gateway/threads/access
import fluxer_gateway/threads/context.{type Context}
import fluxer_gateway/threads/payload
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/threads/thread.{type Thread}
import fluxer_gateway/threads/value.{type Object}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/set

pub type SyncScope {
  AllThreads
  JoinedThreads
  ParentThreads(parents: List(ChannelId))
}

pub fn guild_threads(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  bot: Bool,
) -> List(Json) {
  let scope = case bot {
    True -> AllThreads
    False -> JoinedThreads
  }
  viewable(threads, ctx, user, scope)
  |> list.map(fn(entry) { value.to_json(with_own_member(threads, user, entry)) })
}

fn with_own_member(
  threads: ThreadStore,
  user: UserId,
  entry: Thread,
) -> Object {
  let base = thread.payload(entry)
  case
    rows.get_member(
      store.rows(threads),
      thread.id_int(entry),
      id.user_id_to_int(user),
    )
  {
    Some(member) ->
      value.insert(
        base,
        "member",
        json_tree.Object(thread.own_member_payload(member)),
      )
    None -> base
  }
}

pub fn viewable(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  scope: SyncScope,
) -> List(Thread) {
  candidates(threads, user, scope)
  |> list.filter(fn(entry) { access.can_view(threads, ctx, user, entry) })
}

fn candidates(
  threads: ThreadStore,
  user: UserId,
  scope: SyncScope,
) -> List(Thread) {
  let table = store.rows(threads)
  case scope {
    AllThreads -> rows.threads(table)
    JoinedThreads ->
      rows.user_thread_ids(table, id.user_id_to_int(user))
      |> list.filter_map(fn(key) {
        rows.get_thread(table, key) |> option.to_result(Nil)
      })
    ParentThreads(parents) ->
      parents
      |> list.flat_map(fn(parent) {
        rows.parent_thread_ids(table, id.channel_id_to_int(parent))
      })
      |> list.filter_map(fn(key) {
        rows.get_thread(table, key) |> option.to_result(Nil)
      })
  }
}

pub fn list_sync(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  scope: SyncScope,
) -> Object {
  let visible = viewable(threads, ctx, user, scope)
  let table = store.rows(threads)
  let members =
    list.filter_map(visible, fn(entry) {
      rows.get_member(table, thread.id_int(entry), id.user_id_to_int(user))
      |> option.map(fn(member) {
        json_tree.Object(thread.member_object(member))
      })
      |> option.to_result(Nil)
    })
  let base = [
    #("guild_id", value.id_string(id.guild_id_to_int(ctx.guild))),
    #(
      "threads",
      json_tree.Array(
        list.map(visible, fn(entry) { json_tree.Object(thread.payload(entry)) }),
      ),
    ),
    #("members", json_tree.Array(members)),
  ]
  case scope {
    ParentThreads(parents) ->
      value.insert(
        base,
        "channel_ids",
        json_tree.Array(
          list.map(parents, fn(parent) {
            value.id_string(id.channel_id_to_int(parent))
          }),
        ),
      )
    _ -> base
  }
}

pub fn hide_for_non_viewer(
  ctx: Context,
  user: UserId,
  channels: List(Channel),
) -> List(RawJson) {
  visible_for_non_viewer(ctx, user, channels)
  |> list.map(fn(entry) { mask_channel(channel.raw(entry)) })
}

pub fn visible_for_non_viewer(
  ctx: Context,
  user: UserId,
  channels: List(Channel),
) -> List(Channel) {
  let kept =
    list.filter(channels, fn(entry) {
      !channel.is_thread_only_kind(channel.kind(entry))
    })
  case list.length(kept) == list.length(channels) {
    True -> kept
    False -> drop_forum_only_categories(ctx, user, kept)
  }
}

pub fn mask_channel(wire: RawJson) -> RawJson {
  value.as_object(value.expand(wire))
  |> payload.mask_channel
  |> value.to_raw
}

fn drop_forum_only_categories(
  ctx: Context,
  user: UserId,
  channels: List(Channel),
) -> List(Channel) {
  let parents =
    channels
    |> list.filter(fn(entry) { channel.kind(entry) != channel.Category })
    |> list.filter_map(fn(entry) {
      channel.parent(entry) |> option.to_result(Nil)
    })
    |> set.from_list
  list.filter(channels, fn(entry) {
    channel.kind(entry) != channel.Category
    || set.contains(parents, channel.id(entry))
    || category_visible(ctx, user, channel.id(entry))
  })
}

fn category_visible(ctx: Context, user: UserId, category: ChannelId) -> Bool {
  own_view(ctx, user, category)
  || perms.has_virtual_view(ctx.reader, user, category) == Ok(True)
}

fn own_view(ctx: Context, user: UserId, channel_id: ChannelId) -> Bool {
  case perms.permissions(ctx.reader, user, perms.ChannelScope(channel_id)) {
    Ok(granted) -> permissions.has(granted, permissions.ViewChannel)
    Error(_) -> False
  }
}
