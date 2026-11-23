// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel, type ChannelKind}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/session.{type IdentifyFlags, type SessionRef}
import fluxer_gateway/perms
import fluxer_gateway/threads/context.{type Context}
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/state.{type Viewer, Viewer}
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/threads/thread.{type Thread}
import fluxer_gateway/threads/value.{type Object}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Scope {
  Open
  Viewers
  InThread(thread: Thread)
}

pub fn is_viewer_session(flags: IdentifyFlags, bot: Bool) -> Bool {
  bot || flags.channel_threads
}

pub fn session_joined(
  threads: ThreadStore,
  ref: SessionRef,
  flags: IdentifyFlags,
  bot: Bool,
) -> ThreadStore {
  case is_viewer_session(flags, bot) {
    False -> threads
    True ->
      store.set_viewers(
        threads,
        dict.insert(
          store.viewers(threads),
          key(ref),
          Viewer(session: ref, bot:, subscribed: False, member_lists: []),
        ),
      )
  }
}

pub fn session_left(threads: ThreadStore, session: String) -> ThreadStore {
  store.set_viewers(threads, dict.delete(store.viewers(threads), session))
}

pub fn key(ref: SessionRef) -> String {
  session.session_id_to_string(ref.id)
}

pub fn get(threads: ThreadStore, session: String) -> Option(Viewer) {
  dict.get(store.viewers(threads), session) |> option.from_result
}

pub fn is_viewer(threads: ThreadStore, session: String) -> Bool {
  dict.has_key(store.viewers(threads), session)
}

pub fn all(threads: ThreadStore) -> List(Viewer) {
  dict.values(store.viewers(threads))
}

fn is_gated_kind(kind: ChannelKind) -> Bool {
  channel.is_thread_kind(kind) || channel.is_thread_only_kind(kind)
}

fn gated_code(data: Object) -> Bool {
  case value.int_field(data, "type") {
    Some(code) -> is_gated_kind(channel.kind_from_int(code))
    None -> False
  }
}

fn is_channel_scoped(event: String) -> Bool {
  case event {
    "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_DELETE"
    | "MESSAGE_CREATE"
    | "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK"
    | "MESSAGE_REACTION_ADD"
    | "MESSAGE_REACTION_REMOVE"
    | "MESSAGE_REACTION_REMOVE_ALL"
    | "MESSAGE_REACTION_REMOVE_EMOJI"
    | "TYPING_START"
    | "CHANNEL_PINS_UPDATE"
    | "WEBHOOKS_UPDATE" -> True
    _ -> False
  }
}

pub fn event_scope(
  threads: ThreadStore,
  ctx: Context,
  event: String,
  data: Object,
) -> Scope {
  case event {
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      case gated_code(data) {
        True -> Viewers
        False -> Open
      }
    "MESSAGE_CREATE" ->
      case value.int_field(data, "type") {
        Some(18) | Some(21) ->
          case channel_scoped(threads, ctx, data) {
            InThread(found) -> InThread(found)
            _ -> Viewers
          }
        _ -> channel_scoped(threads, ctx, data)
      }
    "MESSAGE_UPDATE" ->
      case value.get(data, "__thread_only_update") {
        Ok(json_tree.Bool(True)) -> Viewers
        _ -> channel_scoped(threads, ctx, data)
      }
    "GUILD_AUDIT_LOG_ENTRY_CREATE" ->
      case value.has(data, "__thread_scoped") {
        True -> Viewers
        False -> Open
      }
    _ ->
      case is_channel_scoped(event) {
        True -> channel_scoped(threads, ctx, data)
        False -> Open
      }
  }
}

fn channel_scoped(threads: ThreadStore, ctx: Context, data: Object) -> Scope {
  case value.snowflake_field(data, "channel_id") {
    None -> Open
    Some(raw) ->
      case thread_or_context(threads, raw, data), id.channel_id(raw) {
        Some(found), _ -> InThread(found)
        None, Ok(channel_id) ->
          case is_gated_channel(ctx, channel_id) {
            True -> Viewers
            False -> Open
          }
        None, Error(_) -> Open
      }
  }
}

pub fn thread_or_context(
  threads: ThreadStore,
  thread_id: Int,
  data: Object,
) -> Option(Thread) {
  stored(threads, thread_id)
  |> option.lazy_or(fn() { thread_context(thread_id, data) })
}

pub fn stored(threads: ThreadStore, thread_id: Int) -> Option(Thread) {
  rows.get_thread(store.rows(threads), thread_id)
}

fn thread_context(thread_id: Int, data: Object) -> Option(Thread) {
  use raw <- option.then(value.object_field(data, "_fluxer_thread"))
  use found <- option.then(thread.from_event(raw) |> option.from_result)
  case thread.id_int(found) == thread_id {
    True -> Some(found)
    False -> None
  }
}

fn is_gated_channel(ctx: Context, channel_id: ChannelId) -> Bool {
  case ctx.channel(channel_id) {
    None -> True
    Some(found) -> is_gated_kind(channel.kind(found))
  }
}

pub fn forum_categories(channels: List(Channel)) -> Dict(Int, List(ChannelId)) {
  let categories =
    channels
    |> list.filter(fn(entry) {
      channel.is_thread_only_kind(channel.kind(entry))
    })
    |> list.filter_map(fn(entry) {
      option.to_result(channel.parent(entry), Nil)
    })
    |> list.map(fn(parent) { #(id.channel_id_to_int(parent), []) })
    |> dict.from_list
  list.fold(channels, categories, fn(acc, entry) {
    case
      channel.parent(entry),
      channel.is_thread_only_kind(channel.kind(entry))
    {
      Some(parent), False -> {
        let key = id.channel_id_to_int(parent)
        case dict.get(acc, key) {
          Ok(children) -> dict.insert(acc, key, [channel.id(entry), ..children])
          Error(Nil) -> acc
        }
      }
      _, _ -> acc
    }
  })
}

pub fn channel_visible(
  threads: ThreadStore,
  ctx: Context,
  categories: Dict(Int, List(ChannelId)),
  viewer: Bool,
  user: UserId,
  channel_id: ChannelId,
) -> Bool {
  viewer
  || case ctx.channel(channel_id) {
    Some(found) ->
      case channel.kind(found) {
        channel.Category -> category_visible(ctx, categories, user, channel_id)
        kind -> !is_gated_kind(kind)
      }
    None -> stored(threads, id.channel_id_to_int(channel_id)) == None
  }
}

fn category_visible(
  ctx: Context,
  categories: Dict(Int, List(ChannelId)),
  user: UserId,
  category: ChannelId,
) -> Bool {
  case dict.get(categories, id.channel_id_to_int(category)) {
    Error(Nil) -> True
    Ok(children) ->
      option.is_some(ctx.member(user))
      && {
        can_view_channel(ctx, user, category)
        || list.any(children, fn(child) { can_view_channel(ctx, user, child) })
      }
  }
}

fn can_view_channel(ctx: Context, user: UserId, channel_id: ChannelId) -> Bool {
  perms.can_view(ctx.reader, user, channel_id) |> result.unwrap(False)
}
