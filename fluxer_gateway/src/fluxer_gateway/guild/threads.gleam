// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions as core_permissions
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type IdentifyFlags, type SessionRef}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/push/thread_scope.{type ThreadScope, ThreadScope}
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import fluxer_gateway/push/settings
import fluxer_gateway/threads/access
import fluxer_gateway/threads/dispatch as thread_dispatch
import fluxer_gateway/threads/message.{type ThreadMsg}
import fluxer_gateway/threads/payload as thread_payload
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/runtime
import fluxer_gateway/threads/state.{
  type Effect, ForumLastMessage, MergeParentSettings, Viewer,
}
import fluxer_gateway/threads/store.{type Handoff, type ThreadStore}
import fluxer_gateway/threads/subscriptions
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/value as thread_value
import fluxer_gateway/threads/viewers
import fluxer_gateway/util/iso8601
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

const thread_starter_message = 21

const scope_keys = [
  "id", "channel_id", "type", "ids", "last_pin_timestamp", "user", "user_id",
  "message_id", "__thread_only_update", "__thread_scoped", "_fluxer_thread",
]

pub fn install(state: GuildState, handoff: Option(Handoff)) -> GuildState {
  let threads = case handoff {
    Some(saved) -> runtime.restore(saved, guild_state.thread_context(state))
    None -> store.new()
  }
  guild_sessions.entries(guild_state.sessions(state))
  |> list.fold(threads, fn(acc, found) {
    viewers.session_joined(
      acc,
      entry.ref(found),
      entry.flags(found),
      entry.is_bot(found),
    )
  })
  |> settle(state, _)
}

pub fn handle(state: GuildState, message: ThreadMsg) -> GuildState {
  settle(
    state,
    runtime.handle(
      guild_state.threads(state),
      guild_state.thread_context(state),
      message,
    ),
  )
}

pub fn apply_event(
  state: GuildState,
  event: String,
  data: thread_value.Object,
  deliver: Bool,
) -> GuildState {
  let ctx = guild_state.thread_context(state)
  let threads = guild_state.threads(state)
  let threads = case deliver || !thread_dispatch.handles(event) {
    True -> thread_dispatch.handle_event(threads, ctx, event, data)
    False -> store.apply(threads, event, data, store.Live)
  }
  settle(state, threads)
}

pub fn scope_object(data: JsonTree) -> thread_value.Object {
  case json_tree.expand(data) {
    Ok(json_tree.Object(fields)) ->
      json_tree.Object(
        list.filter(fields, fn(pair) { list.contains(scope_keys, pair.0) }),
      )
      |> thread_value.object_from_tree
    _ -> []
  }
}

pub fn session_joined(
  state: GuildState,
  ref: SessionRef,
  flags: IdentifyFlags,
  bot: Bool,
) -> GuildState {
  guild_state.set_threads(
    state,
    viewers.session_joined(guild_state.threads(state), ref, flags, bot),
  )
}

pub fn session_rebound(state: GuildState, ref: SessionRef) -> GuildState {
  let threads = guild_state.threads(state)
  let key = viewers.key(ref)
  case viewers.get(threads, key) {
    Some(viewer) ->
      guild_state.set_threads(
        state,
        store.set_viewers(
          threads,
          dict.insert(
            store.viewers(threads),
            key,
            Viewer(..viewer, session: ref),
          ),
        ),
      )
    None -> state
  }
}

pub fn session_left(state: GuildState, key: String) -> GuildState {
  guild_state.set_threads(
    state,
    viewers.session_left(guild_state.threads(state), key),
  )
}

pub fn update_subscriptions(
  state: GuildState,
  key: String,
  subscribe: Option(Bool),
  member_lists: Option(List(Int)),
) -> GuildState {
  subscriptions.update(
    guild_state.threads(state),
    guild_state.thread_context(state),
    key,
    subscribe,
    member_lists,
  )
  |> guild_state.set_threads(state, _)
}

pub fn session_passive(state: GuildState, key: String) -> GuildState {
  guild_state.set_threads(
    state,
    subscriptions.clear_on_passive(guild_state.threads(state), key),
  )
}

pub fn presence_changed(state: GuildState, user: Int) -> GuildState {
  let threads = guild_state.threads(state)
  case dict.has_key(store.subs(threads).presence_users, user) {
    False -> state
    True ->
      store.presence_changed(threads, user)
      |> subscriptions.arm(guild_state.thread_context(state))
      |> guild_state.set_threads(state, _)
  }
}

pub fn joined_last_messages(state: GuildState, found: Entry) -> Dict(Int, Int) {
  let threads = guild_state.threads(state)
  case viewers.is_viewer(threads, entry.key(found)) {
    False -> dict.new()
    True -> {
      let table = store.rows(threads)
      rows.user_thread_ids(table, entry.user_int(found))
      |> list.filter_map(fn(thread_id) {
        rows.get_thread(table, thread_id)
        |> option.then(thread.last_message_id)
        |> option.map(fn(message) { #(thread_id, message) })
        |> option.to_result(Nil)
      })
      |> dict.from_list
    }
  }
}

pub fn release(state: GuildState) -> Nil {
  store.release(guild_state.threads(state))
}

fn settle(state: GuildState, threads: ThreadStore) -> GuildState {
  let threads = runtime.start(threads, guild_state.thread_context(state))
  let #(threads, effects) = store.take_effects(threads)
  list.fold(effects, guild_state.set_threads(state, threads), apply_effect)
}

fn apply_effect(state: GuildState, effect: Effect) -> GuildState {
  case effect {
    ForumLastMessage(parent:, thread: thread_id) ->
      forum_last_message(state, parent, thread_id)
    MergeParentSettings(settings: parents) ->
      merge_parent_settings(state, parents)
  }
}

fn forum_last_message(
  state: GuildState,
  parent: ChannelId,
  thread_id: ChannelId,
) -> GuildState {
  let guild_data = guild_state.guild_data(state)
  let forum =
    data.find_channel(guild_data, parent)
    |> option.map(fn(row) { channel.is_thread_only_kind(channel.kind(row)) })
    |> option.unwrap(False)
  case forum {
    True ->
      guild_state.set_guild_data(
        state,
        data.set_channel_field(
          guild_data,
          parent,
          "last_message_id",
          raw_json.from_json(id.json(id.channel_id_to_int(thread_id))),
        ),
      )
    False -> state
  }
}

fn merge_parent_settings(
  state: GuildState,
  parents: List(#(ChannelId, RawJson)),
) -> GuildState {
  let guild_data =
    list.fold(parents, guild_state.guild_data(state), fn(acc, pair) {
      let #(parent, raw) = pair
      case data.has_channel(acc, parent) {
        False -> acc
        True ->
          raw_json.fields(raw)
          |> result.unwrap([])
          |> list.fold(acc, fn(inner, pair) {
            data.set_channel_field(
              inner,
              parent,
              pair.0,
              raw_json.detach(pair.1),
            )
          })
      }
    })
  guild_state.set_guild_data(state, guild_data)
}

pub fn variants(
  state: GuildState,
  event: String,
  data: JsonTree,
  recipients: List(Entry),
) -> List(#(List(Entry), JsonTree)) {
  let data = strip_internal(data)
  case recipients {
    [] -> []
    _ ->
      starter_groups(state, event, data, recipients)
      |> list.flat_map(fn(group) {
        let #(members, payload) = group
        masked_groups(state, event, payload, members)
      })
  }
}

pub fn strip_internal(data: JsonTree) -> JsonTree {
  case data {
    json_tree.Object(fields) ->
      case list.any(fields, fn(pair) { thread.is_internal_key(pair.0) }) {
        True ->
          json_tree.Object(
            list.filter(fields, fn(pair) { !thread.is_internal_key(pair.0) }),
          )
        False -> data
      }
    other -> other
  }
}

fn starter_groups(
  state: GuildState,
  event: String,
  data: JsonTree,
  recipients: List(Entry),
) -> List(#(List(Entry), JsonTree)) {
  case
    event == "MESSAGE_CREATE"
    && json_tree.field(data, "type")
    == Ok(json_tree.Int(thread_starter_message))
  {
    False -> [#(recipients, data)]
    True -> {
      let by_key =
        list.map(recipients, fn(found) { #(entry.key(found), found) })
        |> dict.from_list
      thread_dispatch.starter_variants(
        guild_state.thread_context(state),
        event,
        data,
        list.map(recipients, entry.ref),
      )
      |> list.map(fn(variant) {
        #(
          list.filter_map(variant.0, fn(ref) {
            dict.get(by_key, session.session_id_to_string(ref.id))
          }),
          variant.1,
        )
      })
    }
  }
}

fn masked_groups(
  state: GuildState,
  event: String,
  data: JsonTree,
  recipients: List(Entry),
) -> List(#(List(Entry), JsonTree)) {
  let masked = thread_payload.mask(event, data)
  case masked == data {
    True -> [#(recipients, data)]
    False -> {
      let threads = guild_state.threads(state)
      let #(seen, hidden) =
        list.partition(recipients, fn(found) {
          viewers.is_viewer(threads, entry.key(found))
        })
      case hidden {
        [] -> [#(recipients, data)]
        _ -> {
          thread_dispatch.count_masked(event, list.length(hidden))
          [#(seen, data), #(hidden, masked)]
          |> list.filter(fn(group) { group.0 != [] })
        }
      }
    }
  }
}

pub fn mention_universe(
  state: GuildState,
  channel_id: ChannelId,
) -> Option(Set(Int)) {
  let threads = guild_state.threads(state)
  use found <- option.map(store.thread(threads, channel_id))
  rows.members(store.rows(threads), thread.id_int(found))
  |> list.map(fn(pair) { pair.0 })
  |> set.from_list
}

pub fn permissions(
  state: GuildState,
  user: UserId,
  channel_id: ChannelId,
) -> Option(Int) {
  let threads = guild_state.threads(state)
  use found <- option.map(store.thread(threads, channel_id))
  access.thread_permissions(
    threads,
    guild_state.thread_context(state),
    user,
    found,
  )
  |> core_permissions.to_int
}

pub fn can_view(
  state: GuildState,
  user: UserId,
  channel_id: ChannelId,
) -> Bool {
  let threads = guild_state.threads(state)
  case store.thread(threads, channel_id) {
    Some(found) ->
      access.can_view(threads, guild_state.thread_context(state), user, found)
    None ->
      perms.can_view(guild_state.perms_reader(state), user, channel_id)
      == Ok(True)
  }
}

pub fn push_scope(
  state: GuildState,
  channel_id: ChannelId,
) -> Option(ThreadScope) {
  let threads = guild_state.threads(state)
  use found <- option.then(store.thread(threads, channel_id))
  use parent <- option.map(thread.parent(found))
  let thread_id = thread.id_int(found)
  ThreadScope(
    thread: thread_id,
    parent: id.channel_id_to_int(parent),
    name: thread.name(found),
    private: thread.is_private(found),
    archived: thread.is_archived(found),
    members: rows.members(store.rows(threads), thread_id)
      |> list.map(fn(pair) { #(pair.0, push_member(pair.1)) })
      |> dict.from_list,
  )
}

fn push_member(member: thread.ThreadMember) -> settings.ThreadMember {
  let object = thread.member_object(member)
  settings.ThreadMember(
    flags: option.unwrap(thread_value.int_field(object, "flags"), 0),
    mute: case thread_value.is_true(object, "muted") {
      False -> settings.NotMuted
      True ->
        case mute_end(object) {
          Some(end_ms) -> settings.MutedUntil(end_ms)
          None -> settings.MutedForever
        }
    },
  )
}

fn mute_end(object: thread_value.Object) -> Option(Int) {
  thread_value.object_field(object, "mute_config")
  |> option.then(thread_value.string_field(_, "end_time"))
  |> option.then(fn(text) { iso8601.parse_ms(text) |> option.from_result })
}
