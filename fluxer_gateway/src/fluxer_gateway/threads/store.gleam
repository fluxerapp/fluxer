// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/json_tree
import fluxer_gateway/threads/load as loader
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/rows.{type Rows} as row_table
import fluxer_gateway/threads/state.{
  type Effect, type Load, type Subs, type Viewer, Loading, Ready,
}
import fluxer_gateway/threads/thread.{type Thread} as thread_model
import fluxer_gateway/threads/value.{type Object}
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

const load_queue_cap = 10_000

const handoff_row_budget = 50_000

fn handoff_max_age_ms() -> Int {
  durations.threads().store_handoff_max_age_ms
}

const thread_starter_message = 21

pub opaque type ThreadStore {
  ThreadStore(
    rows: Rows,
    load: Load,
    viewers: Dict(String, Viewer),
    subs: Subs,
    recent: Dict(Int, List(Int)),
    effects: List(Effect),
    counted: Int,
  )
}

pub opaque type Handoff {
  Handoff(
    rows: Option(Rows),
    at_ms: Int,
    sync_sessions: List(String),
    dirty_lists: List(Int),
  )
}

pub type Mode {
  Live
  Replayed
}

pub fn new() -> ThreadStore {
  ThreadStore(
    rows: row_table.empty(),
    load: loading(0),
    viewers: dict.new(),
    subs: state.no_subs(),
    recent: dict.new(),
    effects: [],
    counted: 0,
  )
}

pub fn install_payload(
  store: ThreadStore,
  payload: loader.Payload,
) -> ThreadStore {
  let store =
    set_rows(
      store,
      row_table.load(store.rows, payload.threads, payload.members),
    )
  case payload.parent_settings {
    [] -> store
    settings -> push_effect(store, state.MergeParentSettings(settings))
  }
}

fn loading(attempt: Int) -> Load {
  Loading(queue: [], queued: 0, attempt:, request: None, timer: None)
}

pub fn thread(store: ThreadStore, thread_id: ChannelId) -> Option(Thread) {
  row_table.get_thread(store.rows, id.channel_id_to_int(thread_id))
}

pub fn apply(
  store: ThreadStore,
  event: String,
  data: Object,
  mode: Mode,
) -> ThreadStore {
  store
  |> queue_while_loading(event, data, mode, True)
  |> apply_rows(event, data, mode)
}

pub fn apply_guild_event(
  store: ThreadStore,
  event: String,
  data: Object,
  guild_channel: fn(Int) -> Bool,
) -> ThreadStore {
  let queued = !regular_channel_event(event, data, guild_channel)
  store
  |> queue_while_loading(event, data, Live, queued)
  |> apply_rows(event, data, Live)
}

fn regular_channel_event(
  event: String,
  data: Object,
  guild_channel: fn(Int) -> Bool,
) -> Bool {
  case event {
    "MESSAGE_CREATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK"
    | "CHANNEL_PINS_UPDATE" ->
      case value.snowflake_field(data, "channel_id") {
        Some(channel) -> guild_channel(channel)
        None -> True
      }
    _ -> False
  }
}

fn queue_while_loading(
  store: ThreadStore,
  event: String,
  data: Object,
  mode: Mode,
  queued: Bool,
) -> ThreadStore {
  case
    mode,
    store.load,
    queued && { is_thread_event(event) || is_store_event(event) }
  {
    Live,
      Loading(queue:, queued:, attempt:, request: Some(request), timer:),
      True
    ->
      case queued >= load_queue_cap {
        True -> {
          metrics.fetch("refetch")
          cancel(timer)
          ThreadStore(..store, load: loading(attempt + 1))
        }
        False ->
          ThreadStore(
            ..store,
            load: Loading(
              queue: [#(event, queued_data(event, data)), ..queue],
              queued: queued + 1,
              attempt:,
              request: Some(request),
              timer:,
            ),
          )
      }
    _, _, _ -> store
  }
}

pub fn cancel(timer: Option(process.Timer)) -> Nil {
  case timer {
    Some(timer) -> {
      process.cancel_timer(timer)
      Nil
    }
    None -> Nil
  }
}

fn queued_data(event: String, data: Object) -> Object {
  case is_thread_event(event) {
    True -> data
    False ->
      value.with(data, [
        "id", "channel_id", "type", "ids", "last_pin_timestamp", "user",
      ])
  }
}

pub fn is_thread_event(event: String) -> Bool {
  case event {
    "THREAD_CREATE"
    | "THREAD_UPDATE"
    | "THREAD_DELETE"
    | "THREAD_MEMBER_UPDATE"
    | "THREAD_MEMBERS_UPDATE"
    | "THREAD_LIST_SYNC"
    | "THREAD_MEMBER_LIST_UPDATE"
    | "FORUM_UNREADS" -> True
    _ -> False
  }
}

fn is_store_event(event: String) -> Bool {
  case event {
    "MESSAGE_CREATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK"
    | "CHANNEL_PINS_UPDATE"
    | "CHANNEL_DELETE"
    | "GUILD_MEMBER_REMOVE" -> True
    _ -> False
  }
}

fn apply_rows(
  store: ThreadStore,
  event: String,
  data: Object,
  mode: Mode,
) -> ThreadStore {
  case event, mode {
    "THREAD_CREATE", _ -> thread_create(store, data)
    "THREAD_UPDATE", _ -> thread_update(store, data)
    "THREAD_DELETE", _ ->
      case value.snowflake_field(data, "id") {
        Some(thread_id) ->
          set_rows(store, row_table.remove_thread(store.rows, thread_id))
        None -> store
      }
    "THREAD_MEMBER_UPDATE", _ -> put_known_member(store, data)
    "THREAD_MEMBERS_UPDATE", _ -> thread_members_update(store, data)
    "MESSAGE_CREATE", Live ->
      with_thread(store, data, fn(thread_id, entry) {
        count_message(thread_id, data, entry)
      })
    "MESSAGE_CREATE", Replayed ->
      with_thread(store, data, fn(thread_id, entry) {
        case newer(value.snowflake_field(data, "id"), entry) {
          True -> count_message(thread_id, data, entry)
          False -> entry
        }
      })
    "MESSAGE_DELETE", Live ->
      with_thread(store, data, fn(thread_id, entry) {
        case value.snowflake_field(data, "id") == Some(thread_id) {
          True -> entry
          False -> decrement(entry, 1)
        }
      })
    "MESSAGE_DELETE_BULK", Live ->
      with_thread(store, data, fn(thread_id, entry) {
        let removed =
          value.snowflake_list(data, "ids")
          |> list.filter(fn(message) { message != thread_id })
        decrement(entry, list.length(removed))
      })
    "CHANNEL_PINS_UPDATE", _ ->
      with_thread(store, data, fn(_, entry) {
        thread_model.put(
          entry,
          "last_pin_timestamp",
          option.unwrap(value.field(data, "last_pin_timestamp"), json_tree.Null),
        )
      })
    "CHANNEL_DELETE", _ ->
      case value.snowflake_field(data, "id") {
        Some(parent) ->
          set_rows(
            store,
            list.fold(
              row_table.parent_thread_ids(store.rows, parent),
              store.rows,
              row_table.remove_thread,
            ),
          )
        None -> store
      }
    "GUILD_MEMBER_REMOVE", _ ->
      case event_user(data) {
        Some(user) -> {
          let #(next, thread_ids) = row_table.remove_user(store.rows, user)
          list.fold(thread_ids, set_rows(store, next), member_lists_changed)
        }
        None -> store
      }
    "GUILD_MEMBER_UPDATE", _ ->
      case event_user(data) {
        Some(user) -> presence_changed(store, user)
        None -> store
      }
    _, _ -> store
  }
}

fn event_user(data: Object) -> Option(Int) {
  case value.object_field(data, "user") {
    Some(user) -> value.snowflake_field(user, "id")
    None -> value.snowflake_field(data, "user_id")
  }
}

fn thread_create(store: ThreadStore, data: Object) -> ThreadStore {
  case thread_model.from_event(data) {
    Error(Nil) -> store
    Ok(entry) ->
      case thread_model.is_archived(entry) {
        True -> store
        False -> {
          let store =
            set_rows(store, row_table.put_thread(store.rows, entry))
            |> put_members(created_members(data))
          case thread_model.parent(entry) {
            Some(parent) ->
              push_effect(
                store,
                state.ForumLastMessage(
                  parent: parent,
                  thread: thread_model.id(entry),
                ),
              )
            None -> store
          }
        }
      }
  }
}

fn created_members(data: Object) -> List(Object) {
  let members = value.object_list(data, "_fluxer_members")
  case value.object_field(data, "member"), value.get(data, "id") {
    Some(own), Ok(thread_id) ->
      case value.has(own, "user_id") {
        True -> [value.insert(own, "id", thread_id), ..members]
        False -> members
      }
    _, _ -> members
  }
}

fn thread_update(store: ThreadStore, data: Object) -> ThreadStore {
  case thread_model.from_event(data) {
    Error(Nil) -> store
    Ok(entry) -> {
      let key = thread_model.id_int(entry)
      case thread_model.is_archived(entry) {
        True -> set_rows(store, row_table.remove_thread(store.rows, key))
        False -> {
          let known = row_table.has_thread(store.rows, key)
          let store = set_rows(store, row_table.put_thread(store.rows, entry))
          case known {
            True -> store
            False ->
              put_members(store, value.object_list(data, "_fluxer_members"))
          }
        }
      }
    }
  }
}

fn thread_members_update(store: ThreadStore, data: Object) -> ThreadStore {
  case value.snowflake_field(data, "id") {
    None -> store
    Some(thread_id) -> {
      let store = ensure_thread(store, thread_id, data)
      case row_table.has_thread(store.rows, thread_id) {
        False -> store
        True -> {
          let store =
            put_members(store, value.object_list(data, "added_members"))
          let next =
            value.snowflake_list(data, "removed_member_ids")
            |> list.fold(store.rows, fn(acc, user) {
              row_table.remove_member(acc, thread_id, user)
            })
          let store = set_rows(store, next)
          case value.field(data, "member_count") {
            Some(json_tree.Int(count)) ->
              set_rows(
                store,
                row_table.update_thread(store.rows, thread_id, fn(entry) {
                  thread_model.put(entry, "member_count", json_tree.Int(count))
                }),
              )
            _ -> store
          }
        }
      }
    }
  }
}

fn ensure_thread(
  store: ThreadStore,
  thread_id: Int,
  data: Object,
) -> ThreadStore {
  let existing = row_table.get_thread(store.rows, thread_id)
  let context = case value.object_field(data, "_fluxer_thread") {
    Some(raw) -> thread_model.from_event(raw) |> option.from_result
    None -> None
  }
  case existing, context {
    None, Some(entry) ->
      case thread_model.is_archived(entry) {
        True -> store
        False -> set_rows(store, row_table.put_thread(store.rows, entry))
      }
    Some(known), Some(entry) ->
      set_rows(
        store,
        row_table.put_thread(store.rows, thread_model.merge(known, entry)),
      )
    _, _ -> store
  }
}

fn put_members(store: ThreadStore, members: List(Object)) -> ThreadStore {
  list.fold(members, store, put_known_member)
}

fn put_known_member(store: ThreadStore, raw: Object) -> ThreadStore {
  case thread_model.member_from_object(raw) {
    Ok(member) ->
      case
        row_table.has_thread(
          store.rows,
          id.channel_id_to_int(thread_model.member_thread(member)),
        )
      {
        True -> set_rows(store, row_table.put_member(store.rows, member))
        False -> store
      }
    Error(Nil) -> store
  }
}

fn with_thread(
  store: ThreadStore,
  data: Object,
  change: fn(Int, Thread) -> Thread,
) -> ThreadStore {
  case value.snowflake_field(data, "channel_id") {
    Some(channel) ->
      case row_table.get_thread(store.rows, channel) {
        Some(entry) ->
          set_rows(
            store,
            row_table.put_thread(store.rows, change(channel, entry)),
          )
        None -> store
      }
    None -> store
  }
}

fn count_message(thread_id: Int, data: Object, entry: Thread) -> Thread {
  let message = value.snowflake_field(data, "id")
  let starter = value.int_field(data, "type") == Some(thread_starter_message)
  case message == Some(thread_id) || starter {
    True -> entry
    False -> {
      let entry = case message, newer(message, entry) {
        Some(message), True ->
          thread_model.put(entry, "last_message_id", value.id_string(message))
        _, _ -> entry
      }
      entry
      |> thread_model.put(
        "message_count",
        json_tree.Int(thread_model.counter(entry, "message_count") + 1),
      )
      |> thread_model.put(
        "total_message_sent",
        json_tree.Int(thread_model.counter(entry, "total_message_sent") + 1),
      )
    }
  }
}

fn newer(message: Option(Int), entry: Thread) -> Bool {
  case message, thread_model.last_message_id(entry) {
    Some(message), Some(last) -> message > last
    _, _ -> True
  }
}

fn decrement(entry: Thread, count: Int) -> Thread {
  thread_model.put(
    entry,
    "message_count",
    json_tree.Int(int.max(
      0,
      thread_model.counter(entry, "message_count") - count,
    )),
  )
}

pub fn member_lists_changed(store: ThreadStore, thread_id: Int) -> ThreadStore {
  case dict.has_key(store.subs.list_sessions, thread_id) {
    True -> mark_dirty(request_list_refresh(store), [thread_id])
    False -> store
  }
}

pub fn presence_changed(store: ThreadStore, user: Int) -> ThreadStore {
  case dict.get(store.subs.presence_users, user) {
    Ok(thread_ids) -> mark_dirty(store, thread_ids)
    Error(Nil) -> store
  }
}

pub fn request_list_refresh(store: ThreadStore) -> ThreadStore {
  ThreadStore(..store, subs: state.Subs(..store.subs, list_refresh: True))
}

pub fn mark_dirty(store: ThreadStore, thread_ids: List(Int)) -> ThreadStore {
  case thread_ids {
    [] -> store
    _ ->
      ThreadStore(
        ..store,
        subs: state.Subs(
          ..store.subs,
          list_dirty: set.union(
            store.subs.list_dirty,
            set.from_list(thread_ids),
          ),
        ),
      )
  }
}

pub fn rows(store: ThreadStore) -> Rows {
  store.rows
}

fn set_rows(store: ThreadStore, next: Rows) -> ThreadStore {
  let count = row_table.thread_count(next)
  case count == store.counted {
    True -> Nil
    False -> metrics.threads_held(count)
  }
  ThreadStore(..store, rows: next, counted: count)
}

pub fn load_state(store: ThreadStore) -> Load {
  store.load
}

pub fn set_load(store: ThreadStore, load: Load) -> ThreadStore {
  ThreadStore(..store, load:)
}

pub fn is_loading(store: ThreadStore) -> Bool {
  case store.load {
    Ready -> False
    Loading(..) -> True
  }
}

pub fn viewers(store: ThreadStore) -> Dict(String, Viewer) {
  store.viewers
}

pub fn set_viewers(
  store: ThreadStore,
  viewers: Dict(String, Viewer),
) -> ThreadStore {
  ThreadStore(..store, viewers:)
}

pub fn subs(store: ThreadStore) -> Subs {
  store.subs
}

pub fn set_subs(store: ThreadStore, subs: Subs) -> ThreadStore {
  ThreadStore(..store, subs:)
}

pub fn recent(store: ThreadStore) -> Dict(Int, List(Int)) {
  store.recent
}

pub fn set_recent(
  store: ThreadStore,
  recent: Dict(Int, List(Int)),
) -> ThreadStore {
  ThreadStore(..store, recent:)
}

fn push_effect(store: ThreadStore, effect: Effect) -> ThreadStore {
  ThreadStore(..store, effects: [effect, ..store.effects])
}

pub fn take_effects(store: ThreadStore) -> #(ThreadStore, List(Effect)) {
  #(ThreadStore(..store, effects: []), list.reverse(store.effects))
}

pub fn release(store: ThreadStore) -> Nil {
  case store.counted {
    0 -> Nil
    _ -> metrics.threads_held(0)
  }
}

pub fn replace_rows(store: ThreadStore, fresh: Rows) -> ThreadStore {
  set_rows(store, fresh)
}

pub fn export_handoff(store: ThreadStore) -> Handoff {
  let exported = case is_loading(store) {
    True -> None
    False ->
      case row_table.row_count(store.rows) <= handoff_row_budget {
        True -> Some(store.rows)
        False -> {
          metrics.fetch("refetch")
          None
        }
      }
  }
  Handoff(
    rows: exported,
    at_ms: clock.system_ms(),
    sync_sessions: dict.keys(store.subs.sync_pending),
    dirty_lists: set.to_list(store.subs.list_dirty),
  )
}

pub fn restore_handoff(
  handoff: Handoff,
) -> #(ThreadStore, List(String), List(Int)) {
  let fresh = clock.system_ms() - handoff.at_ms <= handoff_max_age_ms()
  let store = case handoff.rows, fresh {
    Some(saved), True -> set_rows(ThreadStore(..new(), load: Ready), saved)
    _, _ -> {
      metrics.fetch("refetch")
      new()
    }
  }
  #(store, handoff.sync_sessions, handoff.dirty_lists)
}
