// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/threads/access
import fluxer_gateway/threads/context.{type Context}
import fluxer_gateway/threads/message.{ListFlush, SyncDrain, SyncTick}
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/state.{
  type Bucket, type Viewer, Bucket, Queued, Scheduled, Subs, Viewer,
}
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/threads/sync_queue
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/value.{type Object}
import fluxer_gateway/threads/view
import fluxer_gateway/threads/viewers
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

const max_member_lists = 10

const max_list_members = 1000

const max_presence_users = 1000

fn sync_jitter_ms() -> Int {
  durations.threads().subscriptions_sync_jitter_ms
}

const sync_rate = 20

fn list_flush_ms() -> Int {
  durations.threads().subscriptions_list_flush_ms
}

pub fn update(
  threads: ThreadStore,
  ctx: Context,
  session_id: String,
  subscribe: Option(Bool),
  member_lists: Option(List(Int)),
) -> ThreadStore {
  case viewers.get(threads, session_id) {
    Some(entry) -> {
      let #(entry, sync_needed) = case subscribe {
        Some(True) -> #(Viewer(..entry, subscribed: True), !entry.subscribed)
        Some(False) -> #(Viewer(..entry, subscribed: False), False)
        None -> #(entry, False)
      }
      let previous_lists = entry.member_lists
      let #(entry, new_lists) = case member_lists, store.is_loading(threads) {
        Some(requested), True -> #(
          Viewer(
            ..entry,
            member_lists: requested
              |> list.unique
              |> list.sort(int.compare)
              |> list.take(max_member_lists),
          ),
          [],
        )
        Some(requested), False -> {
          let visible = viewable_lists(threads, ctx, entry, requested)
          #(Viewer(..entry, member_lists: visible), visible)
        }
        None, _ -> #(entry, [])
      }
      let threads =
        store.set_viewers(
          threads,
          dict.insert(store.viewers(threads), session_id, entry),
        )
        |> refresh_presence_users(previous_lists != entry.member_lists)
      list.each(new_lists, fn(thread_id) {
        send_member_list(threads, ctx, entry.session, thread_id)
      })
      case sync_needed {
        True -> schedule_sync(threads, ctx, session_id)
        False -> threads
      }
    }
    None -> threads
  }
}

fn viewable_lists(
  threads: ThreadStore,
  ctx: Context,
  entry: Viewer,
  requested: List(Int),
) -> List(Int) {
  requested
  |> list.unique
  |> list.sort(int.compare)
  |> list.filter(fn(thread_id) {
    viewable_thread(threads, ctx, entry.session.user, thread_id)
  })
  |> list.take(max_member_lists)
}

fn viewable_thread(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  thread_id: Int,
) -> Bool {
  case viewers.stored(threads, thread_id) {
    Some(entry) -> access.can_view(threads, ctx, user, entry)
    None -> False
  }
}

pub fn clear_on_passive(
  threads: ThreadStore,
  session_id: String,
) -> ThreadStore {
  case viewers.get(threads, session_id) {
    Some(entry) ->
      store.set_viewers(
        threads,
        dict.insert(
          store.viewers(threads),
          session_id,
          Viewer(..entry, subscribed: False),
        ),
      )
    None -> threads
  }
}

pub fn schedule_sync(
  threads: ThreadStore,
  ctx: Context,
  session_id: String,
) -> ThreadStore {
  let subs = store.subs(threads)
  case dict.has_key(subs.sync_pending, session_id) {
    True -> threads
    False -> {
      process.send_after(
        ctx.self,
        int.random(sync_jitter_ms()),
        SyncTick(session_id),
      )
      store.set_subs(
        threads,
        Subs(
          ..subs,
          sync_pending: dict.insert(subs.sync_pending, session_id, Scheduled),
        ),
      )
    }
  }
}

pub fn handle_tick(
  threads: ThreadStore,
  ctx: Context,
  session_id: String,
) -> ThreadStore {
  let subs = store.subs(threads)
  case store.is_loading(threads), dict.get(subs.sync_pending, session_id) {
    True, _ ->
      store.set_subs(
        threads,
        Subs(..subs, sync_pending: dict.delete(subs.sync_pending, session_id)),
      )
    False, Ok(Queued) -> drain(threads, ctx)
    False, _ ->
      drain(
        store.set_subs(
          threads,
          Subs(
            ..subs,
            sync_pending: dict.insert(subs.sync_pending, session_id, Queued),
            sync_queue: sync_queue.push(subs.sync_queue, session_id),
          ),
        ),
        ctx,
      )
  }
}

pub fn handle_drain(threads: ThreadStore, ctx: Context) -> ThreadStore {
  let subs = store.subs(threads)
  drain(store.set_subs(threads, Subs(..subs, drain_armed: False)), ctx)
}

fn drain(threads: ThreadStore, ctx: Context) -> ThreadStore {
  let subs = store.subs(threads)
  case sync_queue.pop(subs.sync_queue) {
    Error(Nil) -> threads
    Ok(#(session_id, rest)) -> {
      let now = clock.monotonic_ms()
      let bucket =
        option.unwrap(subs.bucket, Bucket(int.to_float(sync_rate), now))
      case take_token(now, bucket) {
        Ok(bucket) -> {
          send_full_sync(threads, ctx, session_id)
          store.set_subs(
            threads,
            Subs(
              ..subs,
              bucket: Some(bucket),
              sync_queue: rest,
              sync_pending: dict.delete(subs.sync_pending, session_id),
            ),
          )
          |> drain(ctx)
        }
        Error(bucket) -> {
          let wait =
            float.ceiling(
              { 1.0 -. bucket.tokens } *. 1000.0 /. int.to_float(sync_rate),
            )
            |> float.round
          let subs = Subs(..subs, bucket: Some(bucket))
          case subs.drain_armed {
            True -> store.set_subs(threads, subs)
            False -> {
              process.send_after(ctx.self, int.max(1, wait), SyncDrain)
              store.set_subs(threads, Subs(..subs, drain_armed: True))
            }
          }
        }
      }
    }
  }
}

fn take_token(now: Int, bucket: Bucket) -> Result(Bucket, Bucket) {
  let rate = int.to_float(sync_rate)
  let tokens =
    float.min(
      rate,
      bucket.tokens +. int.to_float(now - bucket.last_ms) *. rate /. 1000.0,
    )
  case tokens >=. 1.0 {
    True -> Ok(Bucket(tokens -. 1.0, now))
    False -> Error(Bucket(tokens, now))
  }
}

fn send_full_sync(
  threads: ThreadStore,
  ctx: Context,
  session_id: String,
) -> Nil {
  case viewers.get(threads, session_id) {
    Some(entry) ->
      case ctx.unavailable_for(entry.session.user) {
        True -> Nil
        False -> {
          send_list_sync(threads, ctx, entry.session, sync_scope(entry))
          metrics.list_sync("full")
        }
      }
    None -> Nil
  }
}

fn sync_scope(entry: Viewer) -> view.SyncScope {
  case entry.subscribed || entry.bot {
    True -> view.AllThreads
    False -> view.JoinedThreads
  }
}

fn send_list_sync(
  threads: ThreadStore,
  ctx: Context,
  ref: SessionRef,
  scope: view.SyncScope,
) -> Nil {
  let payload = view.list_sync(threads, ctx, ref.user, scope)
  deliver(ctx, [ref], "THREAD_LIST_SYNC", value.to_raw(payload))
}

pub fn resync_after_load(threads: ThreadStore, ctx: Context) -> ThreadStore {
  let threads =
    store.viewers(threads)
    |> dict.map_values(fn(_, entry) {
      Viewer(
        ..entry,
        member_lists: viewable_lists(threads, ctx, entry, entry.member_lists),
      )
    })
    |> store.set_viewers(threads, _)
    |> refresh_presence_users(True)
  let listed = dict.keys(store.subs(threads).list_sessions)
  let threads = arm(store.mark_dirty(threads, listed), ctx)
  dict.fold(store.viewers(threads), threads, fn(acc, session_id, entry) {
    case needs_resync(acc, entry) {
      True -> schedule_sync(acc, ctx, session_id)
      False -> acc
    }
  })
}

fn needs_resync(threads: ThreadStore, entry: Viewer) -> Bool {
  sync_scope(entry) == view.AllThreads
  || rows.user_thread_ids(
    store.rows(threads),
    id.user_id_to_int(entry.session.user),
  )
  != []
}

pub fn access_gained(
  threads: ThreadStore,
  ctx: Context,
  session_id: String,
  added: List(ChannelId),
) -> Nil {
  case viewers.get(threads, session_id) {
    Some(entry) -> {
      let table = store.rows(threads)
      let parents =
        list.filter(added, fn(parent) {
          rows.parent_thread_ids(table, id.channel_id_to_int(parent)) != []
        })
      case parents {
        [] -> Nil
        _ -> {
          metrics.list_sync("access")
          send_list_sync(
            threads,
            ctx,
            entry.session,
            view.ParentThreads(parents),
          )
        }
      }
    }
    None -> Nil
  }
}

pub fn arm(threads: ThreadStore, ctx: Context) -> ThreadStore {
  let subs = store.subs(threads)
  case set.is_empty(subs.list_dirty) || subs.flush_armed {
    True -> threads
    False -> {
      process.send_after(ctx.self, list_flush_ms(), ListFlush)
      store.set_subs(threads, Subs(..subs, flush_armed: True))
    }
  }
}

pub fn handle_list_flush(threads: ThreadStore, ctx: Context) -> ThreadStore {
  let subs = store.subs(threads)
  case store.is_loading(threads) {
    True -> store.set_subs(threads, Subs(..subs, flush_armed: False))
    False -> {
      let dirty = set.to_list(subs.list_dirty)
      let refresh = subs.list_refresh || stale_list_sessions(threads, dirty)
      let threads =
        store.set_subs(
          threads,
          Subs(
            ..subs,
            list_dirty: set.new(),
            flush_armed: False,
            list_refresh: False,
          ),
        )
        |> refresh_presence_users(refresh)
      send_member_lists(threads, ctx, dirty)
      threads
    }
  }
}

fn stale_list_sessions(threads: ThreadStore, thread_ids: List(Int)) -> Bool {
  let index = store.subs(threads).list_sessions
  list.any(thread_ids, fn(thread_id) {
    dict.get(index, thread_id)
    |> result.unwrap([])
    |> list.any(fn(session_id) {
      case viewers.get(threads, session_id) {
        Some(entry) -> !list.contains(entry.member_lists, thread_id)
        None -> True
      }
    })
  })
}

fn refresh_presence_users(threads: ThreadStore, refresh: Bool) -> ThreadStore {
  case refresh {
    False -> threads
    True -> {
      let subs = store.subs(threads)
      let tracked = store.viewers(threads)
      let thread_ids =
        dict.values(tracked)
        |> list.flat_map(fn(entry) { entry.member_lists })
        |> list.unique
        |> list.sort(int.compare)
      case thread_ids {
        [_, ..] ->
          store.set_subs(
            threads,
            Subs(
              ..subs,
              presence_users: presence_users(threads, thread_ids),
              list_sessions: list_sessions(tracked),
            ),
          )
        [] ->
          store.set_subs(
            threads,
            Subs(..subs, presence_users: dict.new(), list_sessions: dict.new()),
          )
      }
    }
  }
}

fn list_sessions(tracked: Dict(String, Viewer)) -> Dict(Int, List(String)) {
  dict.fold(tracked, dict.new(), fn(acc, session_id, entry) {
    list.fold(entry.member_lists, acc, fn(index, thread_id) {
      dict.upsert(index, thread_id, fn(existing) {
        [session_id, ..option.unwrap(existing, [])]
      })
    })
  })
}

fn presence_users(
  threads: ThreadStore,
  thread_ids: List(Int),
) -> Dict(Int, List(Int)) {
  let table = store.rows(threads)
  list.fold(thread_ids, dict.new(), fn(users, thread_id) {
    rows.member_ids(table, thread_id)
    |> list.take(max_list_members)
    |> list.fold(users, fn(acc, user) {
      case dict.get(acc, user) {
        Ok(listed) -> dict.insert(acc, user, [thread_id, ..listed])
        Error(Nil) ->
          case dict.size(acc) < max_presence_users {
            True -> dict.insert(acc, user, [thread_id])
            False -> acc
          }
      }
    })
  })
}

fn send_member_lists(
  threads: ThreadStore,
  ctx: Context,
  thread_ids: List(Int),
) -> Nil {
  let index = store.subs(threads).list_sessions
  list.each(thread_ids, fn(thread_id) {
    case viewers.stored(threads, thread_id) {
      None -> Nil
      Some(entry) -> {
        let session_ids = dict.get(index, thread_id) |> result.unwrap([])
        let #(targets, _) =
          list.fold(session_ids, #([], access.new_memo()), fn(acc, session_id) {
            case viewers.get(threads, session_id) {
              Some(viewer) ->
                case list.contains(viewer.member_lists, thread_id) {
                  True -> {
                    let #(granted, memo) =
                      access.view_access(
                        threads,
                        ctx,
                        viewer.session.user,
                        entry,
                        None,
                        acc.1,
                      )
                    case granted {
                      True -> #([viewer.session, ..acc.0], memo)
                      False -> #(acc.0, memo)
                    }
                  }
                  False -> acc
                }
              None -> acc
            }
          })
        case targets {
          [] -> Nil
          _ ->
            deliver(
              ctx,
              targets,
              "THREAD_MEMBER_LIST_UPDATE",
              member_list_payload(threads, ctx, thread_id),
            )
        }
      }
    }
  })
}

fn send_member_list(
  threads: ThreadStore,
  ctx: Context,
  ref: SessionRef,
  thread_id: Int,
) -> Nil {
  deliver(
    ctx,
    [ref],
    "THREAD_MEMBER_LIST_UPDATE",
    member_list_payload(threads, ctx, thread_id),
  )
}

fn member_list_payload(
  threads: ThreadStore,
  ctx: Context,
  thread_id: Int,
) -> RawJson {
  let members =
    rows.members(store.rows(threads), thread_id)
    |> list.take(max_list_members)
  [
    #("guild_id", value.id_string(id.guild_id_to_int(ctx.guild))),
    #("thread_id", value.id_string(thread_id)),
    #(
      "members",
      json_tree.Array(
        list.map(members, fn(pair) {
          json_tree.Object(list_member(ctx, pair.0, pair.1))
        }),
      ),
    ),
  ]
  |> value.to_raw
}

fn list_member(ctx: Context, user: Int, entry: thread.ThreadMember) -> Object {
  [
    #("user_id", value.id_string(user)),
    #("join_timestamp", thread.member_join_timestamp(entry)),
    #("flags", thread.member_flags(entry)),
    #("member", guild_member(ctx, thread.member_user(entry))),
    #("presence", presence_value(ctx, thread.member_user(entry), False)),
  ]
}

pub fn guild_member(ctx: Context, user: UserId) -> JsonTree {
  case ctx.member(user) {
    Some(found) -> json_tree.from_raw(member.wire(found))
    None -> json_tree.Null
  }
}

pub fn presence_value(
  ctx: Context,
  user: UserId,
  null_offline: Bool,
) -> JsonTree {
  case ctx.presence(user), null_offline {
    None, True -> json_tree.Null
    None, False -> offline_presence()
    Some(found), False -> json_tree.from_raw(found)
    Some(found), True ->
      raw_json.with_field(
        found,
        "user",
        raw_json.object([#("id", raw_json.from_json(id.user_json(user)))]),
      )
      |> result.unwrap(found)
      |> json_tree.from_raw
  }
}

fn offline_presence() -> JsonTree {
  json_tree.Object([
    #("status", json_tree.String("offline")),
    #("mobile", json_tree.Bool(False)),
    #("afk", json_tree.Bool(False)),
  ])
}

pub fn deliver(
  ctx: Context,
  targets: List(SessionRef),
  event: String,
  payload: RawJson,
) -> Nil {
  case targets {
    [] -> Nil
    _ -> relay.send(targets, session.guild_dispatch(ctx.guild, event, payload))
  }
}
