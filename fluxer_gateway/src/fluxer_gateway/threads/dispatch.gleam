// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/perms
import fluxer_gateway/threads/access.{type Memo}
import fluxer_gateway/threads/context.{type Context}
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/payload
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/state.{type Viewer}
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/threads/subscriptions
import fluxer_gateway/threads/thread.{type Thread}
import fluxer_gateway/threads/value.{type Object}
import fluxer_gateway/threads/viewers
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

const recent_create_cap = 256

type Delivery {
  Delivery(event: String, to: SessionRef, payload: Object)
}

type Batch {
  Batch(order: Int, event: String, payload: Object)
}

pub type Audience {
  Unscoped
  ViewersOnly
  ThreadAudience(sessions: List(SessionRef))
}

pub opaque type CatchUp {
  CatchUp(created: Dict(Int, List(Int)))
}

pub fn handles(event: String) -> Bool {
  store.is_thread_event(event)
}

pub fn scope_sessions(
  threads: ThreadStore,
  ctx: Context,
  event: String,
  data: Object,
  exclude: Option(String),
) -> Audience {
  case viewers.event_scope(threads, ctx, event, data) {
    viewers.Open -> Unscoped
    viewers.Viewers -> ViewersOnly
    viewers.InThread(entry) ->
      ThreadAudience(thread_sessions(threads, ctx, event, data, entry, exclude))
  }
}

fn thread_sessions(
  threads: ThreadStore,
  ctx: Context,
  event: String,
  data: Object,
  entry: Thread,
  exclude: Option(String),
) -> List(SessionRef) {
  let message = case message_access_event(event) {
    True ->
      value.snowflake_field(data, "message_id")
      |> option.lazy_or(fn() { value.snowflake_field(data, "id") })
    False -> None
  }
  let thread_id = thread.id_int(entry)
  let #(targets, _) =
    list.fold(viewers.all(threads), #([], access.new_memo()), fn(acc, viewer) {
      let sid = viewers.key(viewer.session)
      let candidate =
        Some(sid) != exclude
        && {
          viewer.bot
          || list.contains(viewer.member_lists, thread_id)
          || is_member(threads, viewer, thread_id)
          || viewer.subscribed
          && !ctx.passive(sid)
        }
      case candidate {
        False -> acc
        True -> {
          let #(granted, memo) =
            access.view_access(
              threads,
              ctx,
              viewer.session.user,
              entry,
              message,
              acc.1,
            )
          case granted {
            True -> #([viewer.session, ..acc.0], memo)
            False -> #(acc.0, memo)
          }
        }
      }
    })
  targets
}

fn message_access_event(event: String) -> Bool {
  case event {
    "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_REACTION_ADD"
    | "MESSAGE_REACTION_REMOVE"
    | "MESSAGE_REACTION_REMOVE_ALL"
    | "MESSAGE_REACTION_REMOVE_EMOJI" -> True
    _ -> False
  }
}

pub fn starter_variants(
  ctx: Context,
  event: String,
  data: JsonTree,
  sessions: List(SessionRef),
) -> List(#(List(SessionRef), JsonTree)) {
  case payload.starter_source(event, data) {
    payload.NoSource -> [#(sessions, data)]
    source -> {
      let #(readable, hidden, _) =
        list.fold(sessions, #([], [], dict.new()), fn(acc, ref) {
          let key = id.user_id_to_int(ref.user)
          let #(known, memo) = case dict.get(acc.2, key) {
            Ok(known) -> #(known, acc.2)
            Error(Nil) -> {
              let known =
                payload.source_readable(
                  source,
                  fn(parent) {
                    perms.permissions(
                      ctx.reader,
                      ref.user,
                      perms.ChannelScope(parent),
                    )
                    |> result.unwrap(permissions.none())
                  },
                  ctx.history_cutoff_ms,
                )
              #(known, dict.insert(acc.2, key, known))
            }
          }
          case known {
            True -> #([ref, ..acc.0], acc.1, memo)
            False -> #(acc.0, [ref, ..acc.1], memo)
          }
        })
      [#(readable, data), #(hidden, payload.without_source(data))]
      |> list.filter(fn(variant) { variant.0 != [] })
    }
  }
}

pub fn handle_event(
  threads: ThreadStore,
  ctx: Context,
  event: String,
  object: Object,
) -> ThreadStore {
  case handles(event) {
    False ->
      store.apply_guild_event(threads, event, object, fn(channel_id) {
        case id.channel_id(channel_id) {
          Ok(channel_id) -> option.is_some(ctx.channel(channel_id))
          Error(_) -> False
        }
      })
      |> subscriptions.arm(ctx)
    True -> {
      let object =
        value.insert(
          object,
          "guild_id",
          value.id_string(id.guild_id_to_int(ctx.guild)),
        )
      let #(threads, deliveries) =
        handle_thread_event(threads, ctx, event, object)
      deliver(ctx, deliveries)
      after_delivery(threads, event, object)
      |> subscriptions.arm(ctx)
    }
  }
}

fn after_delivery(
  threads: ThreadStore,
  event: String,
  data: Object,
) -> ThreadStore {
  case event, value.snowflake_field(data, "id") {
    "THREAD_MEMBERS_UPDATE", Some(thread_id) ->
      store.member_lists_changed(threads, thread_id)
    _, _ -> threads
  }
}

fn handle_thread_event(
  threads: ThreadStore,
  ctx: Context,
  event: String,
  data: Object,
) -> #(ThreadStore, List(Delivery)) {
  case event, value.snowflake_field(data, "id") {
    "THREAD_CREATE", Some(thread_id) ->
      thread_create(threads, ctx, data, thread_id)
    "THREAD_UPDATE", Some(thread_id) ->
      thread_update(threads, ctx, data, thread_id)
    "THREAD_DELETE", Some(thread_id) ->
      thread_delete(threads, ctx, data, thread_id)
    "THREAD_MEMBER_UPDATE", _ -> thread_member_update(threads, ctx, data)
    "THREAD_MEMBERS_UPDATE", Some(thread_id) ->
      thread_members_update(threads, ctx, data, thread_id)
    _, _ -> #(store.apply(threads, event, data, store.Live), [])
  }
}

fn thread_create(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
  thread_id: Int,
) -> #(ThreadStore, List(Delivery)) {
  let threads = store.apply(threads, "THREAD_CREATE", data, store.Live)
  let entry = thread_or_context(threads, thread_id, data)
  let initial = initial_members(data)
  let initial_set = set.from_list(initial)
  let base = public_payload(data)
  let #(recipients, _) =
    recipients(
      threads,
      ctx,
      entry,
      viewers.all(threads),
      access.new_memo(),
      fn(viewer, passive) {
        set.contains(initial_set, user_int(viewer))
        || !passive
        && interested(threads, viewer, thread_id)
      },
    )
  let deliveries =
    list.map(recipients, fn(viewer) {
      Delivery(
        "THREAD_CREATE",
        viewer.session,
        with_member(threads, base, thread_id, user_int(viewer)),
      )
    })
  metrics.dispatched("THREAD_CREATE", "viewer", list.length(deliveries))
  #(remember_create(threads, thread_id, initial), deliveries)
}

fn thread_update(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
  thread_id: Int,
) -> #(ThreadStore, List(Delivery)) {
  let was_stored = viewers.stored(threads, thread_id) != None
  let audience = viewers.all(threads)
  let before = thread_or_context(threads, thread_id, data)
  let #(before_recipients, memo) =
    recipients(
      threads,
      ctx,
      before,
      audience,
      access.new_memo(),
      joined_or_interested(threads, thread_id),
    )
  let next = store.apply(threads, "THREAD_UPDATE", data, store.Live)
  let after = thread_or_context(next, thread_id, data)
  let memo = case before, after {
    Some(old), Some(new) ->
      case thread.same_parent_public(old, new) {
        True -> memo
        False -> access.new_memo()
      }
    _, _ -> access.new_memo()
  }
  let #(after_recipients, after_memo) =
    recipients(
      next,
      ctx,
      after,
      audience,
      memo,
      joined_or_interested(next, thread_id),
    )
  let update_payload = public_payload(data)
  let deliveries =
    union(before_recipients, after_recipients)
    |> list.map(fn(viewer) {
      Delivery("THREAD_UPDATE", viewer.session, update_payload)
    })
  metrics.dispatched("THREAD_UPDATE", "viewer", list.length(deliveries))
  let member_deliveries = case was_stored, viewers.stored(next, thread_id) {
    False, Some(restored) -> {
      let #(targets, _) =
        recipients(
          next,
          ctx,
          Some(restored),
          audience,
          after_memo,
          fn(viewer, _) { is_member(next, viewer, thread_id) },
        )
      list.filter_map(targets, fn(viewer) {
        case rows.get_member(store.rows(next), thread_id, user_int(viewer)) {
          Some(member) ->
            Ok(Delivery(
              "THREAD_MEMBER_UPDATE",
              viewer.session,
              value.insert(
                thread.member_object(member),
                "guild_id",
                value.id_string(id.guild_id_to_int(ctx.guild)),
              ),
            ))
          None -> Error(Nil)
        }
      })
    }
    _, _ -> []
  }
  metrics.dispatched(
    "THREAD_MEMBER_UPDATE",
    "viewer",
    list.length(member_deliveries),
  )
  #(next, list.append(deliveries, member_deliveries))
}

fn thread_delete(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
  thread_id: Int,
) -> #(ThreadStore, List(Delivery)) {
  let entry = thread_or_context(threads, thread_id, data)
  let joined = joined_or_interested(threads, thread_id)
  let listed = set.from_list(value.snowflake_list(data, "_fluxer_member_ids"))
  let #(targets, _) =
    recipients(
      threads,
      ctx,
      entry,
      viewers.all(threads),
      access.new_memo(),
      fn(viewer, passive) {
        joined(viewer, passive) || set.contains(listed, user_int(viewer))
      },
    )
  let delete_payload = value.with(data, ["id", "guild_id", "parent_id", "type"])
  let next = store.apply(threads, "THREAD_DELETE", data, store.Live)
  let deliveries =
    list.map(targets, fn(viewer) {
      Delivery("THREAD_DELETE", viewer.session, delete_payload)
    })
  metrics.dispatched("THREAD_DELETE", "viewer", list.length(deliveries))
  #(next, deliveries)
}

fn thread_member_update(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
) -> #(ThreadStore, List(Delivery)) {
  let next = store.apply(threads, "THREAD_MEMBER_UPDATE", data, store.Live)
  let deliveries = member_update_deliveries(next, ctx, data, viewers.all(next))
  metrics.dispatched("THREAD_MEMBER_UPDATE", "viewer", list.length(deliveries))
  #(next, deliveries)
}

fn member_update_deliveries(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
  audience: List(Viewer),
) -> List(Delivery) {
  let thread_id = value.snowflake_field(data, "id")
  let target = value.snowflake_field(data, "user_id")
  let entry = case thread_id, target {
    Some(thread_id), Some(target) ->
      thread_or_context(threads, thread_id, data)
      |> option.lazy_or(fn() {
        case value.field(data, "_fluxer_parent_id"), id.channel_id(thread_id) {
          Some(parent), Ok(channel) ->
            Some(
              thread.context(channel, [#("parent_id", parent)], [
                target,
              ]),
            )
          _, _ -> None
        }
      })
    _, _ -> None
  }
  let member_payload = payload.strip_internal(data)
  case entry {
    None -> []
    Some(found) ->
      audience
      |> list.filter(fn(viewer) {
        Some(user_int(viewer)) == target
        && access.can_view(threads, ctx, viewer.session.user, found)
      })
      |> list.map(fn(viewer) {
        Delivery("THREAD_MEMBER_UPDATE", viewer.session, member_payload)
      })
  }
}

fn thread_members_update(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
  thread_id: Int,
) -> #(ThreadStore, List(Delivery)) {
  let removed = set.from_list(value.snowflake_list(data, "removed_member_ids"))
  let removed_viewers =
    removed_recipients(threads, ctx, data, thread_id, fn(viewer) {
      set.contains(removed, user_int(viewer))
    })
  let threads = store.apply(threads, "THREAD_MEMBERS_UPDATE", data, store.Live)
  let entry = thread_or_context(threads, thread_id, data)
  let added =
    value.object_list(data, "added_members")
    |> list.filter_map(fn(member) {
      value.snowflake_field(member, "user_id") |> option.to_result(Nil)
    })
    |> set.from_list
  let #(suppressed, threads) = take_recent_create(threads, thread_id)
  let audience = viewers.all(threads)
  let #(synthesized, memo) =
    synthesized_creates(
      threads,
      ctx,
      entry,
      thread_id,
      set.difference(added, set.from_list(suppressed)),
      audience,
    )
  let update_payload = members_update_payload(ctx, data)
  let #(added_viewers, memo) = case entry {
    None -> #(
      list.filter(audience, fn(viewer) { set.contains(added, user_int(viewer)) }),
      memo,
    )
    Some(_) ->
      recipients(threads, ctx, entry, audience, memo, fn(viewer, _) {
        set.contains(added, user_int(viewer))
      })
  }
  let affected = union(added_viewers, removed_viewers)
  let #(interested_viewers, _) =
    recipients(threads, ctx, entry, audience, memo, fn(viewer, passive) {
      !passive && interested(threads, viewer, thread_id)
    })
  let deliveries =
    union(affected, interested_viewers)
    |> list.map(fn(viewer) {
      Delivery("THREAD_MEMBERS_UPDATE", viewer.session, update_payload)
    })
  metrics.dispatched("THREAD_MEMBERS_UPDATE", "viewer", list.length(deliveries))
  #(threads, list.append(synthesized, deliveries))
}

pub fn catch_up() -> CatchUp {
  CatchUp(created: dict.new())
}

pub fn catch_up_event(
  threads: ThreadStore,
  ctx: Context,
  session_key: String,
  event: String,
  object: Object,
  seen: CatchUp,
) -> CatchUp {
  let object =
    value.insert(
      object,
      "guild_id",
      value.id_string(id.guild_id_to_int(ctx.guild)),
    )
  let seen = case event, value.snowflake_field(object, "id") {
    "THREAD_CREATE", Some(thread_id) ->
      case initial_members(object) {
        [] -> seen
        initial ->
          CatchUp(created: dict.insert(seen.created, thread_id, initial))
      }
    _, _ -> seen
  }
  case viewers.get(threads, session_key) {
    None -> seen
    Some(viewer) -> {
      let #(seen, deliveries) =
        catch_up_deliveries(threads, ctx, viewer, event, object, seen)
      deliver(ctx, deliveries)
      seen
    }
  }
}

fn catch_up_deliveries(
  threads: ThreadStore,
  ctx: Context,
  viewer: Viewer,
  event: String,
  data: Object,
  seen: CatchUp,
) -> #(CatchUp, List(Delivery)) {
  let audience = [viewer]
  let user = user_int(viewer)
  let pick = fn(entry, wants) {
    recipients(threads, ctx, entry, audience, access.new_memo(), wants).0
  }
  let send = fn(targets: List(Viewer), name: String, body: Object) {
    list.map(targets, fn(target) { Delivery(name, target.session, body) })
  }
  case event, value.snowflake_field(data, "id") {
    "THREAD_CREATE", Some(thread_id) -> {
      let initial = initial_members(data)
      let targets =
        pick(thread_or_context(threads, thread_id, data), fn(viewer, passive) {
          list.contains(initial, user)
          || !passive
          && interested(threads, viewer, thread_id)
        })
      let body = with_member(threads, public_payload(data), thread_id, user)
      #(seen, send(targets, "THREAD_CREATE", body))
    }
    "THREAD_UPDATE", Some(thread_id) -> {
      let targets =
        pick(
          thread_or_context(threads, thread_id, data),
          joined_or_interested(threads, thread_id),
        )
      #(seen, send(targets, "THREAD_UPDATE", public_payload(data)))
    }
    "THREAD_DELETE", Some(thread_id) -> {
      let listed = value.snowflake_list(data, "_fluxer_member_ids")
      let joined = joined_or_interested(threads, thread_id)
      let targets =
        pick(thread_or_context(threads, thread_id, data), fn(viewer, passive) {
          joined(viewer, passive) || list.contains(listed, user)
        })
      let body = value.with(data, ["id", "guild_id", "parent_id", "type"])
      #(seen, send(targets, "THREAD_DELETE", body))
    }
    "THREAD_MEMBER_UPDATE", _ -> #(
      seen,
      member_update_deliveries(threads, ctx, data, audience),
    )
    "THREAD_MEMBERS_UPDATE", Some(thread_id) -> {
      let removed = value.snowflake_list(data, "removed_member_ids")
      let added =
        value.object_list(data, "added_members")
        |> list.filter_map(fn(member) {
          value.snowflake_field(member, "user_id") |> option.to_result(Nil)
        })
      let suppressed = dict.get(seen.created, thread_id) |> result.unwrap([])
      let seen = CatchUp(created: dict.delete(seen.created, thread_id))
      let entry = thread_or_context(threads, thread_id, data)
      let is_user = fn(users: List(Int)) {
        fn(_: Viewer, _: Bool) { list.contains(users, user) }
      }
      let affected = case entry {
        None ->
          case list.contains(removed, user) || list.contains(added, user) {
            True -> audience
            False -> []
          }
        Some(_) -> pick(entry, is_user(list.append(removed, added)))
      }
      let interested_viewers =
        pick(entry, fn(viewer, passive) {
          !passive && interested(threads, viewer, thread_id)
        })
      let synthesized =
        synthesized_creates(
          threads,
          ctx,
          entry,
          thread_id,
          set.difference(set.from_list(added), set.from_list(suppressed)),
          audience,
        ).0
      let updates =
        send(
          union(affected, interested_viewers),
          "THREAD_MEMBERS_UPDATE",
          members_update_payload(ctx, data),
        )
      #(seen, list.append(synthesized, updates))
    }
    _, _ -> #(seen, [])
  }
}

fn initial_members(data: Object) -> List(Int) {
  value.object_list(data, "_fluxer_members")
  |> list.filter_map(fn(member) {
    value.snowflake_field(member, "user_id") |> option.to_result(Nil)
  })
}

fn removed_recipients(
  threads: ThreadStore,
  ctx: Context,
  data: Object,
  thread_id: Int,
  removed: fn(Viewer) -> Bool,
) -> List(Viewer) {
  let audience = viewers.all(threads)
  case thread_or_context(threads, thread_id, data) {
    None -> list.filter(audience, removed)
    entry ->
      recipients(
        threads,
        ctx,
        entry,
        audience,
        access.new_memo(),
        fn(viewer, _) { removed(viewer) },
      ).0
  }
}

fn synthesized_creates(
  threads: ThreadStore,
  ctx: Context,
  entry: Option(Thread),
  thread_id: Int,
  users: Set(Int),
  audience: List(Viewer),
) -> #(List(Delivery), Memo) {
  case entry, set.is_empty(users) {
    None, _ | _, True -> #([], access.new_memo())
    Some(found), False -> {
      let base = thread.payload(found)
      let #(targets, memo) =
        recipients(
          threads,
          ctx,
          entry,
          audience,
          access.new_memo(),
          fn(viewer, _) { set.contains(users, user_int(viewer)) },
        )
      let deliveries =
        list.map(targets, fn(viewer) {
          Delivery(
            "THREAD_CREATE",
            viewer.session,
            with_member(threads, base, thread_id, user_int(viewer)),
          )
        })
      metrics.dispatched("THREAD_CREATE", "viewer", list.length(deliveries))
      #(deliveries, memo)
    }
  }
}

fn members_update_payload(ctx: Context, data: Object) -> Object {
  let base = payload.strip_internal(data)
  case value.get(base, "added_members") {
    Ok(json_tree.Array(added)) ->
      value.insert(
        base,
        "added_members",
        json_tree.Array(
          value.objects(json_tree.Array(added))
          |> list.map(fn(member) {
            json_tree.Object(decorate_added(ctx, member))
          }),
        ),
      )
    _ -> base
  }
}

fn decorate_added(ctx: Context, member: Object) -> Object {
  let stripped = value.without(member, ["muted", "mute_config"])
  case
    value.snowflake_field(member, "user_id")
    |> option.then(fn(raw) { id.user_id(raw) |> option.from_result })
  {
    Some(user) ->
      stripped
      |> value.insert("member", subscriptions.guild_member(ctx, user))
      |> value.insert("presence", subscriptions.presence_value(ctx, user, True))
    None ->
      stripped
      |> value.insert("member", json_tree.Null)
      |> value.insert("presence", json_tree.Null)
  }
}

fn recipients(
  threads: ThreadStore,
  ctx: Context,
  entry: Option(Thread),
  audience: List(Viewer),
  memo: Memo,
  wants: fn(Viewer, Bool) -> Bool,
) -> #(List(Viewer), Memo) {
  case entry {
    None -> #([], memo)
    Some(found) -> {
      let #(picked, memo) =
        list.fold(audience, #([], memo), fn(acc, viewer) {
          let passive =
            !ctx.small_guild && ctx.passive(viewers.key(viewer.session))
          case wants(viewer, passive) {
            False -> acc
            True -> {
              let #(granted, memo) =
                access.view_access(
                  threads,
                  ctx,
                  viewer.session.user,
                  found,
                  None,
                  acc.1,
                )
              case granted {
                True -> #([viewer, ..acc.0], memo)
                False -> #(acc.0, memo)
              }
            }
          }
        })
      #(list.reverse(picked), memo)
    }
  }
}

fn joined_or_interested(
  threads: ThreadStore,
  thread_id: Int,
) -> fn(Viewer, Bool) -> Bool {
  fn(viewer, passive) {
    is_member(threads, viewer, thread_id)
    || !passive
    && interested(threads, viewer, thread_id)
  }
}

fn interested(threads: ThreadStore, viewer: Viewer, thread_id: Int) -> Bool {
  viewer.bot || is_member(threads, viewer, thread_id) || viewer.subscribed
}

fn is_member(threads: ThreadStore, viewer: Viewer, thread_id: Int) -> Bool {
  member_of(threads, viewer.session, thread_id)
}

fn member_of(threads: ThreadStore, ref: SessionRef, thread_id: Int) -> Bool {
  rows.is_member(store.rows(threads), thread_id, id.user_id_to_int(ref.user))
}

fn user_int(viewer: Viewer) -> Int {
  id.user_id_to_int(viewer.session.user)
}

fn union(a: List(Viewer), b: List(Viewer)) -> List(Viewer) {
  list.fold(list.append(a, b), dict.new(), fn(acc, viewer) {
    dict.insert(acc, viewers.key(viewer.session), viewer)
  })
  |> dict.values
}

fn thread_or_context(
  threads: ThreadStore,
  thread_id: Int,
  data: Object,
) -> Option(Thread) {
  use <- option.lazy_or(viewers.thread_or_context(threads, thread_id, data))
  case
    value.has(data, "parent_id"),
    value.has(data, "type"),
    id.channel_id(thread_id)
  {
    True, True, Ok(channel) ->
      Some(thread.context(
        channel,
        value.with(data, ["parent_id", "type"]),
        value.snowflake_list(data, "_fluxer_member_ids"),
      ))
    _, _, _ -> None
  }
}

fn public_payload(data: Object) -> Object {
  payload.strip_internal(data)
  |> value.without(["member", "member_ids_preview"])
}

fn with_member(
  threads: ThreadStore,
  base: Object,
  thread_id: Int,
  user: Int,
) -> Object {
  case rows.get_member(store.rows(threads), thread_id, user) {
    Some(member) ->
      value.insert(
        base,
        "member",
        json_tree.Object(thread.member_object(member)),
      )
    None -> base
  }
}

fn remember_create(
  threads: ThreadStore,
  thread_id: Int,
  initial: List(Int),
) -> ThreadStore {
  case initial {
    [] -> threads
    _ -> {
      let recent = store.recent(threads)
      let recent = case dict.size(recent) >= recent_create_cap {
        True -> dict.new()
        False -> recent
      }
      store.set_recent(threads, dict.insert(recent, thread_id, initial))
    }
  }
}

fn take_recent_create(
  threads: ThreadStore,
  thread_id: Int,
) -> #(List(Int), ThreadStore) {
  let recent = store.recent(threads)
  case dict.get(recent, thread_id) {
    Ok(initial) -> #(
      initial,
      store.set_recent(threads, dict.delete(recent, thread_id)),
    )
    Error(Nil) -> #([], threads)
  }
}

fn deliver(ctx: Context, deliveries: List(Delivery)) -> Nil {
  let grouped: Dict(Batch, List(SessionRef)) =
    list.fold(deliveries, dict.new(), fn(acc, delivery) {
      dict.upsert(
        acc,
        Batch(order(delivery.event), delivery.event, delivery.payload),
        fn(existing) { [delivery.to, ..option.unwrap(existing, [])] },
      )
    })
  dict.to_list(grouped)
  |> list.sort(fn(a, b) {
    let #(left, _) = a
    let #(right, _) = b
    int.compare(left.order, right.order)
  })
  |> list.each(fn(group) {
    let #(Batch(event:, payload: body, ..), targets) = group
    subscriptions.deliver(ctx, list.reverse(targets), event, value.to_raw(body))
  })
}

fn order(event: String) -> Int {
  case event {
    "THREAD_CREATE" -> 0
    "THREAD_UPDATE" | "THREAD_DELETE" -> 1
    "THREAD_MEMBER_UPDATE" -> 2
    "THREAD_MEMBERS_UPDATE" -> 3
    _ -> 4
  }
}

pub fn count_masked(event: String, count: Int) -> Nil {
  case payload.masks(event) {
    True -> metrics.dispatched(event, "masked", count)
    False -> Nil
  }
}
