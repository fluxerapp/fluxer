// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/member
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/snowflake
import fluxer_gateway/perms
import fluxer_gateway/threads/context.{type Context}
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/threads/thread.{type Thread}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const max_memo_entries = 8192

pub type Memo =
  Dict(Int, Bool)

pub fn new_memo() -> Memo {
  dict.new()
}

pub fn thread_permissions(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  entry: Thread,
) -> Permissions {
  let parent = case thread.parent(entry) {
    Some(parent) ->
      case ctx.channel(parent) {
        Some(_) -> Some(parent)
        None -> None
      }
    None -> None
  }
  case parent {
    None -> {
      metrics.unknown_parent()
      permissions.none()
    }
    Some(parent) -> {
      let timed_out = case ctx.member(user) {
        Some(found) -> member.is_timed_out(found, now_ms: clock.system_ms())
        None -> False
      }
      perms.thread_permissions(
        ctx.reader,
        user,
        parent,
        thread.is_private(entry),
        is_thread_member(threads, entry, user),
        timed_out,
      )
      |> result.unwrap(permissions.none())
    }
  }
}

fn is_thread_member(threads: ThreadStore, entry: Thread, user: UserId) -> Bool {
  let user = id.user_id_to_int(user)
  let table = store.rows(threads)
  case rows.has_thread(table, thread.id_int(entry)) {
    True -> rows.is_member(table, thread.id_int(entry), user)
    False -> list.contains(thread.listed_members(entry), user)
  }
}

pub fn can_view(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  entry: Thread,
) -> Bool {
  permissions.has(
    thread_permissions(threads, ctx, user, entry),
    permissions.ViewChannel,
  )
}

pub fn message_readable(
  granted: Permissions,
  message: Option(Int),
  cutoff_ms: Option(Int),
) -> Bool {
  case message {
    None -> True
    Some(message) ->
      permissions.has(granted, permissions.ReadMessageHistory)
      || case cutoff_ms {
        Some(cutoff) -> snowflake.timestamp_ms(message) >= cutoff
        None -> False
      }
  }
}

pub fn view_access(
  threads: ThreadStore,
  ctx: Context,
  user: UserId,
  entry: Thread,
  message: Option(Int),
  memo: Memo,
) -> #(Bool, Memo) {
  let key = id.user_id_to_int(user)
  case dict.get(memo, key) {
    Ok(known) -> #(known, memo)
    Error(Nil) -> {
      let granted = thread_permissions(threads, ctx, user, entry)
      let visible =
        permissions.has(granted, permissions.ViewChannel)
        && message_readable(granted, message, ctx.history_cutoff_ms)
      let memo = case dict.size(memo) < max_memo_entries {
        True -> dict.insert(memo, key, visible)
        False -> memo
      }
      #(visible, memo)
    }
  }
}
