// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/threads/sync_queue.{type SyncQueue}
import fluxer_gateway/threads/value.{type Object}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Timer}
import gleam/option.{type Option, None}
import gleam/set.{type Set}

pub type Load {
  Ready
  Loading(
    queue: List(#(String, Object)),
    queued: Int,
    attempt: Int,
    request: Option(Int),
    timer: Option(Timer),
  )
}

pub type Viewer {
  Viewer(
    session: SessionRef,
    bot: Bool,
    subscribed: Bool,
    member_lists: List(Int),
  )
}

pub type SyncMark {
  Scheduled
  Queued
}

pub type Bucket {
  Bucket(tokens: Float, last_ms: Int)
}

pub type Subs {
  Subs(
    sync_pending: Dict(String, SyncMark),
    sync_queue: SyncQueue,
    bucket: Option(Bucket),
    drain_armed: Bool,
    list_dirty: Set(Int),
    list_refresh: Bool,
    flush_armed: Bool,
    presence_users: Dict(Int, List(Int)),
    list_sessions: Dict(Int, List(String)),
  )
}

pub type Effect {
  ForumLastMessage(parent: ChannelId, thread: ChannelId)
  MergeParentSettings(settings: List(#(ChannelId, RawJson)))
}

pub fn no_subs() -> Subs {
  Subs(
    sync_pending: dict.new(),
    sync_queue: sync_queue.new(),
    bucket: None,
    drain_armed: False,
    list_dirty: set.new(),
    list_refresh: False,
    flush_armed: False,
    presence_users: dict.new(),
    list_sessions: dict.new(),
  )
}
