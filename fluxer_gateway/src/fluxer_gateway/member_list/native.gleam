// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/member_list/page.{type Page}
import fluxer_gateway/perms/native.{type NifError, type PermDelta, type Perms}
import gleam/erlang/process.{type Pid}
import gleam/option.{type Option}

pub type MlHandle

pub type MlCounts

pub type MlConfig {
  Config(
    offline_threshold: Int,
    offline_drop: Int,
    work_budget_units: Int,
    fragment_budget_bytes: Int,
    payload_budget_bytes: Int,
    seed: Int,
  )
}

pub type Updates {
  UpdatesOn
  UpdatesOff
}

pub type MlSnapshot {
  Snapshot(
    members: List(#(Int, BitArray)),
    statuses: List(#(Int, Status)),
    connected: List(Int),
    updates: Updates,
  )
}

pub type Change {
  Changed
  Unchanged
}

pub type Timing {
  Immediate
  Deferred
}

pub type Connection {
  Connected(presence: Change)
  Disconnected
}

pub type SyncScope {
  AllLists
  ChannelList(channel: Int)
}

pub type MlEvent {
  MemberUpsert(user: Int, display_name: BitArray, change: Change)
  MemberRemove(user: Int)
  Presence(user: Int, status: Status, change: Change, timing: Timing)
  Connection(user: Int, connection: Connection)
  Subscribe(
    session: BitArray,
    subscriber: Int,
    user: Int,
    channel: Int,
    ranges: List(#(Int, Int)),
    sync: List(#(Int, Int)),
  )
  DropSession(session: BitArray)
  ViewGained(session: BitArray, channel: Int)
  Permissions(scope: SyncScope)
  Flush
  SetUpdates(updates: Updates)
}

pub type MlItem {
  EncodedItem(user: Int, list_bytes: BitArray, page_bytes: BitArray)
}

pub type MlTarget {
  ToSessions(subscribers: List(Int))
  ToSession(subscriber: Int)
}

pub type MlDispatch {
  Dispatch(target: MlTarget, payload: BitArray)
}

pub type TimerAction {
  KeepTimer
  ArmTimer
  CancelTimer
}

pub type MlEffects {
  Effects(dispatches: List(MlDispatch), timer: TimerAction, skipped_absent: Int)
}

pub type MlStep {
  Done(effects: MlEffects)
  PageDone(page: Page)
  Partial(effects: MlEffects)
  NeedItems(users: List(Int))
}

pub type MlQuery {
  SessionRanges(session: BitArray)
  ChannelCounts(channels: List(Int))
  PageIds(offset: Int, limit: Int)
  Subscriptions
}

pub type MlSubscription {
  Subscription(
    session: BitArray,
    subscriber: Int,
    user: Int,
    channel: Int,
    ranges: List(#(Int, Int)),
  )
}

pub type MlReply {
  RangesReply(found: Option(#(Int, List(#(Int, Int)))))
  CountsReply(counts: List(#(Int, Int, Int)))
  IdsReply(ids: PackedIds)
  SubscriptionsReply(subscriptions: List(MlSubscription))
}

pub type MaskUpdate {
  MaskUpdate(
    user: Int,
    all_messages: Bool,
    uncertain: Bool,
    muted: Bool,
    active: Bool,
  )
}

pub type MaskQuery {
  Supersets(epoch: Int)
}

pub type MaskReply {
  Masks(immediate: PackedIds, held: PackedIds)
  Stale
}

@external(erlang, "fluxer_gateway@nif", "ml_new")
pub fn ml_new(
  guild: Int,
  config: MlConfig,
  owner: Pid,
  perms: Perms,
) -> #(MlHandle, MlCounts)

@external(erlang, "fluxer_gateway@nif", "ml_load")
pub fn ml_load(handle: MlHandle, snapshot: MlSnapshot) -> Result(Nil, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_load_dirty")
pub fn ml_load_dirty(
  handle: MlHandle,
  snapshot: MlSnapshot,
) -> Result(Nil, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_apply")
pub fn ml_apply(
  handle: MlHandle,
  events: List(MlEvent),
  delta: Option(PermDelta),
) -> Result(MlStep, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_apply_dirty")
pub fn ml_apply_dirty(
  handle: MlHandle,
  events: List(MlEvent),
  delta: Option(PermDelta),
) -> Result(MlStep, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_fill")
pub fn ml_fill(
  handle: MlHandle,
  items: List(MlItem),
) -> Result(MlStep, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_resume")
pub fn ml_resume(handle: MlHandle) -> Result(MlStep, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_query")
pub fn ml_query(handle: MlHandle, query: MlQuery) -> Result(MlReply, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_query_dirty")
pub fn ml_query_dirty(
  handle: MlHandle,
  query: MlQuery,
) -> Result(MlReply, NifError)

@external(erlang, "fluxer_gateway@nif", "ml_counts")
pub fn ml_counts(counts: MlCounts) -> #(Int, Int)

@external(erlang, "fluxer_gateway@nif", "ml_release")
pub fn ml_release(handle: MlHandle) -> Nil

@external(erlang, "fluxer_gateway@nif", "push_masks_set")
pub fn push_masks_set(
  handle: MlHandle,
  updates: List(MaskUpdate),
) -> Result(Nil, NifError)

@external(erlang, "fluxer_gateway@nif", "push_masks_query")
pub fn push_masks_query(
  handle: MlHandle,
  query: MaskQuery,
) -> Result(MaskReply, NifError)
