// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Nsfw}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/role.{type Hoist, type RankPosition}
import fluxer_gateway/perms/cutoff.{type Cutoff}
import fluxer_gateway/perms/role_list.{type RoleList}
import gleam/erlang/process.{type Pid}
import gleam/option.{type Option}

pub type Perms

pub type Reader

pub type ModelRef

pub type PermDelta

pub type NifError {
  NeedsDirty
  Broken
  Gone
  BadInput(reason: String)
}

pub type Config {
  Config(
    work_budget_units: Int,
    viewable_cache_bytes: Int,
    dead_role_set_floor: Int,
    scan_chunk: Int,
  )
}

pub type Scope {
  GuildScope
  ChannelScope(channel: Int)
}

pub type Need {
  View
  ViewMembers
  MessageAccess(message_ms: Int)
  ChannelBits(bits: Int)
}

pub type Group {
  RoleSetGroup(role_set: Int)
  UserGroup(user: Int)
}

pub type Target {
  GroupTarget(group: Group)
  OneUser(user: Int)
}

pub type Audience {
  Audience(epoch: Int, role_sets: BitArray, users: PackedIds)
}

pub type Regroup {
  Regroup(user: Int, old: Option(Group), new: Option(Group))
}

pub type VisibilityChange {
  VisibilityChange(target: Target, lost: List(Int), gained: List(Int))
}

pub type PermEffects {
  Effects(
    epoch: Int,
    before: ModelRef,
    regroups: List(Regroup),
    changes: List(VisibilityChange),
    delta: PermDelta,
    compaction_due: Bool,
    renumbered: BitArray,
  )
}

pub type PermRole {
  Role(id: Int, permissions: Int, position: RankPosition, hoist: Hoist)
}

pub type ChannelKind {
  Text
  Voice
  Category
  Announcement
  Forum
  Media
  ThreadKind(code: Int)
  OtherKind(code: Int)
}

pub type OverwriteTarget {
  RoleTarget(role: Int)
  MemberTarget(user: Int)
}

pub type PermOverwrite {
  Overwrite(target: OverwriteTarget, allow: Int, deny: Int)
}

pub type PermChannel {
  Channel(
    id: Int,
    kind: ChannelKind,
    parent: Option(Int),
    overwrites: List(PermOverwrite),
    nsfw: Nsfw,
    named: Bool,
  )
}

pub type ChannelCause {
  Created
  Updated
}

pub type PermEvent {
  Roles(roles: List(PermRole))
  Owner(user: Option(Int))
  ChannelUpsert(channel: PermChannel, cause: ChannelCause)
  ChannelDelete(channel: Int)
  ChannelOrder(channels: List(Int))
  MemberUpsert(user: Int, roles: RoleList)
  MemberRemove(user: Int)
  VirtualAccess(user: Int, view: List(Int))
  HistoryCutoff(cutoff: Cutoff)
  GuildNsfw(nsfw: Option(Bool), level: Int)
}

pub type PermSnapshot {
  Snapshot(guild: Int, events: List(PermEvent))
}

pub type PermQuery {
  VisitorChannels(adult: Bool)
  PublicOnline(users: PackedIds)
  MutualOnline(requester: Int, users: PackedIds)
  GroupOf(user: Int)
  PushPrune(
    channel: Int,
    message_ms: Int,
    mentioned: PackedIds,
    roles: List(Int),
    everyone: Bool,
  )
}

pub type PermReply {
  ChannelGrants(grants: List(#(Int, Int)))
  PublicOpen
  PublicEmpty
  PublicCount(count: Int)
  MutualAll
  MutualCount(count: Int)
  GroupReply(group: Option(Group))
  Pruned(immediate: PackedIds, held: PackedIds)
}

pub type ReplyQuery {
  UserPermissions(user: Int, scope: Scope)
  CheckPermission(user: Int, scope: Scope, bits: Int)
  ViewableChannels(user: Int)
}

@external(erlang, "fluxer_gateway@nif", "perm_new")
pub fn perm_new(guild: Int, config: Config, owner: Pid) -> #(Perms, Reader)

@external(erlang, "fluxer_gateway@nif", "perm_load")
pub fn perm_load(perms: Perms, snapshot: PermSnapshot) -> Result(Int, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_load_dirty")
pub fn perm_load_dirty(
  perms: Perms,
  snapshot: PermSnapshot,
) -> Result(Int, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_apply")
pub fn perm_apply(
  perms: Perms,
  events: List(PermEvent),
) -> Result(PermEffects, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_apply_dirty")
pub fn perm_apply_dirty(
  perms: Perms,
  events: List(PermEvent),
) -> Result(PermEffects, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_compact")
pub fn perm_compact(perms: Perms) -> Result(Option(PermEffects), NifError)

@external(erlang, "fluxer_gateway@nif", "perm_compact_dirty")
pub fn perm_compact_dirty(perms: Perms) -> Result(Option(PermEffects), NifError)

@external(erlang, "fluxer_gateway@nif", "perm_model")
pub fn perm_model(reader: Reader) -> ModelRef

@external(erlang, "fluxer_gateway@nif", "perm_permissions")
pub fn perm_permissions(
  reader: Reader,
  user: Int,
  scope: Scope,
) -> Result(Int, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_check")
pub fn perm_check(
  reader: Reader,
  user: Int,
  scope: Scope,
  bits: Int,
) -> Result(Bool, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_visible")
pub fn perm_visible(
  reader: Reader,
  user: Int,
  channel: Int,
) -> Result(#(Bool, Bool), NifError)

@external(erlang, "fluxer_gateway@nif", "perm_has_virtual_view")
pub fn perm_has_virtual_view(
  reader: Reader,
  user: Int,
  channel: Int,
) -> Result(Bool, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_viewable")
pub fn perm_viewable(reader: Reader, user: Int) -> Result(PackedIds, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_shares_view_many")
pub fn perm_shares_view_many(
  reader: Reader,
  user: Int,
  candidates: PackedIds,
) -> Result(PackedIds, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_audience")
pub fn perm_audience(
  reader: Reader,
  channel: Int,
  need: Need,
) -> Result(Audience, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_viewers")
pub fn perm_viewers(reader: Reader, channel: Int) -> Result(PackedIds, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_viewers_dirty")
pub fn perm_viewers_dirty(
  reader: Reader,
  channel: Int,
) -> Result(PackedIds, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_filter_viewers")
pub fn perm_filter_viewers(
  reader: Reader,
  channel: Int,
  users: PackedIds,
) -> Result(PackedIds, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_thread_permissions")
pub fn perm_thread_permissions(
  reader: Reader,
  user: Int,
  parent: Int,
  private: Bool,
  member: Bool,
  timed_out: Bool,
) -> Result(Int, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_query")
pub fn perm_query(
  reader: Reader,
  query: PermQuery,
) -> Result(PermReply, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_query_dirty")
pub fn perm_query_dirty(
  reader: Reader,
  query: PermQuery,
) -> Result(PermReply, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_query")
pub fn perm_query_at(
  model: ModelRef,
  query: PermQuery,
) -> Result(PermReply, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_query_dirty")
pub fn perm_query_at_dirty(
  model: ModelRef,
  query: PermQuery,
) -> Result(PermReply, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_reply")
pub fn perm_reply(
  reader: Reader,
  query: ReplyQuery,
) -> Result(BitArray, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_epoch")
pub fn perm_epoch(reader: Reader) -> Int

@external(erlang, "fluxer_gateway@nif", "perm_revision")
pub fn perm_revision(reader: Reader) -> Int

@external(erlang, "fluxer_gateway@nif", "perm_status")
pub fn perm_status(perms: Perms) -> Result(Nil, NifError)

@external(erlang, "fluxer_gateway@nif", "perm_release")
pub fn perm_release(perms: Perms) -> Nil
