// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{
  type ChannelId, type GuildId, type RoleId, type UserId,
}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/permissions.{type Permissions} as permission_bits
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/perms/audience.{type Audience, type Group, type Need} as perm_audience
import fluxer_gateway/perms/config as perm_config
import fluxer_gateway/perms/event.{type Effects, type Event}
import fluxer_gateway/perms/native
import fluxer_gateway/perms/query.{type Query, type Reply, type ReplyQuery} as perm_query
import fluxer_gateway/perms/snapshot.{type Snapshot} as perm_snapshot
import gleam/bit_array
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import logging

pub opaque type Perms {
  Perms(handle: native.Perms)
}

pub opaque type Reader {
  Reader(handle: native.Reader)
}

pub opaque type Model {
  Model(handle: native.ModelRef)
}

pub opaque type Delta {
  Delta(
    parts: List(native.PermDelta),
    epoch: Int,
    resync: event.Resync,
    regained: Bool,
  )
}

pub type PermsError {
  NotReady
  Broken
  Gone
  BadInput(reason: String)
}

pub type Scope {
  GuildScope
  ChannelScope(ChannelId)
}

const max_events = 64

const max_filter = 1000

const max_candidates = 4096

pub fn new(guild: GuildId, owner: Pid) -> #(Perms, Reader) {
  new_with(guild, owner, perm_config.default())
}

pub fn new_with(
  guild: GuildId,
  owner: Pid,
  tuning: perm_config.Config,
) -> #(Perms, Reader) {
  let #(handle, reader) =
    native.perm_new(
      id.guild_id_to_int(guild),
      perm_config.to_native(tuning),
      owner,
    )
  #(Perms(handle), Reader(reader))
}

pub fn load(perms: Perms, source: Snapshot) -> Result(Int, PermsError) {
  let native_snapshot = perm_snapshot.to_native(source)
  either(fn() { native.perm_load(perms.handle, native_snapshot) }, fn() {
    native.perm_load_dirty(perms.handle, native_snapshot)
  })
}

pub fn apply(perms: Perms, events: List(Event)) -> Result(Effects, PermsError) {
  let applied = case events {
    [] -> apply_chunk(perms, [])
    _ ->
      events
      |> list.map(event.to_native)
      |> list.sized_chunk(max_events)
      |> list.try_map(apply_chunk(perms, _))
      |> result.try(fn(parts) {
        list.reduce(parts, event.concat) |> result.replace_error(Broken)
      })
  }
  result.map(applied, compact_if_due(perms, _))
}

fn apply_chunk(
  perms: Perms,
  chunk: List(native.PermEvent),
) -> Result(Effects, PermsError) {
  use effects <- result.try(
    either(fn() { native.perm_apply(perms.handle, chunk) }, fn() {
      native.perm_apply_dirty(perms.handle, chunk)
    }),
  )
  event.from_native(effects, chunk) |> result.replace_error(Broken)
}

fn compact(perms: Perms) -> Result(Option(Effects), PermsError) {
  use compacted <- result.try(
    either(fn() { native.perm_compact(perms.handle) }, fn() {
      native.perm_compact_dirty(perms.handle)
    }),
  )
  case compacted {
    None -> Ok(None)
    Some(found) ->
      event.from_native(found, [])
      |> result.replace_error(Broken)
      |> result.map(Some)
  }
}

fn compact_if_due(perms: Perms, effects: Effects) -> Effects {
  case event.compaction_due(effects) {
    False -> effects
    True ->
      case compact(perms) {
        Ok(Some(compacted)) -> event.concat(effects, compacted)
        Ok(None) -> effects
        Error(reason) -> {
          logging.log(
            logging.Warning,
            "perms: role set compaction failed: " <> describe(reason),
          )
          effects
        }
      }
  }
}

pub fn model(reader: Reader) -> Model {
  Model(native.perm_model(reader.handle))
}

pub fn delta(effects: Effects) -> Delta {
  Delta(
    parts: event.deltas(effects),
    epoch: event.epoch(effects),
    resync: event.resync(effects),
    regained: list.any(event.changes(effects), fn(change) {
      change.gained != []
    }),
  )
}

pub fn permissions(
  reader: Reader,
  user: UserId,
  scope: Scope,
) -> Result(Permissions, PermsError) {
  native.perm_permissions(
    reader.handle,
    id.user_id_to_int(user),
    scope_to_native(scope),
  )
  |> normal
  |> result.try(to_permissions)
}

pub fn check(
  reader: Reader,
  user: UserId,
  scope: Scope,
  required: Permissions,
) -> Result(Bool, PermsError) {
  native.perm_check(
    reader.handle,
    id.user_id_to_int(user),
    scope_to_native(scope),
    permission_bits.to_int(required),
  )
  |> normal
}

pub fn can_view(
  reader: Reader,
  user: UserId,
  channel: ChannelId,
) -> Result(Bool, PermsError) {
  visible(reader, user, channel) |> result.map(pair.first)
}

pub fn can_view_members(
  reader: Reader,
  user: UserId,
  channel: ChannelId,
) -> Result(Bool, PermsError) {
  visible(reader, user, channel) |> result.map(pair.second)
}

fn visible(
  reader: Reader,
  user: UserId,
  channel: ChannelId,
) -> Result(#(Bool, Bool), PermsError) {
  native.perm_visible(
    reader.handle,
    id.user_id_to_int(user),
    id.channel_id_to_int(channel),
  )
  |> normal
}

pub fn has_virtual_view(
  reader: Reader,
  user: UserId,
  channel: ChannelId,
) -> Result(Bool, PermsError) {
  native.perm_has_virtual_view(
    reader.handle,
    id.user_id_to_int(user),
    id.channel_id_to_int(channel),
  )
  |> normal
}

pub fn viewable(
  reader: Reader,
  user: UserId,
) -> Result(List(ChannelId), PermsError) {
  native.perm_viewable(reader.handle, id.user_id_to_int(user))
  |> normal
  |> result.try(channel_ids)
}

pub fn shares_view_many(
  reader: Reader,
  user: UserId,
  candidates: PackedIds,
) -> Result(PackedIds, PermsError) {
  let user = id.user_id_to_int(user)
  candidates
  |> split(max_candidates)
  |> list.try_map(fn(chunk) {
    native.perm_shares_view_many(reader.handle, user, chunk) |> normal
  })
  |> result.map(join)
}

pub fn audience(
  reader: Reader,
  channel: ChannelId,
  need: Need,
) -> Result(Audience, PermsError) {
  native.perm_audience(
    reader.handle,
    id.channel_id_to_int(channel),
    perm_audience.to_native_need(need),
  )
  |> normal
  |> result.map(perm_audience.from_native)
}

pub fn viewers(
  reader: Reader,
  channel: ChannelId,
) -> Result(PackedIds, PermsError) {
  let channel = id.channel_id_to_int(channel)
  either(fn() { native.perm_viewers(reader.handle, channel) }, fn() {
    native.perm_viewers_dirty(reader.handle, channel)
  })
}

pub fn filter_viewers(
  reader: Reader,
  channel: ChannelId,
  users: PackedIds,
) -> Result(PackedIds, PermsError) {
  let channel = id.channel_id_to_int(channel)
  users
  |> split(max_filter)
  |> list.try_map(fn(chunk) {
    native.perm_filter_viewers(reader.handle, channel, chunk) |> normal
  })
  |> result.map(join)
}

pub fn thread_permissions(
  reader: Reader,
  user: UserId,
  parent: ChannelId,
  private: Bool,
  member: Bool,
  timed_out: Bool,
) -> Result(Permissions, PermsError) {
  native.perm_thread_permissions(
    reader.handle,
    id.user_id_to_int(user),
    id.channel_id_to_int(parent),
    private,
    member,
    timed_out,
  )
  |> normal
  |> result.try(to_permissions)
}

fn query(reader: Reader, query: Query) -> Result(Reply, PermsError) {
  let native_query = perm_query.to_native(query)
  either(fn() { native.perm_query(reader.handle, native_query) }, fn() {
    native.perm_query_dirty(reader.handle, native_query)
  })
  |> result.try(fn(reply) {
    perm_query.from_native(reply) |> result.replace_error(Broken)
  })
}

pub fn reply_json(
  reader: Reader,
  query: ReplyQuery,
) -> Result(RawJson, PermsError) {
  native.perm_reply(reader.handle, perm_query.reply_query_to_native(query))
  |> normal
  |> result.map(raw_json.wrap)
}

pub fn push_prune(
  model: Model,
  channel: ChannelId,
  message_ms: Int,
  mentioned: PackedIds,
  roles: List(RoleId),
  everyone: Bool,
) -> Result(#(PackedIds, PackedIds), PermsError) {
  let native_query =
    native.PushPrune(
      channel: id.channel_id_to_int(channel),
      message_ms:,
      mentioned:,
      roles: list.map(roles, id.role_id_to_int),
      everyone:,
    )
  use reply <- result.try(
    either(fn() { native.perm_query_at(model.handle, native_query) }, fn() {
      native.perm_query_at_dirty(model.handle, native_query)
    }),
  )
  case reply {
    native.Pruned(immediate, held) -> Ok(#(immediate, held))
    _ -> Error(Broken)
  }
}

pub fn group_of(
  reader: Reader,
  user: UserId,
) -> Result(Option(Group), PermsError) {
  use reply <- result.try(query(reader, perm_query.GroupOf(user)))
  case reply {
    perm_query.GroupReply(group) -> Ok(group)
    _ -> Error(Broken)
  }
}

pub fn visitor_channels(
  reader: Reader,
  adult: Bool,
) -> Result(List(#(ChannelId, Permissions)), PermsError) {
  use reply <- result.try(query(reader, perm_query.VisitorChannels(adult)))
  case reply {
    perm_query.ChannelGrants(grants) -> Ok(grants)
    _ -> Error(Broken)
  }
}

pub fn public_online(
  reader: Reader,
  online: PackedIds,
) -> Result(perm_query.PublicOnline, PermsError) {
  use reply <- result.try(query(reader, perm_query.PublicOnline(online)))
  case reply {
    perm_query.PublicOnlineReply(found) -> Ok(found)
    _ -> Error(Broken)
  }
}

pub fn mutual_online(
  reader: Reader,
  requester: UserId,
  online: PackedIds,
) -> Result(perm_query.MutualOnline, PermsError) {
  use reply <- result.try(query(
    reader,
    perm_query.MutualOnline(requester, online),
  ))
  case reply {
    perm_query.MutualOnlineReply(found) -> Ok(found)
    _ -> Error(Broken)
  }
}

pub fn epoch(reader: Reader) -> Int {
  native.perm_epoch(reader.handle)
}

pub fn revision(reader: Reader) -> Int {
  native.perm_revision(reader.handle)
}

pub fn status(perms: Perms) -> Result(Nil, PermsError) {
  native.perm_status(perms.handle) |> normal
}

pub fn release(perms: Perms) -> Nil {
  native.perm_release(perms.handle)
}

@internal
pub fn native_handle(perms: Perms) -> native.Perms {
  perms.handle
}

@internal
pub fn native_deltas(delta: Delta) -> List(native.PermDelta) {
  delta.parts
}

@internal
pub fn delta_epoch(delta: Delta) -> Int {
  delta.epoch
}

@internal
pub fn delta_resync(delta: Delta) -> event.Resync {
  delta.resync
}

@internal
pub fn delta_regained(delta: Delta) -> Bool {
  delta.regained
}

fn scope_to_native(scope: Scope) -> native.Scope {
  case scope {
    GuildScope -> native.GuildScope
    ChannelScope(channel) -> native.ChannelScope(id.channel_id_to_int(channel))
  }
}

fn either(
  normal_call: fn() -> Result(a, native.NifError),
  dirty_call: fn() -> Result(a, native.NifError),
) -> Result(a, PermsError) {
  case normal_call() {
    Error(native.NeedsDirty) -> dirty_call() |> normal
    other -> normal(other)
  }
}

fn normal(found: Result(a, native.NifError)) -> Result(a, PermsError) {
  result.map_error(found, from_native_error)
}

fn describe(error: PermsError) -> String {
  case error {
    NotReady -> "over budget"
    Broken -> "broken"
    Gone -> "released"
    BadInput(reason) -> "bad input: " <> reason
  }
}

fn from_native_error(error: native.NifError) -> PermsError {
  case error {
    native.NeedsDirty -> NotReady
    native.Broken -> Broken
    native.Gone -> Gone
    native.BadInput(reason) -> BadInput(reason)
  }
}

fn to_permissions(bits: Int) -> Result(Permissions, PermsError) {
  permission_bits.from_int(bits) |> result.replace_error(Broken)
}

fn channel_ids(ids: PackedIds) -> Result(List(ChannelId), PermsError) {
  ids
  |> packed_ids.to_list
  |> perm_query.channel_ids
  |> result.replace_error(Broken)
}

fn split(ids: PackedIds, size: Int) -> List(PackedIds) {
  split_bits(packed_ids.to_bits(ids), size * 8, [])
}

fn split_bits(
  bits: BitArray,
  bytes: Int,
  acc: List(PackedIds),
) -> List(PackedIds) {
  case bit_array.byte_size(bits) > bytes {
    False -> list.reverse([packed(bits), ..acc])
    True -> {
      let head = bit_array.slice(bits, 0, bytes) |> result.unwrap(<<>>)
      let rest =
        bit_array.slice(bits, bytes, bit_array.byte_size(bits) - bytes)
        |> result.unwrap(<<>>)
      split_bits(rest, bytes, [packed(head), ..acc])
    }
  }
}

fn packed(bits: BitArray) -> PackedIds {
  packed_ids.from_bits(bits) |> result.unwrap(packed_ids.empty())
}

fn join(parts: List(PackedIds)) -> PackedIds {
  parts
  |> list.map(packed_ids.to_bits)
  |> bit_array.concat
  |> packed
}
