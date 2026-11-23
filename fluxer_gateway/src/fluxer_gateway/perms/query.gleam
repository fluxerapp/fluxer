// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/perms/audience.{type Group}
import fluxer_gateway/perms/native
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Query {
  VisitorChannels(adult: Bool)
  PublicOnline(online: PackedIds)
  MutualOnline(requester: UserId, online: PackedIds)
  GroupOf(user: UserId)
}

pub type PublicOnline {
  OpenAccess
  Restricted(count: Int)
  NoPublicChannel
}

pub type MutualOnline {
  SeesEveryone
  Mutual(count: Int)
}

pub type Reply {
  ChannelGrants(grants: List(#(ChannelId, Permissions)))
  PublicOnlineReply(online: PublicOnline)
  MutualOnlineReply(online: MutualOnline)
  GroupReply(group: Option(Group))
}

pub type ReplyQuery {
  UserPermissions(user: UserId, channel: Option(ChannelId))
  CheckPermission(
    user: UserId,
    channel: Option(ChannelId),
    required: Permissions,
  )
  ViewableChannels(user: UserId)
}

@internal
pub fn scope_to_native(channel: Option(ChannelId)) -> native.Scope {
  case channel {
    None -> native.GuildScope
    Some(channel) -> native.ChannelScope(id.channel_id_to_int(channel))
  }
}

@internal
pub fn to_native(query: Query) -> native.PermQuery {
  case query {
    VisitorChannels(adult) -> native.VisitorChannels(adult)
    PublicOnline(online) -> native.PublicOnline(online)
    MutualOnline(requester, online) ->
      native.MutualOnline(id.user_id_to_int(requester), online)
    GroupOf(user) -> native.GroupOf(id.user_id_to_int(user))
  }
}

@internal
pub fn reply_query_to_native(query: ReplyQuery) -> native.ReplyQuery {
  case query {
    UserPermissions(user, channel) ->
      native.UserPermissions(id.user_id_to_int(user), scope_to_native(channel))
    CheckPermission(user, channel, required) ->
      native.CheckPermission(
        id.user_id_to_int(user),
        scope_to_native(channel),
        permissions.to_int(required),
      )
    ViewableChannels(user) -> native.ViewableChannels(id.user_id_to_int(user))
  }
}

@internal
pub fn from_native(reply: native.PermReply) -> Result(Reply, Nil) {
  case reply {
    native.ChannelGrants(grants) ->
      grants
      |> list.try_map(grant)
      |> result.map(ChannelGrants)
    native.PublicOpen -> Ok(PublicOnlineReply(OpenAccess))
    native.PublicEmpty -> Ok(PublicOnlineReply(NoPublicChannel))
    native.PublicCount(count) -> Ok(PublicOnlineReply(Restricted(count)))
    native.MutualAll -> Ok(MutualOnlineReply(SeesEveryone))
    native.MutualCount(count) -> Ok(MutualOnlineReply(Mutual(count)))
    native.GroupReply(group) ->
      Ok(GroupReply(option.map(group, audience.group_from_native)))
    native.Pruned(_, _) -> Error(Nil)
  }
}

fn grant(found: #(Int, Int)) -> Result(#(ChannelId, Permissions), Nil) {
  let #(channel, bits) = found
  use channel <- result.try(channel_id(channel))
  use granted <- result.try(
    permissions.from_int(bits) |> result.replace_error(Nil),
  )
  Ok(#(channel, granted))
}

@internal
pub fn channel_ids(ids: List(Int)) -> Result(List(ChannelId), Nil) {
  list.try_map(ids, channel_id)
}

fn channel_id(value: Int) -> Result(ChannelId, Nil) {
  id.channel_id(value) |> result.replace_error(Nil)
}
