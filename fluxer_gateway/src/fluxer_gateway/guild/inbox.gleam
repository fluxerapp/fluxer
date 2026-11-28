// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildRef}
import fluxer_gateway/core/id.{type ChannelId, type RoleId, type UserId}
import fluxer_gateway/guild/data.{type Loaded}
import fluxer_gateway/guild/handoff/transfer.{type Transfer}
import fluxer_gateway/telemetry/memory_stats.{type GuildDescription}
import fluxer_gateway/voice/guild_handler.{type VoiceRequest}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json.{type Json}
import gleam/option.{type Option}

pub type Handle {
  Handle(ref: GuildRef, control: Subject(Control), pid: Pid)
}

pub type MentionPage {
  MentionPage(
    channel: ChannelId,
    author: UserId,
    mention_everyone: Bool,
    mention_here: Bool,
    roles: List(RoleId),
    users: List(UserId),
    limit: Int,
    cursor: Option(Int),
  )
}

pub type Query {
  GetGuildData(user: Option(UserId))
  GetAuthContext(user: Option(UserId), channel: Option(ChannelId))
  GetMember(user: UserId)
  HasMember(user: UserId)
  ListMembers(limit: Int, offset: Int)
  ListMembersCursor(limit: Int, after: Option(UserId))
  MembersWithRole(role: RoleId)
  CheckTargetMember(user: UserId, target: UserId)
  CanManageRoles(user: UserId, role: RoleId)
  CanManageRole(user: UserId, role: RoleId)
  ViewableChannels(user: UserId)
  ChannelMentions(channels: List(ChannelId))
  FirstViewableTextChannel
  CategoryChannelCount(category: ChannelId)
  ChannelCount
  MentionSources(page: MentionPage)
  Counts
  EvictVisitors(ip_keys: List(String))
}

pub type QueryError {
  QueryFailed(code: String)
  Forbidden
}

pub type QueryReply =
  Result(Json, QueryError)

pub type Control {
  Dispatch(event: String, data: Dynamic)
  DispatchMany(events: List(#(String, Dynamic)))
  Replay(events: List(#(String, Dynamic)))
  Ask(query: Query, deadline_ms: Int, reply: Subject(QueryReply))
  AskThreadBits(
    user: UserId,
    thread: ChannelId,
    deadline_ms: Int,
    reply: Subject(Result(Int, QueryError)),
  )
  Reload(loaded: Loaded, reply: Subject(Nil))
  Shutdown(reply: Subject(Nil))
  Freeze(owners: List(Pid), reply: Subject(Result(Transfer, Nil)))
  Unfreeze
  Moved(reply: Subject(List(#(String, Dynamic))))
  Disown
  ResyncSessions(reply: Subject(Int))
  HealthProbe(token: Int, pong: fn(Int, Int) -> Nil)
  VoiceRpc(request: VoiceRequest)
  FlushMemberList
  Describe(reply: Subject(GuildDescription))
}
