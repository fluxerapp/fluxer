// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild_create.{type GuildCreate}
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/session.{
  type ClientOp, type IdentifyFlags, type SessionId, type SessionRef,
}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option}

pub type GuildRef {
  GuildRef(guild: GuildId, inbox: Subject(GuildRequest))
}

pub type JoinClass {
  Resume
  Focused
  Background
}

pub type JoinMode {
  AsMember
  AsVisitor(
    operator: Bool,
    adult_content_allowed: Bool,
    tier: String,
    ip_key: Option(String),
  )
}

pub type JoinRequest {
  JoinRequest(
    session: SessionRef,
    flags: IdentifyFlags,
    class: JoinClass,
    mode: JoinMode,
    presence: Presence,
    bot: Bool,
    staff: Bool,
    remaining_ms: Int,
    attempt: Int,
  )
}

pub type JoinReply {
  Joined(guild: GuildRef, guild_create: GuildCreate, attempt: Int)
  JoinOverloaded(retry_after_ms: Int, attempt: Int)
  JoinUnavailable(retry_after_ms: Int, attempt: Int)
  NotMember(attempt: Int)
  JoinNotEligible(attempt: Int)
  VisitRejected(attempt: Int)
}

pub type GuildRequest {
  Join(request: JoinRequest, reply: Subject(JoinReply))
  Leave(session: SessionId, pid: Pid)
  SessionOp(session: SessionRef, op: ClientOp)
  SessionPushHold(session: SessionId, hold: Bool)
  MemberPresence(presence: Presence)
  MemberUserUpdate(user_wire: BitArray)
  UpdateDmPartners(session: SessionId, partners: List(Int))
  CountsQuery(user: UserId, reply: Subject(GuildCounts))
  DropMemberLists(session: SessionId)
  SessionMoved(session: SessionRef, from: SessionRef)
}

pub type GuildCounts {
  GuildCounts(guild: GuildId, member_count: Int, online_count: Int)
}

pub type NsfwLevel {
  NsfwDefault
  NsfwExplicit
  NsfwSafe
  NsfwAgeRestricted
}

pub opaque type GuildFacts {
  GuildFacts(
    id: GuildId,
    owner: Option(UserId),
    nsfw_level: NsfwLevel,
    features: List(String),
    member_count: Int,
    unavailable: Bool,
  )
}

pub fn facts(
  id id: GuildId,
  owner owner: Option(UserId),
  nsfw_level nsfw_level: NsfwLevel,
  features features: List(String),
  member_count member_count: Int,
) -> GuildFacts {
  GuildFacts(
    id:,
    owner:,
    nsfw_level:,
    features:,
    member_count:,
    unavailable: False,
  )
}

pub fn nsfw_level_from_int(level: Int) -> NsfwLevel {
  case level {
    1 -> NsfwExplicit
    2 -> NsfwSafe
    3 -> NsfwAgeRestricted
    _ -> NsfwDefault
  }
}

pub fn id(facts: GuildFacts) -> GuildId {
  facts.id
}

pub fn owner(facts: GuildFacts) -> Option(UserId) {
  facts.owner
}

pub fn nsfw_level(facts: GuildFacts) -> NsfwLevel {
  facts.nsfw_level
}

pub fn features(facts: GuildFacts) -> List(String) {
  facts.features
}

pub fn has_feature(facts: GuildFacts, feature: String) -> Bool {
  list.contains(facts.features, feature)
}

pub fn member_count(facts: GuildFacts) -> Int {
  facts.member_count
}

pub fn is_unavailable(facts: GuildFacts) -> Bool {
  facts.unavailable
}

pub fn set_member_count(facts: GuildFacts, count: Int) -> GuildFacts {
  GuildFacts(..facts, member_count: count)
}

pub fn set_unavailable(facts: GuildFacts, unavailable: Bool) -> GuildFacts {
  GuildFacts(..facts, unavailable:)
}

pub fn send(guild: GuildRef, request: GuildRequest) -> Nil {
  process.send(guild.inbox, request)
}
