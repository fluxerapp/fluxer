// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/permissions
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/core/snowflake
import fluxer_gateway/core/user as core_user
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/guild/push/engagement.{type Snapshot}
import fluxer_gateway/guild/push/guild_info.{type GuildInfo}
import fluxer_gateway/guild/push/masks
import fluxer_gateway/guild/push/profiles.{type Profiles}
import fluxer_gateway/guild/push/state.{type GuildMessage}
import fluxer_gateway/guild/push/thread_scope.{type ThreadScope}
import fluxer_gateway/member_list.{type MemberList}
import fluxer_gateway/perms
import fluxer_gateway/push/format
import fluxer_gateway/push/markdown.{type PreviewContext}
import fluxer_gateway/push/message.{type MessageCreate, type MessageData}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/rule
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub const narrow_limit = 50

const view_chunk = 1000

const max_format_members = 50

pub type Capture {
  Capture(
    guild: GuildId,
    data: MessageData,
    channel: Int,
    message: Int,
    author: Int,
    thread: Option(ThreadScope),
    snapshot: Snapshot,
    reader: perms.Reader,
    model: perms.Model,
    members: MembersTable,
    list: MemberList,
    roles: List(Role),
    facts: core_guild.GuildFacts,
    info: GuildInfo,
    profiles: Profiles,
    inbox: Subject(GuildMessage),
  )
}

pub type Plan {
  Nothing
  Explicit(users: List(Int))
  AllMembers
}

pub type Unavailable {
  PermsUnavailable
  MembersUnavailable
}

pub type Routed {
  Routed(
    eligible: List(Int),
    held: List(#(Int, List(SessionRef))),
    roles: Dict(Int, List(Int)),
  )
}

pub fn guild_int(capture: Capture) -> Int {
  id.guild_id_to_int(capture.guild)
}

pub fn is_large(capture: Capture) -> Bool {
  message.is_large(Some(large_guild(capture)))
}

pub fn canonical(capture: Capture) -> rule.Shape {
  rule.shape(
    data: message.empty_data(),
    guild: guild_int(capture),
    channel: 0,
    author: 0,
    guild_default_level: capture.info.default_level,
    large: Some(large_guild(capture)),
  )
}

fn large_guild(capture: Capture) -> message.LargeGuild {
  message.large_guild(
    core_guild.member_count(capture.facts),
    core_guild.features(capture.facts),
  )
}

fn mention_roles(data: MessageData) -> Set(Int) {
  set.from_list(data.mention_roles)
}

pub fn format_members(data: MessageData) -> List(Int) {
  let author = case data.author.id {
    Some(author) -> [author]
    None -> []
  }
  let mentioned =
    data.mentions
    |> list.filter_map(fn(mention) { option.to_result(mention.id, Nil) })
    |> list.unique
    |> list.filter(fn(user) { !list.contains(author, user) })
    |> list.take(max_format_members)
  list.append(author, mentioned)
}

fn member_of(capture: Capture, user: Int) -> Option(Member) {
  id.user_id(user)
  |> option.from_result
  |> option.then(members.get(capture.members, _))
}

fn role_ints(found: Member) -> List(Int) {
  list.map(member.roles(found), id.role_id_to_int)
}

fn holds_mentioned_role(found: Member, roles: Set(Int)) -> Bool {
  list.any(role_ints(found), fn(role) { set.contains(roles, role) })
}

fn is_bot(found: Member) -> Bool {
  core_user.is_bot(member.user(found))
}

pub fn plan(capture: Capture) -> Plan {
  let direct = message.direct_mention_ids(capture.data)
  case capture.thread, is_large(capture) {
    Some(scope), _ -> thread_plan(capture, scope, direct)
    None, True -> large_plan(capture, direct)
    None, False -> small_plan(capture, direct)
  }
}

fn large_plan(capture: Capture, direct: Set(Int)) -> Plan {
  let data = capture.data
  case
    set.is_empty(direct)
    && data.mention_roles == []
    && !data.mention_everyone
    && !data.mention_here
  {
    True -> Nothing
    False -> pruned_plan(capture, direct, everyone_only(data))
  }
}

fn small_plan(capture: Capture, direct: Set(Int)) -> Plan {
  case masks.supersets(capture.list, perms.epoch(capture.reader)) {
    Error(_) -> AllMembers
    Ok(#(immediate, held)) -> {
      let masked =
        list.append(packed_ids.to_list(immediate), packed_ids.to_list(held))
      case pruned_plan(capture, direct, everyone_only(capture.data)) {
        Explicit(mentioned) ->
          Explicit(list.unique(list.append(masked, mentioned)))
        other -> other
      }
    }
  }
}

fn everyone_only(data: MessageData) -> Bool {
  data.mention_everyone && !data.mention_here
}

fn thread_plan(capture: Capture, scope: ThreadScope, direct: Set(Int)) -> Plan {
  let base =
    dict.keys(scope.members)
    |> list.append(set.to_list(direct))
    |> list.append(format_members(capture.data))
  case
    capture.data.mention_roles != [] && thread_scope.role_add_room(scope) > 0
  {
    True -> AllMembers
    False -> Explicit(base |> list.unique |> list.sort(int.compare))
  }
}

fn pruned_plan(capture: Capture, direct: Set(Int), everyone: Bool) -> Plan {
  let data = capture.data
  let mentioned =
    set.to_list(direct) |> list.sort(int.compare) |> packed_ids.from_list
  let roles =
    list.filter_map(data.mention_roles, fn(role) {
      id.role_id(role) |> result.replace_error(Nil)
    })
  let pruned = {
    use channel_id <- result.try(
      id.channel_id(capture.channel) |> result.replace_error(Nil),
    )
    perms.push_prune(
      capture.model,
      channel_id,
      snowflake.timestamp_ms(capture.message),
      mentioned,
      roles,
      everyone,
    )
    |> result.map(fn(pair) {
      list.append(packed_ids.to_list(pair.0), packed_ids.to_list(pair.1))
    })
    |> result.replace_error(Nil)
  }
  let here = case data.mention_here {
    False -> Ok([])
    True -> here_candidates(capture)
  }
  case pruned, here {
    Ok(users), Ok(here_users) ->
      Explicit(list.unique(list.append(users, here_users)))
    _, _ -> {
      metrics.count("push_skipped_permissions_closed", 1)
      Nothing
    }
  }
}

fn here_candidates(capture: Capture) -> Result(List(Int), Nil) {
  use channel_id <- result.try(
    id.channel_id(capture.channel) |> result.replace_error(Nil),
  )
  use viewers <- result.map(
    perms.viewers(capture.reader, channel_id) |> result.replace_error(Nil),
  )
  packed_ids.to_list(viewers)
  |> list.filter(engagement.is_connected(capture.snapshot, _))
}

pub fn all_member_ids(capture: Capture) -> Result(List(Int), Unavailable) {
  members.fold(capture.members, [], fn(acc, found) {
    [id.user_id_to_int(member.user_id(found)), ..acc]
  })
  |> list.reverse
  |> intact(capture, _)
}

fn intact(capture: Capture, value: a) -> Result(a, Unavailable) {
  case members.is_alive(capture.members) {
    True -> Ok(value)
    False -> Error(MembersUnavailable)
  }
}

fn lost(reason: perms.PermsError) -> Bool {
  case reason {
    perms.Gone | perms.Broken -> True
    perms.NotReady | perms.BadInput(_) -> False
  }
}

pub fn role_adds(
  capture: Capture,
  scope: ThreadScope,
  direct: Set(Int),
) -> Result(Set(Int), Unavailable) {
  let roles = mention_roles(capture.data)
  let room = thread_scope.role_add_room(scope)
  case set.is_empty(roles) || room == 0 {
    True -> Ok(set.new())
    False -> {
      let candidates =
        members.fold(capture.members, [], fn(acc, found) {
          let user = id.user_id_to_int(member.user_id(found))
          case
            user != capture.author
            && !thread_scope.is_member(scope, user)
            && !set.contains(direct, user)
            && holds_mentioned_role(found, roles)
            && !is_bot(found)
          {
            True -> [user, ..acc]
            False -> acc
          }
        })
        |> list.sort(int.compare)
      take_role_adds(capture, candidates, room, set.new())
      |> result.try(intact(capture, _))
    }
  }
}

fn take_role_adds(
  capture: Capture,
  candidates: List(Int),
  left: Int,
  acc: Set(Int),
) -> Result(Set(Int), Unavailable) {
  case candidates, left {
    [], _ -> Ok(acc)
    _, 0 -> Ok(acc)
    [user, ..rest], _ -> {
      use visible <- result.try(visible_recipient(capture, user))
      case visible {
        True -> take_role_adds(capture, rest, left - 1, set.insert(acc, user))
        False -> take_role_adds(capture, rest, left, acc)
      }
    }
  }
}

fn visible_recipient(capture: Capture, user: Int) -> Result(Bool, Unavailable) {
  case member_of(capture, user) {
    Some(found) ->
      visible_members(capture, [#(user, found)])
      |> result.map(set.contains(_, user))
    None -> Ok(False)
  }
}

pub fn route(
  capture: Capture,
  candidates: List(Int),
  thread_adds: Set(Int),
) -> Result(Routed, Unavailable) {
  let direct = message.direct_mention_ids(capture.data)
  let in_thread = fn(user) {
    case capture.thread {
      None -> True
      Some(scope) ->
        thread_scope.is_member(scope, user)
        || set.contains(direct, user)
        || set.contains(thread_adds, user)
    }
  }
  let routed =
    list.filter_map(candidates, fn(user) {
      case member_of(capture, user) {
        Some(found) ->
          case in_thread(user) {
            True -> Ok(#(user, found))
            False -> Error(Nil)
          }
        None -> Error(Nil)
      }
    })
  use visible <- result.try(visible_members(capture, routed))
  list.fold(list.reverse(routed), Routed([], [], dict.new()), fn(acc, pair) {
    let #(user, found) = pair
    case set.contains(visible, user) {
      False -> acc
      True -> add_routed(capture, acc, user, found)
    }
  })
  |> intact(capture, _)
}

fn add_routed(
  capture: Capture,
  acc: Routed,
  user: Int,
  found: Member,
) -> Routed {
  let roles = dict.insert(acc.roles, user, role_ints(found))
  case engagement.is_engaged(capture.snapshot, user) {
    False -> Routed(..acc, eligible: [user, ..acc.eligible], roles:)
    True ->
      case dict.get(capture.snapshot.held, user) {
        Ok(sessions) ->
          Routed(..acc, held: [#(user, sessions), ..acc.held], roles:)
        Error(Nil) -> acc
      }
  }
}

fn visible_members(
  capture: Capture,
  users: List(#(Int, Member)),
) -> Result(Set(Int), Unavailable) {
  case capture.thread {
    None ->
      viewable(capture, capture.channel, list.map(users, fn(pair) { pair.0 }))
      |> result.map(set.from_list)
    Some(scope) -> thread_viewers(capture, scope, users)
  }
}

fn thread_viewers(
  capture: Capture,
  scope: ThreadScope,
  users: List(#(Int, Member)),
) -> Result(Set(Int), Unavailable) {
  case id.channel_id(scope.parent) {
    Error(_) -> Ok(set.new())
    Ok(parent) -> {
      let now_ms = clock.system_ms()
      list.try_fold(users, set.new(), fn(acc, pair) {
        let #(user, found) = pair
        case
          perms.thread_permissions(
            capture.reader,
            member.user_id(found),
            parent,
            scope.private,
            thread_scope.is_member(scope, user),
            member.is_timed_out(found, now_ms:),
          )
        {
          Ok(granted) ->
            case permissions.has(granted, permissions.ViewChannel) {
              True -> Ok(set.insert(acc, user))
              False -> Ok(acc)
            }
          Error(reason) ->
            case lost(reason) {
              True -> Error(PermsUnavailable)
              False -> Ok(acc)
            }
        }
      })
    }
  }
}

fn viewable(
  capture: Capture,
  channel_id: Int,
  users: List(Int),
) -> Result(List(Int), Unavailable) {
  case id.channel_id(channel_id) {
    Error(_) -> Ok([])
    Ok(channel) ->
      users
      |> list.sized_chunk(view_chunk)
      |> list.try_map(fn(chunk) {
        case
          perms.filter_viewers(
            capture.reader,
            channel,
            packed_ids.from_list(chunk),
          )
        {
          Ok(found) -> Ok(packed_ids.to_list(found))
          Error(reason) ->
            case lost(reason) {
              True -> Error(PermsUnavailable)
              False -> Ok([])
            }
        }
      })
      |> result.map(list.flatten)
  }
}

pub fn forum_starter(capture: Capture) -> Option(#(Int, ThreadScope)) {
  case capture.thread, capture.data.id {
    Some(scope), Some(message_id) if message_id == capture.channel ->
      guild_info.lookup_channel(capture.info, scope.parent)
      |> option.then(fn(parent) {
        case channel.is_thread_only_kind(parent.kind) {
          True -> Some(#(scope.parent, scope))
          False -> None
        }
      })
    _, _ -> None
  }
}

pub fn forum_recipients(
  capture: Capture,
  forum: Int,
  scope: ThreadScope,
) -> Result(Routed, Unavailable) {
  let direct = message.direct_mention_ids(capture.data)
  let roles = mention_roles(capture.data)
  let candidates =
    members.fold(capture.members, [], fn(acc, found) {
      let user = id.user_id_to_int(member.user_id(found))
      case
        user != capture.author
        && !thread_scope.is_member(scope, user)
        && !set.contains(direct, user)
        && !holds_mentioned_role(found, roles)
        && !is_bot(found)
      {
        True -> [#(user, found), ..acc]
        False -> acc
      }
    })
  use visible <- result.try(
    viewable(capture, forum, list.map(candidates, fn(pair) { pair.0 }))
    |> result.map(set.from_list),
  )
  list.fold(candidates, Routed([], [], dict.new()), fn(acc, pair) {
    let #(user, found) = pair
    case set.contains(visible, user) {
      False -> acc
      True -> add_routed(capture, acc, user, found)
    }
  })
  |> intact(capture, _)
}

pub fn build(capture: Capture, routed: Routed, forum: Bool) -> MessageCreate {
  let info = capture.info
  let held_users = list.map(routed.held, fn(entry) { entry.0 })
  let #(thread, channel_name) = case capture.thread {
    Some(scope) -> {
      let parent = guild_info.lookup_channel(info, scope.parent)
      let thread =
        message.ThreadPush(
          parent_id: scope.parent,
          parent_name: option.then(parent, fn(found) {
            message.nonempty(found.name)
          }),
          category_id: option.then(parent, fn(found) { found.parent }),
          forum_thread_created: forum,
          members: dict.take(
            scope.members,
            list.append(routed.eligible, held_users),
          ),
        )
      #(Some(thread), option.unwrap(message.nonempty(scope.name), "unknown"))
    }
    None -> {
      let name =
        guild_info.lookup_channel(info, capture.channel)
        |> option.then(fn(found) { found.name })
        |> option.unwrap("unknown")
      #(None, name)
    }
  }
  let data = message.set_thread(capture.data, thread)
  let guild_nsfw =
    core_guild.nsfw_level(capture.facts) == core_guild.NsfwAgeRestricted
  let data = case
    guild_info.effective_nsfw(
      info,
      capture.channel,
      option.map(thread, fn(found) { found.parent_id }),
      guild_nsfw,
    )
  {
    True -> message.mark_age_restricted(data)
    False -> data
  }
  message.MessageCreate(
    data:,
    recipients: routed.eligible,
    guild: guild_int(capture),
    author: capture.author,
    channel: capture.channel,
    message: capture.message,
    guild_default_level: info.default_level,
    guild_name: Some(option.unwrap(info.name, "Unknown")),
    channel_name: Some(channel_name),
    user_roles: routed.roles,
    connected: engagement.connected_set(capture.snapshot),
    markdown: markdown_context(capture),
    large: Some(large_guild(capture)),
    prefilter: message.NotFiltered,
  )
}

fn markdown_context(capture: Capture) -> PreviewContext {
  let nicknames =
    format_members(capture.data)
    |> list.fold(dict.new(), fn(acc, user) {
      case option.then(member_of(capture, user), member.nick) {
        Some(nick) if nick != "" -> dict.insert(acc, user, nick)
        _ -> acc
      }
    })
  let role_names =
    list.fold(capture.roles, dict.new(), fn(acc, item) {
      dict.insert(acc, id.role_id_to_int(role.id(item)), role.name(item))
    })
  let channel_names =
    guild_info.channel_names_for_context(
      capture.info,
      fn(channel_id) {
        guild_info.everyone_can_view(
          capture.info,
          capture.guild,
          capture.roles,
          channel_id,
        )
      },
      format.channel_mention_ids(capture.data.content),
    )
  format.markdown_context(capture.data, nicknames, role_names, channel_names)
}
