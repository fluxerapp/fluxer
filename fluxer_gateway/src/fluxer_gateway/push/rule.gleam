// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/message.{type LargeGuild, type MessageData}
import fluxer_gateway/push/settings.{type Settings}
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

pub type Reason {
  PrivateChannelMessage
  DirectMention
  RoleMention
  EveryoneMention
  HereMention
  LevelAll
  ThreadLevelAll
  ForumThreadCreated
}

pub type Why {
  IsAuthor
  MobilePushOff
  GuildMuted
  CategoryMuted
  ChannelMuted
  ThreadMuted
  ForumSuppressed
  LevelNone
  NotMentioned
}

pub type Verdict {
  Eligible(reason: Reason)
  Ineligible(why: Why)
}

pub type Class {
  Direct
  Bulk
}

pub type Facts {
  Facts(user: Int, settings: Settings, connected: Bool, roles: List(Int))
}

pub type Shape {
  Shape(
    data: MessageData,
    guild: Int,
    channel: Int,
    author: Int,
    guild_default_level: Int,
    large: Bool,
    mentioned: Set(Int),
    mentioned_roles: Set(Int),
  )
}

pub type ProfileVerdict {
  Always(Reason)
  Never(Why)
  IfMentioned(suppress_everyone: Bool, suppress_roles: Bool, otherwise: Verdict)
  PerMember
}

pub fn class(reason: Reason) -> Class {
  case reason {
    PrivateChannelMessage | DirectMention | RoleMention -> Direct
    EveryoneMention
    | HereMention
    | LevelAll
    | ThreadLevelAll
    | ForumThreadCreated -> Bulk
  }
}

pub fn shape(
  data data: MessageData,
  guild guild: Int,
  channel channel: Int,
  author author: Int,
  guild_default_level guild_default_level: Int,
  large large: Option(LargeGuild),
) -> Shape {
  Shape(
    data:,
    guild:,
    channel:,
    author:,
    guild_default_level:,
    large: message.is_large(large),
    mentioned: message.direct_mention_ids(data),
    mentioned_roles: set.from_list(data.mention_roles),
  )
}

pub fn evaluate(shape: Shape, facts: Facts, now_ms now_ms: Int) -> Verdict {
  case facts.user == shape.author {
    True -> Ineligible(IsAuthor)
    False ->
      case shape.guild != 0 && !settings.mobile_push(facts.settings) {
        True -> Ineligible(MobilePushOff)
        False ->
          case shape.data.thread {
            Some(thread) if thread.forum_thread_created ->
              forum(shape, facts.settings, thread, now_ms)
            Some(thread) -> in_thread(shape, facts, thread, now_ms)
            None -> in_channel(shape, facts, now_ms)
          }
      }
  }
}

fn in_channel(shape: Shape, facts: Facts, now_ms: Int) -> Verdict {
  let s = facts.settings
  case settings.is_active(settings.mute(s), now_ms:) {
    True -> Ineligible(GuildMuted)
    False ->
      case settings.override_muted(s, shape.channel, now_ms) {
        True -> Ineligible(ChannelMuted)
        False -> {
          let level =
            settings.resolve_level(s, shape.channel, shape.guild_default_level)
            |> large_override(shape.large)
          allow(level, shape, True, facts, LevelAll)
        }
      }
  }
}

fn in_thread(
  shape: Shape,
  facts: Facts,
  thread: message.ThreadPush,
  now_ms: Int,
) -> Verdict {
  let s = facts.settings
  let member = dict.get(thread.members, facts.user)
  let category_muted = case thread.category_id {
    Some(category) -> settings.override_muted(s, category, now_ms)
    None -> False
  }
  let member_muted = case member {
    Ok(member) -> settings.is_active(member.mute, now_ms:)
    Error(Nil) -> False
  }
  case settings.is_active(settings.mute(s), now_ms:) {
    True -> Ineligible(GuildMuted)
    False ->
      case category_muted {
        True -> Ineligible(CategoryMuted)
        False ->
          case settings.override_muted(s, thread.parent_id, now_ms) {
            True -> Ineligible(ChannelMuted)
            False ->
              case member_muted {
                True -> Ineligible(ThreadMuted)
                False -> {
                  let inherited =
                    settings.resolve_level(
                      s,
                      thread.parent_id,
                      shape.guild_default_level,
                    )
                    |> large_override(shape.large)
                  case member {
                    Error(Nil) ->
                      allow(
                        int.max(inherited, 1),
                        shape,
                        False,
                        facts,
                        ThreadLevelAll,
                      )
                    Ok(member) -> {
                      let level =
                        settings.explicit_thread_level(member.flags)
                        |> option.unwrap(inherited)
                      allow(level, shape, True, facts, ThreadLevelAll)
                    }
                  }
                }
              }
          }
      }
  }
}

fn forum(
  shape: Shape,
  s: Settings,
  thread: message.ThreadPush,
  now_ms: Int,
) -> Verdict {
  let forum_override = settings.override_for(s, thread.parent_id)
  let category_muted = case thread.category_id {
    Some(category) -> settings.override_muted(s, category, now_ms)
    None -> False
  }
  let forum_muted = case forum_override {
    Some(override) ->
      settings.is_active(settings.override_mute(override), now_ms:)
    None -> False
  }
  let forum_off = case forum_override {
    Some(override) ->
      int.bitwise_and(
        settings.override_flags(override),
        settings.new_forum_threads_off,
      )
      != 0
    None -> False
  }
  let suppressed =
    settings.is_active(settings.mute(s), now_ms:)
    || category_muted
    || forum_muted
    || forum_off
  case suppressed {
    True -> Ineligible(ForumSuppressed)
    False -> {
      let inherited =
        settings.resolve_level(s, thread.parent_id, shape.guild_default_level)
        |> large_override(shape.large)
      case inherited {
        0 -> Eligible(ForumThreadCreated)
        _ -> Ineligible(ForumSuppressed)
      }
    }
  }
}

fn large_override(level: Int, large: Bool) -> Int {
  case large, level {
    True, 0 -> 1
    _, _ -> level
  }
}

fn allow(
  level: Int,
  shape: Shape,
  everyone_counts: Bool,
  facts: Facts,
  all: Reason,
) -> Verdict {
  let private = message.is_private_channel(shape.data)
  let mention =
    mention_reason(
      shape,
      everyone_counts,
      facts.user,
      facts.connected,
      facts.roles,
      settings.suppress_everyone(facts.settings),
      settings.suppress_roles(facts.settings),
    )
  case level, private, mention {
    2, _, _ -> Ineligible(LevelNone)
    _, True, _ -> Eligible(PrivateChannelMessage)
    _, False, Some(reason) -> Eligible(reason)
    1, False, None -> Ineligible(NotMentioned)
    _, False, None -> Eligible(all)
  }
}

fn mention_reason(
  shape: Shape,
  everyone_counts: Bool,
  user: Int,
  connected: Bool,
  roles: List(Int),
  suppress_everyone: Bool,
  suppress_roles: Bool,
) -> Option(Reason) {
  let data = shape.data
  let everyone = everyone_counts && data.mention_everyone && !data.mention_here
  let here = everyone_counts && data.mention_here && connected
  case
    set.contains(shape.mentioned, user),
    !suppress_roles && mentions_any_role(shape.mentioned_roles, roles),
    everyone && !suppress_everyone,
    here && !suppress_everyone
  {
    True, _, _, _ -> Some(DirectMention)
    False, True, _, _ -> Some(RoleMention)
    False, False, True, _ -> Some(EveryoneMention)
    False, False, False, True -> Some(HereMention)
    False, False, False, False -> None
  }
}

fn mentions_any_role(mentioned: Set(Int), roles: List(Int)) -> Bool {
  case roles {
    [] -> False
    [role, ..rest] ->
      set.contains(mentioned, role) || mentions_any_role(mentioned, rest)
  }
}

pub fn profile_verdict(shape: Shape, s: Settings) -> ProfileVerdict {
  case shape.guild != 0 && !settings.mobile_push(s) {
    True -> Never(MobilePushOff)
    False ->
      case settings.has_timed_mute(s), shape.data.thread {
        True, _ -> PerMember
        False, Some(thread) if thread.forum_thread_created ->
          case forum(shape, s, thread, 0) {
            Eligible(reason) -> Always(reason)
            Ineligible(why) -> Never(why)
          }
        False, Some(_) -> PerMember
        False, None -> channel_profile(shape, s)
      }
  }
}

fn channel_profile(shape: Shape, s: Settings) -> ProfileVerdict {
  case settings.is_active(settings.mute(s), now_ms: 0) {
    True -> Never(GuildMuted)
    False ->
      case settings.override_muted(s, shape.channel, 0) {
        True -> Never(ChannelMuted)
        False -> {
          let level =
            settings.resolve_level(s, shape.channel, shape.guild_default_level)
            |> large_override(shape.large)
          let suppress_everyone = settings.suppress_everyone(s)
          let suppress_roles = settings.suppress_roles(s)
          case level, message.is_private_channel(shape.data) {
            2, _ -> Never(LevelNone)
            _, True -> Always(PrivateChannelMessage)
            1, False ->
              IfMentioned(
                suppress_everyone,
                suppress_roles,
                Ineligible(NotMentioned),
              )
            _, False ->
              IfMentioned(suppress_everyone, suppress_roles, Eligible(LevelAll))
          }
        }
      }
  }
}

pub fn complete(
  verdict: ProfileVerdict,
  shape: Shape,
  facts: Facts,
  now_ms now_ms: Int,
) -> Verdict {
  case facts.user == shape.author, verdict {
    True, _ -> Ineligible(IsAuthor)
    False, Always(reason) -> Eligible(reason)
    False, Never(why) -> Ineligible(why)
    False, PerMember -> evaluate(shape, facts, now_ms:)
    False, IfMentioned(suppress_everyone, suppress_roles, otherwise) ->
      case
        mention_reason(
          shape,
          True,
          facts.user,
          facts.connected,
          facts.roles,
          suppress_everyone,
          suppress_roles,
        )
      {
        Some(reason) -> Eligible(reason)
        None -> otherwise
      }
  }
}
