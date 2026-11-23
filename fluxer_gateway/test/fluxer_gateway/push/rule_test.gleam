// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/message
import fluxer_gateway/push/rule
import fluxer_gateway/push/settings
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

type Rng {
  Rng(state: Int)
}

fn next(rng: Rng, bound: Int) -> #(Int, Rng) {
  let state = { rng.state * 1_103_515_245 + 12_345 } % 2_147_483_648
  #(state % bound, Rng(state))
}

fn pick(rng: Rng, items: List(a), default: a) -> #(a, Rng) {
  let #(index, rng) = next(rng, list.length(items))
  case list.drop(items, index) {
    [item, ..] -> #(item, rng)
    [] -> #(default, rng)
  }
}

fn bool(rng: Rng) -> #(Bool, Rng) {
  let #(value, rng) = next(rng, 2)
  #(value == 1, rng)
}

const now_ms = 1_000_000

fn mute(rng: Rng) -> #(settings.Mute, Rng) {
  pick(
    rng,
    [
      settings.NotMuted,
      settings.NotMuted,
      settings.MutedForever,
      settings.MutedUntil(now_ms),
      settings.MutedUntil(now_ms + 1),
      settings.MutedUntil(now_ms - 1),
    ],
    settings.NotMuted,
  )
}

fn level(rng: Rng) -> #(option.Option(Int), Rng) {
  pick(rng, [None, Some(-1), Some(0), Some(1), Some(2), Some(3), Some(7)], None)
}

fn random_settings(rng: Rng) -> #(settings.Settings, Rng) {
  let #(mobile, rng) = next(rng, 6)
  let #(guild_level, rng) = level(rng)
  let #(guild_mute, rng) = mute(rng)
  let #(suppress_everyone, rng) = bool(rng)
  let #(suppress_roles, rng) = bool(rng)
  let #(has_override, rng) = next(rng, 3)
  let #(override_channel, rng) = pick(rng, [10, 20, 30], 10)
  let #(override_level, rng) = level(rng)
  let #(override_mute, rng) = mute(rng)
  let #(flags, rng) = pick(rng, [0, 0x2000], 0)
  let overrides = case has_override {
    0 -> []
    _ -> [
      #(
        override_channel,
        settings.channel_override(
          level: override_level,
          mute: override_mute,
          flags:,
        ),
      ),
    ]
  }
  #(
    settings.new(
      mobile_push: mobile != 0,
      level: guild_level,
      mute: guild_mute,
      suppress_everyone:,
      suppress_roles:,
      overrides:,
    ),
    rng,
  )
}

fn random_shape(rng: Rng) -> #(rule.Shape, Rng) {
  let #(everyone, rng) = bool(rng)
  let #(here, rng) = bool(rng)
  let #(mention_user, rng) = bool(rng)
  let #(mention_role, rng) = bool(rng)
  let #(guild, rng) = pick(rng, [0, 77], 77)
  let #(channel_type, rng) = pick(rng, [None, Some(0), Some(1), Some(3)], None)
  let #(thread_kind, rng) = next(rng, 4)
  let #(member_flags, rng) = pick(rng, [0, 0x2, 0x4, 0x8], 0)
  let #(member_mute, rng) = mute(rng)
  let #(default_level, rng) = pick(rng, [-1, 0, 1, 2, 3], 0)
  let #(member_count, rng) = pick(rng, [2499, 2500, 2501], 2500)
  let thread = case thread_kind {
    0 | 1 -> None
    2 ->
      Some(message.ThreadPush(
        parent_id: 10,
        parent_name: None,
        category_id: Some(30),
        forum_thread_created: False,
        members: dict.from_list([
          #(5, settings.ThreadMember(flags: member_flags, mute: member_mute)),
        ]),
      ))
    _ ->
      Some(message.ThreadPush(
        parent_id: 20,
        parent_name: None,
        category_id: Some(30),
        forum_thread_created: True,
        members: dict.new(),
      ))
  }
  let mentions = case mention_user {
    True -> [message.Mention(id: Some(5), username: None, global_name: None)]
    False -> []
  }
  let data =
    message.MessageData(
      ..message.empty_data(),
      mention_everyone: everyone,
      mention_here: here,
      mentions:,
      mention_roles: case mention_role {
        True -> [900]
        False -> []
      },
      channel_type:,
      thread:,
    )
  #(
    rule.shape(
      data:,
      guild:,
      channel: 10,
      author: 1,
      guild_default_level: default_level,
      large: Some(message.large_guild(member_count, [])),
    ),
    rng,
  )
}

fn random_facts(rng: Rng, s: settings.Settings) -> #(rule.Facts, Rng) {
  let #(user, rng) = pick(rng, [1, 5, 6], 5)
  let #(connected, rng) = bool(rng)
  let #(holds_role, rng) = bool(rng)
  let roles = case holds_role {
    True -> [900]
    False -> []
  }
  #(rule.Facts(user:, settings: s, connected:, roles:), rng)
}

fn check(rng: Rng, remaining: Int) -> Nil {
  case remaining {
    0 -> Nil
    _ -> {
      let #(s, rng) = random_settings(rng)
      let #(shape, rng) = random_shape(rng)
      let #(facts, rng) = random_facts(rng, s)
      let direct = rule.evaluate(shape, facts, now_ms:)
      let partial =
        rule.complete(rule.profile_verdict(shape, s), shape, facts, now_ms:)
      assert direct == partial
      check(rng, remaining - 1)
    }
  }
}

pub fn p_rule_property_test() {
  check(Rng(1), 10_000)
}

fn plain_shape(data: message.MessageData, default_level: Int) -> rule.Shape {
  rule.shape(
    data:,
    guild: 77,
    channel: 10,
    author: 1,
    guild_default_level: default_level,
    large: None,
  )
}

fn facts(s: settings.Settings) -> rule.Facts {
  rule.Facts(user: 5, settings: s, connected: True, roles: [])
}

pub fn author_is_never_eligible_test() {
  let shape = plain_shape(message.empty_data(), 0)
  let verdict =
    rule.evaluate(
      shape,
      rule.Facts(
        user: 1,
        settings: settings.defaults(),
        connected: True,
        roles: [],
      ),
      now_ms:,
    )
  let assert rule.Ineligible(rule.IsAuthor) = verdict
}

pub fn level_all_is_bulk_test() {
  let verdict =
    rule.evaluate(
      plain_shape(message.empty_data(), 0),
      facts(settings.defaults()),
      now_ms:,
    )
  let assert rule.Eligible(rule.LevelAll) = verdict
  let assert rule.Bulk = rule.class(rule.LevelAll)
}

pub fn direct_mention_wins_over_level_all_test() {
  let data =
    message.MessageData(..message.empty_data(), mentions: [
      message.Mention(id: Some(5), username: None, global_name: None),
    ])
  let assert rule.Eligible(rule.DirectMention) =
    rule.evaluate(plain_shape(data, 0), facts(settings.defaults()), now_ms:)
}

pub fn only_mentions_without_mention_test() {
  let assert rule.Ineligible(rule.NotMentioned) =
    rule.evaluate(
      plain_shape(message.empty_data(), 1),
      facts(settings.defaults()),
      now_ms:,
    )
}

pub fn here_needs_connection_test() {
  let data = message.MessageData(..message.empty_data(), mention_here: True)
  let assert rule.Eligible(rule.HereMention) =
    rule.evaluate(plain_shape(data, 1), facts(settings.defaults()), now_ms:)
  let offline = rule.Facts(..facts(settings.defaults()), connected: False)
  let assert rule.Ineligible(rule.NotMentioned) =
    rule.evaluate(plain_shape(data, 1), offline, now_ms:)
}

pub fn timed_mute_boundary_test() {
  let muted_until = fn(end_ms) {
    settings.new(
      mobile_push: True,
      level: None,
      mute: settings.MutedUntil(end_ms),
      suppress_everyone: False,
      suppress_roles: False,
      overrides: [],
    )
  }
  let assert rule.Eligible(_) =
    rule.evaluate(
      plain_shape(message.empty_data(), 0),
      facts(muted_until(now_ms)),
      now_ms:,
    )
  let assert rule.Ineligible(rule.GuildMuted) =
    rule.evaluate(
      plain_shape(message.empty_data(), 0),
      facts(muted_until(now_ms + 1)),
      now_ms:,
    )
}

pub fn large_guild_turns_all_into_mentions_test() {
  let shape =
    rule.shape(
      data: message.empty_data(),
      guild: 77,
      channel: 10,
      author: 1,
      guild_default_level: 0,
      large: Some(message.large_guild(2501, [])),
    )
  let assert rule.Ineligible(rule.NotMentioned) =
    rule.evaluate(shape, facts(settings.defaults()), now_ms:)
  let small =
    rule.shape(
      data: message.empty_data(),
      guild: 77,
      channel: 10,
      author: 1,
      guild_default_level: 0,
      large: Some(message.large_guild(2500, [])),
    )
  let assert rule.Eligible(rule.LevelAll) =
    rule.evaluate(small, facts(settings.defaults()), now_ms:)
}

pub fn private_channel_is_direct_test() {
  let data = message.MessageData(..message.empty_data(), channel_type: Some(1))
  let shape =
    rule.shape(
      data:,
      guild: 0,
      channel: 10,
      author: 1,
      guild_default_level: 0,
      large: None,
    )
  let quiet =
    settings.new(
      mobile_push: False,
      level: Some(1),
      mute: settings.NotMuted,
      suppress_everyone: False,
      suppress_roles: False,
      overrides: [],
    )
  let assert rule.Eligible(rule.PrivateChannelMessage) =
    rule.evaluate(shape, facts(quiet), now_ms:)
}
