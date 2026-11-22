// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/util/iso8601
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Mute {
  NotMuted
  MutedForever
  MutedUntil(end_ms: Int)
}

pub opaque type ChannelOverride {
  ChannelOverride(level: Option(Int), mute: Mute, flags: Int)
}

pub opaque type Settings {
  Settings(
    mobile_push: Bool,
    level: Option(Int),
    mute: Mute,
    suppress_everyone: Bool,
    suppress_roles: Bool,
    overrides: Dict(Int, ChannelOverride),
  )
}

pub type ThreadMember {
  ThreadMember(flags: Int, mute: Mute)
}

pub const new_forum_threads_off = 0x2000

pub fn defaults() -> Settings {
  Settings(
    mobile_push: True,
    level: None,
    mute: NotMuted,
    suppress_everyone: False,
    suppress_roles: False,
    overrides: dict.new(),
  )
}

pub fn new(
  mobile_push mobile_push: Bool,
  level level: Option(Int),
  mute mute: Mute,
  suppress_everyone suppress_everyone: Bool,
  suppress_roles suppress_roles: Bool,
  overrides overrides: List(#(Int, ChannelOverride)),
) -> Settings {
  Settings(
    mobile_push:,
    level:,
    mute:,
    suppress_everyone:,
    suppress_roles:,
    overrides: dict.from_list(overrides),
  )
}

pub fn channel_override(
  level level: Option(Int),
  mute mute: Mute,
  flags flags: Int,
) -> ChannelOverride {
  ChannelOverride(level:, mute:, flags:)
}

pub fn mobile_push(settings: Settings) -> Bool {
  settings.mobile_push
}

pub fn level(settings: Settings) -> Option(Int) {
  settings.level
}

pub fn mute(settings: Settings) -> Mute {
  settings.mute
}

pub fn suppress_everyone(settings: Settings) -> Bool {
  settings.suppress_everyone
}

pub fn suppress_roles(settings: Settings) -> Bool {
  settings.suppress_roles
}

pub fn override_for(
  settings: Settings,
  channel: Int,
) -> Option(ChannelOverride) {
  dict.get(settings.overrides, channel) |> option.from_result
}

pub fn override_mute(override: ChannelOverride) -> Mute {
  override.mute
}

pub fn override_flags(override: ChannelOverride) -> Int {
  override.flags
}

pub fn is_active(mute: Mute, now_ms now_ms: Int) -> Bool {
  case mute {
    NotMuted -> False
    MutedForever -> True
    MutedUntil(end_ms) -> now_ms < end_ms
  }
}

fn is_timed(mute: Mute) -> Bool {
  case mute {
    MutedUntil(_) -> True
    NotMuted | MutedForever -> False
  }
}

pub fn has_timed_mute(settings: Settings) -> Bool {
  is_timed(settings.mute)
  || list.any(dict.values(settings.overrides), fn(override) {
    is_timed(override.mute)
  })
}

pub fn has_overrides(settings: Settings) -> Bool {
  dict.size(settings.overrides) > 0
}

pub fn override_muted(settings: Settings, channel: Int, now_ms: Int) -> Bool {
  case override_for(settings, channel) {
    Some(override) -> is_active(override.mute, now_ms:)
    None -> False
  }
}

pub fn resolve_level(
  settings: Settings,
  channel: Int,
  guild_default: Int,
) -> Int {
  let channel_level =
    override_for(settings, channel)
    |> option.map(fn(override) { option.unwrap(override.level, -1) })
  case channel_level {
    Some(-1) | Some(3) | None -> resolve_guild_level(settings, guild_default)
    Some(level) -> normalize_level(level)
  }
}

fn resolve_guild_level(settings: Settings, guild_default: Int) -> Int {
  case option.unwrap(settings.level, -1) {
    -1 | 3 -> normalize_level(guild_default)
    level -> normalize_level(level)
  }
}

fn normalize_level(level: Int) -> Int {
  case level {
    0 | 1 | 2 -> level
    _ -> 0
  }
}

pub fn explicit_thread_level(flags: Int) -> Option(Int) {
  case
    int.bitwise_and(flags, 0x8) != 0,
    int.bitwise_and(flags, 0x4) != 0,
    int.bitwise_and(flags, 0x2) != 0
  {
    True, _, _ -> Some(2)
    False, True, _ -> Some(1)
    False, False, True -> Some(0)
    False, False, False -> None
  }
}

pub fn decoder() -> Decoder(Settings) {
  use mobile_push <- decode.then(lenient("mobile_push", decode.bool, True))
  use level <- decode.then(level_field())
  use mute <- decode.then(mute_decoder())
  use suppress_everyone <- decode.then(lenient(
    "suppress_everyone",
    decode.bool,
    False,
  ))
  use suppress_roles <- decode.then(lenient(
    "suppress_roles",
    decode.bool,
    False,
  ))
  use overrides <- decode.then(lenient(
    "channel_overrides",
    decode.dict(decode.string, decode.dynamic),
    dict.new(),
  ))
  decode.success(Settings(
    mobile_push:,
    level:,
    mute:,
    suppress_everyone:,
    suppress_roles:,
    overrides: parse_overrides(overrides),
  ))
}

fn override_decoder() -> Decoder(ChannelOverride) {
  use level <- decode.then(level_field())
  use mute <- decode.then(mute_decoder())
  use flags <- decode.then(lenient("flags", decode.int, 0))
  decode.success(ChannelOverride(level:, mute:, flags:))
}

fn empty_override() -> ChannelOverride {
  ChannelOverride(level: None, mute: NotMuted, flags: 0)
}

fn parse_overrides(raw: Dict(String, Dynamic)) -> Dict(Int, ChannelOverride) {
  dict.fold(raw, dict.new(), fn(acc, key, value) {
    case id.parse(key) {
      Ok(channel) -> {
        let override =
          decode.run(value, override_decoder())
          |> result.unwrap(empty_override())
        dict.insert(acc, channel, override)
      }
      _ -> acc
    }
  })
}

fn level_field() -> Decoder(Option(Int)) {
  lenient("message_notifications", decode.map(decode.int, Some), None)
}

fn mute_decoder() -> Decoder(Mute) {
  use muted <- decode.then(lenient("muted", decode.bool, False))
  use config <- decode.then(lenient(
    "mute_config",
    decode.map(mute_config_decoder(), Some),
    None,
  ))
  decode.success(case muted, config {
    False, _ -> NotMuted
    True, Some(Some(end_ms)) -> MutedUntil(end_ms)
    True, _ -> MutedForever
  })
}

fn mute_config_decoder() -> Decoder(Option(Int)) {
  use end_time <- decode.then(lenient(
    "end_time",
    decode.map(decode.string, Some),
    None,
  ))
  decode.success(
    option.then(end_time, fn(text) {
      iso8601.parse_ms(text) |> option.from_result
    }),
  )
}

fn lenient(name: String, inner: Decoder(a), default: a) -> Decoder(a) {
  decode.optional_field(
    name,
    default,
    decode.one_of(inner, [decode.success(default)]),
    decode.success,
  )
}
