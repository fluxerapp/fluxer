// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{type GuildRef}
import fluxer_gateway/core/id.{type GuildId}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

fn backoff_base_ms() -> Int {
  durations.session().links_backoff_base_ms
}

fn backoff_cap_ms() -> Int {
  durations.session().links_backoff_cap_ms
}

pub type Link {
  Waiting(guild: GuildId)
  Linked(guild: GuildRef, monitor: Monitor)
  Unavailable(guild: GuildId, cached: Bool)
  Detached(guild: GuildId)
}

pub type InFlight {
  InFlight(guild: GuildId, token: Int, attempt: Int)
}

pub opaque type Links {
  Links(
    links: Dict(Int, Link),
    in_flight: Dict(Int, InFlight),
    tokens: Dict(Int, Int),
    next_token: Int,
    last_sleep: Dict(Int, Int),
    tokens_left: Int,
    tokens_cap: Int,
    last_repair_ms: Int,
  )
}

pub const max_in_flight = 32

pub fn base_sleep_ms() -> Int {
  durations.session().links_base_sleep_ms
}

pub fn cap_sleep_ms() -> Int {
  durations.session().links_cap_sleep_ms
}

pub fn slow_retry_ms() -> Int {
  durations.session().links_slow_retry_ms
}

pub const not_member_retries = 3

fn repair_interval_ms() -> Int {
  durations.session().links_repair_interval_ms
}

const repair_limit = 8

pub fn new(guilds: List(GuildId)) -> Links {
  let links =
    list.fold(guilds, dict.new(), fn(acc, guild) {
      dict.insert(acc, id.guild_id_to_int(guild), Waiting(guild))
    })
  let cap = dict.size(links) + 3
  Links(
    links:,
    in_flight: dict.new(),
    tokens: dict.new(),
    next_token: 1,
    last_sleep: dict.new(),
    tokens_left: cap,
    tokens_cap: cap,
    last_repair_ms: 0,
  )
}

fn key(guild: GuildId) -> Int {
  id.guild_id_to_int(guild)
}

pub fn get(links: Links, guild: GuildId) -> Option(Link) {
  dict.get(links.links, key(guild)) |> option.from_result
}

pub fn has(links: Links, guild: GuildId) -> Bool {
  dict.has_key(links.links, key(guild))
}

pub fn put(links: Links, guild: GuildId, link: Link) -> Links {
  Links(..links, links: dict.insert(links.links, key(guild), link))
}

pub fn remove(links: Links, guild: GuildId) -> Links {
  let links = forget_in_flight(links, guild)
  Links(
    ..links,
    links: dict.delete(links.links, key(guild)),
    last_sleep: dict.delete(links.last_sleep, key(guild)),
  )
}

pub fn guild_ids(links: Links) -> List(GuildId) {
  links.links |> dict.values |> list.map(link_guild)
}

fn link_guild(link: Link) -> GuildId {
  case link {
    Waiting(guild) | Unavailable(guild, _) | Detached(guild) -> guild
    Linked(guild_ref, _) -> guild_ref.guild
  }
}

pub fn linked(links: Links) -> List(GuildRef) {
  links.links
  |> dict.values
  |> list.filter_map(fn(link) {
    case link {
      Linked(guild_ref, _) -> Ok(guild_ref)
      _ -> Error(Nil)
    }
  })
}

pub fn linked_ref(links: Links, guild: GuildId) -> Option(GuildRef) {
  case get(links, guild) {
    Some(Linked(guild_ref, _)) -> Some(guild_ref)
    _ -> None
  }
}

pub fn by_monitor(links: Links, monitor: Monitor) -> Option(GuildId) {
  links.links
  |> dict.values
  |> list.find_map(fn(link) {
    case link {
      Linked(guild_ref, linked_monitor) if linked_monitor == monitor ->
        Ok(guild_ref.guild)
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

pub fn all_resolved(links: Links) -> Bool {
  links.links
  |> dict.values
  |> list.all(fn(link) {
    case link {
      Waiting(_) -> False
      _ -> True
    }
  })
}

pub fn in_flight_count(links: Links) -> Int {
  dict.size(links.in_flight)
}

pub fn is_in_flight(links: Links, guild: GuildId) -> Bool {
  dict.has_key(links.in_flight, key(guild))
}

pub fn begin(links: Links, guild: GuildId, attempt: Int) -> #(Links, Int) {
  let token = links.next_token
  let entry = InFlight(guild:, token:, attempt:)
  let links = forget_in_flight(links, guild)
  #(
    Links(
      ..links,
      in_flight: dict.insert(links.in_flight, key(guild), entry),
      tokens: dict.insert(links.tokens, token, key(guild)),
      next_token: token + 1,
    ),
    token,
  )
}

pub fn settle(links: Links, token: Int) -> #(Links, Option(InFlight)) {
  case dict.get(links.tokens, token) {
    Error(Nil) -> #(links, None)
    Ok(guild) ->
      case dict.get(links.in_flight, guild) {
        Ok(entry) if entry.token == token -> #(
          Links(
            ..links,
            in_flight: dict.delete(links.in_flight, guild),
            tokens: dict.delete(links.tokens, token),
          ),
          Some(entry),
        )
        _ -> #(Links(..links, tokens: dict.delete(links.tokens, token)), None)
      }
  }
}

fn forget_in_flight(links: Links, guild: GuildId) -> Links {
  case dict.get(links.in_flight, key(guild)) {
    Ok(entry) ->
      Links(
        ..links,
        in_flight: dict.delete(links.in_flight, key(guild)),
        tokens: dict.delete(links.tokens, entry.token),
      )
    Error(Nil) -> links
  }
}

pub type Hint {
  NoHint
  Slot(retry_after_ms: Int)
}

pub fn next_sleep(links: Links, guild: GuildId, hint: Hint) -> #(Links, Int) {
  let sleep = case hint {
    Slot(retry_after_ms) -> int.max(retry_after_ms, 0)
    NoHint -> {
      let previous =
        dict.get(links.last_sleep, key(guild)) |> option.from_result
      let high = int.max(option.unwrap(previous, 0) * 3, base_sleep_ms())
      int.min(
        cap_sleep_ms(),
        base_sleep_ms() + int.random(high - base_sleep_ms() + 1),
      )
    }
  }
  #(
    Links(..links, last_sleep: dict.insert(links.last_sleep, key(guild), sleep)),
    sleep,
  )
}

pub fn reset_sleep(links: Links, guild: GuildId) -> Links {
  Links(..links, last_sleep: dict.delete(links.last_sleep, key(guild)))
}

pub fn spend(links: Links) -> Result(Links, Nil) {
  case links.tokens_left > 0 {
    True -> Ok(Links(..links, tokens_left: links.tokens_left - 1))
    False -> Error(Nil)
  }
}

pub fn refund(links: Links) -> Links {
  Links(..links, tokens_left: int.min(links.tokens_left + 1, links.tokens_cap))
}

pub fn tokens_left(links: Links) -> Int {
  links.tokens_left
}

pub fn grow_budget(links: Links) -> Links {
  Links(
    ..links,
    tokens_cap: links.tokens_cap + 1,
    tokens_left: links.tokens_left + 1,
  )
}

pub fn repair_due(links: Links, now_ms: Int) -> Bool {
  now_ms - links.last_repair_ms >= repair_interval_ms()
}

pub fn stalled(links: Links, now_ms: Int) -> #(Links, List(GuildId)) {
  let ids =
    links.links
    |> dict.to_list
    |> list.filter(fn(entry) {
      let #(guild_key, link) = entry
      !dict.has_key(links.in_flight, guild_key)
      && case link {
        Waiting(_) | Unavailable(_, False) -> True
        _ -> False
      }
    })
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
    |> list.take(repair_limit)
    |> list.map(fn(entry) { link_guild(entry.1) })
  #(Links(..links, last_repair_ms: now_ms), ids)
}

pub fn jitter(value_ms: Int, percent: Int) -> Int {
  let spread = value_ms * percent / 100
  case spread > 0 {
    True -> value_ms - spread + int.random(2 * spread + 1)
    False -> value_ms
  }
}

pub fn backoff_ms(attempt: Int) -> Int {
  let base =
    int.min(
      backoff_base_ms() * pow2(int.min(int.max(attempt, 0), 32)),
      backoff_cap_ms(),
    )
  let range = int.max(1, base / 2)
  let jittered = base - range / 2 + int.random(range + 1)
  int.max(1, int.min(backoff_cap_ms(), jittered))
}

fn pow2(exponent: Int) -> Int {
  int.bitwise_shift_left(1, exponent)
}
