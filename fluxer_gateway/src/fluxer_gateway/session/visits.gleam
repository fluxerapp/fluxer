// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/option.{type Option}

pub type Visits {
  Visits(
    guilds: Dict(Int, Bool),
    capable: Bool,
    adult_content_allowed: Bool,
    tier: String,
    ip_key: Option(String),
  )
}

pub fn none() -> Visits {
  Visits(
    guilds: dict.new(),
    capable: False,
    adult_content_allowed: False,
    tier: "unclaimed",
    ip_key: option.None,
  )
}

pub fn is_visiting(visits: Visits, guild: Int) -> Bool {
  dict.has_key(visits.guilds, guild)
}

pub fn put(visits: Visits, guild: Int, operator: Bool) -> Visits {
  Visits(..visits, guilds: dict.insert(visits.guilds, guild, operator))
}

pub fn drop(visits: Visits, guild: Int) -> Visits {
  Visits(..visits, guilds: dict.delete(visits.guilds, guild))
}

pub type StartDecision {
  Ignore
  MarkVisit
  ConnectVisit
}

pub fn decide_start(
  visits: Visits,
  guild: Int,
  bot bot: Bool,
  known known: Bool,
) -> StartDecision {
  case visits.capable && !bot, known, is_visiting(visits, guild) {
    False, _, _ -> Ignore
    True, False, _ -> ConnectVisit
    True, True, True -> MarkVisit
    True, True, False -> Ignore
  }
}
