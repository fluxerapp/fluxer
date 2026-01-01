//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import gleam/int
import gleam/list

pub type Badge {
  Badge(name: String, icon: String)
}

const flag_staff = 1

const flag_ctp_member = 2

const flag_partner = 4

const flag_bug_hunter = 8

pub fn get_user_badges(cdn_endpoint: String, flags: String) -> List(Badge) {
  case int.parse(flags) {
    Ok(flags_int) -> {
      []
      |> add_badge_if_has_flag(
        flags_int,
        flag_staff,
        Badge("Staff", cdn_endpoint <> "/badges/staff.svg"),
      )
      |> add_badge_if_has_flag(
        flags_int,
        flag_ctp_member,
        Badge("CTP Member", cdn_endpoint <> "/badges/ctp.svg"),
      )
      |> add_badge_if_has_flag(
        flags_int,
        flag_partner,
        Badge("Partner", cdn_endpoint <> "/badges/partner.svg"),
      )
      |> add_badge_if_has_flag(
        flags_int,
        flag_bug_hunter,
        Badge("Bug Hunter", cdn_endpoint <> "/badges/bug-hunter.svg"),
      )
      |> list.reverse
    }
    Error(_) -> []
  }
}

fn add_badge_if_has_flag(
  badges: List(Badge),
  flags: Int,
  flag: Int,
  badge: Badge,
) -> List(Badge) {
  case int.bitwise_and(flags, flag) == flag {
    True -> [badge, ..badges]
    False -> badges
  }
}
