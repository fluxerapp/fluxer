// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import support/guild_fixture.{type Guild} as fixture

pub fn with_members(found: Guild, first: Int, count: Int) -> Guild {
  int.range(first, first + count, found, fn(acc, user) {
    fixture.with_member(acc, user, [])
  })
}
