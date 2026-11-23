// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/recipients
import gleam/int
import gleam/list

pub fn blocked_fetch_covers_every_missing_user_past_old_budget_test() {
  let descending = int.range(1, 5002, [], list.prepend)
  let chunks = recipients.blocked_fetch_chunks(descending, fn(_) { False })
  let assert True = list.flatten(chunks) == list.reverse(descending)
  let assert 11 = list.length(chunks)
  let assert True = list.all(chunks, fn(chunk) { list.length(chunk) <= 500 })
}

pub fn blocked_fetch_skips_cached_and_duplicate_users_test() {
  let chunks =
    recipients.blocked_fetch_chunks([9, 3, 4, 3, 8, 9, 2], int.is_even)
  let assert [[3, 9]] = chunks
}
