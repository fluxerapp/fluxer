// SPDX-License-Identifier: AGPL-3.0-or-later

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

pub fn shard_index(key: Int, shards: Int) -> Int {
  case shards >= 1 {
    True -> phash2(key, shards)
    False -> 0
  }
}
