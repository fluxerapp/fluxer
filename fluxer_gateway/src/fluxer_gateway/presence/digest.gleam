// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/list

pub type Digest {
  Digest(xor: Int, sum: Int, count: Int)
}

const mask = 0xFFFFFFFFFFFFFFFF

pub fn empty() -> Digest {
  Digest(xor: 0, sum: 0, count: 0)
}

fn splitmix64(value: Int) -> Int {
  let z = int.bitwise_and(value + 0x9E3779B97F4A7C15, mask)
  let z =
    int.bitwise_and(
      int.bitwise_exclusive_or(z, int.bitwise_shift_right(z, 30))
        * 0xBF58476D1CE4E5B9,
      mask,
    )
  let z =
    int.bitwise_and(
      int.bitwise_exclusive_or(z, int.bitwise_shift_right(z, 27))
        * 0x94D049BB133111EB,
      mask,
    )
  int.bitwise_exclusive_or(z, int.bitwise_shift_right(z, 31))
}

pub fn add(digest: Digest, id: Int) -> Digest {
  let hashed = splitmix64(id)
  Digest(
    xor: int.bitwise_exclusive_or(digest.xor, hashed),
    sum: int.bitwise_and(digest.sum + hashed, mask),
    count: digest.count + 1,
  )
}

pub fn remove(digest: Digest, id: Int) -> Digest {
  let hashed = splitmix64(id)
  Digest(
    xor: int.bitwise_exclusive_or(digest.xor, hashed),
    sum: int.bitwise_and(digest.sum - hashed + mask + 1, mask),
    count: digest.count - 1,
  )
}

pub fn of(ids: List(Int)) -> Digest {
  list.fold(ids, empty(), add)
}
