// SPDX-License-Identifier: AGPL-3.0-or-later

pub const bucket_count = 1024

pub const guild = 0

pub const session = 1

pub const presence = 2

pub const call = 3

pub const push = 4

pub const first_pinned = 16

pub type Key {
  Key(ns: Int, id: Int)
}

pub type BucketId {
  BucketId(ns: Int, bucket: Int)
}

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

pub fn bucket(id: Int) -> Int {
  phash2(id, bucket_count)
}

pub fn bucket_of(key: Key) -> BucketId {
  BucketId(ns: key.ns, bucket: bucket(key.id))
}

pub fn pinned_bucket(pool_ns: Int, index: Int) -> Result(BucketId, Nil) {
  case pool_ns >= first_pinned && index >= 0 && index < bucket_count {
    True -> Ok(BucketId(ns: pool_ns, bucket: index))
    False -> Error(Nil)
  }
}

pub fn is_pinned(bucket_id: BucketId) -> Bool {
  bucket_id.ns >= first_pinned
}
