// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto.{ProtocolRange}
import bouncer_proto/blob
import bouncer_proto/codec_range.{CodecRange}
import bouncer_proto/epoch
import bouncer_proto/key.{BucketId, Key}
import bouncer_proto/lease
import bouncer_proto/rendezvous
import bouncer_proto/wire_epoch.{Shape}
import bouncer_proto/worker.{Profile, WorkerId}
import gleam/bit_array
import gleam/erlang/atom
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/set

pub fn buckets_stay_in_range_test() {
  let ids = [0, 1, 1_234_567_890_123_456_789, 999_999_999_999_999_999_999]
  assert list.all(ids, fn(id) {
    let bucket = key.bucket(id)
    bucket >= 0 && bucket < key.bucket_count
  })
  assert key.bucket_of(Key(ns: key.guild, id: 42))
    == BucketId(ns: key.guild, bucket: key.bucket(42))
  assert key.pinned_bucket(key.guild, 1) == Error(Nil)
  assert key.pinned_bucket(16, 3) == Ok(BucketId(ns: 16, bucket: 3))
  assert key.is_pinned(BucketId(ns: 16, bucket: 3))
}

pub fn select_matches_rendezvous_weights_test() {
  let nodes = list.map(["a@h", "b@h", "c@h", "d@h"], atom.create)
  let keys = [1, 2, 3, 99, 123_456_789]
  assert list.map(keys, rendezvous.select(_, nodes))
    == list.map(keys, rendezvous.select(_, list.reverse(nodes)))
  assert rendezvous.select(7, []) == Error(Nil)
}

pub fn homes_are_the_top_two_ordinals_test() {
  let bucket = BucketId(ns: key.presence, bucket: 17)
  let ranked = rendezvous.rank(bucket, [0, 1, 2])
  assert list.sort(ranked, fn(a, b) {
      case a < b {
        True -> order.Lt
        False -> order.Gt
      }
    })
    == [0, 1, 2]
  let assert [first, second, third] = ranked
  assert rendezvous.homes(bucket, [2, 1, 0])
    == Ok(rendezvous.Homes(home: first, successor: Some(second)))
  assert rendezvous.homes(bucket, [second, third])
    == Ok(rendezvous.Homes(home: second, successor: Some(third)))
  assert rendezvous.homes(bucket, [third])
    == Ok(rendezvous.Homes(home: third, successor: None))
  assert rendezvous.homes(bucket, []) == Error(Nil)
}

pub fn epochs_only_grow_test() {
  let zero = epoch.zero()
  let one = epoch.next(zero)
  assert epoch.max(zero, one) == one
  assert epoch.compare(one, zero) == order.Gt
  assert epoch.from_int(-1) == Error(Nil)
  assert epoch.to_int(epoch.floor_at(1000)) == 1_024_000
}

pub fn blob_round_trips_test() {
  let body = <<"tail state":utf8>>
  let assert Ok(bytes) =
    blob.wrap(
      ns: key.guild,
      codec_version: 3,
      part: blob.Tail,
      flags: 1,
      epoch: 1_800_000_000_000_000,
      body: body,
    )
  assert bit_array.byte_size(bytes) == blob.header_size + 10
  let assert Ok(#(header, decoded)) = blob.unwrap(bytes)
  assert decoded == body
  assert header.ns == key.guild
  assert header.codec_version == 3
  assert header.part == blob.Tail
  assert header.flags == 1
  assert header.epoch == 1_800_000_000_000_000
  assert blob.header(bytes) == Ok(header)
}

pub fn blob_rejects_damage_test() {
  let assert Ok(bytes) =
    blob.wrap(
      ns: 2,
      codec_version: 1,
      part: blob.Inbox,
      flags: 0,
      epoch: 5,
      body: <<1, 2, 3>>,
    )
  let assert Ok(head) = bit_array.slice(bytes, 0, blob.header_size)
  assert blob.unwrap(<<head:bits, 1, 2, 4>>) == Error(blob.ChecksumMismatch)
  assert blob.unwrap(<<head:bits, 1, 2>>)
    == Error(blob.SizeMismatch(declared: 3, actual: 2))
  assert blob.unwrap(<<"XXXX":utf8, 0>>) == Error(blob.BadMagic)
  assert blob.unwrap(<<"FXHB":utf8, 1>>) == Error(blob.Truncated)
  assert blob.wrap(
      ns: 256,
      codec_version: 1,
      part: blob.Inbox,
      flags: 0,
      epoch: 5,
      body: <<>>,
    )
    == Error(blob.FieldOutOfRange("ns"))
}

pub fn chunks_split_and_join_test() {
  let size = blob.chunk_size * 2 + 5
  let bits = size * 8
  let bytes = <<0:size(bits)>>
  let chunks = blob.split(bytes)
  assert list.length(chunks) == 3
  let indexed = list.index_map(chunks, fn(chunk, index) { #(index, chunk) })
  assert blob.join(list.reverse(indexed), 3) == Ok(bytes)
  assert blob.join(list.drop(indexed, 1), 3) == Error(blob.MissingChunk(0))
  assert blob.join([], 0) == Error(blob.NoChunks)
  let assert [first, ..] = indexed
  assert blob.join([first, ..indexed], 3) == Error(blob.DuplicateChunk(0))
  assert blob.split(<<>>) == [<<>>]
}

pub fn codec_versions_negotiate_test() {
  let old = CodecRange(ns: 0, oldest_read: 1, max_read: 1, max_write: 1)
  let new = CodecRange(ns: 0, oldest_read: 1, max_read: 2, max_write: 2)
  assert codec_range.write_version(new, old) == Ok(1)
  assert codec_range.write_version(old, new) == Ok(1)
  assert codec_range.write_version(new, new) == Ok(2)
  let far = CodecRange(ns: 0, oldest_read: 3, max_read: 4, max_write: 4)
  assert codec_range.write_version(old, far) == Error(Nil)
  assert codec_range.write_version(far, old) == Error(Nil)
  assert codec_range.can_move([new], [old], 0) == Ok(1)
  assert codec_range.can_move([new], [old], 2) == Error(Nil)
}

pub fn quorum_counts_configured_bouncers_test() {
  assert lease.quorum(1) == 1
  assert lease.quorum(3) == 2
  assert lease.quorum(5) == 3
  assert lease.has_quorum(2, 3)
  assert !lease.has_quorum(1, 3)
}

pub fn a_record_from_another_home_at_the_same_epoch_is_newer_by_seq_test() {
  let at = epoch.next(epoch.zero())
  let holder = WorkerId(atom.create("holder@node"), 1)
  let held =
    lease.BucketRecord(
      bucket: BucketId(0, 7),
      holder: Some(holder),
      epoch: at,
      pool: "default",
      state: lease.Held,
      written_by: lease.Writer(ordinal: 1, incarnation: 5, seq: 3),
    )
  let moving =
    lease.BucketRecord(
      ..held,
      state: lease.Moving(
        to: WorkerId(atom.create("target@node"), 1),
        to_epoch: epoch.next(at),
        pending: set.new(),
        commits: [],
      ),
      written_by: lease.Writer(ordinal: 0, incarnation: 9, seq: 4),
    )
  assert lease.newer(moving, held)
  assert !lease.newer(held, moving)
  assert !lease.newer(held, held)
}

pub fn top_epoch_is_the_higher_of_epoch_and_move_epoch_test() {
  let at = epoch.next(epoch.zero())
  let held =
    lease.BucketRecord(
      bucket: BucketId(0, 7),
      holder: None,
      epoch: at,
      pool: "default",
      state: lease.Held,
      written_by: lease.Writer(ordinal: 1, incarnation: 5, seq: 3),
    )
  let moving = fn(to_epoch) {
    lease.BucketRecord(
      ..held,
      state: lease.Moving(
        to: WorkerId(atom.create("target@node"), 1),
        to_epoch:,
        pending: set.new(),
        commits: [],
      ),
    )
  }
  assert lease.top_epoch(held) == at
  assert lease.top_epoch(lease.BucketRecord(..held, state: lease.Free)) == at
  assert lease.top_epoch(moving(epoch.next(at))) == epoch.next(at)
  assert lease.top_epoch(moving(epoch.zero())) == at
}

pub fn held_digest_ignores_order_test() {
  let one = lease.Claim(BucketId(0, 1), epoch.next(epoch.zero()))
  let two = lease.Claim(BucketId(2, 9), epoch.zero())
  assert lease.held_digest([one, two]) == lease.held_digest([two, one])
  assert lease.held_digest([one]) != lease.held_digest([two])
}

pub fn fingerprint_is_order_free_and_arity_aware_test() {
  let a = Shape("Inbox", [#("Join", 2), #("Leave", 2)])
  let b = Shape("Input", [#("Connect", 6)])
  assert wire_epoch.fingerprint([a, b])
    == wire_epoch.fingerprint([
      b,
      Shape("Inbox", [#("Leave", 2), #("Join", 2)]),
    ])
  assert wire_epoch.fingerprint([a, b])
    != wire_epoch.fingerprint([Shape("Inbox", [#("Join", 3), #("Leave", 2)]), b])
}

pub fn protocol_and_roster_test() {
  assert bouncer_proto.negotiate(ProtocolRange(1, 2), ProtocolRange(1, 1))
    == Ok(1)
  assert bouncer_proto.negotiate(ProtocolRange(2, 2), ProtocolRange(1, 1))
    == Error(Nil)
  let a = WorkerId(atom.create("a@h"), 1)
  let b = WorkerId(atom.create("b@h"), 1)
  let c = WorkerId(atom.create("c@h"), 1)
  assert worker.roster([a, b, c]) == worker.roster([c, a, b, a])
  assert worker.majority_of(worker.roster([a, b, c])) == 2
}

pub fn adjacent_epochs_are_bridged_only_from_the_first_bridged_epoch_test() {
  assert wire_epoch.compatible(4, 4)
  assert !wire_epoch.compatible(3, 4)
  assert !wire_epoch.compatible(4, 3)
  assert wire_epoch.compatible(4, 5)
  assert wire_epoch.compatible(6, 5)
  assert !wire_epoch.compatible(5, 7)
  assert !wire_epoch.compatible(9, 5)
  assert wire_epoch.floor(4) == 4
  assert wire_epoch.floor(5) == 4
  assert wire_epoch.floor(9) == 8
}

pub fn build_lines_accept_a_bridged_neighbour_epoch_test() {
  let base = Profile("guilds", "default", "a", "t", 5, [])
  assert worker.same_build_line(base, Profile(..base, wire_epoch: 6))
  assert worker.same_build_line(Profile(..base, wire_epoch: 6), base)
  assert !worker.same_build_line(base, Profile(..base, wire_epoch: 7))
  assert !worker.same_build_line(
    Profile(..base, wire_epoch: 3),
    Profile(..base, wire_epoch: 4),
  )
  assert !worker.same_build_line(base, Profile(..base, role: "presence"))
}
