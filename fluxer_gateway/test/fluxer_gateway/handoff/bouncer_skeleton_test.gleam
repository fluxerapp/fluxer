// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range.{CodecRange}
import bouncer_proto/epoch
import bouncer_proto/key.{BucketId, Key}
import fluxer_gateway/cluster/buckets
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/rendezvous
import fluxer_gateway/config/build_info
import fluxer_gateway/core/node
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/handoff/codec.{Codec}
import fluxer_gateway/presence/handoff/codec as presence_codec
import gleam/bit_array
import gleam/erlang/atom
import gleam/list
import gleam/result

fn text_codec() -> codec.Codec(String) {
  Codec(
    ns: 7,
    part: blob.Tail,
    range: CodecRange(ns: 7, oldest_read: 1, max_read: 2, max_write: 2),
    encode_body: fn(value, _version) { Ok(<<value:utf8>>) },
    decode_body: fn(_version, body) {
      bit_array.to_string(body) |> result.replace_error(codec.Malformed("utf8"))
    },
  )
}

pub fn codec_envelope_round_trips_test() {
  let codec = text_codec()
  let assert Ok(bytes) = codec.encode(codec, "guild tail", 1, 9)
  let assert Ok(#(header, value)) = codec.decode(codec, bytes)
  assert value == "guild tail"
  assert header.codec_version == 1
  assert header.epoch == 9
  assert codec.encode(codec, "x", 0, 9) == Error(codec.UnwritableVersion(0))
  assert codec.encode(codec, "x", 3, 9) == Error(codec.UnwritableVersion(3))
}

pub fn codec_rejects_other_kinds_parts_and_versions_test() {
  let codec = text_codec()
  let assert Ok(other_kind) =
    blob.wrap(
      ns: 8,
      codec_version: 1,
      part: blob.Tail,
      flags: 0,
      epoch: 1,
      body: <<>>,
    )
  assert codec.decode(codec, other_kind)
    == Error(codec.WrongKind(expected: 7, found: 8))
  let assert Ok(other_part) =
    blob.wrap(
      ns: 7,
      codec_version: 1,
      part: blob.Inbox,
      flags: 0,
      epoch: 1,
      body: <<>>,
    )
  assert codec.decode(codec, other_part)
    == Error(codec.WrongPart(expected: blob.Tail, found: blob.Inbox))
  let assert Ok(future) =
    blob.wrap(
      ns: 7,
      codec_version: 3,
      part: blob.Tail,
      flags: 0,
      epoch: 1,
      body: <<>>,
    )
  assert codec.decode(codec, future) == Error(codec.UnreadableVersion(3))
  assert codec.decode(codec, <<1, 2, 3>>)
    == Error(codec.Envelope(blob.BadMagic))
}

pub fn codecs_advertise_their_namespaces_test() {
  assert guild_codec.range()
    == CodecRange(ns: key.guild, oldest_read: 1, max_read: 3, max_write: 3)
  assert guild_codec.range_up_to(2)
    == CodecRange(ns: key.guild, oldest_read: 1, max_read: 2, max_write: 2)
  assert guild_codec.range_up_to(1)
    == CodecRange(ns: key.guild, oldest_read: 1, max_read: 1, max_write: 1)
  assert presence_codec.range()
    == CodecRange(ns: key.presence, oldest_read: 1, max_read: 1, max_write: 1)
  assert guild_codec.tail().part == blob.Tail
  assert guild_codec.inbox().part == blob.Inbox
  assert presence_codec.tail().ns == key.presence
  assert build_info.wire_epoch >= 1
}

pub fn lease_validity_subtracts_the_skew_margin_test() {
  assert lease.valid_until(10_000, 6000, 1000) == 15_000
  let bucket = BucketId(ns: key.guild, bucket: 3)
  assert lease.check(bucket) == Error(lease.NotHeld(bucket))
  assert buckets.placement(Key(ns: key.guild, id: 1)) == buckets.Unplaced(50)
  assert epoch.to_int(epoch.zero()) == 0
}

pub fn rendezvous_reexport_keeps_node_identity_test() {
  let peers =
    list.map(["a@h", "b@h", "c@h"], fn(name) {
      node.from_atom(atom.create(name))
    })
  let assert Ok(chosen) = rendezvous.select(42, peers)
  assert list.contains(peers, chosen)
}
