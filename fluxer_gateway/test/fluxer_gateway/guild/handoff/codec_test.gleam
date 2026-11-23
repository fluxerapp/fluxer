// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range
import bouncer_proto/message
import bouncer_proto/worker
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/core/session.{SessionRef}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/handoff/wire_v3
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/handoff/codec
import fluxer_gateway/threads/store
import fluxer_gateway/voice/guild_handler
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import support/guild_handoff_fixtures as fixtures

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "filelib", "is_file")
fn is_file(path: String) -> Bool

fn fixture(part: String, version: Int) -> #(blob.Header, BitArray) {
  let assert Ok(bytes) = read_file(fixtures.path(part, version))
  let assert Ok(unwrapped) = blob.unwrap(bytes)
  unwrapped
}

fn versions() -> List(Int) {
  int.range(
    from: guild_codec.current,
    to: guild_codec.oldest_read - 1,
    with: [],
    run: list.prepend,
  )
}

pub fn every_readable_version_has_fixtures_test() {
  list.each(versions(), fn(version) {
    assert is_file(fixtures.path("tail", version))
  })
}

pub fn tail_fixtures_decode_upgrade_and_reencode_test() {
  list.each(versions(), fn(version) {
    let #(header, body) = fixture("tail", version)
    assert header.codec_version == version
    assert header.part == blob.Tail
    let assert Ok(record) = guild_codec.read_tail(version, body)
    assert record == fixtures.expected_tail()
    assert guild_codec.write_tail(record, version) == Ok(body)
    case version > guild_codec.oldest_read {
      True -> {
        let assert Ok(older) = guild_codec.write_tail(record, version - 1)
        assert guild_codec.read_tail(version - 1, older)
          == Ok(fixtures.expected_tail())
      }
      False -> Nil
    }
  })
}

fn tail_of(loaded: data.Loaded) -> guild_codec.Tail {
  guild_codec.Tail(
    loaded:,
    sessions: [],
    threads: store.export_handoff(store.new()),
    voice: guild_handler.new(loaded.data.facts),
    table: None,
    staged: None,
  )
}

fn writer(version: Int) -> codec.Codec(guild_codec.Tail) {
  guild_codec.tail_with(guild_codec.range_up_to(int.max(version, 2)))
}

pub fn v2_keeps_the_member_count_that_v1_derives_test() {
  let partial = fixtures.loaded([5, 6])
  let loaded =
    data.Loaded(..partial, data: data.set_member_count(partial.data, 5000))
  let count = fn(version) {
    let assert Ok(bytes) =
      codec.encode(writer(version), tail_of(loaded), version, 1)
    let assert Ok(#(_, decoded)) = codec.decode(guild_codec.tail(), bytes)
    core_guild.member_count(decoded.loaded.data.facts)
  }
  assert count(3) == 5000
  assert count(2) == 5000
  assert count(1) == 2
}

pub fn tail_round_trips_live_state_test() {
  let loaded = fixtures.loaded([5])
  let assert Ok(user) = id.user_id(5)
  let assert Ok(session_id) =
    session.session_id_from_string("0123456789abcdef0123456789abcdef")
  let sessions = [
    SessionRef(id: session_id, user:, inbox: process.new_subject()),
  ]
  let voice = guild_handler.new(loaded.data.facts)
  let threads = store.export_handoff(store.new())
  let tail =
    guild_codec.Tail(
      loaded:,
      sessions:,
      threads:,
      voice:,
      table: Some(guild_sessions.new()),
      staged: None,
    )
  list.each([1, 2, 3], fn(version) {
    let assert Ok(bytes) = codec.encode(writer(version), tail, version, 4)
    let assert Ok(#(_, decoded)) = codec.decode(guild_codec.tail(), bytes)
    assert decoded.sessions == sessions
    assert decoded.voice == voice
    assert decoded.threads == threads
    assert decoded.table == Some(guild_sessions.new())
    assert decoded.staged == None
    assert guild_codec.bulk_record(decoded.loaded, 0)
      == guild_codec.bulk_record(loaded, 0)
  })
}

pub fn a_tail_without_guild_state_is_refused_test() {
  list.each([2, 3], fn(version) {
    let assert Ok(body) =
      guild_codec.write_tail(
        wire_v3.GuildTailV3(..fixtures.expected_tail(), loaded: None),
        version,
      )
    let assert Ok(bytes) =
      blob.wrap(
        ns: 0,
        codec_version: version,
        part: blob.Tail,
        flags: 0,
        epoch: 4,
        body:,
      )
    assert codec.decode(guild_codec.tail(), bytes)
      == Error(codec.Malformed("guild tail without state"))
  })
}

pub fn previous_and_current_releases_interoperate_test() {
  let previous = guild_codec.range_up_to(guild_codec.current - 1)
  let current = guild_codec.range()
  assert codec_range.write_version(current, previous) == Ok(2)
  assert codec_range.write_version(previous, current) == Ok(2)
  assert codec_range.write_version(current, current) == Ok(3)
  let tail = tail_of(fixtures.loaded([5, 6]))
  let old_codec = guild_codec.tail_with(previous)
  let new_codec = guild_codec.tail()
  let assert Ok(rolled_back) = codec.encode(new_codec, tail, 2, 3)
  let assert Ok(#(_, on_old)) = codec.decode(old_codec, rolled_back)
  assert list.length(on_old.loaded.members) == 2
  let assert Ok(rolled_forward) = codec.encode(old_codec, tail, 2, 3)
  let assert Ok(#(_, on_new)) = codec.decode(new_codec, rolled_forward)
  assert list.length(on_new.loaded.members) == 2
  let assert Ok(too_new) = codec.encode(new_codec, tail, 3, 3)
  assert codec.decode(old_codec, too_new) == Error(codec.UnreadableVersion(3))
  assert codec.encode(old_codec, tail, 3, 3)
    == Error(codec.UnwritableVersion(3))
  assert codec.encode(new_codec, tail, 1, 3)
    == Error(codec.UnwritableVersion(1))
}

fn move_id() -> message.MoveId {
  message.MoveId(
    source: worker.WorkerId(node: atom.create("codec_source"), incarnation: 2),
    serial: 9,
  )
}

pub fn a_staged_tail_carries_only_changes_and_needs_version_three_test() {
  let tail =
    guild_codec.Tail(
      ..tail_of(fixtures.loaded([6])),
      staged: Some(guild_codec.Staged(move: move_id(), round: 4, removed: [7])),
    )
  let assert Ok(bytes) = codec.encode(guild_codec.tail(), tail, 3, 5)
  let assert Ok(#(header, decoded)) = codec.decode(guild_codec.tail(), bytes)
  assert header.codec_version == 3
  assert decoded.staged
    == Some(guild_codec.Staged(move: move_id(), round: 4, removed: [7]))
  assert guild_codec.bulk_record(decoded.loaded, 0)
    == guild_codec.bulk_record(fixtures.loaded([6]), 0)
  assert codec.encode(guild_codec.tail(), tail, 2, 5)
    == Error(codec.UnwritableVersion(2))
}

pub fn precopy_bulk_and_delta_round_trip_at_version_three_only_test() {
  let loaded = fixtures.loaded([5, 6, 7])
  let assert Ok(bulk) = codec.encode(guild_codec.bulk(), loaded, 3, 0)
  let assert Ok(#(_, back)) = codec.decode(guild_codec.bulk(), bulk)
  assert guild_codec.bulk_record(back, 0) == guild_codec.bulk_record(loaded, 0)
  assert codec.encode(guild_codec.bulk(), loaded, 2, 0)
    == Error(codec.UnwritableVersion(2))
  let delta =
    guild_codec.Delta(
      move: move_id(),
      round: 2,
      changed: fixtures.loaded([6]),
      removed: [5],
    )
  let assert Ok(bytes) = codec.encode(guild_codec.delta(), delta, 3, 0)
  let assert Ok(#(_, decoded)) = codec.decode(guild_codec.delta(), bytes)
  assert decoded.move == move_id()
  assert decoded.round == 2
  assert decoded.removed == [5]
  assert guild_codec.bulk_record(decoded.changed, 0)
    == guild_codec.bulk_record(fixtures.loaded([6]), 0)
  assert codec.decode(guild_codec.delta(), bulk)
    == Error(codec.Malformed("expected guild_delta_v3"))
  let assert Ok(old_tail) = codec.encode(writer(2), tail_of(loaded), 2, 0)
  assert codec.decode(guild_codec.bulk(), old_tail)
    == Error(codec.UnreadableVersion(2))
}

pub fn forwarded_inbox_messages_round_trip_test() {
  let assert Ok(session_id) =
    session.session_id_from_string("0123456789abcdef0123456789abcdef")
  let message = guild_codec.Public(core_guild.Leave(session_id, process.self()))
  let assert Ok(bytes) = codec.encode(guild_codec.inbox(), message, 2, 8)
  let assert Ok(#(header, decoded)) = codec.decode(guild_codec.inbox(), bytes)
  assert header.part == blob.Inbox
  assert decoded == message
}

pub fn corrupt_bodies_are_errors_not_crashes_test() {
  assert guild_codec.read_tail(2, <<1, 2, 3>>)
    == Error(codec.Malformed("unsafe or invalid term"))
  assert guild_codec.read_tail(2, codec.term_body(#("other", 1)))
    == Error(codec.Malformed("expected guild_tail_v2"))
  assert guild_codec.read_tail(3, codec.term_body(#("other", 1)))
    == Error(codec.Malformed("expected guild_tail_v3"))
  assert guild_codec.read_tail(4, <<>>) == Error(codec.UnreadableVersion(4))
}
