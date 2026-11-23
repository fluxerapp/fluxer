// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards
import fluxer_gateway/session/manager/transfer
import fluxer_gateway/session/pending_presences
import fluxer_gateway/session/seed.{type Seed}
import gleam/dict
import gleam/erlang/process.{type Name, type Subject}
import gleam/option.{None, Some}
import gleam/otp/static_supervisor
import gleam/set
import gleeunit/should
import support/isolation

fn ensure_transfer_store() -> Nil {
  case process.named(names.transfer()) {
    Ok(_) -> Nil
    Error(Nil) -> {
      let assert Ok(_) =
        static_supervisor.new(static_supervisor.OneForOne)
        |> static_supervisor.add(transfer.supervised(settings.defaults()))
        |> static_supervisor.start
      Nil
    }
  }
}

fn session_ref(user: Int) -> SessionRef {
  let assert Ok(user) = id.user_id(user)
  session.SessionRef(
    id: session.new_session_id(),
    user:,
    inbox: process.new_subject(),
  )
}

pub fn locate_finds_session_live_on_a_peer_test() {
  registry.create_tables()
  ensure_transfer_store()
  let live = session_ref(7)
  let key = session.session_id_to_string(live.id)
  registry.insert(live)

  transfer.locate_on([node.self()], key, 1000) |> should.equal(Ok(live))

  registry.remove(live)
}

pub fn locate_misses_when_no_peer_has_the_session_test() {
  registry.create_tables()
  ensure_transfer_store()
  let absent = session.session_id_to_string(session.new_session_id())

  transfer.locate_on([node.self()], absent, 1000) |> should.equal(Error(Nil))
  transfer.locate_on([], absent, 1000) |> should.equal(Error(Nil))
}

fn sample_seed() -> Seed {
  let identify =
    session.Identify(
      token: "token",
      properties: session.ClientProperties(os: "", browser: "", device: ""),
      shard: None,
      presence: None,
      flags: session.IdentifyFlags(
        debounce_message_reactions: False,
        channel_threads: False,
        guild_visits: False,
      ),
      ignored_events: [],
      initial_guild_id: None,
      e2ee_capable: False,
      mobile: False,
      latitude: None,
      longitude: None,
    )
  let data = <<"{\"user\":{\"id\":\"123456789012345678\"}}">>
  let assert Ok(found) =
    seed.from_api(session.new_session_id(), identify, 1, data)
  found
}

fn fake_shard(
  name: Name(shards.Message),
  ready: Subject(Subject(Nil)),
  forwarded: Subject(shards.Message),
) -> Nil {
  let original = process.named(name)
  let _ = process.unregister(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  let hold = process.new_subject()
  process.send(ready, hold)
  case process.receive(process.named_subject(name), 2000) {
    Ok(message) -> process.send(forwarded, message)
    Error(Nil) -> Nil
  }
  let _ = process.receive(hold, 3000)
  let _ = process.unregister(name)
  case original {
    Ok(pid) -> {
      let _ = process.register(pid, name)
      Nil
    }
    Error(Nil) -> Nil
  }
}

fn commit_through_fake_shard(
  answer: Result(SessionRef, Nil),
) -> #(Seed, Seed, transfer.Committed) {
  commit_seed_through_fake_shard(sample_seed(), True, answer)
}

fn commit_seed_through_fake_shard(
  found: Seed,
  speaks_moves: Bool,
  answer: Result(SessionRef, Nil),
) -> #(Seed, Seed, transfer.Committed) {
  ensure_transfer_store()
  let key = session.session_id_to_string(found.id)
  let ready = process.new_subject()
  let forwarded = process.new_subject()
  process.spawn_unlinked(fn() {
    fake_shard(names.shard(shards.index(key)), ready, forwarded)
  })
  let assert Ok(hold) = process.receive(ready, 1000)

  let assert Ok(pushed) = transfer.push(node.self(), found, speaks_moves)
  let committed = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(committed, transfer.commit(pushed))
  })

  let assert Ok(shards.Adopt(adopted, reply)) = process.receive(forwarded, 1000)
  process.send(reply, answer)
  let assert Ok(outcome) = process.receive(committed, 2000)
  process.send(hold, Nil)
  #(found, adopted, outcome)
}

pub fn committed_seed_is_handed_to_its_shard_at_once_test() {
  let target = session_ref(9)
  let #(found, adopted, outcome) = commit_through_fake_shard(Ok(target))
  adopted.id |> should.equal(found.id)
  adopted.token_hash |> should.equal(found.token_hash)
  outcome |> should.equal(transfer.Adopted(target))
}

@external(erlang, "erlang", "tuple_size")
fn tuple_size(seed: Seed) -> Int

fn moved_seed() -> Seed {
  seed.Seed(
    ..sample_seed(),
    moved: Some(seed.Moved(
      source: session_ref(3),
      linked: [],
      presence: None,
      presence_targets: set.new(),
      dm_mutual: dict.new(),
      suppress_presences: False,
      pending_presences: pending_presences.new(),
    )),
  )
}

pub fn a_seed_for_an_epoch_four_peer_has_the_epoch_four_shape_test() {
  let found = moved_seed()
  assert tuple_size(found) == 33
  let old = seed.for_peer(found, False)
  assert tuple_size(old) == 32
  assert seed.from_wire(old) == seed.Seed(..found, moved: None)
  assert seed.for_peer(found, True) == found
  assert seed.from_wire(found) == found
}

pub fn a_seed_from_an_epoch_four_source_is_adopted_without_a_move_test() {
  let target = session_ref(11)
  let found = moved_seed()
  let #(_, adopted, outcome) =
    commit_seed_through_fake_shard(found, False, Ok(target))
  assert outcome == transfer.Adopted(target)
  assert tuple_size(adopted) == 33
  assert adopted == seed.Seed(..found, moved: None)
}

pub fn commit_reports_a_target_that_refused_the_seed_test() {
  let #(_, _, outcome) = commit_through_fake_shard(Error(Nil))
  outcome |> should.equal(transfer.Refused)
}

pub fn commit_with_a_stale_nonce_is_refused_at_once_test() {
  ensure_transfer_store()
  let found = sample_seed()
  let key = session.session_id_to_string(found.id)
  let reply = process.new_subject()
  process.send(
    process.named_subject(names.transfer()),
    transfer.Commit(key, -1, reply),
  )
  process.receive(reply, 1000) |> should.equal(Ok(Error(Nil)))
}

pub fn silent_shard_leaves_the_commit_unconfirmed_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      session: settings.SessionDurations(
        ..d.session,
        transfer_push_timeout_ms: 300,
        transfer_locate_timeout_ms: 300,
      ),
    )
  })
  ensure_transfer_store()
  let found = sample_seed()
  let key = session.session_id_to_string(found.id)
  let ready = process.new_subject()
  let forwarded = process.new_subject()
  process.spawn_unlinked(fn() {
    fake_shard(names.shard(shards.index(key)), ready, forwarded)
  })
  let assert Ok(hold) = process.receive(ready, 1000)
  let assert Ok(pushed) = transfer.push(node.self(), found, True)
  transfer.commit(pushed) |> should.equal(transfer.Unconfirmed)
  let assert Ok(shards.Adopt(_, _)) = process.receive(forwarded, 1000)
  process.send(hold, Nil)
}

pub fn shard_dying_before_it_answers_refuses_the_commit_at_once_test() {
  ensure_transfer_store()
  let found = sample_seed()
  let key = session.session_id_to_string(found.id)
  let ready = process.new_subject()
  let forwarded = process.new_subject()
  process.spawn_unlinked(fn() {
    fake_shard(names.shard(shards.index(key)), ready, forwarded)
  })
  let assert Ok(hold) = process.receive(ready, 1000)
  let assert Ok(pushed) = transfer.push(node.self(), found, True)
  let committed = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(committed, transfer.commit(pushed))
  })
  let assert Ok(shards.Adopt(_, _)) = process.receive(forwarded, 1000)
  process.send(hold, Nil)
  process.receive(committed, 1000) |> should.equal(Ok(transfer.Refused))
}

pub fn abort_of_an_unknown_entry_reports_nothing_aborted_test() {
  ensure_transfer_store()
  let reply = process.new_subject()
  process.send(
    process.named_subject(names.transfer()),
    transfer.Abort("missing", 1, reply),
  )
  process.receive(reply, 1000) |> should.equal(Ok(False))
}
