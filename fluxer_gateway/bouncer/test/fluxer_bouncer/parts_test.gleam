// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/blob.{Inbox, Tail}
import bouncer_proto/codec_range
import bouncer_proto/epoch
import bouncer_proto/key.{BucketId}
import bouncer_proto/lease.{BucketRecord, Held, Writer}
import bouncer_proto/message.{
  Beat, Hello, HolderHeard, MoveId, RosterAlive, Superseded,
}
import bouncer_proto/worker.{Draining, Gone, Profile, Ready, WorkerId}
import fluxer_bouncer/blob_store
import fluxer_bouncer/directory
import fluxer_bouncer/freeze_buffer
import fluxer_bouncer/leases
import fluxer_bouncer/sim
import gleam/erlang/atom
import gleam/list
import gleam/option.{Some}
import gleam/set

fn worker_id(name: String) {
  WorkerId(atom.create(name), 1)
}

pub fn freeze_buffer_keeps_order_and_bounds_test() {
  let buffer = freeze_buffer.new(4)
  let assert Ok(buffer) = freeze_buffer.push(buffer, 1, <<"ab">>)
  assert freeze_buffer.push(buffer, 3, <<"c">>)
    == Error(freeze_buffer.OutOfOrder(expected: 2, got: 3))
  let assert Ok(buffer) = freeze_buffer.push(buffer, 2, <<"cd">>)
  assert freeze_buffer.push(buffer, 3, <<"e">>)
    == Error(freeze_buffer.Overflow(4))
  assert freeze_buffer.entries_from(buffer, 1)
    == [#(1, <<"ab">>), #(2, <<"cd">>)]
  let assert Ok(sealed) = freeze_buffer.seal(buffer, 2)
  assert freeze_buffer.is_complete(sealed)
  assert freeze_buffer.push(sealed, 3, <<>>)
    == Error(freeze_buffer.AlreadySealed(2))
  let trimmed = freeze_buffer.drop_through(sealed, 1)
  assert freeze_buffer.entries_from(trimmed, 0) == [#(2, <<"cd">>)]
  assert freeze_buffer.size_bytes(trimmed) == 2
}

pub fn blob_store_budgets_chunks_and_expires_test() {
  let move = MoveId(worker_id("s@x"), 1)
  let store = blob_store.new(10)
  assert blob_store.admit(store, move, 11, 0)
    == Error(blob_store.OverBudget(10))
  let assert Ok(store) = blob_store.admit(store, move, 2, 0)
  let assert Ok(store) = blob_store.put_chunk(store, move, Tail, 1, 2, <<"b">>)
  assert !blob_store.complete(store, move, Tail)
  assert blob_store.put_chunk(store, move, Tail, 1, 3, <<"b">>)
    == Error(blob_store.ChunkMismatch(Tail, 1))
  let assert Ok(store) = blob_store.put_chunk(store, move, Tail, 0, 2, <<"a">>)
  let assert Ok(store) = blob_store.put_chunk(store, move, Tail, 0, 2, <<"a">>)
  assert blob_store.chunks(store, move, Tail)
    == Ok([#(0, <<"a">>), #(1, <<"b">>)])
  assert blob_store.chunks(store, move, Inbox)
    == Error(blob_store.Incomplete(Inbox, 1))
  assert blob_store.put_chunk(store, move, Inbox, 0, 1, <<"123456789">>)
    == Error(blob_store.OverBudget(10))
  let #(kept, gone) = blob_store.expire(store, 50, 60)
  assert gone == []
  let #(emptied, gone) = blob_store.expire(kept, 61, 60)
  assert gone == [move]
  assert blob_store.used(emptied) == 0
}

fn hello(name: String) {
  Hello(
    worker: worker_id(name),
    profile: sim.profile(),
    protocol: bouncer_proto.own_range(),
    pinned: [],
  )
}

fn beat(name: String, draining: Bool) {
  Beat(
    worker: worker_id(name),
    seq: 1,
    sent_mono: 0,
    held_digest: 0,
    roster_hash: 77,
    draining: draining,
    load: 0,
    unreachable: [],
  )
}

pub fn directory_tracks_status_roster_and_silence_test() {
  let assert Ok(#(found, _)) = directory.hello(directory.new(), hello("a@x"), 0)
  let assert Ok(#(found, _)) = directory.hello(found, hello("b@x"), 0)
  assert directory.roster(found).members == []
  let assert Ok(#(found, entry)) =
    directory.beat(found, beat("a@x", False), 100)
  assert entry.status == Ready
  let assert Ok(#(found, entry)) = directory.beat(found, beat("b@x", True), 100)
  assert entry.status == Draining
  assert directory.roster(found).members == [worker_id("a@x"), worker_id("b@x")]
  assert directory.reported_roster(found, worker_id("a@x")) == Ok(77)
  assert list.map(directory.candidates(found, BucketId(0, 1)), fn(e) {
      e.worker
    })
    == [worker_id("a@x")]
  assert directory.silent_ms(found, worker_id("a@x"), 600) == Ok(500)
  let #(found, gone) = directory.expire(found, 700, 550)
  assert gone == [worker_id("a@x")]
    || gone == [worker_id("b@x"), worker_id("a@x")]
  let assert Ok(entry) = directory.lookup(found, atom.create("a@x"))
  assert entry.status == Gone
  let assert Ok(found) = directory.bye(found, worker_id("b@x"))
  assert directory.said_bye(found, worker_id("b@x"))
  let newer = Hello(..hello("a@x"), worker: WorkerId(atom.create("a@x"), 2))
  let assert Ok(#(found, _)) = directory.hello(found, newer, 800)
  assert directory.superseded(found, worker_id("a@x"))
  assert directory.beat(found, beat("a@x", False), 900)
    == Error(directory.StaleIncarnation(worker_id("a@x")))
}

fn held(holder: String, at: Int) {
  let assert Ok(at) = epoch.from_int(at)
  BucketRecord(
    bucket: BucketId(0, 9),
    holder: Some(worker_id(holder)),
    epoch: at,
    pool: "default",
    state: Held,
    written_by: leases.absent_writer(),
  )
}

fn view(silent: Int, beating: Bool) {
  leases.HolderView(
    silent_ms: silent,
    roster_majority_beating: beating,
    said_bye: False,
    new_incarnation: False,
  )
}

pub fn leases_compare_and_set_and_regrant_rules_test() {
  let home = leases.new(0, 1) |> leases.finish_recovery
  let peer = leases.new(1, 1) |> leases.finish_recovery
  let assert Ok(#(home, first)) = leases.propose(home, held("a@x", 5))
  assert first.record.written_by == Writer(0, 1, 1)
  let apply = fn(leases, proposal: leases.Proposal, holder) {
    leases.apply_write(
      leases,
      proposal.expect_epoch,
      proposal.expect_writer,
      proposal.record,
      holder,
      8000,
      121_000,
    )
  }
  let #(peer, outcome) = apply(peer, first, view(0, True))
  assert outcome == Ok(Nil)
  let #(peer, again) = apply(peer, first, view(0, True))
  assert again == Ok(Nil)
  let home = leases.commit(home, first.record)
  let assert Ok(#(_, regrant)) = leases.propose(home, held("b@x", 6))
  assert apply(peer, regrant, view(3000, True)).1 == Error(HolderHeard(3000))
  assert apply(peer, regrant, view(9000, False)).1 == Error(RosterAlive)
  assert apply(peer, regrant, view(122_000, False)).1 == Ok(Nil)
  let #(peer, outcome) = apply(peer, regrant, view(9000, True))
  assert outcome == Ok(Nil)
  let #(_, stale) = apply(peer, first, view(0, True))
  assert stale
    == Error(Superseded(regrant.record.epoch, regrant.record.written_by))
}

pub fn leases_adopt_only_newer_or_retracted_records_test() {
  let home = leases.new(0, 1) |> leases.finish_recovery
  let assert Ok(#(home, first)) = leases.propose(home, held("a@x", 5))
  let home = leases.commit(home, first.record)
  let assert Ok(#(home, junk)) = leases.propose(home, held("b@x", 6))
  let peer = leases.new(1, 1) |> leases.finish_recovery
  let #(peer, _) =
    leases.apply_write(
      peer,
      epoch.zero(),
      leases.absent_writer(),
      first.record,
      view(0, True),
      8000,
      121_000,
    )
  let #(peer, _) =
    leases.apply_write(
      peer,
      junk.expect_epoch,
      junk.expect_writer,
      junk.record,
      view(9000, True),
      8000,
      121_000,
    )
  let never = fn(_) { leases.Present }
  assert leases.record(leases.adopt(peer, first.record, never), BucketId(0, 9))
    == Ok(junk.record)
  let retracted =
    leases.adopt(peer, first.record, fn(writer) {
      case writer.ordinal == 0 {
        True -> leases.Retracting
        False -> leases.Present
      }
    })
  let orphaned = leases.adopt(peer, first.record, fn(_) { leases.Departed })
  assert leases.record(orphaned, BucketId(0, 9)) == Ok(junk.record)
  assert leases.is_confirmed(orphaned, BucketId(0, 9))
  let #(settled, promoted) =
    leases.settle_orphans(peer, fn(_) { leases.Departed })
  assert promoted == [junk.record]
  assert leases.is_confirmed(settled, BucketId(0, 9))
  assert leases.settle_orphans(peer, fn(_) { leases.Present }).1 == []
  assert leases.record(retracted, BucketId(0, 9)) == Ok(first.record)
  assert leases.is_confirmed(retracted, BucketId(0, 9))
  let reverted = leases.revert(home, junk)
  assert leases.record(reverted, BucketId(0, 9)) == Ok(first.record)
  assert leases.held_by(reverted, worker_id("a@x"))
    == [lease.Claim(BucketId(0, 9), first.record.epoch)]
  assert leases.propose(leases.new(0, 1), held("a@x", 1))
    == Error(leases.StillInRecovery)
}

pub fn finishing_a_move_with_entities_left_counts_as_a_regrant_test() {
  let assert Ok(at) = epoch.from_int(5)
  let moving = fn(pending) {
    BucketRecord(
      ..held("a@x", 5),
      state: lease.Moving(
        to: worker_id("b@x"),
        to_epoch: epoch.next(at),
        pending: set.from_list(pending),
        commits: [],
      ),
    )
  }
  let finished = held("b@x", 6)
  assert leases.is_regrant(moving([]), finished) == False
  assert leases.is_regrant(moving([1]), finished)
  assert leases.is_regrant(held("a@x", 5), moving([1])) == False
  assert leases.is_regrant(held("a@x", 5), finished)
}

fn hello_as(name: String, role: String, ns: Int) {
  Hello(
    ..hello(name),
    profile: Profile(..sim.profile(), role:, codecs: [codec_range.unmovable(ns)]),
  )
}

pub fn unmovable_namespaces_place_only_on_workers_that_advertise_them_test() {
  let assert Ok(#(found, _)) = directory.hello(directory.new(), hello("g@x"), 0)
  let assert Ok(#(found, _)) =
    directory.hello(found, hello_as("c@x", "calls", key.call), 0)
  let assert Ok(#(found, _)) =
    directory.hello(found, hello_as("p@x", "push", key.push), 0)
  let assert Ok(#(found, _)) = directory.beat(found, beat("g@x", False), 10)
  let assert Ok(#(found, _)) = directory.beat(found, beat("c@x", False), 10)
  let assert Ok(#(found, _)) = directory.beat(found, beat("p@x", False), 10)
  let workers = fn(bucket) {
    list.map(directory.candidates(found, bucket), fn(e) { e.worker })
  }
  assert workers(BucketId(key.call, 5)) == [worker_id("c@x")]
  assert workers(BucketId(key.push, 5)) == [worker_id("p@x")]
  assert workers(BucketId(key.guild, 5)) == [worker_id("g@x")]
  assert workers(BucketId(key.session, 5)) == []
  let assert Ok(#(found, _)) = directory.beat(found, beat("c@x", True), 20)
  assert list.map(directory.candidates(found, BucketId(key.call, 5)), fn(e) {
      e.worker
    })
    == []
}

fn claim(at: Int) {
  let assert Ok(at) = epoch.from_int(at)
  lease.Claim(BucketId(0, 9), at)
}

pub fn a_claim_at_a_higher_epoch_wins_over_an_earlier_claim_test() {
  let rebuilt =
    leases.new(0, 1)
    |> leases.claim(worker_id("a@x"), "default", [claim(5)])
    |> leases.claim(worker_id("b@x"), "default", [claim(7)])
    |> leases.claim(worker_id("c@x"), "default", [claim(6)])
  let assert Ok(found) = leases.record(rebuilt, BucketId(0, 9))
  assert found.holder == Some(worker_id("b@x"))
  assert found.epoch == held("b@x", 7).epoch
  assert leases.is_confirmed(rebuilt, BucketId(0, 9))
}

pub fn sync_orders_records_by_epoch_before_writer_seq_test() {
  let rebuilt =
    leases.new(0, 1)
    |> leases.claim(worker_id("b@x"), "default", [claim(7)])
  let stale = BucketRecord(..held("a@x", 6), written_by: Writer(1, 1, 40))
  let kept = leases.adopt(rebuilt, stale, fn(_) { leases.Present })
  assert leases.record(kept, BucketId(0, 9))
    == leases.record(rebuilt, BucketId(0, 9))
  let chained = BucketRecord(..held("b@x", 7), written_by: Writer(1, 1, 41))
  let taken = leases.adopt(rebuilt, chained, fn(_) { leases.Present })
  assert leases.record(taken, BucketId(0, 9)) == Ok(chained)
}

pub fn a_write_built_on_a_record_the_acceptor_lacks_is_refused_test() {
  let fresh = leases.new(1, 1) |> leases.finish_recovery
  let base = BucketRecord(..held("a@x", 5), written_by: Writer(0, 1, 3))
  let regrant = BucketRecord(..held("b@x", 6), written_by: Writer(0, 1, 4))
  let #(after, outcome) =
    leases.apply_write(
      fresh,
      base.epoch,
      base.written_by,
      regrant,
      view(0, True),
      8000,
      121_000,
    )
  assert outcome == Error(message.StillRecovering)
  assert leases.record(after, BucketId(0, 9))
    == Error(leases.UnknownBucket(BucketId(0, 9)))
  let first = BucketRecord(..held("a@x", 5), written_by: Writer(0, 1, 1))
  let #(_, accepted) =
    leases.apply_write(
      fresh,
      epoch.zero(),
      leases.absent_writer(),
      first,
      view(0, True),
      8000,
      121_000,
    )
  assert accepted == Ok(Nil)
}
