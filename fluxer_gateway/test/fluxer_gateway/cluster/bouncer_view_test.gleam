// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{BucketId}
import bouncer_proto/lease.{
  BucketRecord, Claim, CommitRow, Free, Held, Moving, Writer,
}
import bouncer_proto/worker.{Entry, Gone, Profile, Ready, WorkerId}
import fluxer_gateway/cluster/bouncer_view.{Drop, Take}
import fluxer_gateway/cluster/lease as worker_lease
import fluxer_gateway/cluster/lease_probe
import gleam/dict
import gleam/erlang/atom
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string

fn at(name: String) -> atom.Atom {
  atom.create(name)
}

fn epoch_of(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

fn entry(name: String, incarnation: Int, beat: Int) {
  Entry(
    worker: WorkerId(at(name), incarnation),
    profile: Profile("guilds", "default", "dev", "", 1, []),
    status: Ready,
    last_beat_ms: beat,
  )
}

fn three() -> bouncer_view.View {
  bouncer_view.new([at("b0@h"), at("b1@h"), at("b2@h")])
}

fn roster() -> worker.Roster {
  worker.roster([WorkerId(at("w@h"), 1)])
}

pub fn a_beat_counts_once_a_majority_of_welcomed_bouncers_matched_test() {
  let view =
    three()
    |> bouncer_view.welcome(0, 10, [], roster(), 0)
    |> bouncer_view.welcome(1, 11, [], roster(), 0)
  let #(view, seq) = bouncer_view.beat(view, 100, 6000)
  let #(view, first) = bouncer_view.ack(view, 0, seq, True, roster(), 101)
  assert first == None
  let #(view, unmatched) = bouncer_view.ack(view, 1, seq, False, roster(), 101)
  assert unmatched == None
  let #(view, stranger) = bouncer_view.ack(view, 2, seq, True, roster(), 101)
  assert stranger == None
  let #(view, second) = bouncer_view.ack(view, 1, seq, True, roster(), 102)
  assert second == Some(100)
  let #(view, again) = bouncer_view.ack(view, 1, seq, True, roster(), 103)
  assert again == None
  assert bouncer_view.acked_mono(view) == Some(100)
}

pub fn an_older_beat_never_moves_validity_back_test() {
  let view =
    three()
    |> bouncer_view.welcome(0, 10, [], roster(), 0)
    |> bouncer_view.welcome(1, 11, [], roster(), 0)
  let #(view, old) = bouncer_view.beat(view, 100, 6000)
  let #(view, new) = bouncer_view.beat(view, 200, 6000)
  let #(view, _) = bouncer_view.ack(view, 0, new, True, roster(), 201)
  let #(view, newest) = bouncer_view.ack(view, 1, new, True, roster(), 201)
  assert newest == Some(200)
  let #(view, _) = bouncer_view.ack(view, 0, old, True, roster(), 202)
  let #(view, late) = bouncer_view.ack(view, 1, old, True, roster(), 202)
  assert late == None
  assert bouncer_view.acked_mono(view) == Some(200)
}

pub fn majority_follows_the_configured_count_test() {
  let view = three() |> bouncer_view.welcome(2, 1, [], roster(), 0)
  assert !bouncer_view.majority(view)
  let view = bouncer_view.welcome(view, 0, 1, [], roster(), 0)
  assert bouncer_view.majority(view)
  assert bouncer_view.welcomed(view) == [0, 2]
  assert bouncer_view.unwelcomed(view) == [1]
  let view = bouncer_view.drop(view, 2)
  assert !bouncer_view.majority(view)
  let single = bouncer_view.new([at("b0@h")])
  assert !bouncer_view.majority(single)
  assert bouncer_view.majority(bouncer_view.welcome(
    single,
    0,
    1,
    [],
    roster(),
    0,
  ))
}

pub fn silent_bouncers_are_reported_after_the_ttl_test() {
  let view =
    three()
    |> bouncer_view.welcome(0, 1, [], roster(), 0)
    |> bouncer_view.welcome(1, 1, [], roster(), 0)
  let #(view, seq) = bouncer_view.beat(view, 5000, 6000)
  let #(view, _) = bouncer_view.ack(view, 1, seq, True, roster(), 5000)
  assert bouncer_view.silent(view, 6500, 6000) == [0]
  assert bouncer_view.silent(view, 5500, 6000) == []
}

pub fn the_directory_merges_bouncers_and_removes_only_the_named_incarnation_test() {
  let view =
    three()
    |> bouncer_view.welcome(
      0,
      1,
      [entry("a@h", 1, 10), entry("b@h", 1, 10)],
      roster(),
      0,
    )
    |> bouncer_view.welcome(1, 1, [entry("a@h", 1, 30)], roster(), 0)
  let merged =
    bouncer_view.directory(view)
    |> list.map(fn(found) {
      #(atom.to_string(found.worker.node), found.last_beat_ms)
    })
    |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
  assert merged == [#("a@h", 30), #("b@h", 10)]
  let view =
    bouncer_view.directory_delta(view, 0, [], [WorkerId(at("b@h"), 2)], 1)
  assert list.length(bouncer_view.directory(view)) == 2
  let view =
    bouncer_view.directory_delta(view, 0, [], [WorkerId(at("b@h"), 1)], 1)
  assert list.map(bouncer_view.directory(view), fn(found) { found.worker.node })
    == [at("a@h")]
  let gone = Entry(..entry("a@h", 1, 40), status: Gone)
  let view = bouncer_view.directory_delta(view, 1, [gone], [], 2)
  assert list.map(bouncer_view.directory(view), fn(found) { found.status })
    == [Gone]
}

fn record(bucket: Int, holder, at_epoch: Int, state) {
  BucketRecord(
    bucket: BucketId(key.guild, bucket),
    holder:,
    epoch: epoch_of(at_epoch),
    pool: "default",
    state:,
    written_by: Writer(0, 1, 1),
  )
}

pub fn reconcile_takes_grants_and_drops_lost_buckets_test() {
  let me = WorkerId(at("me@h"), 5)
  let other = WorkerId(at("other@h"), 1)
  let older_me = WorkerId(at("me@h"), 4)
  let held = [
    Claim(BucketId(key.guild, 2), epoch_of(3)),
    Claim(BucketId(key.guild, 3), epoch_of(9)),
    Claim(BucketId(key.guild, 4), epoch_of(2)),
  ]
  let records = [
    record(1, Some(me), 7, Held),
    record(2, Some(other), 4, Held),
    record(3, Some(other), 8, Held),
    record(4, Some(me), 2, Held),
    record(5, Some(other), 6, Moving(me, epoch_of(7), set.new(), [])),
    record(6, Some(older_me), 3, Held),
    record(7, None, 1, Free),
  ]
  assert bouncer_view.reconcile(me, held, records, dict.new())
    == [
      Take(BucketId(key.guild, 1), epoch_of(7)),
      Drop(BucketId(key.guild, 2)),
      Take(BucketId(key.guild, 5), epoch_of(7)),
    ]
  let source =
    record(
      8,
      Some(me),
      4,
      Moving(other, epoch_of(5), set.from_list([1]), [CommitRow(2, <<>>)]),
    )
  assert bouncer_view.reconcile(
      me,
      [Claim(BucketId(key.guild, 8), epoch_of(4))],
      [source],
      dict.new(),
    )
    == []
}

pub fn a_fenced_bucket_is_taken_again_only_at_a_higher_epoch_test() {
  let me = WorkerId(at("me@h"), 5)
  let fenced = dict.from_list([#(BucketId(key.guild, 1), epoch_of(7))])
  assert bouncer_view.reconcile(me, [], [record(1, Some(me), 7, Held)], fenced)
    == []
  assert bouncer_view.reconcile(me, [], [record(1, Some(me), 8, Held)], fenced)
    == [Take(BucketId(key.guild, 1), epoch_of(8))]
}

pub fn the_roster_rule_needs_an_isolated_majority_of_every_kept_roster_test() {
  let self = at("me@h")
  let small =
    worker.roster([
      WorkerId(self, 1),
      WorkerId(at("p@h"), 1),
      WorkerId(at("q@h"), 1),
    ])
  let bigger =
    worker.roster([
      WorkerId(self, 1),
      WorkerId(at("p@h"), 1),
      WorkerId(at("q@h"), 1),
      WorkerId(at("r@h"), 1),
      WorkerId(at("s@h"), 1),
    ])
  let answers =
    dict.from_list([
      #(at("p@h"), worker_lease.NoBouncerMajority),
      #(at("q@h"), worker_lease.SeesBouncers),
    ])
  assert lease_probe.verdict([small], self, answers)
  assert !lease_probe.verdict([small, bigger], self, answers)
  let more = dict.insert(answers, at("r@h"), worker_lease.NoBouncerMajority)
  assert lease_probe.verdict([small, bigger], self, more)
  assert !lease_probe.verdict([], self, more)
  assert lease_probe.verdict([small, worker.roster([])], self, answers)
  assert !lease_probe.verdict([worker.roster([])], self, answers)
  assert lease_probe.verdict(
    [worker.roster([WorkerId(self, 1)])],
    self,
    dict.new(),
  )
}

pub fn one_bouncer_of_three_cannot_keep_a_worker_valid_test() {
  let view =
    three()
    |> bouncer_view.welcome(0, 1, [], roster(), 0)
    |> bouncer_view.welcome(1, 1, [], roster(), 0)
    |> bouncer_view.welcome(2, 1, [], roster(), 0)
  let #(view, seq) = bouncer_view.beat(view, 100, 6000)
  let #(view, acked) = bouncer_view.ack(view, 2, seq, True, roster(), 101)
  assert acked == None
  assert bouncer_view.acked_mono(view) == None
  assert lease.quorum(3) == 2
  assert lease.quorum(5) == 3
}

pub fn a_grant_overtaken_by_a_newer_record_or_a_fence_is_ignored_test() {
  let me = WorkerId(at("me@h"), 5)
  let other = WorkerId(at("other@h"), 1)
  let bucket = BucketId(key.guild, 1)
  assert bouncer_view.accepts_grant(bucket, epoch_of(7), Error(Nil), dict.new())
  assert bouncer_view.accepts_grant(
    bucket,
    epoch_of(7),
    Ok(record(1, Some(me), 7, Held)),
    dict.new(),
  )
  assert bouncer_view.accepts_grant(
    bucket,
    epoch_of(7),
    Ok(record(1, Some(other), 6, Moving(me, epoch_of(7), set.new(), []))),
    dict.new(),
  )
  assert !bouncer_view.accepts_grant(
    bucket,
    epoch_of(7),
    Ok(record(1, Some(other), 8, Held)),
    dict.new(),
  )
  assert !bouncer_view.accepts_grant(
    bucket,
    epoch_of(7),
    Ok(record(1, Some(other), 6, Moving(other, epoch_of(8), set.new(), []))),
    dict.new(),
  )
  assert !bouncer_view.accepts_grant(
    bucket,
    epoch_of(7),
    Error(Nil),
    dict.from_list([#(bucket, epoch_of(7))]),
  )
  assert bouncer_view.accepts_grant(
    bucket,
    epoch_of(8),
    Error(Nil),
    dict.from_list([#(bucket, epoch_of(7))]),
  )
}
