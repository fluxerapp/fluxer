// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{type BucketId, BucketId, Key}
import bouncer_proto/lease.{
  type BucketRecord, BucketRecord, CommitRow, Held, Moving, Writer,
}
import bouncer_proto/message.{
  type ToBouncer, type ToWorker, BeatAck, BucketDelta, Granted, Welcome,
  WorkerBeat, WorkerBye, WorkerClaim, WorkerHello,
}
import bouncer_proto/transport
import bouncer_proto/worker.{type WorkerId, Entry, Profile, Ready, WorkerId}
import fluxer_gateway/app/drain_report
import fluxer_gateway/cluster/bouncer_client.{ClientConfig}
import fluxer_gateway/cluster/buckets
import fluxer_gateway/cluster/lease as worker_lease
import fluxer_gateway/cluster/lease_probe
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/metrics as cluster_metrics
import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{BouncerSettings, Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/telemetry/metrics
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import support/wait

type Control {
  Silence(Bool)
  Mismatch(Bool)
  Push(ToWorker)
  Count(reply: Subject(#(Int, Int, Int)))
}

type Fake {
  Fake(
    silent: Bool,
    mismatch: Bool,
    records: List(BucketRecord),
    roster: worker.Roster,
    hellos: Int,
    beats: Int,
    claims: Int,
    worker: Atom,
  )
}

fn self_atom() -> Atom {
  node.to_atom(node.self())
}

fn epoch_of(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

fn me() -> WorkerId {
  WorkerId(self_atom(), worker_lease.incarnation())
}

fn guild_bucket(guild: Int) -> BucketId {
  key.bucket_of(Key(key.guild, guild))
}

fn held(bucket: BucketId, holder: WorkerId, at: Int) -> BucketRecord {
  BucketRecord(
    bucket:,
    holder: Some(holder),
    epoch: epoch_of(at),
    pool: "default",
    state: Held,
    written_by: Writer(0, 1, at),
  )
}

fn start_fake(
  records: List(BucketRecord),
  roster: List(WorkerId),
) -> #(process.Pid, Subject(Control)) {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let assert Ok(Nil) =
        process.register(process.self(), transport.bouncer_name())
      let control = process.new_subject()
      process.send(ready, control)
      let selector =
        process.new_selector()
        |> process.select_map(control, Ok)
        |> process.select_map(
          process.named_subject(transport.bouncer_name()),
          Error,
        )
      fake_loop(
        selector,
        Fake(False, False, records, worker.roster(roster), 0, 0, 0, self_atom()),
      )
    })
  let assert Ok(control) = process.receive(ready, 1000)
  #(pid, control)
}

fn reply(fake: Fake, outgoing: ToWorker) -> Nil {
  let _ =
    transport.send(
      to: fake.worker,
      name: transport.worker_name(),
      message: outgoing,
    )
  Nil
}

fn fake_loop(
  selector: process.Selector(Result(Control, ToBouncer)),
  fake: Fake,
) -> Nil {
  let fake = case process.selector_receive_forever(selector) {
    Ok(Silence(silent)) -> Fake(..fake, silent:)
    Ok(Mismatch(mismatch)) -> Fake(..fake, mismatch:)
    Ok(Push(outgoing)) -> {
      reply(fake, outgoing)
      fake
    }
    Ok(Count(answer)) -> {
      process.send(answer, #(fake.hellos, fake.beats, fake.claims))
      fake
    }
    Error(WorkerHello(hello)) -> {
      let fake =
        Fake(..fake, worker: hello.worker.node, hellos: fake.hellos + 1)
      case fake.silent {
        True -> Nil
        False ->
          reply(
            fake,
            Welcome(
              bouncer: 0,
              incarnation: 7,
              protocol: 1,
              directory: [
                Entry(hello.worker, hello.profile, Ready, clock.system_ms()),
              ],
              records: fake.records,
              roster: fake.roster,
            ),
          )
      }
      fake
    }
    Error(WorkerBeat(beat)) -> {
      case fake.silent {
        True -> Nil
        False -> reply(fake, BeatAck(0, beat.seq, !fake.mismatch, fake.roster))
      }
      Fake(..fake, beats: fake.beats + 1)
    }
    Error(WorkerClaim(..)) -> Fake(..fake, claims: fake.claims + 1)
    Error(WorkerBye(..)) -> fake
    Error(_) -> fake
  }
  fake_loop(selector, fake)
}

fn histogram_count(name: String, labels: List(#(String, String))) -> Int {
  metrics.histograms()
  |> list.filter(fn(series) { series.name == name && series.labels == labels })
  |> list.fold(0, fn(total, series) { total + series.count })
}

fn beat_acks(outcome: String) -> Int {
  metrics.read_all()
  |> metrics.value(cluster_metrics.beat_acks, [#("outcome", outcome)])
}

fn counts(control: Subject(Control)) -> #(Int, Int, Int) {
  process.call(control, 1000, Count)
}

fn config() -> bouncer_client.ClientConfig {
  ClientConfig(
    dns_name: "gateway-bouncer.test",
    basename: "gateway-bouncer",
    count: 1,
    role: "guilds",
    pool: "default",
    template_hash: "abc",
    build: "dev",
    codecs: [],
    pinned: [],
  )
}

fn clean() -> Nil {
  let _ = worker_lease.start()
  let _ = buckets.start()
  let _ = links.start()
  let _ = worker_lease.fence()
  buckets.clear()
  let _ = links.replace([])
  links.set_joined(False)
  links.release_serving()
}

fn with_bouncer_mode(cap_ms: Int, run: fn() -> Nil) -> Nil {
  let original = settings.get()
  let d = original.durations
  settings.store(
    Settings(
      ..original,
      bouncer: Some(BouncerSettings(
        dns_name: "gateway-bouncer.test",
        basename: "gateway-bouncer",
        count: 1,
        pool: "default",
        template_hash: "abc",
      )),
      durations: settings.Durations(
        ..d,
        bouncer: settings.BouncerDurations(
          ..d.bouncer,
          beat_interval_ms: 50,
          lease_ttl_ms: 400,
          skew_margin_ms: 50,
          isolation_cap_ms: cap_ms,
          probe_timeout_ms: 100,
        ),
      ),
    ),
  )
  clean()
  run()
  clean()
  settings.store(original)
  Nil
}

fn stop(pid: process.Pid) -> Nil {
  let monitor = process.monitor(pid)
  process.unlink(pid)
  process.kill(pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(5000)
  Nil
}

fn start_client() -> process.Pid {
  let assert Ok(started) = bouncer_client.start_with(config(), [self_atom()])
  process.unlink(started.pid)
  started.pid
}

pub fn a_worker_owns_its_granted_buckets_once_a_bouncer_majority_acks_test() {
  use <- with_bouncer_mode(120_000)
  let guild = 4242
  let bucket = guild_bucket(guild)
  let #(fake, control) = start_fake([held(bucket, me(), 3)], [me()])
  let client = start_client()
  assert wait.until(2000, links.joined)
  assert wait.until(2000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  let assert Ok(guild_id) = id.parse_guild_id(int.to_string(guild))
  assert router.joined()
  assert router.owner(router.GuildKey(guild_id), node.Guilds) == router.Local
  assert router.start_allowed(guild_id)
  assert router.nodes_with(node.Guilds) == [node.self()]
  let assert Ok(view) = bouncer_client.view()
  assert view.majority && view.welcomed == [0]
  let #(hellos, _, claims) = counts(control)
  assert hellos >= 1 && claims >= 1
  let rtts = histogram_count(cluster_metrics.beat_rtt, [])
  let matched = beat_acks("matched")
  let #(_, before, _) = counts(control)
  process.sleep(500)
  let #(_, after, _) = counts(control)
  assert after - before >= 3 && after - before <= 12
  assert histogram_count(cluster_metrics.beat_rtt, []) >= rtts + 3
  assert beat_acks("matched") >= matched + 3
  let other = WorkerId(atom.create("other@h"), 1)
  process.send(control, Push(BucketDelta(0, [held(bucket, other, 4)])))
  assert wait.until(2000, fn() {
    worker_lease.check(bucket) == Error(worker_lease.NotHeld(bucket))
  })
  let assert router.OwnerUnavailable(_) =
    router.owner(router.GuildKey(guild_id), node.Guilds)
  assert !router.start_allowed(guild_id)
  let granted = BucketId(key.guild, 77)
  process.send(control, Push(Granted(granted, epoch_of(5), [])))
  assert wait.until(2000, fn() {
    worker_lease.check(granted) == Ok(epoch_of(5))
  })
  assert bouncer_client.bye() == Ok(Nil)
  stop(client)
  stop(fake)
}

pub fn a_worker_that_loses_every_bouncer_keeps_serving_under_the_roster_rule_until_the_cap_test() {
  use <- with_bouncer_mode(1500)
  let bucket = guild_bucket(9001)
  let record = held(bucket, me(), 3)
  let #(fake, control) = start_fake([record], [me()])
  let probe = start_probe()
  let client = start_client()
  assert wait.until(2000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  assert router.in_service()
  let probes =
    histogram_count(cluster_metrics.probe_duration, [#("outcome", "isolated")])
  process.send(control, Silence(True))
  let silenced = clock.monotonic_ms()
  assert wait.until(2000, fn() { !links.joined() })
  assert router.in_service()
  assert wait.until(2000, fn() {
    worker_lease.isolated()
    && worker_lease.check(bucket) == Ok(epoch_of(3))
    && case worker_lease.validity(bucket) {
      Ok(validity) -> validity.state == worker_lease.Isolated
      Error(_) -> False
    }
  })
  assert worker_lease.probe_answer() == worker_lease.NoBouncerMajority
  assert wait.until(2000, fn() {
    histogram_count(cluster_metrics.probe_duration, [#("outcome", "isolated")])
    > probes
  })
  assert wait.until(4000, fn() {
    worker_lease.check(bucket) == Error(worker_lease.NotHeld(bucket))
  })
  assert clock.monotonic_ms() - silenced >= 1500
  assert wait.until(2000, fn() { !router.in_service() })
  process.send(control, Push(BucketDelta(0, [record])))
  process.sleep(300)
  assert worker_lease.check(bucket) == Error(worker_lease.NotHeld(bucket))
  process.send(control, Silence(False))
  assert wait.until(3000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  assert wait.until(3000, router.in_service)
  process.send(control, Push(BucketDelta(0, [held(bucket, me(), 4)])))
  assert wait.until(2000, fn() { worker_lease.check(bucket) == Ok(epoch_of(4)) })
  stop(client)
  stop(probe)
  stop(fake)
}

pub fn an_isolated_worker_fences_when_a_roster_majority_does_not_answer_test() {
  use <- with_bouncer_mode(60_000)
  let bucket = guild_bucket(9002)
  let roster = [
    me(),
    WorkerId(atom.create("ghost_a@127.0.0.1"), 1),
    WorkerId(atom.create("ghost_b@127.0.0.1"), 1),
  ]
  let #(fake, control) = start_fake([held(bucket, me(), 3)], roster)
  let probe = start_probe()
  let client = start_client()
  assert wait.until(2000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  assert router.in_service()
  assert !bouncer_client.fenced()
  assert bouncer_client.bouncers_seen() == 1
  let _ = drain_report.begin()
  draining.set_draining()
  process.send(control, Silence(True))
  let silenced = clock.monotonic_ms()
  assert wait.until(3000, fn() {
    worker_lease.check(bucket) == Error(worker_lease.NotHeld(bucket))
  })
  assert clock.monotonic_ms() - silenced < 3000
  assert wait.until(1000, fn() { !router.in_service() })
  assert wait.until(1000, bouncer_client.fenced)
  let failed =
    drain_report.settle(drain_report.Left(entities: 0, buckets: 0), True, False)
  assert failed.phase == drain_report.Failed
  assert failed.reason == "fenced"
  draining.clear_draining()
  drain_report.reset()
  stop(client)
  stop(probe)
  stop(fake)
}

pub fn a_fenced_worker_says_hello_again_and_retakes_its_buckets_test() {
  use <- with_bouncer_mode(60_000)
  let bucket = guild_bucket(9004)
  let roster = [
    me(),
    WorkerId(atom.create("ghost_a@127.0.0.1"), 1),
    WorkerId(atom.create("ghost_b@127.0.0.1"), 1),
  ]
  let #(fake, control) = start_fake([held(bucket, me(), 3)], roster)
  let probe = start_probe()
  let client = start_client()
  assert wait.until(2000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  let #(hellos, _, _) = counts(control)
  let mismatched = beat_acks("mismatched")
  process.send(control, Mismatch(True))
  assert wait.until(3000, fn() {
    let #(now, _, _) = counts(control)
    now > hellos
  })
  assert beat_acks("mismatched") > mismatched
  assert worker_lease.check(bucket) != Ok(epoch_of(3))
  process.send(control, Mismatch(False))
  assert wait.until(3000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  stop(client)
  stop(probe)
  stop(fake)
}

pub fn without_the_tier_grace_a_bouncer_outage_fences_at_the_lease_lapse_test() {
  use <- with_bouncer_mode(0)
  let bucket = guild_bucket(9003)
  let #(fake, control) = start_fake([held(bucket, me(), 3)], [me()])
  let probe = start_probe()
  let client = start_client()
  assert wait.until(2000, fn() { worker_lease.check(bucket) == Ok(epoch_of(3)) })
  process.send(control, Silence(True))
  let silenced = clock.monotonic_ms()
  assert wait.until(2000, fn() {
    worker_lease.check(bucket) == Error(worker_lease.NotHeld(bucket))
  })
  assert clock.monotonic_ms() - silenced < 1000
  stop(client)
  stop(probe)
  stop(fake)
}

fn start_probe() -> process.Pid {
  let assert Ok(started) = lease_probe.start()
  process.unlink(started.pid)
  started.pid
}

pub fn the_probe_answers_whether_this_node_sees_a_bouncer_majority_test() {
  use <- with_bouncer_mode(120_000)
  let probe = start_probe()
  let reply = process.new_subject()
  let ask = fn() {
    let assert Ok(Nil) =
      remote.send_named(
        node.self(),
        lease_probe.name(),
        lease_probe.Probe(self_atom(), reply),
      )
    let assert Ok(answer) = process.receive(reply, 1000)
    answer
  }
  assert ask()
    == lease_probe.ProbeReply(self_atom(), worker_lease.NoBouncerMajority)
  let assert Ok(Nil) = worker_lease.extend(clock.monotonic_ms())
  assert ask() == lease_probe.ProbeReply(self_atom(), worker_lease.SeesBouncers)
  stop(probe)
}

pub fn a_valid_lease_without_a_fresh_ack_answers_no_majority_test() {
  use <- with_bouncer_mode(120_000)
  let timings = durations.bouncer()
  let assert Ok(Nil) =
    worker_lease.extend(
      clock.monotonic_ms() - 2 * timings.beat_interval_ms - 50,
    )
  assert worker_lease.sees_bouncers(clock.monotonic_ms())
  assert worker_lease.probe_answer() == worker_lease.NoBouncerMajority
  let assert Ok(Nil) = worker_lease.extend(clock.monotonic_ms())
  assert worker_lease.probe_answer() == worker_lease.SeesBouncers
}

pub fn placement_reads_route_rows_then_the_bucket_record_test() {
  use <- with_bouncer_mode(120_000)
  let self = me()
  let other = WorkerId(atom.create("other@h"), 2)
  let other_node = node.from_atom(atom.create("other@h"))
  let pending = 11
  let bucket = BucketId(key.guild, key.bucket(pending))
  let committed = next_in(bucket, pending + 1)
  let fresh = next_in(bucket, committed + 1)
  assert buckets.placement(Key(key.guild, pending)) == buckets.Unplaced(50)
  let assert Ok(Nil) = buckets.apply([held(bucket, self, 3)])
  assert buckets.placement(Key(key.guild, pending)) == buckets.Here(epoch_of(3))
  let assert Ok(Nil) = buckets.apply([held(bucket, other, 2)])
  assert buckets.placement(Key(key.guild, pending)) == buckets.Here(epoch_of(3))
  let moving =
    BucketRecord(
      ..held(bucket, self, 3),
      state: Moving(other, epoch_of(4), set.from_list([pending, committed]), [
        CommitRow(committed, <<1>>),
      ]),
      written_by: Writer(0, 1, 9),
    )
  let assert Ok(Nil) = buckets.apply([moving])
  assert buckets.placement(Key(key.guild, pending)) == buckets.Here(epoch_of(3))
  assert buckets.placement(Key(key.guild, committed))
    == buckets.There(other_node, epoch_of(4))
  assert buckets.placement(Key(key.guild, fresh))
    == buckets.There(other_node, epoch_of(4))
  let assert Ok(Nil) =
    buckets.put_route(
      Key(key.guild, pending),
      other_node,
      epoch_of(4),
      clock.monotonic_ms() + 60_000,
    )
  assert buckets.placement(Key(key.guild, pending))
    == buckets.There(other_node, epoch_of(4))
  let assert Ok(Nil) =
    buckets.put_route(
      Key(key.guild, pending),
      other_node,
      epoch_of(4),
      clock.monotonic_ms() - 1,
    )
  assert buckets.placement(Key(key.guild, pending)) == buckets.Here(epoch_of(3))
  assert buckets.expire_routes(clock.monotonic_ms()) == 1
}

fn next_in(bucket: BucketId, from: Int) -> Int {
  case key.bucket(from) == bucket.bucket {
    True -> from
    False -> next_in(bucket, from + 1)
  }
}

pub fn pinned_guilds_live_in_their_pool_bucket_test() {
  use <- with_bouncer_mode(120_000)
  let assert Ok(pinned) = id.parse_guild_id("555")
  let original = settings.get()
  settings.store(Settings(..original, pinned_guild_ids: [pinned]))
  assert buckets.bucket_for(Key(key.guild, 555))
    == BucketId(key.first_pinned, 0)
  assert buckets.bucket_for(Key(key.presence, 555))
    == key.bucket_of(Key(key.presence, 555))
  assert buckets.bucket_for(Key(key.guild, 556))
    == key.bucket_of(Key(key.guild, 556))
  settings.store(original)
  Nil
}

pub fn links_list_live_workers_by_role_and_dial_only_known_peers_test() {
  use <- with_bouncer_mode(120_000)
  let profile = fn(role) { Profile(role, "default", "dev", "", 1, []) }
  let a = WorkerId(atom.create("a@h"), 1)
  let b = WorkerId(atom.create("b@h"), 1)
  let c = WorkerId(atom.create("c@h"), 1)
  let before = links.version()
  assert links.replace([
    Entry(a, profile("guilds"), Ready, 1),
    Entry(b, profile("presence"), worker.Draining, 1),
    Entry(c, profile("all"), worker.Gone, 1),
  ])
  assert links.version() == before + 1
  assert !links.replace([
    Entry(a, profile("guilds"), Ready, 2),
    Entry(b, profile("presence"), worker.Draining, 2),
    Entry(c, profile("all"), worker.Gone, 2),
  ])
  let a_node = node.from_atom(atom.create("a@h"))
  let b_node = node.from_atom(atom.create("b@h"))
  let c_node = node.from_atom(atom.create("c@h"))
  assert links.live_with(node.Guilds) == [a_node]
  assert links.live_with(node.Presence) == [b_node]
  assert links.serving_with(node.Presence) == []
  assert links.is_live(b_node)
  assert !links.is_live(c_node)
  assert !links.ensure(c_node)
  assert !links.ensure(node.from_atom(atom.create("stranger@h")))
  assert links.ensure(node.self())
  assert router.nodes_with(node.Presence) == [node.self(), b_node]
  assert list.contains(router.members(), a_node)
  assert !list.contains(router.members(), c_node)
}

pub fn bouncer_settings_come_from_the_environment_test() {
  let source = fn(vars) {
    env.Source(
      getenv: fn(name) { dict.get(dict.from_list(vars), name) },
      read_file: fn(_) { Error("none") },
    )
  }
  let assert Ok(off) = settings.read(source([]))
  assert off.bouncer == None
  assert settings.read(source([#("FLUXER_GATEWAY_BOUNCER_COUNT", "3")]))
    == Error(settings.MissingBouncerDnsName)
  let assert Ok(on) =
    settings.read(
      source([
        #("FLUXER_GATEWAY_BOUNCER_COUNT", "3"),
        #("FLUXER_GATEWAY_BOUNCER_DNS_NAME", "gateway-bouncer.ns.svc"),
        #("FLUXER_GATEWAY_POOL", "large"),
        #("FLUXER_GATEWAY_TEMPLATE_HASH", "5d8f"),
      ]),
    )
  assert on.bouncer
    == Some(BouncerSettings(
      dns_name: "gateway-bouncer.ns.svc",
      basename: "gateway-bouncer",
      count: 3,
      pool: "large",
      template_hash: "5d8f",
    ))
  let assert Ok(client) = bouncer_client.config_from(on, [])
  assert bouncer_client.bouncer_nodes(client)
    == [
      atom.create("gateway-bouncer@gateway-bouncer-0.gateway-bouncer.ns.svc"),
      atom.create("gateway-bouncer@gateway-bouncer-1.gateway-bouncer.ns.svc"),
      atom.create("gateway-bouncer@gateway-bouncer-2.gateway-bouncer.ns.svc"),
    ]
  assert bouncer_client.config_from(off, []) == Error(bouncer_client.NoBouncers)
}
