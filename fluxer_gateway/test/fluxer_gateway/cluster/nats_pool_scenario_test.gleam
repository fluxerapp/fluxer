// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/nats/pool.{type NatsMessage}
import gleam/bit_array
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string
import support/cluster_app
import support/cluster_fake_nats.{type FakeNats} as fake
import support/isolation

const slots = 16

fn with_pool(run: fn(FakeNats, Pid) -> Nil) -> Nil {
  let server = fake.start()
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_nats_pool")
  let pid = start_pool(server)
  run(server, pid)
  let assert Ok(current) = pool.pool_pid()
  cluster_app.stop_linked(current)
  cluster_app.resume(paused)
}

fn start_pool(server: FakeNats) -> Pid {
  let configured =
    settings.Settings(..settings.get(), nats_core_url: fake.url(server))
  let spec = pool.supervised(configured)
  let assert Ok(started) = spec.start()
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server)) == slots
  })
  assert fake.wait_until(5000, pool.has_ready_connection)
  started.pid
}

fn receive_payload(inbox: Subject(NatsMessage)) -> Result(BitArray, Nil) {
  process.receive(inbox, 2000)
  |> result_map(fn(message) { message.payload })
}

fn result_map(found: Result(a, Nil), f: fn(a) -> b) -> Result(b, Nil) {
  case found {
    Ok(value) -> Ok(f(value))
    Error(Nil) -> Error(Nil)
  }
}

fn conns_of(subs: List(fake.Sub)) -> List(Int) {
  list.map(subs, fn(sub) { sub.conn })
}

pub fn single_subscription_moves_to_a_live_slot_when_its_connection_drops_test() {
  use server, _ <- with_pool
  let inbox = process.new_subject()
  let assert Ok(_) = pool.subscribe("scn.one", Some("q"), inbox)
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.one")) == 1
  })
  fake.inject(server, "scn.one", None, "first")
  assert receive_payload(inbox) == Ok(<<"first":utf8>>)

  let assert [placed] = fake.subs_on(server, "scn.one")
  fake.drop(server, placed.conn)
  assert fake.wait_until(2000, fn() {
    case fake.subs_on(server, "scn.one") {
      [moved] -> moved.conn != placed.conn
      _ -> False
    }
  })
  fake.inject(server, "scn.one", None, "second")
  assert receive_payload(inbox) == Ok(<<"second":utf8>>)

  assert fake.wait_until(6000, fn() {
    list.length(fake.connections(server)) == slots
  })
  process.sleep(300)
  assert list.length(fake.subs_on(server, "scn.one")) == 1
  fake.inject(server, "scn.one", None, "third")
  assert receive_payload(inbox) == Ok(<<"third":utf8>>)
  assert process.receive(inbox, 200) == Error(Nil)
}

pub fn every_slot_subscription_is_restored_on_reconnected_slots_test() {
  use server, _ <- with_pool
  let inbox = process.new_subject()
  let assert Ok(_) = pool.subscribe_every_slot("scn.all", Some("q"), inbox)
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.all")) == slots
  })
  let assert [first, second, third, ..] = fake.connections(server)
  fake.drop(server, first)
  fake.drop(server, second)
  fake.drop(server, third)
  assert fake.wait_until(1000, fn() {
    list.length(fake.subs_on(server, "scn.all")) == slots - 3
  })
  fake.inject(server, "scn.all", None, "during")
  assert receive_payload(inbox) == Ok(<<"during":utf8>>)
  assert fake.wait_until(6000, fn() {
    list.length(fake.subs_on(server, "scn.all")) == slots
  })
  let placed = conns_of(fake.subs_on(server, "scn.all"))
  assert set.size(set.from_list(placed)) == slots
  assert !list.contains(placed, first)
  fake.inject(server, "scn.all", None, "after")
  assert receive_payload(inbox) == Ok(<<"after":utf8>>)
  assert process.receive(inbox, 200) == Error(Nil)
}

pub fn subscriptions_of_a_dead_owner_are_removed_test() {
  use server, _ <- with_pool
  let done = process.new_subject()
  process.spawn(fn() {
    let inbox = process.new_subject()
    let _ = pool.subscribe("scn.owned", None, inbox)
    let _ = pool.subscribe_every_slot("scn.owned.all", None, inbox)
    process.send(done, Nil)
  })
  assert process.receive(done, 2000) == Ok(Nil)
  assert fake.wait_until(2000, fn() {
    fake.subs_on(server, "scn.owned") == []
    && fake.subs_on(server, "scn.owned.all") == []
  })
}

pub fn request_reaches_a_responder_and_reports_no_responders_test() {
  use server, _ <- with_pool
  process.spawn(fn() {
    let inbox = process.new_subject()
    let assert Ok(_) = pool.subscribe("scn.echo", Some("echo"), inbox)
    echo_loop(inbox)
  })
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.echo")) == 1
  })
  assert pool.request("scn.echo", <<"ping":utf8>>, 2000) == Ok(<<"ping":utf8>>)
  assert pool.request("scn.nobody", <<"ping":utf8>>, 2000)
    == Error(pool.NoResponders)
  assert pool.request("bad subject", <<>>, 2000)
    == Error(pool.ProtocolError("invalid subject"))
}

fn echo_loop(inbox: Subject(NatsMessage)) -> Nil {
  case process.receive_forever(inbox) {
    pool.NatsMessage(reply_to: Some(reply), payload:, ..) -> {
      let _ = pool.publish_reply(reply, payload)
      Nil
    }
    _ -> Nil
  }
  echo_loop(inbox)
}

pub fn rpc_server_resubscribes_after_the_pool_restarts_test() {
  use server, first <- with_pool
  assert fake.wait_until(6000, fn() {
    list.length(fake.subs_on(server, "rpc.gateway.>")) == slots
  })
  fake.inject(server, "rpc.gateway.nope.method", Some("scn.reply.1"), "{}")
  assert fake.wait_until(3000, fn() {
    case fake.published_on(server, "scn.reply.1") {
      [reply] -> string.contains(reply.payload, "nope.method")
      _ -> False
    }
  })

  cluster_app.stop_linked(first)
  assert fake.wait_until(3000, fn() {
    fake.subs_on(server, "rpc.gateway.>") == []
  })
  let _ = start_pool(server)
  assert fake.wait_until(8000, fn() {
    list.length(fake.subs_on(server, "rpc.gateway.>")) == slots
  })
  fake.inject(server, "rpc.gateway.nope.method", Some("scn.reply.2"), "{}")
  assert fake.wait_until(3000, fn() {
    list.length(fake.published_on(server, "scn.reply.2")) == 1
  })
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_native() -> Int

@external(erlang, "erlang", "convert_time_unit")
fn convert(time: Int, from: atom.Atom, to: atom.Atom) -> Int

fn now_ms() -> Int {
  convert(monotonic_native(), atom.create("native"), atom.create("millisecond"))
}

pub fn request_fails_fast_when_its_connection_drops_test() {
  use server, _ <- with_pool
  process.spawn(fn() {
    let inbox = process.new_subject()
    let assert Ok(_) = pool.subscribe("scn.silent", None, inbox)
    process.sleep_forever()
  })
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.silent")) == 1
  })
  process.spawn(fn() {
    process.sleep(300)
    list.each(fake.connections(server), fake.drop(server, _))
  })
  let started = now_ms()
  assert pool.request("scn.silent", <<"x":utf8>>, 4000)
    == Error(pool.ConnectionClosed)
  assert now_ms() - started < 2000
  assert fake.wait_until(8000, fn() {
    list.length(fake.connections(server)) == slots
    && list.length(fake.subs_on(server, "scn.silent")) == 1
  })
}

pub fn oversized_payloads_are_refused_before_the_wire_test() {
  use server, _ <- with_pool
  let big = <<0:size(8_388_616)>>
  assert pool.request("scn.big", big, 1000)
    == Error(pool.ProtocolError("max_payload"))
  assert pool.publish_reply("scn.big", big)
    == Error(pool.ProtocolError("max_payload"))
  assert fake.published_on(server, "scn.big") == []
}

@external(erlang, "sys", "suspend")
fn suspend(pid: Pid) -> atom.Atom

@external(erlang, "sys", "resume")
fn resume(pid: Pid) -> atom.Atom

pub fn subscribe_retried_after_a_timeout_does_not_double_subscribe_test() {
  use server, pool_pid <- with_pool
  let inbox = process.new_subject()
  suspend(pool_pid)
  assert pool.subscribe_every_slot("scn.retry", Some("q"), inbox)
    == Error(pool.NatsTimeout)
  resume(pool_pid)
  let assert Ok(_) = pool.subscribe_every_slot("scn.retry", Some("q"), inbox)
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.retry")) >= slots
  })
  process.sleep(300)
  assert list.length(fake.subs_on(server, "scn.retry")) == slots
}

pub fn connects_started_by_a_dead_pool_never_reach_its_successor_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      nats: settings.NatsDurations(..d.nats, pool_reconnect_base_ms: 100),
    )
  })
  let server = fake.start()
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_nats_pool")
  fake.hold_pongs(server, True)
  let configured =
    settings.Settings(..settings.get(), nats_core_url: fake.url(server))
  let assert Ok(first) = pool.supervised(configured).start()
  assert fake.wait_until(5000, fn() { fake.connected_total(server) == slots })
  cluster_app.stop_linked(first.pid)
  let assert Ok(second) = pool.supervised(configured).start()
  assert fake.wait_until(5000, fn() {
    fake.connected_total(server) == 2 * slots
  })
  fake.hold_pongs(server, False)
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server)) == slots
  })
  process.sleep(1000)
  assert fake.connected_total(server) == 2 * slots
  assert list.length(fake.connections(server)) == slots
  assert pool.pool_pid() == Ok(second.pid)
  assert pool.has_ready_connection()
  cluster_app.stop_linked(second.pid)
  cluster_app.resume(paused)
}

fn quick_nats(change: fn(settings.NatsDurations) -> settings.NatsDurations) {
  isolation.set_durations(fn(d) {
    settings.Durations(..d, nats: change(d.nats))
  })
}

pub fn silent_server_is_dropped_by_pings_and_subscriptions_come_back_test() {
  quick_nats(fn(n) {
    settings.NatsDurations(
      ..n,
      connection_ping_interval_ms: 150,
      connection_pong_timeout_ms: 100,
      pool_reconnect_base_ms: 100,
      pool_reconnect_max_ms: 200,
    )
  })
  use server, _ <- with_pool
  let inbox = process.new_subject()
  let assert Ok(_) = pool.subscribe("scn.ping", None, inbox)
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.ping")) == 1
  })
  let silent = set.from_list(fake.connections(server))
  assert set.size(silent) == slots
  fake.mute_pongs(server, True)
  assert fake.wait_until(3000, fn() {
    !list.any(fake.connections(server), set.contains(silent, _))
    && !pool.has_ready_connection()
  })
  fake.mute_pongs(server, False)
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server)) == slots
    && list.length(fake.subs_on(server, "scn.ping")) == 1
  })
  fake.inject(server, "scn.ping", None, "back")
  assert receive_payload(inbox) == Ok(<<"back":utf8>>)
}

pub fn silent_server_is_dropped_one_pong_timeout_after_the_next_ping_test() {
  quick_nats(fn(n) {
    settings.NatsDurations(
      ..n,
      connection_ping_interval_ms: 1000,
      connection_pong_timeout_ms: 100,
      pool_reconnect_base_ms: 100,
      pool_reconnect_max_ms: 200,
    )
  })
  use server, _ <- with_pool
  let silent = set.from_list(fake.connections(server))
  fake.mute_pongs(server, True)
  let started = now_ms()
  assert fake.wait_until(2500, fn() {
    !list.any(fake.connections(server), set.contains(silent, _))
  })
  assert now_ms() - started < 2000
  fake.mute_pongs(server, False)
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server)) == slots
  })
}

pub fn answering_server_keeps_its_connections_test() {
  quick_nats(fn(n) {
    settings.NatsDurations(
      ..n,
      connection_ping_interval_ms: 100,
      connection_pong_timeout_ms: 1500,
    )
  })
  use server, _ <- with_pool
  let total = fake.connected_total(server)
  let first = fake.connections(server)
  assert fake.wait_until(20_000, fn() {
    list.all(first, fn(conn) { fake.answered(server, conn) >= 20 })
  })
  assert fake.connected_total(server) == total
  assert fake.connections(server) == first
  assert pool.has_ready_connection()
}

pub fn stalled_handshakes_time_out_and_leave_no_extra_connections_test() {
  quick_nats(fn(n) {
    settings.NatsDurations(
      ..n,
      pool_connect_timeout_ms: 300,
      pool_connect_slack_ms: 100,
      pool_reconnect_base_ms: 100,
      pool_reconnect_max_ms: 200,
    )
  })
  let server = fake.start()
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_nats_pool")
  fake.hold_pongs(server, True)
  let configured =
    settings.Settings(..settings.get(), nats_core_url: fake.url(server))
  let assert Ok(started) = pool.supervised(configured).start()
  assert fake.wait_until(5000, fn() {
    fake.connected_total(server) >= 2 * slots
  })
  assert !pool.has_ready_connection()
  fake.hold_pongs(server, False)
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server)) == slots
  })
  assert fake.wait_until(2000, pool.has_ready_connection)
  process.sleep(500)
  assert list.length(fake.connections(server)) == slots
  let inbox = process.new_subject()
  let assert Ok(_) = pool.subscribe_every_slot("scn.stall", None, inbox)
  assert fake.wait_until(2000, fn() {
    list.length(fake.subs_on(server, "scn.stall")) == slots
  })
  cluster_app.stop_linked(started.pid)
  cluster_app.resume(paused)
}

fn placed_once(server: FakeNats, subjects: List(String)) -> Bool {
  let live = fake.connections(server)
  list.all(subjects, fn(subject) {
    case fake.subs_on(server, subject) {
      [only] -> list.contains(live, only.conn)
      _ -> False
    }
  })
}

fn placed_everywhere(server: FakeNats, subject: String) -> Bool {
  let live = fake.connections(server)
  let placed = conns_of(fake.subs_on(server, subject))
  list.length(placed) == slots && list.sort(placed, int.compare) == live
}

fn drain_payloads(
  inbox: Subject(NatsMessage),
  seen: List(String),
) -> List(String) {
  case process.receive(inbox, 300) {
    Ok(message) ->
      drain_payloads(inbox, [
        result.unwrap(bit_array.to_string(message.payload), ""),
        ..seen
      ])
    Error(Nil) -> list.sort(seen, string.compare)
  }
}

fn requester(report: Subject(Int), rounds: Int, worst: Int) -> Nil {
  case rounds {
    0 -> process.send(report, worst)
    _ -> {
      let started = now_ms()
      let _ = pool.request("scn.storm.echo", <<"r":utf8>>, 500)
      let took = now_ms() - started
      process.sleep(5)
      requester(report, rounds - 1, int.max(worst, took))
    }
  }
}

pub fn reconnect_storm_leaves_every_subscription_placed_exactly_once_test() {
  quick_nats(fn(n) {
    settings.NatsDurations(
      ..n,
      pool_reconnect_base_ms: 20,
      pool_reconnect_max_ms: 80,
      pool_breaker_cooldown_ms: 200,
    )
  })
  use server, _ <- with_pool
  let singles = ["scn.storm.a", "scn.storm.b", "scn.storm.c"]
  let inbox = process.new_subject()
  list.each(singles, fn(subject) {
    let assert Ok(_) = pool.subscribe(subject, None, inbox)
  })
  let assert Ok(_) =
    pool.subscribe_every_slot("scn.storm.all", Some("q"), inbox)
  process.spawn(fn() {
    let echo_inbox = process.new_subject()
    let assert Ok(_) = pool.subscribe("scn.storm.echo", Some("e"), echo_inbox)
    echo_loop(echo_inbox)
  })
  assert fake.wait_until(3000, fn() {
    placed_once(server, ["scn.storm.echo", ..singles])
    && placed_everywhere(server, "scn.storm.all")
  })

  let report = process.new_subject()
  process.spawn(fn() { requester(report, 150, 0) })
  int.range(from: 0, to: 12, with: Nil, run: fn(_, round) {
    let conns = fake.connections(server)
    let victims = case round % 3 {
      0 -> conns
      1 -> list.take(conns, slots / 2)
      _ -> list.drop(conns, slots / 2)
    }
    list.each(victims, fake.drop(server, _))
    process.sleep({ round * 17 } % 70)
  })
  let assert Ok(worst) = process.receive(report, 20_000)
  assert worst < 1500

  assert fake.wait_until(8000, fn() {
    list.length(fake.connections(server)) == slots
    && placed_once(server, ["scn.storm.echo", ..singles])
    && placed_everywhere(server, "scn.storm.all")
  })
  assert fake.wait_until(4000, fn() {
    placed_everywhere(server, "rpc.gateway.>")
  })
  process.sleep(600)
  assert list.length(fake.connections(server)) == slots
  assert placed_once(server, ["scn.storm.echo", ..singles])
  assert placed_everywhere(server, "scn.storm.all")
  assert placed_everywhere(server, "rpc.gateway.>")
  fake.inject(server, "rpc.gateway.nope.method", Some("scn.storm.reply"), "{}")
  assert fake.wait_until(2000, fn() {
    list.length(fake.published_on(server, "scn.storm.reply")) == 1
  })

  let _ = drain_payloads(inbox, [])
  list.each(["scn.storm.all", ..singles], fn(subject) {
    fake.inject(server, subject, None, subject)
  })
  assert drain_payloads(inbox, [])
    == list.sort(["scn.storm.all", ..singles], string.compare)
  assert pool.request("scn.storm.echo", <<"done":utf8>>, 2000)
    == Ok(<<"done":utf8>>)
}

@external(erlang, "erlang", "process_info")
fn monitored(pid: Pid, item: atom.Atom) -> #(atom.Atom, List(#(atom.Atom, Pid)))

fn connection_pids(pool_pid: Pid) -> List(Pid) {
  let #(_, monitors) = monitored(pool_pid, atom.create("monitors"))
  list.map(monitors, fn(entry) { entry.1 })
  |> list.filter(fn(pid) { pid != process.self() })
}

pub fn subscribe_that_times_out_on_a_stalled_connection_is_not_doubled_test() {
  quick_nats(fn(n) {
    settings.NatsDurations(
      ..n,
      pool_subscribe_timeout_ms: 200,
      pool_subscribe_retry_base_ms: 600,
    )
  })
  use server, pool_pid <- with_pool
  let stalled = connection_pids(pool_pid)
  list.each(stalled, fn(pid) { suspend(pid) })
  let inbox = process.new_subject()
  let assert Ok(_) = pool.subscribe("scn.stall.one", None, inbox)
  list.each(stalled, fn(pid) { resume(pid) })
  assert fake.wait_until(3000, fn() {
    list.length(fake.subs_on(server, "scn.stall.one")) >= 1
  })
  process.sleep(1500)
  assert list.length(fake.subs_on(server, "scn.stall.one")) == 1
  fake.inject(server, "scn.stall.one", None, "once")
  assert receive_payload(inbox) == Ok(<<"once":utf8>>)
  assert process.receive(inbox, 300) == Error(Nil)
}
