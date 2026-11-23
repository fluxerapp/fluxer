// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/core/session
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import support/guild_api
import support/guild_fixture.{RoleDeny} as fixture
import support/guild_session as fake
import support/wait

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

fn observed(name: String, labels: List(#(String, String))) -> Int {
  let labels = metrics.normalise(labels)
  metrics.histograms()
  |> list.find(fn(series) { series.name == name && series.labels == labels })
  |> result.map(fn(series) { series.count })
  |> result.unwrap(0)
}

fn message(channel: Int, message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn lazy_list(found: fake.FakeSession, handle, channel: Int) -> Nil {
  fake.op(
    found,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(
        json.object([
          #(
            "member_list_channels",
            json.object([
              #(
                int_text(channel),
                json.preprocessed_array([json.array([0, 99], json.int)]),
              ),
            ]),
          ),
        ]),
      ),
    ),
  )
}

pub fn a_guild_lifecycle_counts_starts_loads_joins_and_stops_test() {
  guild_metrics.register()
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
  let fetched = [#("origin", "fetched")]
  let ok = [#("outcome", "ok")]
  let focused = [#("class", "focused")]
  let joined = [#("outcome", "joined")]
  let shutdown = [#("reason", "shutdown")]
  let starts = value(guild_metrics.starts, fetched)
  let loads = observed(guild_metrics.load_ms, ok)
  let members = value(guild_metrics.members_held, [])
  let sessions = value(guild_metrics.sessions_held, [])
  let admitted = value(guild_metrics.joins_admitted, focused)
  let waits = observed(guild_metrics.join_wait_ms, joined)
  let batches = observed(guild_metrics.join_batch_ms, [])
  let stops = value(guild_metrics.stops, shutdown)
  let handle = fixture.started(api, found)
  assert value(guild_metrics.starts, fetched) == starts + 1
  assert observed(guild_metrics.load_ms, ok) == loads + 1
  assert value(guild_metrics.members_held, []) == members + 3
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  assert value(guild_metrics.sessions_held, []) == sessions + 1
  assert value(guild_metrics.joins_admitted, focused) == admitted + 1
  assert observed(guild_metrics.join_wait_ms, joined) == waits + 1
  assert observed(guild_metrics.join_batch_ms, []) >= batches + 1
  fixture.stop(found)
  assert value(guild_metrics.stops, shutdown) == stops + 1
  assert value(guild_metrics.members_held, []) == members
  assert value(guild_metrics.sessions_held, []) == sessions
}

pub fn dispatches_count_events_deliveries_fan_out_and_latency_test() {
  guild_metrics.register()
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(owner, 50)
  let _ = fake.drain(member, 0)
  let family = [#("family", "message_create")]
  let events = value(guild_metrics.events, family)
  let deliveries = value(guild_metrics.deliveries, family)
  let latencies = observed(guild_metrics.dispatch_ms, family)
  let fan_outs = observed(guild_metrics.fanout, [])
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  let assert Some(_) = fake.await_event(owner, "MESSAGE_CREATE", 1000)
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  fixture.sync(handle)
  assert value(guild_metrics.events, family) == events + 1
  assert value(guild_metrics.deliveries, family) == deliveries + 2
  assert observed(guild_metrics.dispatch_ms, family) == latencies + 1
  assert observed(guild_metrics.fanout, []) == fan_outs + 1
  fixture.stop(found)
}

pub fn member_list_ops_count_subscribes_ranges_and_denials_test() {
  guild_metrics.register()
  use api <- guild_api.with_api
  let found = fixture.new(1)
  let found =
    found
    |> fixture.with_text(10)
    |> fixture.with_channel(20, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
    ])
    |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(owner, 50)
  let _ = fake.drain(member, 0)
  let subscribe = [#("op", "subscribe")]
  let denied = [#("op", "denied")]
  let lists = [#("family", "member_list")]
  let subscribes = value(guild_metrics.member_list_ops, subscribe)
  let denials = value(guild_metrics.member_list_ops, denied)
  let ranges = value(guild_metrics.member_list_ranges, [])
  let latencies = observed(guild_metrics.member_list_ms, [])
  let deliveries = value(guild_metrics.deliveries, lists)
  lazy_list(owner, handle, 10)
  let assert Some(_) = fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  lazy_list(member, handle, 20)
  fixture.sync(handle)
  assert value(guild_metrics.member_list_ops, subscribe) == subscribes + 1
  assert value(guild_metrics.member_list_ops, denied) == denials + 1
  assert value(guild_metrics.member_list_ranges, []) == ranges + 1
  assert observed(guild_metrics.member_list_ms, []) == latencies + 1
  assert value(guild_metrics.deliveries, lists) >= deliveries + 1
  fixture.stop(found)
}

pub fn failed_and_crashed_guilds_are_counted_test() {
  guild_metrics.register()
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  guild_api.put_data(api, found.id, "roles", json.object([]))
  let failures = fn() {
    list.fold(guild_metrics.failure_reasons, 0, fn(total, reason) {
      total + value(guild_metrics.start_failures, [#("reason", reason)])
    })
  }
  let failed = failures()
  let errors = observed(guild_metrics.load_ms, [#("outcome", "error")])
  let assert Error(shard.LookupUnavailable(_)) =
    manager.start_or_lookup(node.self(), fixture.guild_id(found), 5000)
  assert failures() == failed + 1
  assert observed(guild_metrics.load_ms, [#("outcome", "error")]) == errors + 1
  let crashed = [#("reason", "crashed")]
  let crashes = value(guild_metrics.stops, crashed)
  let restarts = value(guild_metrics.crash_restarts, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  process.kill(handle.pid)
  assert wait.until(2000, fn() {
    value(guild_metrics.stops, crashed) == crashes + 1
  })
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  assert value(guild_metrics.crash_restarts, []) == restarts + 1
  fixture.stop(found)
}

pub fn modes_are_sampled_from_live_guild_processes_test() {
  guild_metrics.register()
  let frozen = [#("mode", "frozen")]
  let handing = [#("mode", "handing")]
  let before_frozen = value(guild_metrics.in_mode, frozen)
  let before_handing = value(guild_metrics.in_mode, handing)
  let ready = process.new_subject()
  let done = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let step = process.new_subject()
      guild_metrics.enter(guild_metrics.Frozen)
      process.send(ready, step)
      let _ = process.receive_forever(step)
      guild_metrics.enter(guild_metrics.Handing)
      process.send(done, Nil)
      process.sleep_forever()
    })
  let assert Ok(step) = process.receive(ready, 2000)
  assert value(guild_metrics.in_mode, frozen) == before_frozen + 1
  process.send(step, Nil)
  let assert Ok(Nil) = process.receive(done, 2000)
  assert value(guild_metrics.in_mode, frozen) == before_frozen
  assert value(guild_metrics.in_mode, handing) == before_handing + 1
  process.kill(pid)
  assert wait.until(2000, fn() {
    value(guild_metrics.in_mode, handing) == before_handing
  })
  list.each(guild_metrics.modes, fn(mode) {
    assert list.any(metrics.read_all(), fn(sample) {
      sample.name == guild_metrics.in_mode && sample.labels == [#("mode", mode)]
    })
  })
}

pub fn event_families_stay_within_the_closed_set_test() {
  guild_metrics.register()
  let names = [
    #("MESSAGE_CREATE", "message_create"),
    #("MESSAGE_DELETE_BULK", "message"),
    #("MESSAGE_REACTION_ADD", "reaction"),
    #("TYPING_START", "typing"),
    #("PRESENCE_UPDATE", "presence"),
    #("GUILD_MEMBER_LIST_UPDATE", "member_list"),
    #("GUILD_MEMBER_UPDATE", "member"),
    #("GUILD_ROLE_CREATE", "role"),
    #("CHANNEL_PINS_UPDATE", "channel"),
    #("WEBHOOKS_UPDATE", "channel"),
    #("THREAD_CREATE", "thread"),
    #("VOICE_STATE_UPDATE", "voice"),
    #("GUILD_EMOJIS_UPDATE", "expressions"),
    #("INVITE_CREATE", "invite"),
    #("GUILD_UPDATE", "guild"),
    #("SOMETHING_NEW", "other"),
  ]
  list.each(names, fn(pair) {
    let labels = [#("family", pair.1)]
    let before = value(guild_metrics.events, labels)
    let delivered = value(guild_metrics.deliveries, labels)
    guild_metrics.event_handled(pair.0, 3)
    guild_metrics.delivered(pair.0, 2)
    assert value(guild_metrics.events, labels) == before + 1
    assert value(guild_metrics.deliveries, labels) == delivered + 2
  })
  let families =
    metrics.read_all()
    |> list.filter(fn(sample) { sample.name == guild_metrics.events })
    |> list.flat_map(fn(sample) { sample.labels })
    |> list.map(fn(label) { label.1 })
  assert list.all(families, list.contains(guild_metrics.families, _))
  assert list.length(guild_metrics.families) == 15
}
