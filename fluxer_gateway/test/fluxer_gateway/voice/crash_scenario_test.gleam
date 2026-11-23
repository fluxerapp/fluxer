// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/rollout
import fluxer_gateway/config/settings.{type Durations, Durations, VoiceDurations}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/permission_sync
import fluxer_gateway/voice/rpc
import fluxer_gateway/voice/token
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import support/cluster_app
import support/isolation
import support/rollout_guard
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake
import support/wait

@external(erlang, "sys", "get_state")
fn get_state(pid: process.Pid) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn to_entry(value: Dynamic) -> Entry

const ok = "{\"success\":true}"

fn dm_channel(recipients: List(Int)) -> api.Rule {
  api.Reply(
    api.Data(
      json.object([
        #(
          "channel",
          json.object([
            #("type", json.int(1)),
            #(
              "recipients",
              json.array(recipients, fn(user) {
                json.object([#("id", json.string(int.to_string(user)))])
              }),
            ),
          ]),
        ),
      ]),
    ),
  )
}

fn create_call(channel: Int, recipients: List(Int)) -> Nil {
  let assert Ok(_) =
    rpc.handle(
      "call.create",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("message_id", json.string(int.to_string(channel + 1000))),
          #("region", json.null()),
          #("ringing", json.array([], json.string)),
          #(
            "recipients",
            json.array(recipients, fn(user) { json.string(int.to_string(user)) }),
          ),
        ]),
      ),
    )
  Nil
}

fn call_pid(channel: Int) -> process.Pid {
  let assert Ok(call) = manager.lookup(g.channel_id(channel))
  let assert Ok(pid) = process.subject_owner(call)
  pid
}

fn call_connections(channel: Int) -> List(String) {
  case manager.lookup(g.channel_id(channel)) {
    Ok(call) ->
      case call_server.get_state(call, 2000) {
        Ok(view) -> list.map(view.connections, voice_ids.of)
        Error(Nil) -> []
      }
    Error(_) -> []
  }
}

fn call_gone(channel: Int) -> Bool {
  manager.lookup(g.channel_id(channel)) == Error(manager.NotFound)
}

fn send(session: fake.Fake, data: Json) -> Nil {
  client.dm_voice_update(session.ref, g.client_update(data), calls: [])
}

fn dm_server_updates(session: fake.Fake) -> Int {
  list.length(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
}

fn dm_join(session: fake.Fake, channel: Int, count: Int) -> String {
  send(session, g.join(channel))
  assert api.eventually(fn() { dm_server_updates(session) >= count })
  let assert Ok(update) =
    list.last(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert api.eventually(fn() { list.contains(call_connections(channel), conn) })
  conn
}

fn dm_entries(session: fake.Fake) -> List(Entry) {
  let assert Ok(subject) = dm_server.lookup(session.ref)
  let assert Ok(pid) = process.subject_owner(subject)
  let assert Ok(states) =
    decode.run(
      get_state(pid),
      decode.at([5, 1], decode.dict(decode.dynamic, decode.dynamic)),
    )
  states |> dict.values |> list.map(to_entry)
}

fn counted(conn: String) -> Bool {
  list.contains(voice_ids.counted(), conn)
}

fn settle() -> Nil {
  let _ = wait.until(300, fn() { False })
  Nil
}

pub fn crashed_dm_call_drops_the_dm_states_and_their_count_rows_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6101, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = dm_join(one, 6101, 1)
  let c2 = dm_join(two, 6101, 1)
  assert counted(c1) && counted(c2)
  process.kill(call_pid(6101))
  assert api.eventually(fn() { call_gone(6101) })
  assert api.eventually(fn() { dm_entries(one) == [] && dm_entries(two) == [] })
  assert api.eventually(fn() { !counted(c1) && !counted(c2) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn in_place_update_after_a_dm_call_crash_does_not_count_the_connection_again_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6102, [1, 2])
  let one = fake.start(1)
  let c1 = dm_join(one, 6102, 1)
  process.kill(call_pid(6102))
  assert api.eventually(fn() { call_gone(6102) })
  assert api.eventually(fn() { dm_entries(one) == [] })
  send(one, g.update(Some(6102), Some(c1), [#("self_mute", json.bool(True))]))
  settle()
  assert !counted(c1)
  assert dm_entries(one) == []
  fake.stop(one)
  api.stop(stub)
}

pub fn dm_call_crash_frees_the_per_channel_connection_limit_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6103, [1, 2])
  let one = fake.start(1)
  list.each([1, 2, 3, 4, 5], fn(count) {
    let _ = dm_join(one, 6103, count)
    Nil
  })
  assert list.length(dm_entries(one)) == 5
  process.kill(call_pid(6103))
  assert api.eventually(fn() { call_gone(6103) })
  create_call(6103, [1, 2])
  let again = dm_join(one, 6103, 6)
  assert call_connections(6103) == [again]
  assert list.map(dm_entries(one), voice_ids.of) == [again]
  send(one, g.update(None, None, []))
  assert api.eventually(fn() { call_gone(6103) })
  fake.stop(one)
  api.stop(stub)
}

pub fn dm_states_survive_a_call_handoff_exit_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6104, [1, 2])
  let one = fake.start(1)
  let c1 = dm_join(one, 6104, 1)
  let assert Ok(old) = manager.lookup(g.channel_id(6104))
  let old_pid = call_pid(6104)
  let assert Ok(exported) = call_server.export(old, 2000)
  let assert Ok(started) = call_server.start(call_server.Restored(exported))
  process.unlink(started.pid)
  call_server.stop_for_handoff(old, started.data)
  assert api.eventually(fn() { !process.is_alive(old_pid) })
  settle()
  assert list.map(dm_entries(one), voice_ids.of) == [c1]
  process.kill(started.pid)
  fake.stop(one)
  api.stop(stub)
}

fn voice_world(guild: Int) -> g.World {
  g.World(..g.world(), guild:)
}

fn nonce_for(stub: api.Api, conn: String) -> option.Option(String) {
  let tokens = api.calls_of(stub, "voice_get_token") |> list.reverse
  let exact =
    list.find(tokens, fn(call) {
      api.field(call, "connection_id") == Some(conn)
    })
  let fresh =
    list.find(tokens, fn(call) { api.field(call, "connection_id") == None })
  case exact, fresh {
    Ok(call), _ -> api.field(call, "token_nonce")
    Error(Nil), Ok(call) -> api.field(call, "token_nonce")
    Error(Nil), Error(Nil) -> None
  }
}

fn guild_join(stub: api.Api, host: g.Host, ref: SessionRef, channel: Int) {
  let updates = fn() { g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE") }
  let before = list.length(updates())
  g.client(host, ref, g.join(channel))
  assert api.eventually(fn() { list.length(updates()) > before })
  let assert Ok(update) = list.last(updates())
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.confirm(host, conn, nonce_for(stub, conn)) == ok
  conn
}

pub fn crashed_guild_count_rows_are_cleared_without_waiting_for_the_rebuild_test() {
  let stub = api.start()
  let host = g.start(voice_world(911))
  let conn = guild_join(stub, host, g.session(1), 10)
  assert api.eventually(fn() { counted(conn) })
  process.kill(host.pid)
  assert api.eventually(fn() { !counted(conn) })
  api.stop(stub)
}

pub fn same_node_guild_hand_off_keeps_the_count_rows_of_the_target_test() {
  let world = voice_world(912)
  let stub = api.start()
  let source = g.start(world)
  let conn = guild_join(stub, source, g.session(1), 10)
  let #(voice, _successor) = g.freeze(source)
  let target = g.adopt(world, voice)
  g.forward(source)
  assert api.eventually(fn() { !process.is_alive(source.pid) })
  settle()
  assert counted(conn)
  let assert [row] =
    list.filter(count_table.rows(), fn(row) { voice_ids.row_text(row) == conn })
  assert row.owner == target.pid
  g.stop(target)
  assert api.eventually(fn() { !counted(conn) })
  api.stop(stub)
}

fn quick_rebuild() -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      voice: VoiceDurations(
        ..d.voice,
        counts_sync_interval_ms: 40,
        counts_collect_timeout_ms: 300,
      ),
    )
  })
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_voice_counts")
  cluster_app.resume(paused)
}

fn ghost(guild: Int, conn: String) -> Entry {
  entry.new(
    user: g.user_id(3),
    guild: Some(g.guild_id(voice_world(guild))),
    channel: g.channel_id(10),
    connection_id: voice_ids.conn(conn),
    session_id: Some("ghost-session"),
    flags: entry.Flags(False, False, False, False, False, False),
    mute: False,
    deaf: False,
  )
}

fn plant(host: g.Host, value: Entry) -> Nil {
  g.run(host, fn(voice, _ctx) {
    count_table.upsert(value)
    #(voice, [])
  })
}

fn stall(host: g.Host, ms: Int) -> Nil {
  g.run(host, fn(voice, _ctx) {
    process.sleep(ms)
    #(voice, [])
  })
}

pub fn a_stalled_guild_does_not_block_stale_count_removal_for_the_others_test() {
  let stub = api.start()
  let busy = g.start(voice_world(913))
  let idle = g.start(voice_world(914))
  let live = guild_join(stub, busy, g.session(1), 10)
  let kept = guild_join(stub, idle, g.session(2), 10)
  plant(idle, ghost(914, "ghost-914"))
  assert api.eventually(fn() { counted("ghost-914") })
  stall(busy, 3000)
  quick_rebuild()
  assert wait.until(2000, fn() { !counted("ghost-914") })
  assert counted(live) && counted(kept)
  g.stop(busy)
  g.stop(idle)
  api.stop(stub)
}

pub fn restarted_count_janitor_still_clears_a_crashed_owner_test() {
  let stub = api.start()
  let host = g.start(voice_world(915))
  let conn = guild_join(stub, host, g.session(1), 10)
  assert api.eventually(fn() { counted(conn) })
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_voice_counts")
  cluster_app.resume(paused)
  assert counted(conn)
  process.kill(host.pid)
  assert api.eventually(fn() { !counted(conn) })
  api.stop(stub)
}

fn pace(host: g.Host) -> Nil {
  stall(host, 150)
  process.sleep(150)
  pace(host)
}

pub fn a_connection_removed_during_a_slow_rebuild_is_not_counted_again_test() {
  let stub = api.start()
  let slow = g.start(voice_world(916))
  let quick = g.start(voice_world(917))
  let _ = guild_join(stub, slow, g.session(1), 10)
  quick_rebuild()
  let pacer = process.spawn_unlinked(fn() { pace(slow) })
  let resurrected =
    [1, 2, 3, 4, 5, 6, 7, 8]
    |> list.any(fn(round) {
      let ref = g.session(4)
      let conn = guild_join(stub, quick, ref, 10)
      assert api.eventually(fn() { counted(conn) })
      let _ = wait.until(round * 17, fn() { False })
      g.client(quick, ref, g.update(None, Some(conn), []))
      assert api.eventually(fn() { !counted(conn) })
      wait.until_every(400, 1, fn() { counted(conn) })
    })
  process.kill(pacer)
  assert !resurrected
  g.stop(slow)
  g.stop(quick)
  api.stop(stub)
}

pub fn failed_e2ee_body_still_restores_the_rollout_config_test() {
  let runner =
    process.spawn_unlinked(fn() {
      rollout_guard.with_platform_e2ee(fn() {
        let assert True = list.is_empty([1])
        Nil
      })
    })
  let monitor = process.monitor(runner)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(5000)
  assert api.eventually(rollout_guard.restored)
}

pub fn killed_e2ee_test_still_restores_the_rollout_config_test() {
  let entered = process.new_subject()
  let runner =
    process.spawn_unlinked(fn() {
      rollout_guard.with_platform_e2ee(fn() {
        process.send(entered, Nil)
        process.sleep_forever()
      })
    })
  let assert Ok(Nil) = process.receive(entered, 5000)
  assert rollout.voice_e2ee_scope() == rollout.PlatformWide
  process.kill(runner)
  assert api.eventually(rollout_guard.restored)
}

fn quick_permission_retries() -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      voice: VoiceDurations(
        ..d.voice,
        permission_sync_retry_base_ms: 10,
        permission_sync_retry_jitter_ms: 0,
        permission_sync_retry_max_ms: 20,
      ),
    )
  })
}

fn sync_participant(user: Int) -> Nil {
  permission_sync.enqueue_participant(
    g.guild_id(voice_world(918)),
    g.channel_id(10),
    g.user_id(user),
    False,
    False,
    token.Permissions(can_speak: True, can_stream: True, can_video: True),
  )
}

fn restart_permission_sync() -> Nil {
  let assert Ok(paused) = cluster_app.pause("fluxer_voice_permission_sync")
  cluster_app.resume(paused)
}

pub fn a_dead_permission_sync_worker_never_retries_into_its_successor_test() {
  quick_permission_retries()
  let stub = api.start()
  api.set(stub, "voice_update_participant", api.Hold)
  sync_participant(5)
  assert api.await_held(stub, "voice_update_participant", 1)
  restart_permission_sync()
  sync_participant(5)
  assert api.await_held(stub, "voice_update_participant", 2)
  assert api.release(stub, "voice_update_participant", api.Status(500, "{}"))
  settle()
  assert list.length(api.calls_of(stub, "voice_update_participant")) == 2
  assert api.held(stub, "voice_update_participant") == 1
  assert api.release(stub, "voice_update_participant", api.success())
  api.stop(stub)
}

pub fn permission_sync_restart_keeps_the_voice_registries_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6105, [1, 2])
  let one = fake.start(1)
  let c1 = dm_join(one, 6105, 1)
  let assert Ok(before) = dm_server.lookup(one.ref)
  restart_permission_sync()
  assert dm_server.lookup(one.ref) == Ok(before)
  assert counted(c1)
  send(one, g.update(None, None, []))
  assert api.eventually(fn() { call_gone(6105) })
  assert api.eventually(fn() { !counted(c1) })
  fake.stop(one)
  api.stop(stub)
}
