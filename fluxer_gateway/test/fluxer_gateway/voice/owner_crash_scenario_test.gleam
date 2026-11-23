// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/voice_key
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/registry
import fluxer_gateway/voice/rpc
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import support/cluster_app
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as guild_fake
import support/guild_voice as voice
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake
import support/wait

const voice_channel = 40

fn voice_guild() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_channel(voice_channel, 2, None, [])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
}

fn counted(conn: String) -> Bool {
  list.contains(voice_ids.counted(), conn)
}

fn confirm_through_rpc(
  found: fixture.Guild,
  conn: String,
  nonce: option.Option(String),
) -> Result(Json, Nil) {
  rpc.handle(
    "voice.confirm_connection",
    raw_json.from_json(
      json.object([
        #("guild_id", json.string(int.to_string(found.id))),
        #("channel_id", json.string(int.to_string(voice_channel))),
        #("connection_id", json.string(conn)),
        #("token_nonce", json.nullable(nonce, json.string)),
      ]),
    ),
  )
  |> result_to_nil
}

fn result_to_nil(value: Result(a, b)) -> Result(a, Nil) {
  case value {
    Ok(found) -> Ok(found)
    Error(_) -> Error(Nil)
  }
}

fn succeeded(reply: Json) -> Bool {
  decode.run(g.to_dynamic(reply), decode.at(["success"], decode.bool))
  == Ok(True)
}

pub fn a_pending_join_cut_by_a_guild_crash_is_refused_and_disconnected_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = guild_fake.start(2)
  assert guild_fake.is_joined(guild_fake.join(member, handle))
  voice.serve(api, "conn-pending-2")
  voice.request_join(member, handle, voice_channel)
  let assert Some(_) =
    guild_fake.await_event(member, "VOICE_SERVER_UPDATE", 2000)
  let nonce = voice.nonce_of(api, 2)
  assert nonce != None
  process.kill(handle.pid)
  assert wait.until(3000, fn() {
    voice.forced_connection(api, "conn-pending-2")
  })
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  assert guild_fake.is_joined(guild_fake.join(member, again))
  let assert Ok(reply) = confirm_through_rpc(found, "conn-pending-2", nonce)
  assert !succeeded(reply)
  assert !voice.in_voice(again, 2)
  assert !counted("conn-pending-2")
  fixture.stop(found)
}

pub fn a_confirmed_join_after_a_guild_restart_is_refused_test() {
  use api <- guild_api.with_api
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = guild_fake.start(2)
  assert guild_fake.is_joined(guild_fake.join(member, handle))
  voice.connect(api, handle, member, voice_channel, "conn-again-2")
  let nonce = voice.nonce_of(api, 2)
  process.send_abnormal_exit(handle.pid, "injected")
  assert wait.until(3000, fn() { voice.forced_connection(api, "conn-again-2") })
  let again = fixture.started(api, found)
  assert guild_fake.is_joined(guild_fake.join(member, again))
  let assert Ok(reply) = confirm_through_rpc(found, "conn-again-2", nonce)
  assert !succeeded(reply)
  assert !voice.in_voice(again, 2)
  fixture.stop(found)
}

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

fn dm_join(session: fake.Fake, channel: Int, count: Int) -> String {
  client.dm_voice_update(
    session.ref,
    g.client_update(g.join(channel)),
    calls: [],
  )
  assert api.eventually(fn() {
    list.length(fake.dispatched(session, "VOICE_SERVER_UPDATE")) >= count
  })
  let assert Ok(update) =
    list.last(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert api.eventually(fn() { list.contains(call_connections(channel), conn) })
  conn
}

fn call_confirm(channel: Int, conn: String) -> String {
  let assert Ok(reply) =
    rpc.handle(
      "call.confirm_connection",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("connection_id", json.string(conn)),
        ]),
      ),
    )
  json.to_string(reply)
}

fn restart_call_manager() -> Nil {
  let assert Ok(pid) = cluster_app.registered("fluxer_voice_call_manager")
  process.kill(pid)
  assert wait.until(3000, fn() {
    case cluster_app.registered("fluxer_voice_call_manager") {
      Ok(again) -> again != pid
      Error(Nil) -> False
    }
  })
}

pub fn killed_call_manager_ends_its_calls_and_frees_their_rows_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6201, [1, 2])
  let one = fake.start(1)
  let c1 = dm_join(one, 6201, 1)
  assert call_confirm(6201, c1) == ok
  assert counted(c1)
  restart_call_manager()
  assert api.eventually(fn() { call_gone(6201) })
  assert api.eventually(fn() { !counted(c1) })
  assert api.eventually(fn() {
    case dm_server.lookup(one.ref) {
      Ok(_) -> dm_connections(one) == []
      Error(_) -> True
    }
  })
  create_call(6201, [1, 2])
  let again = dm_join(one, 6201, 2)
  assert again != c1
  assert call_connections(6201) == [again]
  client.dm_voice_update(
    one.ref,
    g.client_update(g.update(None, None, [])),
    calls: [],
  )
  assert api.eventually(fn() { call_gone(6201) })
  fake.stop(one)
  api.stop(stub)
}

@external(erlang, "sys", "get_state")
fn get_state(pid: process.Pid) -> Dynamic

fn dm_connections(session: fake.Fake) -> List(String) {
  let assert Ok(subject) = dm_server.lookup(session.ref)
  let assert Ok(pid) = process.subject_owner(subject)
  let assert Ok(states) =
    decode.run(get_state(pid), decode.at([5, 2], decode.dynamic))
  states |> connection_keys |> list.map(voice_ids.text)
}

@external(erlang, "maps", "keys")
fn connection_keys(value: Dynamic) -> List(voice_key.ConnectionId)

@external(erlang, "ets", "info")
fn ets_info(table: Atom, item: Atom) -> Dynamic

fn table_owner(table: Atom) -> Result(process.Pid, Nil) {
  let found = ets_info(table, atom.create("owner"))
  case decode.run(found, decode.dynamic) {
    Ok(value) ->
      case is_pid(value) {
        True -> Ok(coerce(value))
        False -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

pub fn voice_tables_outlive_their_creator_without_changing_owner_test() {
  let tables = [
    registry.guilds_table(),
    registry.dm_sessions_table(),
    registry.connections_table(),
    registry.count_owners_table(),
  ]
  let before = list.map(tables, table_owner)
  assert list.all(before, fn(owner) { owner != Error(Nil) })
  let assert Ok(paused) = cluster_app.pause("fluxer_voice_permission_sync")
  cluster_app.resume(paused)
  let assert Ok(sync) = cluster_app.registered("fluxer_voice_permission_sync")
  assert list.map(tables, table_owner) == before
  assert !list.contains(before, Ok(sync))
}

fn dm_server_pid(session: fake.Fake) -> process.Pid {
  let assert Ok(subject) = dm_server.lookup(session.ref)
  let assert Ok(pid) = process.subject_owner(subject)
  pid
}

pub fn killed_dm_voice_server_does_not_leave_a_ghost_in_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(6202, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = dm_join(one, 6202, 1)
  let c2 = dm_join(two, 6202, 1)
  assert call_confirm(6202, c1) == ok
  process.kill(dm_server_pid(one))
  client.dm_voice_update(
    one.ref,
    g.client_update(g.update(None, None, [])),
    calls: [],
  )
  assert api.eventually(fn() { call_connections(6202) == [c2] })
  assert api.eventually(fn() { !counted(c1) })
  assert counted(c2)
  client.dm_voice_update(
    two.ref,
    g.client_update(g.update(None, None, [])),
    calls: [],
  )
  assert api.eventually(fn() { call_gone(6202) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}
