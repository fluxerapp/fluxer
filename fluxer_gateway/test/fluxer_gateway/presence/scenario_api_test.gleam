// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/client
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/rpc
import gleam/dynamic
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import support/presence_api_stub as stub
import support/presence_scenario as scn

fn params(fields: List(#(String, dynamic.Dynamic))) -> dynamic.Dynamic {
  dynamic.properties(
    list.map(fields, fn(pair) { #(dynamic.string(pair.0), pair.1) }),
  )
}

fn guild_call(method: String, user: Int, guild: Int) {
  rpc.handle(
    method,
    params([
      #("user_id", dynamic.string(int.to_string(user))),
      #("guild_id", dynamic.string(int.to_string(guild))),
    ]),
  )
}

fn ok_reply(_request: stub.Request) -> String {
  "{\"data\":{}}"
}

fn kicks(api: stub.Stub, guild: Int, timeout_ms: Int) -> Bool {
  case stub.next_request(api, "kick_temporary_member", timeout_ms) {
    Ok(found) ->
      case string.contains(found.body, int.to_string(guild)) {
        True -> True
        False -> kicks(api, guild, timeout_ms)
      }
    Error(Nil) -> False
  }
}

pub fn logout_kicks_temporary_guilds_test() {
  let api = stub.start(ok_reply)
  use <- stub.with_api(api)
  let user = 7_400_000_000_000_001
  let guild = 7_400_000_000_000_801
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  guild_call("presence.add_temporary_guild", user, guild)
  |> should.equal(Ok(json.bool(True)))
  scn.sync(user)
  client.session_disconnect(link)
  kicks(api, guild, 2000) |> should.be_true
}

pub fn removed_temporary_guild_is_not_kicked_test() {
  let api = stub.start(ok_reply)
  use <- stub.with_api(api)
  let user = 7_400_000_000_000_002
  let guild = 7_400_000_000_000_802
  let session = scn.fake_session(user, 2)
  let link = scn.connect(session, Online, [])
  let _ = guild_call("presence.add_temporary_guild", user, guild)
  let _ = guild_call("presence.remove_temporary_guild", user, guild)
  scn.sync(user)
  client.session_disconnect(link)
  kicks(api, guild, 300) |> should.be_false
}

pub fn session_handoff_does_not_kick_temporary_guilds_test() {
  let api = stub.start(ok_reply)
  use <- stub.with_api(api)
  let user = 7_400_000_000_000_003
  let guild = 7_400_000_000_000_803
  let session = scn.fake_session(user, 3)
  let _old = scn.connect(session, Online, [])
  let _ = guild_call("presence.add_temporary_guild", user, guild)
  scn.sync(user)
  scn.exit_session(session, protocol.handoff_exit())
  scn.sync(user)
  let moved = scn.fake_session(user, 3)
  let link = scn.connect(moved, Online, [])
  kicks(api, guild, 300) |> should.be_false
  client.session_disconnect(link)
  kicks(api, guild, 2000) |> should.be_true
}

pub fn crashed_presence_still_kicks_temporary_guilds_test() {
  let api = stub.start(ok_reply)
  use <- stub.with_api(api)
  let user = 7_400_000_000_000_004
  let guild = 7_400_000_000_000_804
  let session = scn.fake_session(user, 4)
  let _link = scn.connect(session, Online, [])
  let _ = guild_call("presence.add_temporary_guild", user, guild)
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  process.kill(pid)
  kicks(api, guild, 2000) |> should.be_true
}

pub fn joined_guild_reaches_sessions_test() {
  let user = 7_400_000_000_000_005
  let guild = 7_400_000_000_000_805
  let session = scn.fake_session(user, 5)
  let link = scn.connect(session, Online, [])
  let _ = scn.drain_seen(session, 50)
  guild_call("presence.join_guild", user, guild)
  |> should.equal(Ok(json.bool(True)))
  guild_call("presence.join_guild", user, guild)
  |> should.equal(Ok(json.bool(True)))
  let seen = scn.drain_seen(session, 100)
  list.count(seen, fn(found) {
    case found {
      scn.Notice(notice) -> string.inspect(notice) |> string.contains("Join")
      _ -> False
    }
  })
  |> should.equal(1)
  guild_call("presence.leave_guild", user, guild)
  |> should.equal(Ok(json.bool(True)))
  scn.drain_seen(session, 100)
  |> list.count(fn(found) {
    case found {
      scn.Notice(notice) -> string.inspect(notice) |> string.contains("Leave")
      _ -> False
    }
  })
  |> should.equal(1)
  client.session_disconnect(link)
}

fn validating_reply(request: stub.Request) -> String {
  case request.kind {
    "validate_custom_status" ->
      "{\"data\":{\"custom_status\":{\"text\":\"checked\"}}}"
    _ -> "{\"data\":{}}"
  }
}

pub fn custom_status_update_is_validated_before_publish_test() {
  let api = stub.start(validating_reply)
  use <- stub.with_api(api)
  let user = 7_400_000_000_000_006
  let guild = 7_400_000_000_000_906
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 6)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  client.session_update(
    link,
    Online,
    False,
    False,
    Some(raw_json.wrap(<<"{\"text\":\"raw\"}":utf8>>)),
  )
  let assert Ok(request) =
    stub.next_request(api, "validate_custom_status", 2000)
  string.contains(request.body, "raw") |> should.be_true
  let assert Ok(shown) = scn.next_presence(observer, user, 2000)
  presence.custom_status(shown)
  |> option.map(fn(raw) {
    string.contains(string.inspect(raw_json.to_bits(raw)), "checked")
  })
  |> should.equal(Some(True))
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

fn rejecting_reply(request: stub.Request) -> String {
  case request.kind {
    "validate_custom_status" -> "{\"data\":{\"custom_status\":null}}"
    _ -> "{\"data\":{}}"
  }
}

pub fn custom_status_cleared_by_validation_stays_hidden_test() {
  let api = stub.start(rejecting_reply)
  use <- stub.with_api(api)
  let user = 7_400_000_000_000_007
  let guild = 7_400_000_000_000_907
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 7)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  client.session_update(
    link,
    Online,
    False,
    False,
    Some(raw_json.wrap(<<"{\"text\":\"spam\"}":utf8>>)),
  )
  let assert Ok(_) = stub.next_request(api, "validate_custom_status", 2000)
  scn.sync(user)
  process.sleep(50)
  scn.presences(observer, user, 100)
  |> list.filter(fn(found) { presence.custom_status(found) != None })
  |> should.equal([])
  client.session_disconnect(link)
  scn.stop_observing(guild)
}
