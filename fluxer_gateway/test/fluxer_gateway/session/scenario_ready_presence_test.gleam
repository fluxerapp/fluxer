// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{type Status, Idle, Online}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/presence/client.{type PresenceLink}
import fluxer_gateway/session/control
import fluxer_gateway/session/links
import fluxer_gateway/session/state.{type PresenceState, PresenceUp}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit/should
import support/guild_api
import support/guild_fixture as fixture
import support/presence_scenario as scn
import support/session_fake_guild as fake_guild
import support/session_harness.{type Frame, Frame} as h

const quiet_ms = 400

fn online(user: Int, friends: List(Int)) -> PresenceLink {
  let fake = scn.fake_session(user, int.random(1_000_000_000))
  let link = scn.connect(fake, Online, friends)
  scn.sync(user)
  link
}

fn friend_row(user: Int) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(user))),
    #("type", json.int(1)),
    #("user", h.user_json(user, False)),
  ])
}

fn channel_row(channel: Int, kind: Int, recipients: List(Int)) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(channel))),
    #("type", json.int(kind)),
    #("recipients", json.array(recipients, h.user_json(_, False))),
  ])
}

fn identify(
  me: Int,
  guilds: List(Int),
  extra: List(#(String, json.Json)),
  token: String,
  run: fn(SessionRef, h.FakeSocket, Dynamic) -> Nil,
) -> Nil {
  use <- h.with_api(
    h.session_api(h.session_data_with(me, False, guilds, extra)),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify(token), socket)
  let assert Ok(Frame(_, "READY", ready)) = h.await_event(socket, "READY", 8000)
  run(ref, socket, ready)
  h.terminate(ref)
}

fn id_text() -> decode.Decoder(String) {
  decode.one_of(decode.string, [decode.int |> decode.map(int.to_string)])
}

fn ready_statuses(ready: Dynamic) -> List(#(String, String)) {
  let entry = {
    use user <- decode.subfield(["user", "id"], id_text())
    use status <- decode.field("status", decode.string)
    decode.success(#(user, status))
  }
  case decode.run(ready, decode.at(["presences"], decode.list(entry))) {
    Ok(found) -> list.sort(found, fn(a, b) { string.compare(a.0, b.0) })
    Error(_) -> []
  }
}

fn updates_about(frames: List(Frame), user: Int) -> List(#(String, String)) {
  let wanted = int.to_string(user)
  list.filter_map(frames, fn(frame) {
    case frame {
      Frame(_, "PRESENCE_UPDATE", payload) ->
        case decode.run(payload, decode.at(["user", "id"], id_text())) {
          Ok(found) if found == wanted ->
            Ok(#(scope_of(payload), status_of(payload)))
          _ -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
}

fn scope_of(payload: Dynamic) -> String {
  case decode.run(payload, decode.at(["guild_id"], decode.string)) {
    Ok(guild) -> guild
    Error(_) -> "direct"
  }
}

fn status_of(payload: Dynamic) -> String {
  case decode.run(payload, decode.at(["status"], decode.string)) {
    Ok(status) -> status
    Error(_) -> ""
  }
}

fn await_update_about(
  socket: h.FakeSocket,
  user: Int,
) -> Result(#(String, String), Nil) {
  case
    h.await_frame(socket, 3000, fn(frame) { updates_about([frame], user) != [] })
  {
    Ok(frame) ->
      case updates_about([frame], user) {
        [found] -> Ok(found)
        _ -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

fn wire(guild: Option(Int), user: Int, status: Status) -> RawJson {
  let scope = case guild {
    Some(guild) -> "\"guild_id\":\"" <> int.to_string(guild) <> "\","
    None -> ""
  }
  raw_json.wrap(<<
    "{":utf8,
    scope:utf8,
    "\"custom_status\":null,\"afk\":false,\"mobile\":false,\"status\":\"":utf8,
    presence.status_to_wire(status):utf8,
    "\",\"user\":{\"username\":\"scenario\",\"id\":\"":utf8,
    int.to_string(user):utf8,
    "\"}}":utf8,
  >>)
}

fn from_guild(ref: SessionRef, guild: Int, user: Int, status: Status) -> Nil {
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "PRESENCE_UPDATE",
      wire(Some(guild), user, status),
    ),
  )
}

fn from_presence(ref: SessionRef, user: Int, status: Status) -> Nil {
  let assert Ok(user_id) = id.user_id(user)
  session.send(
    ref,
    session.PresenceFrame(
      user_id,
      wire(None, user, status),
      dispatch_facts.scan("PRESENCE_UPDATE", wire(None, user, status)),
    ),
  )
}

fn guild_create(guild: Int, members: List(Int)) -> RawJson {
  raw_json.from_json(
    json.object([
      #("id", json.string(int.to_string(guild))),
      #("name", json.string("g" <> int.to_string(guild))),
      #(
        "members",
        json.array(members, fn(member) {
          json.object([#("user", h.user_json(member, False))])
        }),
      ),
    ]),
  )
}

fn mutual(guild: Int, partners: List(Int)) -> control.Control {
  control.DmPartnerMutual(
    h.guild_id(guild),
    packed_ids.from_list(list.sort(partners, int.compare)),
  )
}

pub fn ready_friends_are_not_followed_by_a_presence_burst_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let other = h.unique_user()
  let friend_link = online(friend, [me])
  let other_link = online(other, [me])
  let rows = [friend_row(friend), friend_row(other)]
  use ref, socket, ready <- identify(
    me,
    [],
    [#("relationships", json.preprocessed_array(rows))],
    "tok-rp-friends",
  )
  ready_statuses(ready)
  |> should.equal(
    list.sort(
      [#(int.to_string(friend), "online"), #(int.to_string(other), "online")],
      fn(a, b) { string.compare(a.0, b.0) },
    ),
  )
  let after = h.collect(socket, quiet_ms)
  updates_about(after, friend) |> should.equal([])
  updates_about(after, other) |> should.equal([])
  client.session_update(friend_link, Idle, False, False, None)
  await_update_about(socket, friend) |> should.equal(Ok(#("direct", "idle")))
  client.session_update(friend_link, Online, False, False, None)
  await_update_about(socket, friend) |> should.equal(Ok(#("direct", "online")))
  from_presence(ref, friend, Online)
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  client.session_disconnect(friend_link)
  client.session_disconnect(other_link)
}

pub fn group_dm_recipients_in_ready_are_not_repeated_test() {
  let me = h.unique_user()
  let recipient = h.unique_user()
  let link = online(recipient, [])
  let channels = [channel_row(h.unique_user(), 3, [me, recipient])]
  use _ref, socket, ready <- identify(
    me,
    [],
    [#("private_channels", json.preprocessed_array(channels))],
    "tok-rp-group",
  )
  ready_statuses(ready) |> should.equal([#(int.to_string(recipient), "online")])
  updates_about(h.collect(socket, quiet_ms), recipient) |> should.equal([])
  client.session_update(link, Idle, False, False, None)
  await_update_about(socket, recipient) |> should.equal(Ok(#("direct", "idle")))
  client.session_disconnect(link)
}

pub fn a_dm_partner_made_visible_after_ready_arrives_once_test() {
  let me = h.unique_user()
  let partner = h.unique_user()
  let guild = h.unique_user()
  let second = h.unique_user()
  let first_guild = fake_guild.start(guild, fake_guild.Accept)
  let second_guild = fake_guild.start(second, fake_guild.Accept)
  let link = online(partner, [])
  let channels = [channel_row(h.unique_user(), 1, [me, partner])]
  use ref, socket, ready <- identify(
    me,
    [guild, second],
    [#("private_channels", json.preprocessed_array(channels))],
    "tok-rp-dm",
  )
  ready_statuses(ready) |> should.equal([])
  h.control(ref, mutual(guild, [partner]))
  await_update_about(socket, partner)
  |> should.equal(Ok(#("direct", "online")))
  updates_about(h.collect(socket, quiet_ms), partner) |> should.equal([])
  h.control(ref, mutual(second, [partner]))
  h.control(ref, mutual(guild, [partner]))
  from_presence(ref, partner, Online)
  updates_about(h.collect(socket, quiet_ms), partner) |> should.equal([])
  client.session_update(link, Idle, False, False, None)
  await_update_about(socket, partner) |> should.equal(Ok(#("direct", "idle")))
  client.session_disconnect(link)
  fake_guild.crash(first_guild)
  fake_guild.crash(second_guild)
}

pub fn guild_presences_are_sent_once_per_guild_and_content_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let member = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  let link = online(friend, [me])
  use ref, socket, ready <- identify(
    me,
    [guild],
    [#("relationships", json.preprocessed_array([friend_row(friend)]))],
    "tok-rp-guild",
  )
  ready_statuses(ready) |> should.equal([#(int.to_string(friend), "online")])
  let scope = int.to_string(guild)
  from_guild(ref, guild, member, Online)
  from_guild(ref, guild, member, Online)
  updates_about(h.collect(socket, quiet_ms), member)
  |> should.equal([#(scope, "online")])
  from_guild(ref, guild, member, Idle)
  from_guild(ref, guild, member, Idle)
  updates_about(h.collect(socket, quiet_ms), member)
  |> should.equal([#(scope, "idle")])
  from_guild(ref, guild, friend, Online)
  from_guild(ref, guild, friend, Online)
  from_presence(ref, friend, Online)
  updates_about(h.collect(socket, quiet_ms), friend)
  |> should.equal([#(scope, "online")])
  client.session_disconnect(link)
  fake_guild.crash(fake)
}

pub fn a_guild_joined_after_ready_does_not_repeat_its_members_presences_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let guild = h.unique_user()
  let link = online(friend, [me])
  use ref, socket, ready <- identify(
    me,
    [],
    [#("relationships", json.preprocessed_array([friend_row(friend)]))],
    "tok-rp-lazy",
  )
  ready_statuses(ready) |> should.equal([#(int.to_string(friend), "online")])
  let fake =
    fake_guild.start(
      guild,
      fake_guild.AcceptWith(guild_create(guild, [me, friend])),
    )
  h.control(ref, scn.guild_join(guild))
  let assert Ok(Frame(_, "GUILD_CREATE", _)) =
    h.await_event(socket, "GUILD_CREATE", 3000)
  from_guild(ref, guild, friend, Online)
  from_presence(ref, friend, Online)
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "GUILD_DELETE",
      raw_json.from_json(
        json.object([
          #("id", json.string(int.to_string(guild))),
          #("unavailable", json.bool(True)),
        ]),
      ),
    ),
  )
  let assert Ok(Frame(_, "GUILD_DELETE", _)) =
    h.await_event(socket, "GUILD_DELETE", 3000)
  from_guild(ref, guild, friend, Online)
  updates_about(h.collect(socket, quiet_ms), friend)
  |> should.equal([#(int.to_string(guild), "online")])
  client.session_disconnect(link)
  fake_guild.crash(fake)
}

pub fn a_guild_collected_into_ready_does_not_repeat_its_members_presences_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let guild = h.unique_user()
  let fake =
    fake_guild.start(
      guild,
      fake_guild.AcceptWith(guild_create(guild, [me, friend])),
    )
  let link = online(friend, [me])
  use ref, socket, ready <- identify(
    me,
    [guild],
    [#("relationships", json.preprocessed_array([friend_row(friend)]))],
    "tok-rp-collected",
  )
  ready_statuses(ready) |> should.equal([#(int.to_string(friend), "online")])
  from_guild(ref, guild, friend, Online)
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  from_guild(ref, guild, friend, Idle)
  updates_about(h.collect(socket, quiet_ms), friend)
  |> should.equal([#(int.to_string(guild), "idle")])
  client.session_disconnect(link)
  fake_guild.crash(fake)
}

pub fn a_change_while_ready_is_pending_is_not_lost_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let guild = h.unique_user()
  let fake =
    fake_guild.start(guild, fake_guild.SlowWith(600, guild_create(guild, [me])))
  let link = online(friend, [me])
  let data =
    h.session_data_with(me, False, [guild], [
      #("relationships", json.preprocessed_array([friend_row(friend)])),
    ])
  use <- h.with_api(h.session_api(data))
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-rp-pending"), socket)
  process.sleep(200)
  client.session_update(link, Idle, False, False, None)
  scn.sync(friend)
  let assert Ok(Frame(_, "READY", ready)) = h.await_event(socket, "READY", 8000)
  let shown = case ready_statuses(ready) {
    [#(_, status)] -> status
    _ -> ""
  }
  let after = updates_about(h.collect(socket, quiet_ms), friend)
  let last = case list.last(after) {
    Ok(#(_, status)) -> status
    Error(Nil) -> shown
  }
  last |> should.equal("idle")
  list.filter(after, fn(update) { update.1 == shown }) |> should.equal([])
  h.terminate(ref)
  client.session_disconnect(link)
  fake_guild.crash(fake)
}

pub fn a_change_after_ready_is_delivered_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let link = online(friend, [me])
  use _ref, socket, _ready <- identify(
    me,
    [],
    [#("relationships", json.preprocessed_array([friend_row(friend)]))],
    "tok-rp-after",
  )
  client.session_update(link, Idle, False, False, None)
  await_update_about(socket, friend) |> should.equal(Ok(#("direct", "idle")))
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  client.session_disconnect(link)
}

pub fn resume_does_not_replay_a_presence_burst_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let link = online(friend, [me])
  use ref, socket, ready <- identify(
    me,
    [],
    [#("relationships", json.preprocessed_array([friend_row(friend)]))],
    "tok-rp-resume",
  )
  ready_statuses(ready) |> should.equal([#(int.to_string(friend), "online")])
  let frames = h.collect(socket, quiet_ms)
  updates_about(frames, friend) |> should.equal([])
  let seq = h.state_of(ref).seq
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  from_presence(ref, friend, Online)
  let fresh = h.fake_socket()
  h.resume(ref, "tok-rp-resume", seq, fresh) |> should.equal(Ok(ref))
  let resumed = h.collect(fresh, quiet_ms)
  h.events(resumed) |> list.contains("RESUMED") |> should.be_true
  updates_about(resumed, friend) |> should.equal([])
  client.session_update(link, Idle, False, False, None)
  await_update_about(fresh, friend) |> should.equal(Ok(#("direct", "idle")))
  client.session_disconnect(link)
}

pub fn a_presence_relink_does_not_repeat_presences_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let link = online(friend, [me])
  use ref, socket, ready <- identify(
    me,
    [],
    [#("relationships", json.preprocessed_array([friend_row(friend)]))],
    "tok-rp-relink",
  )
  ready_statuses(ready) |> should.equal([#(int.to_string(friend), "online")])
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  let assert Ok(pid) = scn.presence_pid(me)
  process.kill(pid)
  h.wait_until(
    fn() {
      case scn.presence_pid(me) {
        Ok(next) -> next != pid && process.is_alive(next)
        Error(Nil) -> False
      }
    },
    3000,
  )
  |> should.be_true
  h.wait_until(fn() { is_up(h.state_of(ref).presence) }, 3000)
  |> should.be_true
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  client.session_update(link, Idle, False, False, None)
  await_update_about(socket, friend) |> should.equal(Ok(#("direct", "idle")))
  client.session_disconnect(link)
}

fn is_up(found: PresenceState) -> Bool {
  case found {
    PresenceUp(_, _) -> True
    _ -> False
  }
}

pub fn a_real_guild_member_subscription_is_not_repeated_test() {
  let me = h.unique_user()
  let friend = h.unique_user()
  let member = h.unique_user()
  use api <- guild_api.with_api
  let found =
    fixture.new(me)
    |> fixture.with_member(friend, [])
    |> fixture.with_member(member, [])
    |> fixture.with_text(fixture.unique_guild_id())
  fixture.publish(api, found)
  let friend_link = online(friend, [me])
  let member_link = online(member, [])
  guild_api.override(
    api,
    "session",
    None,
    guild_api.Data(
      h.session_data_with(me, False, [found.id], [
        #("relationships", json.preprocessed_array([friend_row(friend)])),
      ]),
    ),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-rp-real"), socket)
  let assert Ok(Frame(_, "READY", ready)) = h.await_event(socket, "READY", 8000)
  ready_statuses(ready) |> should.equal([#(int.to_string(friend), "online")])
  updates_about(h.collect(socket, quiet_ms), friend) |> should.equal([])
  let scope = int.to_string(found.id)
  subscribe_members(ref, found.id, [friend, member])
  let first = h.collect(socket, quiet_ms)
  updates_about(first, member) |> should.equal([#(scope, "online")])
  list.length(updates_about(first, friend)) |> should.equal(1)
  subscribe_members(ref, found.id, [])
  subscribe_members(ref, found.id, [friend, member])
  let again = h.collect(socket, quiet_ms)
  updates_about(again, member) |> should.equal([])
  updates_about(again, friend) |> should.equal([])
  client.session_update(member_link, Idle, False, False, None)
  await_update_about(socket, member) |> should.equal(Ok(#(scope, "idle")))
  h.terminate(ref)
  client.session_disconnect(friend_link)
  client.session_disconnect(member_link)
  fixture.stop(found)
}

fn subscribe_members(ref: SessionRef, guild: Int, members: List(Int)) -> Nil {
  let data =
    json.object([
      #(
        "subscriptions",
        json.object([
          #(
            int.to_string(guild),
            json.object([
              #(
                "members",
                json.array(members, fn(user) {
                  json.string(int.to_string(user))
                }),
              ),
            ]),
          ),
        ]),
      ),
    ])
  session.send(
    ref,
    session.SocketOp(session.LazyRequest(fixture.to_dynamic(data))),
  )
}

fn ready_with_guild_dm_partner(
  token: String,
  run: fn(SessionRef, h.FakeSocket, Int, Int, Int) -> Nil,
) -> Nil {
  let me = h.unique_user()
  let partner = h.unique_user()
  let guild = h.unique_user()
  let slow = h.unique_user()
  let fake =
    fake_guild.start(
      guild,
      fake_guild.AcceptWith(guild_create(guild, [me, partner])),
    )
  let slow_fake =
    fake_guild.start(slow, fake_guild.SlowWith(800, guild_create(slow, [me])))
  let link = online(partner, [])
  let channels = [channel_row(h.unique_user(), 1, [me, partner])]
  let data =
    h.session_data_with(me, False, [guild, slow], [
      #("private_channels", json.preprocessed_array(channels)),
    ])
  use <- h.with_api(h.session_api(data))
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify(token), socket)
  h.wait_until(
    fn() { links.linked_ref(h.state_of(ref).links, h.guild_id(guild)) != None },
    3000,
  )
  |> should.be_true
  h.control(ref, mutual(guild, [partner]))
  let assert Ok(Frame(_, "READY", ready)) = h.await_event(socket, "READY", 8000)
  ready_statuses(ready) |> should.equal([#(int.to_string(partner), "online")])
  updates_about(h.collect(socket, quiet_ms), partner) |> should.equal([])
  run(ref, socket, me, partner, guild)
  h.terminate(ref)
  client.session_disconnect(link)
  fake_guild.crash(fake)
  fake_guild.crash(slow_fake)
}

fn guild_gone(ref: SessionRef, socket: h.FakeSocket, guild: Int) -> Nil {
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "GUILD_DELETE",
      raw_json.from_json(
        json.object([
          #("id", json.string(int.to_string(guild))),
          #("unavailable", json.bool(True)),
        ]),
      ),
    ),
  )
  let assert Ok(Frame(_, "GUILD_DELETE", _)) =
    h.await_event(socket, "GUILD_DELETE", 3000)
  Nil
}

pub fn a_guild_dm_partner_is_restored_after_the_guild_comes_back_test() {
  use ref, socket, me, partner, guild <- ready_with_guild_dm_partner(
    "tok-rp-dm-guild-back",
  )
  guild_gone(ref, socket, guild)
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "GUILD_CREATE",
      guild_create(guild, [me, partner]),
    ),
  )
  let assert Ok(Frame(_, "GUILD_CREATE", _)) =
    h.await_event(socket, "GUILD_CREATE", 3000)
  h.control(ref, mutual(guild, [partner]))
  from_presence(ref, partner, Online)
  from_guild(ref, guild, partner, Online)
  updates_about(h.collect(socket, quiet_ms), partner)
  |> list.contains(#("direct", "online"))
  |> should.be_true
}

pub fn a_guild_dm_partner_removed_from_the_guild_is_resent_test() {
  use ref, socket, _me, partner, guild <- ready_with_guild_dm_partner(
    "tok-rp-dm-guild-removed",
  )
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "GUILD_MEMBER_REMOVE",
      raw_json.from_json(
        json.object([
          #("guild_id", json.string(int.to_string(guild))),
          #("user", h.user_json(partner, False)),
        ]),
      ),
    ),
  )
  let assert Ok(Frame(_, "GUILD_MEMBER_REMOVE", _)) =
    h.await_event(socket, "GUILD_MEMBER_REMOVE", 3000)
  from_presence(ref, partner, Online)
  updates_about(h.collect(socket, quiet_ms), partner)
  |> should.equal([#("direct", "online")])
}

fn bare_wire(guild: Int, user: Int, status: Status) -> RawJson {
  raw_json.wrap(<<
    "{\"user\":{\"id\":":utf8,
    int.to_string(user):utf8,
    ",\"username\":\"scenario\"},\"status\":\"":utf8,
    presence.status_to_wire(status):utf8,
    "\",\"mobile\":false,\"guild_id\":":utf8,
    int.to_string(guild):utf8,
    ",\"afk\":false,\"custom_status\":null}":utf8,
  >>)
}

pub fn a_presence_sent_with_bare_and_quoted_ids_arrives_once_test() {
  let me = h.unique_user()
  let member = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  use ref, socket, _ready <- identify(me, [guild], [], "tok-rp-bare-ids")
  from_guild(ref, guild, member, Online)
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "PRESENCE_UPDATE",
      bare_wire(guild, member, Online),
    ),
  )
  updates_about(h.collect(socket, quiet_ms), member)
  |> should.equal([#(int.to_string(guild), "online")])
  from_guild(ref, guild, member, Idle)
  session.send(
    ref,
    session.guild_dispatch(
      h.guild_id(guild),
      "PRESENCE_UPDATE",
      bare_wire(guild, member, Idle),
    ),
  )
  updates_about(h.collect(socket, quiet_ms), member)
  |> should.equal([#(int.to_string(guild), "idle")])
  fake_guild.crash(fake)
}
