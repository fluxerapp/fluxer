// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{Dnd, Idle, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{SessionRef}
import fluxer_gateway/handoff/codec
import fluxer_gateway/presence/custom_status
import fluxer_gateway/presence/dispatch_id
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event
import fluxer_gateway/presence/handoff/codec as presence_codec
import fluxer_gateway/presence/handoff/terms
import fluxer_gateway/presence/handoff/wire_v1
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/protocol.{
  type Input, Carried, CarriedSession, Published, StartRequest,
}
import fluxer_gateway/presence/push_buffer
import fluxer_gateway/presence/version
import gleam/bit_array
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import support/presence_scenario as scn

fn status(text: String) -> custom_status.CustomStatus {
  let assert Ok(found) =
    custom_status.op_value(Some(raw_json.wrap(<<text:utf8>>)))
    |> fn(value) {
      case value {
        custom_status.OpMap(found) -> Ok(found)
        _ -> Error(Nil)
      }
    }
  found
}

fn full_request(user: Int) -> protocol.StartRequest {
  let first = scn.fake_session(user, 41)
  let second = scn.fake_session(user, 42)
  let shown = display.new(status: Dnd, mobile: True, afk: False)
  let current_version = version.next(version.first(version.new_epoch()))
  let user_wire = raw_json.to_bits(scn.wire_for(user))
  let current =
    event.status_changed(
      user:,
      version: current_version,
      display: shown,
      custom_status: <<"{\"text\":\"busy\"}":utf8>>,
      user_wire:,
    )
  let #(push, _) =
    push_buffer.on_message(
      push_buffer.new(),
      push_buffer.DmMessage(
        author: 77,
        channel: Some(900),
        message: Some(901),
        suppressed: False,
        data: raw_json.wrap(<<"{\"content\":\"hi\"}":utf8>>),
      ),
      user,
      False,
      1000,
      push_buffer.default_limits(),
    )
  let push = push_buffer.on_ack(push, 950, 960)
  let delivered =
    list.fold([1, 2, 3], dispatch_id.empty(), fn(acc, _) {
      dispatch_id.remember(acc, dispatch_id.mint())
    })
  StartRequest(
    user:,
    profile: profile.new(
      fields: [
        #(profile.Id, <<"\"123\"":utf8>>),
        #(profile.Username, <<"\"name\"":utf8>>),
        #(profile.Flags, <<"4":utf8>>),
      ],
      restricted: True,
      bot: False,
    ),
    custom_status: Some(status("{\"text\":\"hello\"}")),
    settings_invisible: True,
    guilds: id_set.from_list([5, 3, 9]),
    targets: id_set.from_list([11, 12]),
    push_limits: push_buffer.Limits(max_entries: 10, max_bytes: 4096),
    carried: Some(
      Carried(
        version: current_version,
        last: Some(Published(shown, <<"k":utf8>>, <<"w":utf8>>)),
        current: Some(current),
        temporary_guilds: id_set.from_list([42]),
        push:,
        sessions: [
          CarriedSession(
            session: first.ref,
            status: Online,
            afk: True,
            mobile: False,
          ),
          CarriedSession(
            session: second.ref,
            status: Idle,
            afk: False,
            mobile: True,
          ),
        ],
        validating: Some(status("{\"text\":\"pending\"}")),
        delivered:,
        doomed: [
          #(scn.session_id(43), protocol.TerminateEvery, 7),
          #(scn.session_id(44), protocol.TerminateMatching(["a", "b"]), 8),
        ],
      ),
    ),
  )
}

pub fn presence_tail_round_trips_every_carried_field_test() {
  let request = full_request(8_100_000_000_000_001)
  let assert Ok(bytes) = codec.encode(presence_codec.tail(), request, 1, 77)
  let assert Ok(#(header, decoded)) = codec.decode(presence_codec.tail(), bytes)
  assert header.part == blob.Tail
  assert header.epoch == 77
  assert decoded == request
}

pub fn presence_tail_without_carried_state_round_trips_test() {
  let request =
    StartRequest(..full_request(8_100_000_000_000_002), carried: None)
  let assert Ok(bytes) = codec.encode(presence_codec.tail(), request, 1, 1)
  let assert Ok(#(_, decoded)) = codec.decode(presence_codec.tail(), bytes)
  assert decoded == request
}

fn inputs(user: Int) -> List(Input(Nil)) {
  let fake = scn.fake_session(user, 51)
  let replies = process.new_subject()
  let terminations = process.new_subject()
  let subscriber_inbox = process.new_subject()
  let guild_inbox = process.new_subject()
  let dm =
    push_buffer.DmMessage(
      author: 4,
      channel: None,
      message: Some(5),
      suppressed: True,
      data: raw_json.wrap(<<"{}":utf8>>),
    )
  let assert Ok(user_id) = id.user_id(user)
  let foreign =
    event.status_changed(
      user: 5,
      version: version.first(version.new_epoch()),
      display: display.new(status: Online, mobile: False, afk: True),
      custom_status: <<"null":utf8>>,
      user_wire: <<"{\"id\":\"5\"}":utf8>>,
    )
  [
    protocol.ConnectSession(
      session: SessionRef(..fake.ref, user: user_id),
      status: Idle,
      afk: False,
      mobile: True,
      guilds: id_set.from_list([1]),
      targets: id_set.from_list([2, 3]),
      reply: replies,
    ),
    protocol.Disconnect(fake.ref.id, fake.ref.inbox),
    protocol.Update(fake.ref.id, Dnd, True, False, custom_status.OpNull),
    protocol.Update(
      fake.ref.id,
      Online,
      False,
      False,
      custom_status.OpMap(status("{\"text\":\"x\"}")),
    ),
    protocol.Update(fake.ref.id, Online, False, False, custom_status.OpAbsent),
    protocol.Update(fake.ref.id, Online, False, False, custom_status.OpOther),
    protocol.SubscribeTargets(id_set.from_list([7, 8])),
    protocol.UnsubscribeTargets(id_set.from_list([7])),
    protocol.Foreign(foreign),
    protocol.Foreign(event.as_user_update(foreign)),
    protocol.Resend(protocol.PresenceSubscriber(5, subscriber_inbox)),
    protocol.ResendNode(atom.create("presence_codec_peer@host")),
    protocol.ResendSince(protocol.GuildSubscriber(9, guild_inbox), 100),
    protocol.Joined(
      protocol.PresenceSubscriber(5, subscriber_inbox),
      protocol.Everything,
    ),
    protocol.UserDispatch(
      dispatch_id.mint(),
      "USER_UPDATE",
      raw_json.wrap(<<"{}":utf8>>),
      protocol.UserUpdate(profile.new(
        fields: [#(profile.Id, <<"\"5\"":utf8>>)],
        restricted: False,
        bot: True,
      )),
    ),
    protocol.UserDispatch(
      dispatch_id.mint(),
      "USER_SETTINGS_UPDATE",
      raw_json.wrap(<<"{}":utf8>>),
      protocol.UserSettingsUpdate(
        custom_status.SettingsMap(status("{\"text\":\"s\"}")),
        Some(True),
      ),
    ),
    protocol.UserDispatch(
      dispatch_id.mint(),
      "MESSAGE_CREATE",
      raw_json.wrap(<<"{}":utf8>>),
      protocol.MessageCreate(Some(dm)),
    ),
    protocol.UserDispatch(
      dispatch_id.mint(),
      "MESSAGE_ACK",
      raw_json.wrap(<<"{}":utf8>>),
      protocol.MessageAck(Some(1), None),
    ),
    protocol.UserDispatch(
      dispatch_id.mint(),
      "OTHER",
      raw_json.wrap(<<"{}":utf8>>),
      protocol.OtherEvent,
    ),
    protocol.Guild(protocol.JoinGuild(1)),
    protocol.Guild(protocol.LeaveGuild(2)),
    protocol.Guild(protocol.AddTemporaryGuild(3)),
    protocol.Guild(protocol.RemoveTemporaryGuild(4)),
    protocol.VisitStart(6, True),
    protocol.VisitEnd(6),
    protocol.TerminateSessions(["h"], terminations),
    protocol.TerminateAll,
    protocol.Rejoin,
  ]
}

pub fn presence_inbox_round_trips_every_crossing_input_test() {
  let all = inputs(8_100_000_000_000_003)
  list.each(all, fn(input) {
    let assert Ok(bytes) = codec.encode(presence_codec.inbox(), input, 1, 3)
    let assert Ok(#(header, decoded)) =
      codec.decode(presence_codec.inbox(), bytes)
    assert header.part == blob.Inbox
    assert decoded == input
  })
  assert list.all(all, presence_codec.crosses)
}

pub fn joined_since_a_stamp_arrives_as_a_full_catch_up_test() {
  let inbox = process.new_subject()
  let input =
    protocol.Joined(
      protocol.PresenceSubscriber(5, inbox),
      protocol.ChangedSince(ms: 10, stamp: 20),
    )
  let assert Ok(bytes) = codec.encode(presence_codec.inbox(), input, 1, 3)
  let assert Ok(#(_, decoded)) = codec.decode(presence_codec.inbox(), bytes)
  assert decoded
    == protocol.Joined(
      protocol.PresenceSubscriber(5, inbox),
      protocol.Everything,
    )
}

pub fn local_only_inputs_never_cross_test() {
  let local: List(Input(Nil)) = [
    protocol.ExpiryTick,
    protocol.IdleCheck,
    protocol.ShedTick,
    protocol.GraceExpired(1),
    protocol.SessionDown(Nil, protocol.Closed),
    protocol.MoveRetry(None),
  ]
  list.each(local, fn(input) {
    assert !presence_codec.crosses(input)
    assert result.is_error(codec.encode(presence_codec.inbox(), input, 1, 1))
  })
}

pub fn subject_from_a_node_this_vm_never_saw_decodes_test() {
  let node = "presence_codec_unseen_7341@nowhere"
  assert atom.get(node) == Error(Nil)
  let size = bit_array.byte_size(<<node:utf8>>)
  let term = <<
    131, 104, 3, 119, 7, "subject":utf8, 88, 119, size, node:utf8, 0, 0, 0, 5, 0,
    0, 0, 0, 0, 0, 0, 1, 119, 2, "ok":utf8,
  >>
  let assert Ok(decoded) =
    terms.to_subject(wire_v1.SubjectV1(node:, term:))
    |> result.map(fn(subject: process.Subject(Nil)) { subject })
  let assert Ok(pid) = process.subject_owner(decoded)
  assert atom.to_string(pid_node(pid)) == node
  assert terms.subject(decoded) == wire_v1.SubjectV1(node:, term:)
}

@external(erlang, "erlang", "node")
fn pid_node(pid: process.Pid) -> atom.Atom

pub fn garbage_bodies_fail_without_crashing_test() {
  assert terms.decode(<<1, 2, 3>>, terms.subject_wire()) |> result.is_error
  assert presence_codec.decode_target(<<131, 1>>) == Error(Nil)
  let assert Ok(bytes) =
    blob.wrap(
      ns: 2,
      codec_version: 1,
      part: blob.Tail,
      flags: 0,
      epoch: 1,
      body: <<131, 100, 0, 1, 120>>,
    )
  let assert Error(codec.Malformed(_)) =
    codec.decode(presence_codec.tail(), bytes)
  let assert Ok(subject_bytes) =
    blob.wrap(
      ns: 2,
      codec_version: 1,
      part: blob.Inbox,
      flags: 0,
      epoch: 1,
      body: terms.encode(#(1, 2)),
    )
  let assert Error(codec.Malformed(_)) =
    codec.decode(presence_codec.inbox(), subject_bytes)
}

pub fn wrong_version_is_refused_test() {
  let request = full_request(8_100_000_000_000_005)
  assert codec.encode(presence_codec.tail(), request, 2, 1)
    == Error(codec.UnwritableVersion(2))
}

pub fn target_ref_round_trips_test() {
  let target = process.new_subject()
  assert presence_codec.decode_target(presence_codec.encode_target(target))
    == Ok(target)
}
