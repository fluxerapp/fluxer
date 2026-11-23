// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/member_list/config
import fluxer_gateway/member_list/event
import fluxer_gateway/member_list/item
import fluxer_gateway/member_list/native
import fluxer_gateway/member_list/snapshot
import fluxer_gateway/perms/fixture
import gleam/option.{None, Some}

const offline = "{\"afk\":false,\"mobile\":false,\"status\":\"offline\"}"

fn online(user: Int) -> presence.Presence {
  presence.new(
    user: fixture.user_id(user),
    status: presence.Online,
    mobile: False,
    afk: False,
    custom_status: None,
    wire: raw_json.wrap(<<"{\"status\":\"online\"}">>),
  )
}

pub fn display_name_prefers_nick_then_global_then_username_test() {
  let json = "{\"username\":\"name\",\"global_name\":\"Global\"}"
  assert item.display_name(fixture.make_member(1, Some("Nick"), json))
    == <<"Nick">>
  assert item.display_name(fixture.make_member(1, Some(""), json))
    == <<"Global">>
  assert item.display_name(fixture.make_member(
      1,
      None,
      "{\"username\":\"name\",\"global_name\":null}",
    ))
    == <<"name">>
  assert item.display_name(fixture.make_member(1, None, "{\"username\":42}"))
    == <<"42">>
  assert item.display_name(fixture.make_member(1, None, "{}")) == <<>>
}

pub fn list_item_carries_the_visible_presence_test() {
  let source = fixture.make_member(1, None, "{}")
  let #(list_bytes, page_bytes) = item.encode(source, Some(online(1)), True)
  assert page_bytes == <<"{\"nick\":null}">>
  assert list_bytes
    == <<
      "{\"member\":{\"nick\":null,\"presence\":{\"status\":\"online\"}}}",
    >>
  let #(hidden, _) = item.encode(source, Some(online(1)), False)
  assert hidden
    == <<
      "{\"member\":{\"nick\":null,\"presence\":":utf8,
      offline:utf8,
      "}}":utf8,
    >>
  let #(missing, _) = item.encode(source, None, True)
  assert missing == hidden
}

pub fn snapshot_keeps_input_order_test() {
  let built =
    snapshot.new()
    |> snapshot.add_member(fixture.make_member(1, Some("a"), "{}"))
    |> snapshot.add_member(fixture.make_member(2, Some("b"), "{}"))
    |> snapshot.add_presence(online(2))
    |> snapshot.add_connected(fixture.user_id(2))
    |> snapshot.set_updates(False)
  assert snapshot.to_native(built)
    == native.Snapshot(
      members: [#(1, <<"a">>), #(2, <<"b">>)],
      statuses: [#(2, presence.Online)],
      connected: [2],
      updates: native.UpdatesOff,
    )
}

pub fn events_use_plain_ids_and_session_keys_test() {
  let assert Ok(id) =
    session.session_id_from_string("0123456789abcdef0123456789ABCDEF")
  assert event.to_native(event.view_gained(id, fixture.channel_id(4)))
    == native.ViewGained(<<"0123456789abcdef0123456789ABCDEF">>, 4)
  assert event.to_native(event.presence(
      fixture.user_id(3),
      presence.Idle,
      event.Changed,
      event.Deferred,
    ))
    == native.Presence(3, presence.Idle, native.Changed, native.Deferred)
  assert event.to_native(event.disconnected(fixture.user_id(3)))
    == native.Connection(3, native.Disconnected)
}

pub fn config_derives_the_offline_drop_test() {
  let tuned = config.default() |> config.set_offline_threshold(0)
  assert config.offline_threshold(tuned) == 1
  let native_config = config.to_native(config.default())
  assert native_config.offline_drop == 1500
  assert native_config.work_budget_units == 250_000
}
