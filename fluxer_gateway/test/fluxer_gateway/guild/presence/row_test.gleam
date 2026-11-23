// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/member
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/user
import fluxer_gateway/guild/presence/row
import gleam/json
import gleam/option.{None, Some}

fn owner(restricted: Bool) -> member.Member {
  let assert Ok(user_id) = id.user_id(7)
  let wire = raw_json.from_json(json.object([#("id", json.string("7"))]))
  member.new(
    user: user.new(id: user_id, bot: False, flags: 0, restricted:, wire:),
    nick: None,
    roles: [],
    joined_at_ms: None,
    timed_out_until_ms: None,
    wire:,
  )
}

fn status(
  value: presence.Status,
  mobile: Bool,
  afk: Bool,
) -> presence.Presence {
  let assert Ok(user_id) = id.user_id(7)
  presence.new(
    user: user_id,
    status: value,
    mobile:,
    afk:,
    custom_status: Some(raw_json.from_json(json.object([]))),
    wire: raw_json.null(),
  )
}

pub fn invisible_shows_as_offline_without_custom_status_test() {
  let found =
    row.from_presence(status(presence.Invisible, False, False), owner(False))
  assert row.status(found) == presence.Offline
  assert row.display(found) == row.offline_display()
}

pub fn restricted_user_hides_custom_status_test() {
  let found =
    row.from_presence(status(presence.Online, False, False), owner(True))
  assert row.display(found).custom_status == None
  let open =
    row.from_presence(status(presence.Online, False, False), owner(False))
  assert row.display(open).custom_status == Some(<<"{}">>)
}

pub fn engagement_test() {
  assert row.is_engaged(row.from_presence(
    status(presence.Online, False, False),
    owner(False),
  ))
  assert row.is_engaged(row.from_presence(
    status(presence.Dnd, False, False),
    owner(False),
  ))
  assert !row.is_engaged(row.from_presence(
    status(presence.Idle, False, False),
    owner(False),
  ))
  assert !row.is_engaged(row.from_presence(
    status(presence.Online, True, False),
    owner(False),
  ))
  assert !row.is_engaged(row.from_presence(
    status(presence.Online, False, True),
    owner(False),
  ))
}

pub fn same_and_list_change_test() {
  let online =
    row.from_presence(status(presence.Online, False, False), owner(False))
  let afk =
    row.from_presence(status(presence.Online, False, True), owner(False))
  assert row.same(online, online)
  assert !row.same(online, afk)
  assert !row.list_relevant_change(Some(online), afk)
  assert row.list_relevant_change(None, afk)
}

pub fn update_payload_carries_guild_id_test() {
  let assert Ok(guild) = id.guild_id(3)
  let online =
    row.from_presence(status(presence.Online, False, False), owner(False))
  let encoded = raw_json.to_bits(row.update_payload(online, guild))
  assert encoded
    == <<
      "{\"guild_id\":\"3\",\"user\":{\"id\":\"7\"},\"status\":\"online\",\"mobile\":false,\"afk\":false,\"custom_status\":{}}",
    >>
}
