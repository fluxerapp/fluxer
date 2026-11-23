// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/packed_ids
import fluxer_gateway/session/control
import gleam/json
import gleam/list
import gleam/option.{None}
import support/guild_api
import support/guild_fixture.{MemberAllow, RoleDeny} as fixture
import support/guild_session as fake

fn mutual(found: fake.FakeSession) -> List(List(Int)) {
  fake.drain(found, 150)
  |> list.filter_map(fn(item) {
    case item {
      fake.Control(control.DmPartnerMutual(_, partners)) ->
        Ok(packed_ids.to_list(partners))
      _ -> Error(Nil)
    }
  })
}

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

pub fn dm_partners_follow_shared_channels_and_membership_test() {
  use api <- guild_api.with_api
  let base = fixture.new(1)
  let found =
    base
    |> fixture.with_everyone(0)
    |> fixture.with_channel(10, 0, None, [
      RoleDeny(base.id, fixture.view_channel),
      MemberAllow(2, fixture.view_channel),
      MemberAllow(3, fixture.view_channel),
    ])
    |> fixture.with_channel(20, 0, None, [
      RoleDeny(base.id, fixture.view_channel),
      MemberAllow(4, fixture.view_channel),
    ])
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
    |> fixture.with_member(4, [])
  let handle = fixture.started(api, found)
  let session = fake.start(2)
  assert fake.is_joined(fake.join(session, handle))
  let _ = fake.drain(session, 50)
  guild.send(handle.ref, guild.UpdateDmPartners(session.ref.id, [3, 4, 99]))
  assert mutual(session) == [[3]]
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, 20, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
      MemberAllow(4, fixture.view_channel),
      MemberAllow(2, fixture.view_channel),
    ]),
  )
  assert list.contains(mutual(session), [3, 4])
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("user", fixture.user(3)),
    ]),
  )
  assert mutual(session) == [[4]]
  fixture.stop(found)
}
