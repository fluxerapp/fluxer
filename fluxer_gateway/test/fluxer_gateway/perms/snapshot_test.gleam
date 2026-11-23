// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/role
import fluxer_gateway/perms/cutoff
import fluxer_gateway/perms/event
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/native
import fluxer_gateway/perms/role_list
import fluxer_gateway/perms/snapshot
import gleam/option.{None, Some}

pub fn snapshot_orders_its_events_test() {
  let built =
    snapshot.new(fixture.guild(100))
    |> snapshot.set_owner(Some(fixture.user_id(9)))
    |> snapshot.set_roles([fixture.make_role(100, 1024)])
    |> snapshot.add_channel(
      fixture.make_channel(10, channel.Category, None, []),
    )
    |> snapshot.add_channel(
      fixture.make_channel(11, channel.Text, Some(10), []),
    )
    |> snapshot.add_member(fixture.make_member(9, None, "{}"))
    |> snapshot.add_malformed_member(fixture.user_id(4))
    |> snapshot.add_virtual_access(fixture.user_id(5), [fixture.channel_id(11)])
    |> snapshot.set_cutoff(cutoff.At(1000))
    |> snapshot.set_nsfw(None, 3)
  let upsert = fn(id, kind, parent) {
    native.ChannelUpsert(
      native.Channel(id, kind, parent, [], channel.InheritNsfw, True),
      native.Created,
    )
  }
  assert snapshot.to_native(built)
    == native.Snapshot(guild: 100, events: [
      native.Roles([native.Role(100, 1024, role.Position(0), role.NotHoisted)]),
      native.Owner(Some(9)),
      upsert(10, native.Category, None),
      upsert(11, native.Text, Some(10)),
      native.ChannelOrder([10, 11]),
      native.MemberUpsert(9, role_list.Roles([7, 3])),
      native.MemberUpsert(4, role_list.Malformed),
      native.VirtualAccess(5, [11]),
      native.HistoryCutoff(cutoff.At(1000)),
      native.GuildNsfw(None, 3),
    ])
}

pub fn event_builders_use_plain_ids_test() {
  assert event.to_native(event.channel_delete(fixture.channel_id(3)))
    == native.ChannelDelete(3)
  assert event.to_native(event.owner(None)) == native.Owner(None)
  assert event.to_native(event.malformed_member(fixture.user_id(8)))
    == native.MemberUpsert(8, role_list.Malformed)
  assert event.to_native(
      event.channel_order([fixture.channel_id(2), fixture.channel_id(1)]),
    )
    == native.ChannelOrder([2, 1])
}
