// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/role
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/native
import fluxer_gateway/perms/role_list
import fluxer_gateway/perms/strip
import gleam/option.{None, Some}

pub fn role_keeps_the_permission_fields_test() {
  assert strip.of_role(fixture.make_role(5, 1024))
    == native.Role(
      id: 5,
      permissions: 1024,
      position: role.Position(0),
      hoist: role.NotHoisted,
    )
}

pub fn channel_maps_kinds_targets_and_parent_test() {
  let source =
    fixture.make_channel(11, channel.PrivateThread, Some(10), [
      fixture.make_overwrite(channel.RoleTarget(fixture.role_id(1)), 0, 1024),
      fixture.make_overwrite(channel.MemberTarget(fixture.user_id(2)), 1024, 0),
    ])
  assert strip.of_channel(source)
    == native.Channel(
      id: 11,
      kind: native.ThreadKind(12),
      parent: Some(10),
      overwrites: [
        native.Overwrite(native.RoleTarget(1), 0, 1024),
        native.Overwrite(native.MemberTarget(2), 1024, 0),
      ],
      nsfw: channel.InheritNsfw,
      named: True,
    )
}

pub fn direct_messages_become_other_kinds_test() {
  assert strip.of_kind(channel.Dm) == native.OtherKind(1)
  assert strip.of_kind(channel.GroupDm) == native.OtherKind(3)
  assert strip.of_kind(channel.Category) == native.Category
  assert strip.of_kind(channel.OtherKind(99)) == native.OtherKind(99)
}

pub fn member_role_list_keeps_member_order_test() {
  let source = fixture.make_member(9, None, "{}")
  assert role_list.of_member(source) == role_list.Roles([7, 3])
}
