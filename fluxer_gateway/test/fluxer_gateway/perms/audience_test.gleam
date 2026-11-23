// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/permissions
import fluxer_gateway/perms/audience
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/native
import fluxer_gateway/perms/query
import fluxer_gateway/perms/view
import gleam/option.{None, Some}

pub fn view_set_bits_count_from_the_first_byte_test() {
  let set = view.ViewSet(epoch: 3, bits: <<0b1010_0000, 0b0000_0001>>)
  assert view.contains(set, 0)
  assert !view.contains(set, 1)
  assert view.contains(set, 2)
  assert view.contains(set, 15)
  assert !view.contains(set, 16)
  assert !view.contains(set, -1)
}

pub fn audience_tests_role_sets_and_exceptions_test() {
  let found =
    audience.from_native(native.Audience(
      epoch: 4,
      role_sets: <<0b0100_0000>>,
      users: packed_ids.from_list([3, 8, 12]),
    ))
  let role_set = fn(value) {
    audience.group_from_native(native.RoleSetGroup(value))
  }
  let user = fn(value) { audience.group_from_native(native.UserGroup(value)) }
  assert audience.epoch(found) == 4
  assert audience.contains(found, role_set(1))
  assert !audience.contains(found, role_set(0))
  assert audience.contains(found, user(8))
  assert !audience.contains(found, user(9))
  assert audience.is_exception(user(8))
  assert audience.user(user(8)) == Some(fixture.user_id(8))
  assert audience.user(role_set(1)) == None
}

pub fn needs_carry_plain_values_test() {
  let assert Ok(required) = permissions.from_int(16)
  assert audience.to_native_need(audience.ChannelBits(required))
    == native.ChannelBits(16)
  assert audience.to_native_need(audience.ViewMembers) == native.ViewMembers
}

pub fn query_replies_convert_online_counts_test() {
  assert query.from_native(native.MutualCount(5))
    == Ok(query.MutualOnlineReply(query.Mutual(5)))
  assert query.from_native(native.PublicCount(5))
    == Ok(query.PublicOnlineReply(query.Restricted(5)))
}

pub fn reply_queries_use_guild_scope_without_channel_test() {
  assert query.reply_query_to_native(query.UserPermissions(
      fixture.user_id(1),
      None,
    ))
    == native.UserPermissions(1, native.GuildScope)
  assert query.reply_query_to_native(query.ViewableChannels(fixture.user_id(1)))
    == native.ViewableChannels(1)
  assert query.scope_to_native(Some(fixture.channel_id(6)))
    == native.ChannelScope(6)
}
