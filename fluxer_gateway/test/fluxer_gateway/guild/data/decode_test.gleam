// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/role
import fluxer_gateway/core/user
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/nif
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

const member_one =
  "{\"user\":{\"username\":\"a\",\"id\":\"2\",\"global_name\":null,\"is_staff\":true},\"roles\":[\"100\",\"x\"],\"nick\":\"n\",\"joined_at\":\"2026-01-01T00:00:00.000Z\"}"

const member_two =
  "{\"user\":{\"id\":\"3\",\"bot\":true,\"flags\":\"4\"},\"nick\":null}"

fn load(members: data_decode.Collection) {
  let assert Ok(Nil) = nif.load()
  let assert Ok(guild) = id.guild_id(100)
  let collected =
    dict.from_list([
      #(
        "guild",
        data_decode.Whole(<<
          "{\"id\":\"100\",\"owner_id\":\"2\",\"nsfw_level\":\"3\",\"features\":[\"A\",1]}",
        >>),
      ),
      #(
        "roles",
        data_decode.Whole(<<
          "[{\"id\":\"100\",\"permissions\":\"8\"},{\"id\":0}]",
        >>),
      ),
      #("channels", data_decode.Items([<<"{\"id\":\"5\",\"type\":0}">>])),
      #(
        "emojis",
        data_decode.Items([<<"{\"id\":\"7\"}">>, <<"{\"id\":\"8\"}">>]),
      ),
      #("stickers", data_decode.Whole(<<"{}">>)),
      #("members", members),
    ])
  let assert Ok(found) = data_decode.collections(guild, collected)
  found
}

pub fn members_keep_their_bytes_test() {
  let found =
    load(
      data_decode.Items([<<member_one:utf8>>, <<member_two:utf8>>, <<"[]">>]),
    )
  let assert [first, second] = found.members
  assert raw_json.to_bits(member.wire(first)) == <<member_one:utf8>>
  assert raw_json.to_bits(user.wire(member.user(first)))
    == <<
      "{\"username\":\"a\",\"id\":\"2\",\"global_name\":null,\"is_staff\":true}",
    >>
  assert user.is_staff(member.user(first))
  assert user.name(member.user(first)) == Some("a")
  assert member.nick(first) == Some("n")
  assert list.map(member.roles(first), id.role_id_to_int) == [100]
  assert member.joined_at_ms(first) == Some(1_767_225_600_000)
  assert user.is_bot(member.user(second))
  assert user.flags(member.user(second)) == 4
  assert member.nick(second) == None
}

pub fn members_from_one_array_test() {
  let found =
    load(
      data_decode.Whole(<<
        "[":utf8,
        member_one:utf8,
        ",":utf8,
        member_two:utf8,
        "]":utf8,
      >>),
    )
  assert list.length(found.members) == 2
  assert core_guild.member_count(found.data.facts) == 2
}

pub fn guild_facts_and_lists_test() {
  let found = load(data_decode.Items([]))
  let data = found.data
  assert option.map(core_guild.owner(data.facts), id.user_id_to_int) == Some(2)
  assert core_guild.features(data.facts) == ["A"]
  assert list.map(data.roles, fn(found) { id.role_id_to_int(role.id(found)) })
    == [100]
  assert option.map(
      dict.get(data.properties, "nsfw_level") |> option.from_result,
      raw_json.to_bits,
    )
    == Some(<<"\"3\"">>)
  assert raw_json.to_bits(data.emojis) == <<"[{\"id\":\"7\"},{\"id\":\"8\"}]">>
  assert raw_json.to_bits(data.stickers) == <<"[]">>
  assert list.length(data.channels) == 1
}
