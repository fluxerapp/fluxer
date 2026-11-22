// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{
  type DispatchFacts, Channel, Guild, MemberRemove, Message, NotObject, Plain,
  Presence, Reaction, Recipient, Relationship, Scoped, Unmarked, Unscoped,
  UserSettings, UserUpdate,
}
import fluxer_gateway/core/raw_json
import fluxer_gateway/session/payload
import gleam/bit_array
import gleam/option.{None, Some}
import gleam/set

fn scan(event: String, text: String) -> DispatchFacts {
  dispatch_facts.scan(event, raw_json.wrap(bit_array.from_string(text)))
}

fn bits(raw: raw_json.RawJson) -> BitArray {
  raw_json.to_bits(raw)
}

pub fn top_level_ids_accept_strings_and_numbers_test() {
  let facts = scan("TYPING_START", "{\"guild_id\":\"41\",\"id\":42}")
  assert facts.guild_id == Some(41)
  assert facts.id == Some(42)
  assert facts.detail == Plain
}

pub fn invalid_ids_are_absent_test() {
  assert scan("X", "{\"id\":\"0\"}").id == None
  assert scan("X", "{\"id\":-5}").id == None
  assert scan("X", "{\"id\":\"012\"}").id == None
  assert scan("X", "{\"id\":1.5}").id == None
  assert scan("X", "{\"id\":null}").id == None
  assert scan("X", "{\"id\":\"12345678901234567890\"}").id == None
  assert scan("X", "{\"id\":\"9223372036854775807\"}").id
    == Some(9_223_372_036_854_775_807)
}

pub fn escaped_id_strings_are_decoded_test() {
  assert scan("X", "{\"id\":\"\\u0031\\u0032\"}").id == Some(12)
}

pub fn thread_markers_are_top_level_keys_only_test() {
  assert scan("X", "{\"__thread_scoped\":true,\"id\":\"1\"}").thread == Scoped
  assert scan("X", "{\"__thread_unscoped\":1}").thread == Unscoped
  assert scan("X", "{\"__thread_unscoped\":1,\"__thread_scoped\":1}").thread
    == Scoped
  assert scan(
      "X",
      "{\"content\":\"__thread_scoped\",\"a\":{\"__thread_scoped\":1}}",
    ).thread
    == Unmarked
}

pub fn non_objects_have_no_facts_test() {
  let facts = scan("MESSAGE_CREATE", "not json")
  assert facts.detail == NotObject
  assert facts.id == None
  assert scan("MESSAGE_CREATE", "[1,2]").detail == NotObject
}

pub fn mentions_keep_object_ids_only_test() {
  let facts =
    scan(
      "MESSAGE_CREATE",
      "{\"mention_everyone\":true,\"mentions\":[{\"id\":\"5\"},\"6\",{\"name\":\"x\"},{\"id\":7}]}",
    )
  assert facts.detail == Message(everyone: True, here: False, mentions: [5, 7])
  assert scan("MESSAGE_CREATE", "{\"mentions\":null,\"mention_here\":1}").detail
    == Message(everyone: False, here: False, mentions: [])
}

pub fn reactions_carry_ints_and_input_slices_test() {
  let facts =
    scan(
      "MESSAGE_REACTION_ADD",
      "{\"channel_id\":\"10\",\"message_id\":\"11\",\"user_id\":\"12\",\"guild_id\":null,\"emoji\":{\"name\":\"a\",\"id\":\"13\"},\"member\":{\"nick\":null}}",
    )
  let assert Reaction(reaction) = facts.detail
  assert reaction.guild == None
  assert reaction.channel == Some(10)
  assert reaction.message == Some(11)
  assert reaction.user == Some(12)
  assert reaction.emoji_id == Some(13)
  assert reaction.emoji_name == Some("a")
  assert reaction.guild_raw == None
  assert option.map(reaction.channel_raw, bits) == Some(<<"\"10\"">>)
  assert option.map(reaction.emoji_raw, bits)
    == Some(<<"{\"name\":\"a\",\"id\":\"13\"}">>)
  assert option.map(reaction.member_raw, bits) == Some(<<"{\"nick\":null}">>)
}

pub fn reactions_without_emoji_object_test() {
  let assert Reaction(reaction) =
    scan("MESSAGE_REACTION_REMOVE", "{\"emoji\":null,\"user_id\":\"3\"}").detail
  assert reaction.emoji_id == None
  assert reaction.emoji_name == None
  assert reaction.emoji_raw == None
  assert reaction.member_raw == None
}

pub fn channel_recipients_merge_ids_and_users_test() {
  let facts =
    scan(
      "CHANNEL_CREATE",
      "{\"id\":\"9\",\"type\":3,\"recipient_ids\":[\"1\",\"x\"],\"recipients\":[{\"id\":\"7\"},\"8\",{\"username\":\"u\"}]}",
    )
  assert facts.id == Some(9)
  assert facts.detail
    == Channel(kind: Some(3), recipients: Some(set.from_list([1, 7, 8])))
  assert scan("CHANNEL_UPDATE", "{\"id\":\"9\",\"type\":\"1\"}").detail
    == Channel(kind: None, recipients: None)
}

pub fn recipients_and_relationships_test() {
  assert scan(
      "CHANNEL_RECIPIENT_ADD",
      "{\"channel_id\":\"9\",\"user\":{\"id\":\"8\"}}",
    ).detail
    == Recipient(channel: Some(9), user: Some(8))
  let relationship = scan("RELATIONSHIP_ADD", "{\"id\":\"5\",\"type\":1}")
  assert relationship.id == Some(5)
  assert relationship.detail == Relationship(kind: Some(1))
}

pub fn guild_create_members_are_read_lazily_test() {
  let facts =
    scan(
      "GUILD_CREATE",
      "{\"id\":\"40\",\"unavailable\":false,\"members\":[{\"user\":{\"id\":\"7\"}},{\"user\":{\"id\":\"8\"}},{\"user\":null}]}",
    )
  let assert Guild(unavailable: False, members: Some(members)) = facts.detail
  assert dispatch_facts.member_ids(members) == [7, 8]
  let assert Guild(members: Some(mixed), ..) =
    scan(
      "GUILD_CREATE",
      "{\"id\":\"40\",\"members\":[{\"user\":{\"id\":\"7\"}},1]}",
    ).detail
  assert dispatch_facts.member_ids(mixed) == []
  assert scan("GUILD_DELETE", "{\"id\":\"40\",\"unavailable\":true}").detail
    == Guild(unavailable: True, members: None)
  assert scan(
      "GUILD_MEMBER_REMOVE",
      "{\"guild_id\":\"40\",\"user\":{\"id\":\"7\"}}",
    ).detail
    == MemberRemove(user: Some(7))
}

pub fn presence_content_ignores_key_order_and_guild_test() {
  let a =
    scan(
      "PRESENCE_UPDATE",
      "{\"guild_id\":\"40\",\"user\":{\"id\":\"7\"},\"status\":\"idle\",\"afk\":false}",
    )
  let b =
    scan(
      "PRESENCE_UPDATE",
      "{\"afk\":false , \"status\":\"idle\",\"user\":{\"id\":\"7\"}}",
    )
  let c =
    scan("PRESENCE_UPDATE", "{\"user\":{\"id\":\"7\"},\"status\":\"offline\"}")
  let assert Presence(user: Some(7), online: True, content: first) = a.detail
  let assert Presence(user: Some(7), online: True, content: second) = b.detail
  let assert Presence(user: Some(7), online: False, content: third) = c.detail
  assert first == second
  assert first != third
  assert a.guild_id == Some(40)
}

pub fn user_events_test() {
  assert scan("USER_SETTINGS_UPDATE", "{\"status\":\"invisible\"}").detail
    == UserSettings(status: Some("invisible"))
  assert scan("USER_SETTINGS_UPDATE", "{\"status\":5}").detail
    == UserSettings(status: None)
  assert scan("USER_UPDATE", "{}").detail == UserUpdate(restricted: None)
  assert scan("USER_UPDATE", "{\"profile_restricted\":true}").detail
    == UserUpdate(restricted: Some(True))
  assert scan("USER_UPDATE", "{\"profile_restricted\":\"yes\"}").detail
    == UserUpdate(restricted: Some(False))
}

pub fn mark_available_splices_in_place_test() {
  let mark = fn(text) {
    payload.mark_available(raw_json.wrap(bit_array.from_string(text)))
    |> raw_json.to_bits
  }
  assert mark("{\"id\":\"1\",\"unavailable\":true,\"name\":\"g\"}")
    == <<"{\"id\":\"1\",\"unavailable\":false,\"name\":\"g\"}">>
  assert mark("{\"id\":\"1\"}") == <<"{\"id\":\"1\",\"unavailable\":false}">>
  assert mark("{}") == <<"{\"unavailable\":false}">>
  assert mark("[]") == <<"[]">>
}
