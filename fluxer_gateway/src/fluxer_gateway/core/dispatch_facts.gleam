// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/crypto
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type DispatchFacts {
  DispatchFacts(
    guild_id: Option(Int),
    id: Option(Int),
    thread: ThreadMarker,
    detail: Detail,
  )
}

pub type ThreadMarker {
  Unmarked
  Scoped
  Unscoped
}

pub type Detail {
  NotObject
  Plain
  Guild(unavailable: Bool, members: Option(RawJson))
  MemberRemove(user: Option(Int))
  Presence(user: Option(Int), online: Bool, content: BitArray)
  Message(everyone: Bool, here: Bool, mentions: List(Int))
  Reaction(ReactionFacts)
  Channel(kind: Option(Int), recipients: Option(Set(Int)))
  Recipient(channel: Option(Int), user: Option(Int))
  Relationship(kind: Option(Int))
  UserSettings(status: Option(String))
  UserUpdate(restricted: Option(Bool))
}

pub type ReactionFacts {
  ReactionFacts(
    guild: Option(Int),
    channel: Option(Int),
    message: Option(Int),
    user: Option(Int),
    emoji_id: Option(Int),
    emoji_name: Option(String),
    guild_raw: Option(RawJson),
    channel_raw: Option(RawJson),
    message_raw: Option(RawJson),
    user_raw: Option(RawJson),
    emoji_raw: Option(RawJson),
    member_raw: Option(RawJson),
  )
}

pub const scoped_marker = "__thread_scoped"

pub const unscoped_marker = "__thread_unscoped"

pub fn scan(event: String, payload: RawJson) -> DispatchFacts {
  case raw_json.fields(payload) {
    Error(Nil) -> unreadable()
    Ok(pairs) -> {
      let fields = dict.from_list(pairs)
      case detail(event, payload, fields) {
        NotObject -> unreadable()
        found ->
          DispatchFacts(
            guild_id: id_field(fields, "guild_id"),
            id: id_field(fields, "id"),
            thread: thread_marker(fields),
            detail: found,
          )
      }
    }
  }
}

pub fn plain() -> DispatchFacts {
  DispatchFacts(guild_id: None, id: None, thread: Unmarked, detail: Plain)
}

fn unreadable() -> DispatchFacts {
  DispatchFacts(guild_id: None, id: None, thread: Unmarked, detail: NotObject)
}

fn thread_marker(fields: Dict(String, RawJson)) -> ThreadMarker {
  case
    dict.has_key(fields, scoped_marker),
    dict.has_key(fields, unscoped_marker)
  {
    True, _ -> Scoped
    False, True -> Unscoped
    False, False -> Unmarked
  }
}

fn detail(
  event: String,
  payload: RawJson,
  fields: Dict(String, RawJson),
) -> Detail {
  case event {
    "GUILD_CREATE" ->
      Guild(
        unavailable: bool_field(fields, "unavailable"),
        members: present(fields, "members"),
      )
    "GUILD_DELETE" ->
      Guild(unavailable: bool_field(fields, "unavailable"), members: None)
    "GUILD_MEMBER_REMOVE" -> MemberRemove(user: nested_id(fields, "user"))
    "PRESENCE_UPDATE" -> presence(payload, fields)
    "MESSAGE_CREATE" ->
      Message(
        everyone: bool_field(fields, "mention_everyone"),
        here: bool_field(fields, "mention_here"),
        mentions: mentions(fields),
      )
    "MESSAGE_REACTION_ADD" | "MESSAGE_REACTION_REMOVE" ->
      Reaction(reaction(fields))
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      Channel(kind: int_field(fields, "type"), recipients: recipients(fields))
    "CHANNEL_RECIPIENT_ADD" | "CHANNEL_RECIPIENT_REMOVE" ->
      Recipient(
        channel: id_field(fields, "channel_id"),
        user: nested_id(fields, "user"),
      )
    "RELATIONSHIP_ADD" | "RELATIONSHIP_UPDATE" | "RELATIONSHIP_REMOVE" ->
      Relationship(kind: int_field(fields, "type"))
    "USER_SETTINGS_UPDATE" ->
      UserSettings(status: string_field(fields, "status"))
    "USER_UPDATE" -> UserUpdate(restricted: restricted(fields))
    _ -> Plain
  }
}

pub fn member_ids(members: RawJson) -> List(Int) {
  case raw_json.items(members) {
    Error(Nil) -> []
    Ok(items) ->
      case list.all(items, is_object) {
        False -> []
        True ->
          list.filter_map(items, fn(item) {
            raw_json.field(item, "user")
            |> result.try(raw_json.field(_, "id"))
            |> result.try(raw_json.snowflake)
          })
      }
  }
}

type Content {
  Absent
  Flag(Bool)
  Whole(Int)
  Fraction(Float)
  Text(String)
  Snowflake(Int)
  Items(List(Content))
  Members(List(#(String, Content)))
}

fn presence(payload: RawJson, fields: Dict(String, RawJson)) -> Detail {
  case json_tree.deep(json_tree.from_raw(payload)) {
    json_tree.Object(pairs) ->
      Presence(
        user: nested_id(fields, "user"),
        online: case string_field(fields, "status") {
          Some("online") | Some("idle") | Some("dnd") -> True
          _ -> False
        },
        content: pairs
          |> list.filter(fn(pair) { pair.0 != "guild_id" })
          |> members
          |> fingerprint,
      )
    _ -> NotObject
  }
}

fn fingerprint(content: Content) -> BitArray {
  crypto.hash(crypto.Sha256, term_to_binary(content, []))
}

fn content(tree: json_tree.JsonTree) -> Content {
  case tree {
    json_tree.Bool(flag) -> Flag(flag)
    json_tree.Int(number) -> Whole(number)
    json_tree.Float(number) -> Fraction(number)
    json_tree.String(text) -> Text(text)
    json_tree.Array(items) -> Items(list.map(items, content))
    json_tree.Object(pairs) -> members(pairs)
    json_tree.Null | json_tree.Raw(_) -> Absent
  }
}

fn members(pairs: List(#(String, json_tree.JsonTree))) -> Content {
  pairs
  |> list.map(fn(pair) { #(pair.0, identified(pair.0, content(pair.1))) })
  |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
  |> Members
}

fn identified(key: String, value: Content) -> Content {
  case value {
    Items(items) ->
      case string.ends_with(key, "_ids") {
        True -> Items(list.map(items, as_snowflake))
        False -> value
      }
    _ ->
      case key == "id" || string.ends_with(key, "_id") {
        True -> as_snowflake(value)
        False -> value
      }
  }
}

fn as_snowflake(value: Content) -> Content {
  let parsed = case value {
    Text(text) -> id.parse(text)
    Whole(number) -> id.check(number) |> result.replace_error(Nil)
    _ -> Error(Nil)
  }
  case parsed {
    Ok(number) -> Snowflake(number)
    Error(Nil) -> value
  }
}

fn mentions(fields: Dict(String, RawJson)) -> List(Int) {
  case dict.get(fields, "mentions") |> result.try(raw_json.items) {
    Ok(items) ->
      list.filter_map(items, fn(item) {
        raw_json.field(item, "id") |> result.try(raw_json.snowflake)
      })
    Error(Nil) -> []
  }
}

fn reaction(fields: Dict(String, RawJson)) -> ReactionFacts {
  let emoji = dict.get(fields, "emoji")
  ReactionFacts(
    guild: id_field(fields, "guild_id"),
    channel: id_field(fields, "channel_id"),
    message: id_field(fields, "message_id"),
    user: id_field(fields, "user_id"),
    emoji_id: emoji
      |> result.try(raw_json.field(_, "id"))
      |> result.try(raw_json.snowflake)
      |> option.from_result,
    emoji_name: emoji
      |> result.try(raw_json.field(_, "name"))
      |> result.try(raw_json.string)
      |> option.from_result,
    guild_raw: present(fields, "guild_id"),
    channel_raw: present(fields, "channel_id"),
    message_raw: present(fields, "message_id"),
    user_raw: present(fields, "user_id"),
    emoji_raw: present(fields, "emoji"),
    member_raw: present(fields, "member"),
  )
}

fn recipients(fields: Dict(String, RawJson)) -> Option(Set(Int)) {
  let listed =
    dict.get(fields, "recipient_ids")
    |> result.try(raw_json.items)
    |> result.map(list.filter_map(_, raw_json.snowflake))
    |> result.unwrap([])
  let users =
    dict.get(fields, "recipients")
    |> result.try(raw_json.items)
    |> result.map(list.filter_map(_, recipient))
    |> result.unwrap([])
  case list.append(listed, users) {
    [] -> None
    ids -> Some(set.from_list(ids))
  }
}

fn recipient(item: RawJson) -> Result(Int, Nil) {
  case is_object(item) {
    True -> raw_json.field(item, "id") |> result.try(raw_json.snowflake)
    False -> raw_json.snowflake(item)
  }
}

fn restricted(fields: Dict(String, RawJson)) -> Option(Bool) {
  case dict.get(fields, "profile_restricted") {
    Error(Nil) -> None
    Ok(value) -> Some(raw_json.to_bits(value) == <<"true">>)
  }
}

fn present(fields: Dict(String, RawJson), key: String) -> Option(RawJson) {
  case dict.get(fields, key) {
    Ok(value) ->
      case raw_json.to_bits(value) {
        <<"null">> -> None
        _ -> Some(value)
      }
    Error(Nil) -> None
  }
}

fn id_field(fields: Dict(String, RawJson), key: String) -> Option(Int) {
  dict.get(fields, key) |> result.try(raw_json.snowflake) |> option.from_result
}

fn nested_id(fields: Dict(String, RawJson), key: String) -> Option(Int) {
  dict.get(fields, key)
  |> result.try(raw_json.field(_, "id"))
  |> result.try(raw_json.snowflake)
  |> option.from_result
}

fn bool_field(fields: Dict(String, RawJson), key: String) -> Bool {
  case dict.get(fields, key) {
    Ok(value) -> raw_json.to_bits(value) == <<"true">>
    Error(Nil) -> False
  }
}

fn int_field(fields: Dict(String, RawJson), key: String) -> Option(Int) {
  dict.get(fields, key)
  |> result.try(raw_json.read(_, decode.int))
  |> option.from_result
}

fn string_field(fields: Dict(String, RawJson), key: String) -> Option(String) {
  dict.get(fields, key) |> result.try(raw_json.string) |> option.from_result
}

fn is_object(value: RawJson) -> Bool {
  case raw_json.to_bits(value) {
    <<"{", _:bits>> -> True
    _ -> False
  }
}

@external(erlang, "erlang", "term_to_binary")
fn term_to_binary(term: a, options: List(Atom)) -> BitArray
