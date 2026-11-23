// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{
  type DispatchFacts, Channel, Recipient, Relationship,
}
import fluxer_gateway/session/payload
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

const friend = 1

const incoming_request = 3

const direct_message = 1

const group_direct_message = 3

pub type DmChannel {
  DmChannel(kind: Int, recipients: Set(Int))
}

pub opaque type Relations {
  Relations(relationships: Dict(Int, Int), channels: Dict(Int, DmChannel))
}

pub fn new(
  relationships relationships: Dict(Int, Int),
  channels channels: Dict(Int, DmChannel),
) -> Relations {
  Relations(relationships:, channels:)
}

pub fn friends(relations: Relations) -> List(Int) {
  relations.relationships
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    case entry.1 == friend {
      True -> Ok(entry.0)
      False -> Error(Nil)
    }
  })
}

pub fn group_dm_recipients(
  relations: Relations,
  me: Int,
  eligible: Set(Int),
) -> Set(Int) {
  dict.fold(relations.channels, set.new(), fn(acc, _id, channel) {
    let others = set.delete(channel.recipients, me)
    case channel.kind {
      3 -> set.union(acc, others)
      1 -> set.union(acc, set.intersection(others, eligible))
      _ -> acc
    }
  })
}

pub fn me_context(relations: Relations, me: Int) -> Set(Int) {
  let requested =
    dict.fold(relations.relationships, set.new(), fn(acc, user, kind) {
      case kind == friend || kind == incoming_request {
        True -> set.insert(acc, user)
        False -> acc
      }
    })
  relations.channels
  |> dict.fold(requested, fn(acc, _id, channel) {
    case channel.kind == group_direct_message {
      True -> set.union(acc, channel.recipients)
      False -> acc
    }
  })
  |> set.delete(me)
}

pub fn presence_targets(
  relations: Relations,
  me: Int,
  eligible: Set(Int),
) -> Set(Int) {
  relations
  |> friends
  |> set.from_list
  |> set.union(group_dm_recipients(relations, me, eligible))
  |> set.delete(me)
}

pub fn allows_presence(
  relations: Relations,
  user: Int,
  me: Int,
  eligible: Set(Int),
) -> Bool {
  dict.get(relations.relationships, user) == Ok(friend)
  || set.contains(group_dm_recipients(relations, me, eligible), user)
}

pub fn private_channel_ids(relations: Relations) -> List(Int) {
  relations.channels
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    case { entry.1 }.kind {
      1 | 3 -> Ok(entry.0)
      _ -> Error(Nil)
    }
  })
}

pub fn direct_partners(relations: Relations, me: Int) -> List(Int) {
  relations.channels
  |> dict.values
  |> list.flat_map(fn(channel) {
    case channel.kind {
      1 -> set.to_list(set.delete(channel.recipients, me))
      _ -> []
    }
  })
  |> list.unique
  |> list.sort(int.compare)
}

pub type ChannelEvent {
  ChannelEvent(id: Option(Int), kind: Option(Int), recipients: Option(Set(Int)))
}

pub type Change {
  Change(relations: Relations, released: List(Int), dm_changed: Bool)
}

pub fn apply(
  relations: Relations,
  event: String,
  facts: DispatchFacts,
  me: Int,
) -> Change {
  case event, facts.detail {
    "CHANNEL_CREATE", Channel(kind:, recipients:) ->
      on_channel(relations, facts.id, kind, recipients, me, replace: True)
    "CHANNEL_UPDATE", Channel(kind:, recipients:) ->
      on_channel(relations, facts.id, kind, recipients, me, replace: False)
    "CHANNEL_DELETE", Channel(kind:, ..) ->
      on_channel_delete(relations, facts.id, kind)
    "CHANNEL_RECIPIENT_ADD", Recipient(channel:, user:) ->
      on_recipient(relations, channel, user, add: True)
    "CHANNEL_RECIPIENT_REMOVE", Recipient(channel:, user:) ->
      on_recipient(relations, channel, user, add: False)
    "RELATIONSHIP_ADD", Relationship(kind:)
    | "RELATIONSHIP_UPDATE", Relationship(kind:)
    -> on_relationship(relations, facts.id, kind)
    "RELATIONSHIP_REMOVE", Relationship(..) ->
      on_relationship_remove(relations, facts.id)
    _, _ -> Change(relations, [], False)
  }
}

pub fn changes_targets(event: String) -> Bool {
  case event {
    "RELATIONSHIP_ADD"
    | "RELATIONSHIP_UPDATE"
    | "RELATIONSHIP_REMOVE"
    | "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_DELETE"
    | "CHANNEL_RECIPIENT_ADD"
    | "CHANNEL_RECIPIENT_REMOVE"
    | "GUILD_DELETE" -> True
    _ -> False
  }
}

fn on_channel(
  relations: Relations,
  id: Option(Int),
  kind: Option(Int),
  recipients: Option(Set(Int)),
  me: Int,
  replace replace: Bool,
) -> Change {
  case id, kind {
    Some(id), Some(kind)
      if kind == direct_message || kind == group_direct_message
    -> {
      let existing = dict.get(relations.channels, id)
      let kept = case recipients, existing, replace {
        Some(ids), _, _ -> ids
        None, Ok(previous), False -> previous.recipients
        None, _, _ -> set.new()
      }
      let channels =
        dict.insert(relations.channels, id, DmChannel(kind:, recipients: kept))
      let released = case kind == group_direct_message {
        True -> set.to_list(set.delete(kept, me))
        False -> []
      }
      Change(
        Relations(..relations, channels:),
        released,
        kind == direct_message,
      )
    }
    _, _ -> Change(relations, [], False)
  }
}

fn on_channel_delete(
  relations: Relations,
  id: Option(Int),
  kind: Option(Int),
) -> Change {
  case id {
    Some(id) ->
      Change(
        Relations(..relations, channels: dict.delete(relations.channels, id)),
        [],
        kind == Some(direct_message),
      )
    None -> Change(relations, [], False)
  }
}

fn on_recipient(
  relations: Relations,
  channel: Option(Int),
  user: Option(Int),
  add add: Bool,
) -> Change {
  case channel, user {
    Some(channel_id), Some(user) ->
      case dict.get(relations.channels, channel_id) {
        Ok(DmChannel(kind: 3, recipients:)) -> {
          let recipients = case add {
            True -> set.insert(recipients, user)
            False -> set.delete(recipients, user)
          }
          let channels =
            dict.insert(
              relations.channels,
              channel_id,
              DmChannel(kind: group_direct_message, recipients:),
            )
          Change(Relations(..relations, channels:), released(add, user), False)
        }
        _ -> Change(relations, released(add, user), False)
      }
    None, Some(user) -> Change(relations, released(add, user), False)
    _, None -> Change(relations, [], False)
  }
}

fn released(released: Bool, user: Int) -> List(Int) {
  case released {
    True -> [user]
    False -> []
  }
}

fn on_relationship(
  relations: Relations,
  user: Option(Int),
  kind: Option(Int),
) -> Change {
  case user, kind {
    Some(user), Some(kind) -> {
      let relationships = dict.insert(relations.relationships, user, kind)
      Change(
        Relations(..relations, relationships:),
        released(kind == friend, user),
        False,
      )
    }
    _, _ -> Change(relations, [], False)
  }
}

fn on_relationship_remove(relations: Relations, user: Option(Int)) -> Change {
  case user {
    Some(user) ->
      Change(
        Relations(
          ..relations,
          relationships: dict.delete(relations.relationships, user),
        ),
        [],
        False,
      )
    None -> Change(relations, [], False)
  }
}

pub fn channel_decoder() -> Decoder(ChannelEvent) {
  use id <- decode.then(payload.id_field("id"))
  use kind <- decode.then(payload.int_field("type"))
  use recipient_ids <- decode.then(decode.optional_field(
    "recipient_ids",
    None,
    decode.one_of(decode.list(payload.id()) |> decode.map(Some), [
      decode.success(None),
    ]),
    decode.success,
  ))
  use recipients <- decode.then(decode.optional_field(
    "recipients",
    None,
    decode.one_of(
      decode.list(decode.one_of(payload.id_field("id"), [payload.id()]))
        |> decode.map(Some),
      [decode.success(None)],
    ),
    decode.success,
  ))
  decode.success(ChannelEvent(
    id:,
    kind:,
    recipients: merge(recipient_ids, recipients),
  ))
}

fn merge(
  a: Option(List(Option(Int))),
  b: Option(List(Option(Int))),
) -> Option(Set(Int)) {
  let ids =
    [a, b]
    |> option.values
    |> list.flatten
    |> option.values
  case ids {
    [] -> None
    _ -> Some(set.from_list(ids))
  }
}
