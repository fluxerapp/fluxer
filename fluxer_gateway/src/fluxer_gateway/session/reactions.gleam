// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/dispatch_facts.{type ReactionFacts}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn interval_ms() -> Int {
  durations.session().reactions_interval_ms
}

const max_buffered = 512

type ReactionKey {
  ReactionKey(guild: Option(Int), channel: Option(Int), message: Option(Int))
}

pub opaque type Reaction {
  Reaction(key: ReactionKey, facts: ReactionFacts, payload: RawJson)
}

pub opaque type Reactions {
  Reactions(held: List(Reaction), count: Int)
}

pub type Buffered {
  Buffered(reactions: Reactions, arm_timer: Bool)
}

pub fn new() -> Reactions {
  Reactions(held: [], count: 0)
}

pub fn is_empty(reactions: Reactions) -> Bool {
  reactions.count == 0
}

pub fn hold(
  reactions: Reactions,
  facts: ReactionFacts,
  raw: RawJson,
) -> Buffered {
  let kept = case reactions.count >= max_buffered {
    True -> drop_oldest(reactions)
    False -> reactions
  }
  Buffered(
    reactions: Reactions(
      held: [reaction(facts, raw), ..kept.held],
      count: kept.count + 1,
    ),
    arm_timer: reactions.count == 0,
  )
}

fn reaction(facts: ReactionFacts, raw: RawJson) -> Reaction {
  Reaction(
    key: ReactionKey(
      guild: facts.guild,
      channel: facts.channel,
      message: facts.message,
    ),
    facts:,
    payload: raw,
  )
}

fn drop_oldest(reactions: Reactions) -> Reactions {
  let held =
    reactions.held
    |> list.reverse
    |> list.drop(reactions.count - max_buffered + 1)
    |> list.reverse
  Reactions(held:, count: list.length(held))
}

pub fn cancel(
  reactions: Reactions,
  removal: ReactionFacts,
) -> Result(Reactions, Nil) {
  case reactions.count {
    0 -> Error(Nil)
    _ -> remove_first(list.reverse(reactions.held), removal, [])
  }
}

fn remove_first(
  oldest_first: List(Reaction),
  removal: ReactionFacts,
  seen: List(Reaction),
) -> Result(Reactions, Nil) {
  case oldest_first {
    [] -> Error(Nil)
    [entry, ..rest] ->
      case matches(entry, removal) {
        True -> {
          let remaining = list.append(list.reverse(seen), rest)
          Ok(Reactions(
            held: list.reverse(remaining),
            count: list.length(remaining),
          ))
        }
        False -> remove_first(rest, removal, [entry, ..seen])
      }
  }
}

fn matches(entry: Reaction, removal: ReactionFacts) -> Bool {
  entry.facts.message == removal.message
  && entry.facts.user == removal.user
  && entry.facts.emoji_id == removal.emoji_id
  && entry.facts.emoji_name == removal.emoji_name
}

pub fn flush(reactions: Reactions) -> #(Reactions, List(#(String, RawJson))) {
  let frames =
    reactions.held
    |> list.reverse
    |> group
    |> list.map(group_frame)
  #(new(), frames)
}

fn group(oldest_first: List(Reaction)) -> List(List(Reaction)) {
  let #(keys, groups) =
    list.fold(oldest_first, #([], []), fn(acc, entry) {
      let #(keys, groups) = acc
      case list.key_find(groups, entry.key) {
        Ok(members) -> #(
          keys,
          list.key_set(groups, entry.key, [entry, ..members]),
        )
        Error(Nil) -> #([entry.key, ..keys], [#(entry.key, [entry]), ..groups])
      }
    })
  keys
  |> list.reverse
  |> list.filter_map(fn(key) {
    list.key_find(groups, key) |> result.map(list.reverse)
  })
}

fn group_frame(entries: List(Reaction)) -> #(String, RawJson) {
  case entries {
    [single] -> #("MESSAGE_REACTION_ADD", single.payload)
    [first, ..] -> #("MESSAGE_REACTION_ADD_MANY", many_payload(first, entries))
    [] -> #("MESSAGE_REACTION_ADD_MANY", raw_json.null())
  }
}

fn many_payload(first: Reaction, entries: List(Reaction)) -> RawJson {
  let base = [
    #("channel_id", raw_or_null(first.facts.channel_raw)),
    #("message_id", raw_or_null(first.facts.message_raw)),
    #("reactions", json.preprocessed_array(list.map(entries, reaction_json))),
  ]
  let fields = case first.facts.guild_raw {
    Some(guild) -> list.append(base, [#("guild_id", raw_json.to_json(guild))])
    None -> base
  }
  raw_json.from_json(json.object(fields))
}

fn reaction_json(entry: Reaction) -> json.Json {
  let base = [
    #("user_id", raw_or_null(entry.facts.user_raw)),
    #("emoji", case entry.facts.emoji_raw {
      Some(emoji) -> raw_json.to_json(emoji)
      None -> json.object([])
    }),
  ]
  case entry.facts.member_raw {
    Some(member) ->
      json.object(list.append(base, [#("member", raw_json.to_json(member))]))
    None -> json.object(base)
  }
}

fn raw_or_null(value: Option(RawJson)) -> json.Json {
  case value {
    Some(raw) -> raw_json.to_json(raw)
    None -> json.null()
  }
}
