// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}

pub opaque type Store(value) {
  Store(
    primary: Dict(VoiceKey, value),
    by_connection: Dict(ConnectionId, VoiceKey),
    by_user: Dict(UserId, Set(VoiceKey)),
  )
}

pub fn new() -> Store(value) {
  Store(primary: dict.new(), by_connection: dict.new(), by_user: dict.new())
}

pub fn from_list(pairs: List(#(VoiceKey, value))) -> Store(value) {
  list.fold(pairs, new(), fn(acc, pair) { put(acc, pair.0, pair.1) })
}

pub fn put(store: Store(value), key: VoiceKey, value: value) -> Store(value) {
  let store = case dict.get(store.by_connection, voice_key.connection(key)) {
    Ok(held) if held != key -> unindex(store, held)
    _ -> store
  }
  Store(
    primary: dict.insert(store.primary, key, value),
    by_connection: dict.insert(
      store.by_connection,
      voice_key.connection(key),
      key,
    ),
    by_user: dict.upsert(store.by_user, voice_key.user(key), fn(found) {
      case found {
        Some(keys) -> set.insert(keys, key)
        None -> set.from_list([key])
      }
    }),
  )
}

pub fn rekey(
  store: Store(value),
  connection: ConnectionId,
  to channel: ChannelId,
  with update: fn(value) -> value,
) -> Result(#(VoiceKey, Store(value)), Nil) {
  use #(key, value) <- result.map(by_connection(store, connection))
  let moved = voice_key.move(key, channel)
  #(moved, put(unindex(store, key), moved, update(value)))
}

pub fn delete(store: Store(value), key: VoiceKey) -> Store(value) {
  case dict.has_key(store.primary, key) {
    True -> unindex(store, key)
    False -> store
  }
}

pub fn delete_connection(
  store: Store(value),
  connection: ConnectionId,
) -> Store(value) {
  case dict.get(store.by_connection, connection) {
    Ok(key) -> unindex(store, key)
    Error(Nil) -> store
  }
}

pub fn drop(store: Store(value), keys: List(VoiceKey)) -> Store(value) {
  list.fold(keys, store, delete)
}

fn unindex(store: Store(value), key: VoiceKey) -> Store(value) {
  let user = voice_key.user(key)
  let by_user = case dict.get(store.by_user, user) {
    Ok(keys) -> {
      let rest = set.delete(keys, key)
      case set.is_empty(rest) {
        True -> dict.delete(store.by_user, user)
        False -> dict.insert(store.by_user, user, rest)
      }
    }
    Error(Nil) -> store.by_user
  }
  let by_connection = case
    dict.get(store.by_connection, voice_key.connection(key))
  {
    Ok(held) if held == key ->
      dict.delete(store.by_connection, voice_key.connection(key))
    _ -> store.by_connection
  }
  Store(primary: dict.delete(store.primary, key), by_connection:, by_user:)
}

pub fn get(store: Store(value), key: VoiceKey) -> Result(value, Nil) {
  dict.get(store.primary, key)
}

pub fn has_key(store: Store(value), key: VoiceKey) -> Bool {
  dict.has_key(store.primary, key)
}

pub fn by_connection(
  store: Store(value),
  connection: ConnectionId,
) -> Result(#(VoiceKey, value), Nil) {
  use key <- result.try(dict.get(store.by_connection, connection))
  use value <- result.map(dict.get(store.primary, key))
  #(key, value)
}

pub fn has_connection(store: Store(value), connection: ConnectionId) -> Bool {
  dict.has_key(store.by_connection, connection)
}

pub fn key_of(
  store: Store(value),
  connection: ConnectionId,
) -> Result(VoiceKey, Nil) {
  dict.get(store.by_connection, connection)
}

pub fn keys_of_user(store: Store(value), user: UserId) -> List(VoiceKey) {
  case dict.get(store.by_user, user) {
    Ok(keys) -> set.to_list(keys)
    Error(Nil) -> []
  }
}

pub fn of_user(store: Store(value), user: UserId) -> List(#(VoiceKey, value)) {
  keys_of_user(store, user)
  |> list.filter_map(fn(key) {
    dict.get(store.primary, key) |> result.map(fn(value) { #(key, value) })
  })
}

pub fn has_user(store: Store(value), user: UserId) -> Bool {
  dict.has_key(store.by_user, user)
}

pub fn users(store: Store(value)) -> List(UserId) {
  dict.keys(store.by_user)
}

pub fn connections(store: Store(value)) -> List(ConnectionId) {
  dict.keys(store.by_connection)
}

pub fn keys(store: Store(value)) -> List(VoiceKey) {
  dict.keys(store.primary)
}

pub fn values(store: Store(value)) -> List(value) {
  dict.values(store.primary)
}

pub fn to_list(store: Store(value)) -> List(#(VoiceKey, value)) {
  dict.to_list(store.primary)
}

pub fn to_dict(store: Store(value)) -> Dict(VoiceKey, value) {
  store.primary
}

pub fn size(store: Store(value)) -> Int {
  dict.size(store.primary)
}

pub fn is_empty(store: Store(value)) -> Bool {
  dict.is_empty(store.primary)
}

pub fn in_channel(
  store: Store(value),
  channel: ChannelId,
) -> List(#(VoiceKey, value)) {
  dict.fold(store.primary, [], fn(acc, key, value) {
    case voice_key.channel(key) == channel {
      True -> [#(key, value), ..acc]
      False -> acc
    }
  })
}

pub fn filter(
  store: Store(value),
  keeping: fn(VoiceKey, value) -> Bool,
) -> Store(value) {
  dict.fold(store.primary, store, fn(acc, key, value) {
    case keeping(key, value) {
      True -> acc
      False -> unindex(acc, key)
    }
  })
}

pub fn partition(
  store: Store(value),
  keeping: fn(VoiceKey, value) -> Bool,
) -> #(Store(value), List(#(VoiceKey, value))) {
  dict.fold(store.primary, #(store, []), fn(acc, key, value) {
    case keeping(key, value) {
      True -> acc
      False -> #(unindex(acc.0, key), [#(key, value), ..acc.1])
    }
  })
}

pub fn map_values(
  store: Store(value),
  with: fn(VoiceKey, value) -> value,
) -> Store(value) {
  Store(..store, primary: dict.map_values(store.primary, with))
}

pub fn fold(
  store: Store(value),
  from: acc,
  with: fn(acc, VoiceKey, value) -> acc,
) -> acc {
  dict.fold(store.primary, from, with)
}
