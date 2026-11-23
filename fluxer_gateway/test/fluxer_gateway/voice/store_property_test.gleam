// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/voice/store.{type Store}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/set
import support/voice_ids

type Op {
  Put(VoiceKey, Int)
  Delete(VoiceKey)
  DeleteConnection(ConnectionId)
  Rekey(ConnectionId, ChannelId)
  DropUser(UserId)
  Bump
}

type Model =
  Dict(ConnectionId, #(VoiceKey, Int))

fn next(seed: Int) -> Int {
  { seed * 1_103_515_245 + 12_345 } % 2_147_483_648
}

fn pick(seed: Int, bound: Int) -> #(Int, Int) {
  let seed = next(seed)
  #(seed, { seed / 65_536 } % bound)
}

fn channel(index: Int) -> ChannelId {
  let assert Ok(found) = id.channel_id(100 + index)
  found
}

fn user(index: Int) -> UserId {
  let assert Ok(found) = id.user_id(1 + index)
  found
}

fn connection(index: Int) -> ConnectionId {
  voice_ids.conn("c" <> int.to_string(index))
}

fn key(seed: Int) -> #(Int, VoiceKey) {
  let #(seed, c) = pick(seed, 3)
  let #(seed, u) = pick(seed, 3)
  let #(seed, n) = pick(seed, 6)
  #(
    seed,
    voice_key.new(channel: channel(c), user: user(u), connection: connection(n)),
  )
}

fn op(seed: Int) -> #(Int, Op) {
  let #(seed, kind) = pick(seed, 10)
  case kind {
    0 | 1 | 2 | 3 -> {
      let #(seed, found) = key(seed)
      let #(seed, value) = pick(seed, 1000)
      #(seed, Put(found, value))
    }
    4 -> {
      let #(seed, found) = key(seed)
      #(seed, Delete(found))
    }
    5 -> {
      let #(seed, n) = pick(seed, 6)
      #(seed, DeleteConnection(connection(n)))
    }
    6 | 7 -> {
      let #(seed, n) = pick(seed, 6)
      let #(seed, c) = pick(seed, 3)
      #(seed, Rekey(connection(n), channel(c)))
    }
    8 -> {
      let #(seed, u) = pick(seed, 3)
      #(seed, DropUser(user(u)))
    }
    _ -> #(seed, Bump)
  }
}

fn apply(pair: #(Store(Int), Model), op: Op) -> #(Store(Int), Model) {
  let #(held, model) = pair
  case op {
    Put(found, value) -> #(
      store.put(held, found, value),
      dict.insert(model, voice_key.connection(found), #(found, value)),
    )
    Delete(found) -> #(
      store.delete(held, found),
      case dict.get(model, voice_key.connection(found)) {
        Ok(#(stored, _)) if stored == found ->
          dict.delete(model, voice_key.connection(found))
        _ -> model
      },
    )
    DeleteConnection(conn) -> #(
      store.delete_connection(held, conn),
      dict.delete(model, conn),
    )
    Rekey(conn, target) ->
      case store.rekey(held, conn, to: target, with: fn(value) { value + 1 }) {
        Ok(#(moved, held)) -> {
          let assert Ok(#(_, value)) = dict.get(model, conn)
          assert voice_key.channel(moved) == target
          #(held, dict.insert(model, conn, #(moved, value + 1)))
        }
        Error(Nil) -> {
          assert !dict.has_key(model, conn)
          #(held, model)
        }
      }
    DropUser(gone) -> #(
      store.filter(held, fn(found, _) { voice_key.user(found) != gone }),
      dict.filter(model, fn(_, pair) { voice_key.user(pair.0) != gone }),
    )
    Bump -> #(
      store.map_values(held, fn(_, value) { value + 1 }),
      dict.map_values(model, fn(_, pair) { #(pair.0, pair.1 + 1) }),
    )
  }
}

fn sorted(keys: List(VoiceKey)) -> List(VoiceKey) {
  list.sort(keys, voice_key.compare)
}

fn agrees(held: Store(Int), model: Model) -> Bool {
  let expected = dict.values(model)
  let keys = store.keys(held)
  let users =
    expected
    |> list.map(fn(pair) { voice_key.user(pair.0) })
    |> set.from_list
  let by_user_total =
    store.users(held)
    |> list.fold(0, fn(total, found) {
      total + list.length(store.keys_of_user(held, found))
    })
  sorted(keys) == sorted(list.map(expected, fn(pair) { pair.0 }))
  && store.size(held) == dict.size(model)
  && list.length(store.connections(held)) == dict.size(model)
  && set.from_list(store.users(held)) == users
  && by_user_total == dict.size(model)
  && list.all(expected, fn(pair) {
    let #(found, value) = pair
    store.by_connection(held, voice_key.connection(found))
    == Ok(#(found, value))
    && store.get(held, found) == Ok(value)
    && list.contains(store.keys_of_user(held, voice_key.user(found)), found)
  })
  && list.all([0, 1, 2], fn(index) {
    sorted(
      list.map(store.in_channel(held, channel(index)), fn(pair) { pair.0 }),
    )
    == sorted(
      expected
      |> list.map(fn(pair) { pair.0 })
      |> list.filter(fn(found) { voice_key.channel(found) == channel(index) }),
    )
  })
}

fn run(seed: Int, steps: Int, pair: #(Store(Int), Model)) -> Bool {
  case steps {
    0 -> True
    _ -> {
      let #(seed, step) = op(seed)
      let pair = apply(pair, step)
      agrees(pair.0, pair.1) && run(seed, steps - 1, pair)
    }
  }
}

pub fn indexes_follow_the_primary_store_under_random_operations_test() {
  let failing =
    int.range(from: 300, to: 0, with: [], run: list.prepend)
    |> list.filter(fn(seed) {
      !run(seed * 7919, 80, #(store.new(), dict.new()))
    })
  assert failing == []
}

pub fn at_most_one_state_per_connection_any_number_per_user_test() {
  let a =
    voice_key.new(channel: channel(0), user: user(0), connection: connection(1))
  let b =
    voice_key.new(channel: channel(0), user: user(0), connection: connection(2))
  let c =
    voice_key.new(channel: channel(1), user: user(0), connection: connection(3))
  let moved = voice_key.move(a, to: channel(2))
  let held =
    store.new()
    |> store.put(a, 1)
    |> store.put(b, 2)
    |> store.put(c, 3)
  assert list.length(store.keys_of_user(held, user(0))) == 3
  let held = store.put(held, moved, 4)
  assert store.size(held) == 3
  assert store.get(held, a) == Error(Nil)
  assert store.by_connection(held, connection(1)) == Ok(#(moved, 4))
  let assert Ok(#(back, held)) =
    store.rekey(held, connection(1), to: channel(0), with: fn(value) { value })
  assert back == a
  assert store.in_channel(held, channel(2)) == []
  assert sorted(
      list.map(store.in_channel(held, channel(0)), fn(pair) { pair.0 }),
    )
    == sorted([a, b])
  let held = store.delete_connection(held, connection(2))
  assert sorted(store.keys_of_user(held, user(0))) == sorted([a, c])
  assert store.rekey(held, connection(2), to: channel(1), with: fn(value) {
      value
    })
    == Error(Nil)
}
