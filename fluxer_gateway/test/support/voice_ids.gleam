// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_state.{type Pending}
import fluxer_gateway/voice/store.{type Store}
import gleam/list
import gleam/result

pub fn conn(text: String) -> ConnectionId {
  let assert Ok(found) = voice_key.connection_id(text)
  found
}

pub fn text(connection: ConnectionId) -> String {
  voice_key.connection_text(connection)
}

pub fn entries(values: List(Entry)) -> Store(Entry) {
  values
  |> list.map(fn(value) {
    let assert Ok(key) = entry.key(value)
    #(key, value)
  })
  |> store.from_list
}

pub fn pendings(values: List(Pending)) -> Store(Pending) {
  values
  |> list.map(fn(pending) { #(guild_state.pending_key(pending), pending) })
  |> store.from_list
}

pub fn has(states: Store(a), connection: String) -> Bool {
  store.has_connection(states, conn(connection))
}

pub fn at(states: Store(a), connection: String) -> Result(a, Nil) {
  store.by_connection(states, conn(connection))
  |> result.map(fn(pair) { pair.1 })
}

pub fn of(value: Entry) -> String {
  text(entry.connection_id(value))
}

pub fn counted() -> List(String) {
  count_table.rows()
  |> list.map(fn(row) { text(voice_key.connection(row.key)) })
}

pub fn row_text(row: count_table.Row) -> String {
  text(voice_key.connection(row.key))
}
