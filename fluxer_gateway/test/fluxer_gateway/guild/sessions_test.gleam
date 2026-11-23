// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{SessionRef}
import fluxer_gateway/guild/sessions
import fluxer_gateway/guild/sessions/entry
import gleam/erlang/process
import gleam/option.{None, Some}

fn session_entry(user: Int) -> entry.Entry {
  let assert Ok(user_id) = id.user_id(user)
  entry.new(
    ref: SessionRef(
      id: session.new_session_id(),
      user: user_id,
      inbox: process.new_subject(),
    ),
    pid: process.self(),
    monitor: process.monitor(process.self()),
    flags: session.no_flags(),
    bot: False,
    staff: None,
    mode: entry.Passive,
  )
}

pub fn find_by_monitor_follows_put_rebind_and_delete_test() {
  let first = session_entry(1)
  let second = session_entry(2)
  let table =
    sessions.new()
    |> sessions.put(first)
    |> sessions.put(second)
  assert sessions.find_by_monitor(table, entry.monitor(first)) == Some(first)
  assert sessions.find_by_monitor(table, entry.monitor(second)) == Some(second)
  let rebound =
    entry.rebind(
      first,
      ref: entry.ref(first),
      pid: process.self(),
      monitor: process.monitor(process.self()),
    )
  let table = sessions.put(table, rebound)
  assert sessions.find_by_monitor(table, entry.monitor(first)) == None
  assert sessions.find_by_monitor(table, entry.monitor(rebound))
    == Some(rebound)
  let table = sessions.delete(table, entry.id(rebound))
  assert sessions.find_by_monitor(table, entry.monitor(rebound)) == None
  assert sessions.find_by_monitor(table, entry.monitor(second)) == Some(second)
  assert sessions.count(table) == 1
}

pub fn delete_of_unknown_session_keeps_index_test() {
  let kept = session_entry(3)
  let table =
    sessions.new()
    |> sessions.put(kept)
    |> sessions.delete(entry.id(session_entry(4)))
  assert sessions.find_by_monitor(table, entry.monitor(kept)) == Some(kept)
}
