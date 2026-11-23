// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{SessionRef}
import fluxer_gateway/guild/sessions/connect
import fluxer_gateway/guild/visitors/table as visitor_table
import gleam/erlang/process
import gleam/option.{None}
import gleam/set

fn visitor(thread_viewer: Bool) -> visitor_table.Visitor {
  let assert Ok(user) = id.user_id(7)
  visitor_table.Visitor(
    ref: SessionRef(
      id: session.new_session_id(),
      user:,
      inbox: process.new_subject(),
    ),
    pid: process.self(),
    monitor: process.monitor(process.self()),
    adult: True,
    thread_viewer:,
    operator: False,
    tier: visitor_table.Verified,
    ip_key: None,
    since_ms: 0,
    focus: None,
    view: set.new(),
  )
}

pub fn promoted_thread_viewer_keeps_channel_threads_test() {
  let flags = connect.promoted_flags(visitor(True))
  assert flags.channel_threads
  assert flags.guild_visits
}

pub fn promoted_plain_visitor_has_no_channel_threads_test() {
  let flags = connect.promoted_flags(visitor(False))
  assert !flags.channel_threads
  assert flags.guild_visits
}
