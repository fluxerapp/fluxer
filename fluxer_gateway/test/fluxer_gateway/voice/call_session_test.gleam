// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/voice/call/state
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry
import gleam/dict
import gleam/erlang/process
import gleam/option.{None, Some}
import support/voice_ids

fn user(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn member(owner: Int, monitor: option.Option(process.Monitor)) -> state.Member {
  state.Member(
    user: user(owner),
    inbox: process.new_subject(),
    monitor:,
    owner: None,
    owner_monitor: None,
  )
}

fn call_with(session_id: String, value: state.Member) -> state.Call {
  let assert Ok(channel) = id.channel_id(50)
  let assert Ok(message) = id.message_id(50)
  let call =
    state.new(
      state.Init(
        channel:,
        message:,
        region: None,
        ringing: [],
        recipients: [],
        caller: None,
      ),
      0,
    )
  state.Call(..call, sessions: dict.from_list([#(session_id, value)]))
}

fn dead_monitor() -> process.Monitor {
  let pid = process.spawn_unlinked(fn() { Nil })
  process.monitor(pid)
}

pub fn monitored_member_keeps_its_session_without_a_liveness_probe_test() {
  let call = call_with("s1", member(1, Some(dead_monitor())))
  assert state.session_held(call, state.PendingJoin(user(1), "s1", 0))
}

pub fn pending_for_another_user_does_not_hold_the_session_test() {
  let call = call_with("s1", member(1, Some(dead_monitor())))
  assert !state.session_held(call, state.PendingJoin(user(2), "s1", 0))
}

pub fn unmonitored_member_does_not_hold_the_session_test() {
  let call = call_with("s1", member(1, None))
  assert !state.session_held(call, state.PendingJoin(user(1), "s1", 0))
}

pub fn detached_member_holds_its_session_until_rebound_test() {
  let call =
    call_with("s1", member(1, Some(dead_monitor())))
    |> state.detach("s1", 10)
  assert state.session_held(call, state.PendingJoin(user(1), "s1", 0))
  assert state.detach_expired(call, "s1", 10)
  assert !state.detach_expired(call, "s1", 9)
}

pub fn rebind_moves_the_session_to_the_new_inbox_test() {
  let call =
    call_with("s1", member(1, Some(dead_monitor())))
    |> state.detach("s1", 10)
  let target = member(1, None)
  let assert Ok(#(_, rebound)) = state.rebind(call, "s1", target)
  assert dict.get(rebound.sessions, "s1")
    |> option.from_result
    |> option.map(fn(found) { found.inbox })
    == Some(target.inbox)
  assert !state.detach_expired(rebound, "s1", 10)
}

pub fn rebind_refuses_another_user_test() {
  let call = call_with("s1", member(1, None))
  assert state.rebind(call, "s1", member(2, None)) == Error(Nil)
  assert state.rebind(call, "s2", member(1, None)) == Error(Nil)
}

pub fn adoption_takes_only_this_sessions_connections_test() {
  let assert Ok(session_id) =
    session.session_id_from_string("0123456789abcdef0123456789abcdef")
  let ref =
    session.SessionRef(
      id: session_id,
      user: user(1),
      inbox: process.new_subject(),
    )
  let assert Ok(channel) = id.channel_id(50)
  let make = fn(owner, conn, session_text) {
    entry.new(
      user: user(owner),
      guild: None,
      channel:,
      connection_id: voice_ids.conn(conn),
      session_id: Some(session_text),
      flags: entry.no_flags(),
      mute: False,
      deaf: False,
    )
  }
  let mine = make(1, "c1", "0123456789abcdef0123456789abcdef")
  let owned =
    dm_server.owned(ref, [
      mine,
      make(1, "c2", "s2"),
      make(2, "c3", "0123456789abcdef0123456789abcdef"),
    ])
  assert owned == [mine]
}
