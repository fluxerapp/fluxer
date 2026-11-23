// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/gateway/abuse_protection
import fluxer_gateway/session/join_budget
import fluxer_gateway/util/table
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

const table_name = "fluxer_gateway_session_leases"

@external(erlang, "ets", "take")
fn ets_take(table: Atom, key: Pid) -> List(#(Pid, UserId))

pub fn create_table() -> Nil {
  let _ = table.ensure_public(table.named(table_name))
  Nil
}

pub fn open(owner: Pid, user: UserId) -> Nil {
  table.insert(table.named(table_name), #(owner, user))
  abuse_protection.increment_user_sessions(user)
}

pub fn reserve(
  owner: Pid,
  user: UserId,
) -> Result(Nil, abuse_protection.AbuseError) {
  table.insert(table.named(table_name), #(owner, user))
  case abuse_protection.reserve_user_session(user) {
    Ok(Nil) -> Ok(Nil)
    Error(reason) -> {
      let _ = ets_take(atom.create(table_name), owner)
      Error(reason)
    }
  }
}

pub fn release(owner: Pid) -> Nil {
  case ets_take(atom.create(table_name), owner) {
    [#(_, user), ..] -> abuse_protection.decrement_user_sessions(user)
    [] -> Nil
  }
  join_budget.release(owner)
}

pub fn sweep() -> Nil {
  table.to_list(table.named(table_name))
  |> list.each(fn(row: #(Pid, UserId)) {
    case process.is_alive(row.0) {
      True -> Nil
      False -> release(row.0)
    }
  })
}
