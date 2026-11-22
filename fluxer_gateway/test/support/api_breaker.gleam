// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/http
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/result

@external(erlang, "erlang", "whereis")
fn whereis(name: Atom) -> Dynamic

@external(erlang, "sys", "suspend")
fn sys_suspend(pid: Pid) -> Dynamic

@external(erlang, "sys", "resume")
fn sys_resume(pid: Pid) -> Dynamic

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: Dynamic) -> Pid

pub fn reset() -> Bool {
  let _ = background_caller() |> result.map(sys_resume)
  http.reset_breaker()
  http.is_closed()
}

pub fn hold_background_callers() -> Nil {
  let _ = background_caller() |> result.map(sys_suspend)
  Nil
}

fn background_caller() -> Result(Pid, Nil) {
  let found = whereis(atom.create("fluxer_gateway@config@rollout"))
  case is_pid(found) {
    True -> Ok(coerce(found))
    False -> Error(Nil)
  }
}
