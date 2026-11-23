// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/process.{type Name, type Pid}

@external(erlang, "erlang", "send")
fn erlang_send(pid: Pid, message: #(Name(a), a)) -> #(Name(a), a)

pub fn send(name: Name(message), message: message) -> Result(Nil, Nil) {
  case process.named(name) {
    Ok(pid) -> Ok(send_to(pid, name, message))
    Error(Nil) -> Error(Nil)
  }
}

pub fn send_to(pid: Pid, name: Name(message), message: message) -> Nil {
  let _ = erlang_send(pid, #(name, message))
  Nil
}
