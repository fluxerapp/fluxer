// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam
import gleam/erlang/atom.{type Atom}

pub type Crash

pub opaque type Reply {
  Ok(String)
  Error(Crash)
  Throw(Crash)
  Exit(Crash)
}

@external(erlang, "erpc", "multicall")
pub fn call(
  nodes: List(Atom),
  module: Atom,
  function: Atom,
  args: List(Int),
  timeout_ms: Int,
) -> List(Reply)

pub fn text(reply: Reply) -> gleam.Result(String, Nil) {
  case reply {
    Ok(text) -> gleam.Ok(text)
    Error(_) | Throw(_) | Exit(_) -> gleam.Error(Nil)
  }
}
