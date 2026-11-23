// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node.{type Node}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom.{type Atom}
import gleam/result

pub type Arg

@external(erlang, "gleam_stdlib", "identity")
pub fn arg(value: a) -> Arg

@external(erlang, "rpc", "call")
fn rpc_call(
  node: Atom,
  module: Atom,
  function: Atom,
  args: List(Arg),
  timeout: Int,
) -> Dynamic

pub fn call(
  target: Node,
  module: String,
  function: String,
  args: List(Arg),
  timeout_ms: Int,
  expecting decoder: Decoder(reply),
) -> Result(reply, Nil) {
  rpc_call(
    node.to_atom(target),
    atom.create(module),
    atom.create(function),
    args,
    timeout_ms,
  )
  |> decode.run(decoder)
  |> result.replace_error(Nil)
}
