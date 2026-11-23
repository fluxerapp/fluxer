// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/process.{type Name}
import gleam/int

@external(erlang, "erlang", "binary_to_atom")
fn to_name(text: String) -> Name(message)

pub fn router() -> Name(message) {
  to_name("fluxer_gateway_session_manager")
}

pub fn transfer() -> Name(message) {
  to_name("fluxer_gateway_session_transfer")
}

pub fn shard(index: Int) -> Name(message) {
  to_name("fluxer_gateway_session_shard_" <> int.to_string(index))
}

pub fn factory(index: Int) -> Name(message) {
  to_name("fluxer_gateway_session_factory_" <> int.to_string(index))
}
