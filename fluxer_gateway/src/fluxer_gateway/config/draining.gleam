// SPDX-License-Identifier: AGPL-3.0-or-later

import logging

type Key {
  FluxerGatewayDraining
}

type Stored

@external(erlang, "persistent_term", "put")
fn put_term(key: Key, value: Bool) -> Stored

@external(erlang, "persistent_term", "get")
fn get_term(key: Key, default: Bool) -> Bool

@external(erlang, "persistent_term", "erase")
fn erase_term(key: Key) -> Bool

pub fn is_draining() -> Bool {
  get_term(FluxerGatewayDraining, False)
}

pub fn is_ready() -> Bool {
  !is_draining()
}

pub fn set_draining() -> Nil {
  put_term(FluxerGatewayDraining, True)
  Nil
}

pub fn clear_draining() -> Nil {
  erase_term(FluxerGatewayDraining)
  logging.log(logging.Info, "Gateway un-cordoned: draining flag cleared")
}
