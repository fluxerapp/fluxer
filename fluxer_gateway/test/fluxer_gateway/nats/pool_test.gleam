// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/nats/address
import fluxer_gateway/nats/pool
import gleam/option.{None, Some}

pub fn reconnect_delay_test() {
  assert pool.reconnect_delay(1) == 2000
  assert pool.reconnect_delay(2) == 4000
  assert pool.reconnect_delay(4) == 16_000
  assert pool.reconnect_delay(5) == 15_000
  assert pool.reconnect_delay(9) == 15_000
}

pub fn subscribe_retry_delay_test() {
  assert pool.subscribe_retry_delay(0) == 250
  assert pool.subscribe_retry_delay(1) == 250
  assert pool.subscribe_retry_delay(3) == 1000
  assert pool.subscribe_retry_delay(5) == 4000
  assert pool.subscribe_retry_delay(6) == 5000
}

pub fn address_test() {
  assert address.parse("nats://nats:4222") == Ok(address.Address("nats", 4222))
  assert address.parse("tls://host:4443") == Ok(address.Address("host", 4443))
  assert address.parse("127.0.0.1") == Ok(address.Address("127.0.0.1", 4222))
  assert address.parse("nats://host:port") == Error(address.InvalidPort)
  assert address.parse("nats://") == Error(address.EmptyHost)
}

pub fn auth_token_test() {
  assert address.auth_token("") == None
  assert address.auth_token("secret") == Some("secret")
}
