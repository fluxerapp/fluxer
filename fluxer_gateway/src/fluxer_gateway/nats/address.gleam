// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Address {
  Address(host: String, port: Int)
}

pub type AddressError {
  InvalidPort
  EmptyHost
}

const default_port = 4222

pub fn parse(url: String) -> Result(Address, AddressError) {
  let rest = case url {
    "nats://" <> rest -> rest
    "tls://" <> rest -> rest
    _ -> url
  }
  case string.split_once(rest, ":") {
    Ok(#(host, port_text)) ->
      case id.parse_decimal(port_text) {
        Ok(port) -> host_address(host, port)
        Error(Nil) -> Error(InvalidPort)
      }
    Error(Nil) -> host_address(rest, default_port)
  }
}

fn host_address(host: String, port: Int) -> Result(Address, AddressError) {
  case host {
    "" -> Error(EmptyHost)
    _ -> Ok(Address(host:, port:))
  }
}

pub fn auth_token(token: String) -> Option(String) {
  case token {
    "" -> None
    _ -> Some(token)
  }
}
