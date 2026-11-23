// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/gateway/counter_table
import fluxer_gateway/gateway/limits
import gleam/erlang/process.{type Pid}
import gleam/list

const connection_table = "gateway_ip_connections"

const owner_table = "gateway_ip_connection_owners"

pub fn owner_sweep_interval_ms() -> Int {
  durations.gateway().ip_connections_owner_sweep_interval_ms
}

const max_connections_per_ip = 256

pub type IpConnectionError {
  TooManyConnections
}

pub fn create_tables() -> Nil {
  counter_table.create(connection_table)
  counter_table.create(owner_table)
}

pub fn acquire(peer_ip: String) -> Result(Nil, IpConnectionError) {
  case peer_ip == "unknown" || limits.rate_limits_disabled() {
    True -> Ok(Nil)
    False ->
      case counter_table.bump(connection_table, peer_ip) {
        Ok(count) if count > max_connections_per_ip -> {
          counter_table.drop(connection_table, peer_ip)
          Error(TooManyConnections)
        }
        Ok(_) -> {
          counter_table.insert_marker(owner_table, #(process.self(), peer_ip))
          Ok(Nil)
        }
        Error(Nil) -> Ok(Nil)
      }
  }
}

pub fn release(peer_ip: String) -> Nil {
  release_owner(process.self(), peer_ip)
}

fn release_owner(owner: Pid, peer_ip: String) -> Nil {
  case
    peer_ip != "unknown"
    && counter_table.take_marker(owner_table, #(owner, peer_ip))
  {
    True -> counter_table.drop(connection_table, peer_ip)
    False -> Nil
  }
}

pub fn reclaim_dead_owners() -> Nil {
  counter_table.markers(owner_table)
  |> list.each(fn(owner: #(Pid, String)) {
    case process.is_alive(owner.0) {
      True -> Nil
      False -> release_owner(owner.0, owner.1)
    }
  })
}
