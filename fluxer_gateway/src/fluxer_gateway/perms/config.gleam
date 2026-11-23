// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/perms/native
import gleam/int

pub opaque type Config {
  Config(
    work_budget_units: Int,
    viewable_cache_bytes: Int,
    dead_role_set_floor: Int,
    scan_chunk: Int,
  )
}

pub fn default() -> Config {
  Config(
    work_budget_units: 250_000,
    viewable_cache_bytes: 1_048_576,
    dead_role_set_floor: 4096,
    scan_chunk: 4096,
  )
}

pub fn set_work_budget_units(config: Config, units: Int) -> Config {
  Config(..config, work_budget_units: int.max(units, 1))
}

pub fn set_scan_chunk(config: Config, members: Int) -> Config {
  Config(..config, scan_chunk: int.max(members, 1))
}

@internal
pub fn to_native(config: Config) -> native.Config {
  native.Config(
    work_budget_units: config.work_budget_units,
    viewable_cache_bytes: config.viewable_cache_bytes,
    dead_role_set_floor: config.dead_role_set_floor,
    scan_chunk: config.scan_chunk,
  )
}
