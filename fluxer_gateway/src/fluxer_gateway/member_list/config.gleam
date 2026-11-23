// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/member_list/native
import gleam/int

pub opaque type Config {
  Config(
    offline_threshold: Int,
    work_budget_units: Int,
    fragment_budget_bytes: Int,
    payload_budget_bytes: Int,
  )
}

const seed = 0x6d6c5f636f726501

pub fn default() -> Config {
  Config(
    offline_threshold: 1000,
    work_budget_units: 250_000,
    fragment_budget_bytes: 2_097_152,
    payload_budget_bytes: 1_048_576,
  )
}

pub fn offline_threshold(config: Config) -> Int {
  config.offline_threshold
}

pub fn set_offline_threshold(config: Config, threshold: Int) -> Config {
  Config(..config, offline_threshold: int.max(threshold, 1))
}

pub fn set_work_budget_units(config: Config, units: Int) -> Config {
  Config(..config, work_budget_units: int.max(units, 1))
}

@internal
pub fn to_native(config: Config) -> native.MlConfig {
  native.Config(
    offline_threshold: config.offline_threshold,
    offline_drop: config.offline_threshold + config.offline_threshold / 2,
    work_budget_units: config.work_budget_units,
    fragment_budget_bytes: config.fragment_budget_bytes,
    payload_budget_bytes: config.payload_budget_bytes,
    seed:,
  )
}
