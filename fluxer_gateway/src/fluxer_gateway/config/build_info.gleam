// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/env
import gleam/option

pub const wire_epoch = 5

const version_header_name = "x-fluxer-version"

fn build_version() -> String {
  env.lookup("BUILD_VERSION") |> option.unwrap("dev")
}

pub fn version() -> String {
  build_version()
}

pub fn revision() -> String {
  env.lookup("BUILD_REVISION") |> option.unwrap("dev")
}

pub fn version_header() -> #(String, String) {
  #(version_header_name, build_version())
}
