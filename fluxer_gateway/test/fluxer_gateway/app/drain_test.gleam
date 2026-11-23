// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain
import fluxer_gateway/config/draining

pub fn undrain_without_cluster_processes_clears_draining_test() {
  draining.set_draining()
  assert draining.is_draining()
  assert drain.undrain() == Ok(Nil)
  assert !draining.is_draining()
  assert draining.is_ready()
}
