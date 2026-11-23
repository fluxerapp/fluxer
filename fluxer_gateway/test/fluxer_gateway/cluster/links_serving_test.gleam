// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/links

pub fn a_lost_majority_keeps_the_worker_in_service_until_the_hold_ends_test() {
  links.set_joined(False)
  links.release_serving()
  assert !links.serving(1000)
  links.hold_serving(2000)
  assert links.serving(1999)
  assert !links.serving(2000)
  links.release_serving()
  assert !links.serving(0)
  links.set_joined(True)
  assert links.serving(5000)
  links.set_joined(False)
  assert !links.serving(5000)
}
