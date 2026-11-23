// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/dist_metrics.{Reading}

pub fn new_links_count_from_zero_and_old_links_count_their_growth_test() {
  let last = [Reading(1, 100, 40, 0), Reading(2, 50, 50, 0)]
  let current = [Reading(1, 130, 45, 9), Reading(3, 7, 11, 0)]
  assert dist_metrics.advance(last, current) == #(37, 16)
}

pub fn a_link_whose_counters_went_back_adds_nothing_test() {
  assert dist_metrics.advance([Reading(1, 100, 40, 0)], [Reading(1, 10, 4, 0)])
    == #(0, 0)
}

pub fn the_distribution_totals_never_go_back_between_scrapes_test() {
  let first = dist_metrics.traffic()
  let second = dist_metrics.traffic()
  assert second.sent >= first.sent
  assert second.received >= first.received
  assert first.visible_nodes >= 0 && first.hidden_nodes >= 0
}
