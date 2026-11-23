// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/member_list/ranges

fn items(found: ranges.Ranges) -> List(#(Int, Int)) {
  ranges.to_list(found)
}

pub fn parse_request_keeps_the_first_ten_valid_items_test() {
  let request = [
    #(-1, 5),
    #(5, 1),
    #(0, 200),
    #(0, 99),
    #(100, 199),
    #(0, 100_001),
    #(200, 299),
    #(300, 399),
    #(400, 499),
    #(500, 599),
    #(600, 699),
    #(700, 799),
    #(800, 899),
    #(900, 999),
    #(1000, 1099),
  ]
  assert items(ranges.parse_request(request))
    == [
      #(0, 99),
      #(100, 199),
      #(200, 299),
      #(300, 399),
      #(400, 499),
      #(500, 599),
      #(600, 699),
      #(700, 799),
      #(800, 899),
      #(900, 999),
    ]
}

pub fn normalize_merges_and_splits_test() {
  assert items(ranges.from_list([#(50, 99), #(0, 60), #(100, 120)]))
    == [#(0, 99), #(100, 120)]
  assert items(ranges.normalize(ranges.parse_request([#(10, 20), #(0, 5)])))
    == [#(0, 5), #(10, 20)]
}

pub fn to_sync_matches_the_subscribe_rules_test() {
  let first = ranges.from_list([#(0, 99)])
  let wider = ranges.from_list([#(0, 99), #(100, 199)])
  let elsewhere = ranges.from_list([#(300, 399)])
  assert items(ranges.to_sync(new: first, old: ranges.empty())) == [#(0, 99)]
  assert items(ranges.to_sync(new: first, old: first)) == [#(0, 99)]
  assert items(ranges.to_sync(new: first, old: wider)) == []
  assert items(ranges.to_sync(new: wider, old: first)) == [#(100, 199)]
  assert items(ranges.to_sync(new: elsewhere, old: first)) == [#(300, 399)]
  assert items(ranges.to_sync(new: ranges.empty(), old: first)) == []
}

pub fn subset_and_delta_test() {
  let outer = ranges.from_list([#(0, 99), #(200, 299)])
  assert ranges.is_subset(ranges.from_list([#(10, 20)]), of: outer)
  assert !ranges.is_subset(ranges.from_list([#(90, 110)]), of: outer)
  assert ranges.is_subset(ranges.empty(), of: ranges.empty())
  assert items(ranges.delta(
      new: ranges.from_list([#(50, 149), #(150, 249)]),
      old: outer,
    ))
    == [#(100, 199)]
}
