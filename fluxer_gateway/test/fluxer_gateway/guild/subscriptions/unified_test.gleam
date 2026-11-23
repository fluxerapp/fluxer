// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree as value
import fluxer_gateway/guild/subscriptions/unified
import gleam/list
import gleam/option.{None, Some}

fn range(start: Int, stop: Int) -> value.JsonTree {
  value.Array([value.Int(start), value.Int(stop)])
}

pub fn ranges_drop_invalid_and_cap_at_ten_test() {
  let items =
    value.Array([
      range(0, 99),
      range(0, 100),
      range(5, 4),
      range(-1, 3),
      range(99_950, 100_001),
    ])
  assert unified.ranges(items) == [#(0, 99)]
  let many =
    value.Array([
      range(0, 1),
      range(2, 3),
      range(4, 5),
      range(6, 7),
      range(8, 9),
      range(10, 11),
      range(12, 13),
      range(14, 15),
      range(16, 17),
      range(18, 19),
      range(20, 21),
    ])
  assert list.length(unified.ranges(many)) == 10
}

pub fn for_guild_picks_the_guild_entry_test() {
  let data =
    value.Object([
      #(
        "subscriptions",
        value.Object([
          #(
            "10",
            value.Object([
              #("active", value.Bool(True)),
              #("sync", value.Bool(True)),
              #("members", value.Array([value.String("5"), value.String("x")])),
              #(
                "member_list_channels",
                value.Object([#("20", value.Array([range(0, 99)]))]),
              ),
            ]),
          ),
          #("11", value.Object([#("active", value.Bool(False))])),
        ]),
      ),
    ])
  let assert Some(found) = unified.for_guild(data, 10)
  assert found.active == Some(True)
  assert found.sync
  assert found.members == Some([5])
  assert found.channels == [#(20, [#(0, 99)])]
  assert found.typing == None
  assert unified.for_guild(data, 12) == None
}

pub fn focus_parses_null_as_unfocus_test() {
  let found = unified.parse(value.Object([#("focused_channel_id", value.Null)]))
  assert found.focus == Some(None)
}
