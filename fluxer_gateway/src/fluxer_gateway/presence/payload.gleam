// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/presence/display.{type Display}
import fluxer_gateway/presence/status
import gleam/bit_array

fn bool(value: Bool) -> BitArray {
  case value {
    True -> <<"true":utf8>>
    False -> <<"false":utf8>>
  }
}

fn quoted(text: String) -> BitArray {
  <<"\"":utf8, text:utf8, "\"":utf8>>
}

fn guild_head(shown: Display, custom_status: BitArray) -> BitArray {
  bit_array.concat([
    <<"{\"afk\":":utf8>>,
    bool(display.afk(shown)),
    <<",\"custom_status\":":utf8>>,
    custom_status,
    <<",":utf8>>,
  ])
}

fn guild_tail(shown: Display, user_wire: BitArray) -> BitArray {
  bit_array.concat([
    <<"\"mobile\":":utf8>>,
    bool(display.mobile(shown)),
    <<",\"status\":":utf8>>,
    quoted(status.to_wire(display.status(shown))),
    <<",\"user\":":utf8>>,
    user_wire,
    <<"}":utf8>>,
  ])
}

pub fn status_wire(
  shown: Display,
  custom_status: BitArray,
  user_wire: BitArray,
) -> BitArray {
  bit_array.append(
    guild_head(shown, custom_status),
    guild_tail(shown, user_wire),
  )
}

pub fn forced_offline_wire(user: Int) -> BitArray {
  bit_array.concat([
    <<
      "{\"afk\":false,\"custom_status\":null,\"mobile\":false,\"status\":\"offline\",\"user\":{\"id\":":utf8,
    >>,
    quoted(id.write(user)),
    <<"}}":utf8>>,
  ])
}
