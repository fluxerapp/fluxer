// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type MessageId}

const fluxer_epoch_ms = 1_420_070_400_000

const timestamp_shift = 4_194_304

pub fn timestamp_ms(snowflake: Int) -> Int {
  snowflake / timestamp_shift + fluxer_epoch_ms
}

pub fn message_timestamp_ms(message: MessageId) -> Int {
  timestamp_ms(id.message_id_to_int(message))
}
