// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client.{type ApiError}
import fluxer_gateway/threads/load.{type Payload}

pub type ThreadMsg {
  LoadRetry
  LoadResult(request: Int, result: Result(Payload, ApiError))
  SyncTick(session: String)
  SyncDrain
  ListFlush
}
