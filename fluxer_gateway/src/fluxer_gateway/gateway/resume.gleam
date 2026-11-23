// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic/decode.{type Decoder}

pub type ResumeRequest {
  ResumeRequest(token: String, session_id: String, seq: Int)
}

pub type ResumeDecodeError {
  InvalidResumePayload
}

pub fn decoder() -> Decoder(Result(ResumeRequest, ResumeDecodeError)) {
  let request = {
    use token <- decode.field("token", decode.string)
    use session_id <- decode.field("session_id", decode.string)
    use seq <- decode.field("seq", decode.int)
    decode.success(Ok(ResumeRequest(token:, session_id:, seq:)))
  }
  decode.one_of(request, [decode.success(Error(InvalidResumePayload))])
}
