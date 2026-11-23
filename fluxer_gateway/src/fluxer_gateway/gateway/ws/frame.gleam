// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/codec
import gleam/bool
import gleam/bytes_tree.{type BytesTree}
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Opcode {
  Continuation
  Text
  Binary
  Close
  Ping
  Pong
}

pub type Frame {
  Frame(fin: Bool, opcode: Opcode, payload: BitArray)
}

pub type Parsed {
  Incomplete
  Parsed(frame: Frame, rest: BitArray)
}

pub type FrameError {
  BadFrame
  BadEncoding
  BadSize
}

pub fn error_code(error: FrameError) -> Int {
  case error {
    BadFrame -> 1002
    BadEncoding -> 1007
    BadSize -> 1009
  }
}

pub fn parse(data: BitArray, max_size: Int) -> Result(Parsed, FrameError) {
  case data {
    <<fin:1, rsv:3, code:4, masked:1, length:7, rest:bits>> -> {
      use opcode <- result.try(decode_opcode(code))
      use <- bool.guard(rsv != 0 || masked != 1, Error(BadFrame))
      use <- bool.guard(
        is_control(opcode) && { fin == 0 || length > 125 },
        Error(BadFrame),
      )
      use extended <- result.try(payload_length(length, rest))
      case extended {
        None -> Ok(Incomplete)
        Some(#(size, _)) if size > max_size -> Error(BadSize)
        Some(#(size, rest)) -> Ok(payload(fin == 1, opcode, size, rest))
      }
    }
    _ -> Ok(Incomplete)
  }
}

fn payload(fin: Bool, opcode: Opcode, size: Int, data: BitArray) -> Parsed {
  case data {
    <<key:bytes-size(4), body:bytes-size(size), rest:bits>> ->
      Parsed(Frame(fin:, opcode:, payload: unmask(body, key, size)), rest)
    _ -> Incomplete
  }
}

fn payload_length(
  length: Int,
  rest: BitArray,
) -> Result(Option(#(Int, BitArray)), FrameError) {
  case length, rest {
    126, <<size:16, rest:bits>> if size > 125 -> Ok(Some(#(size, rest)))
    126, <<_:16, _:bits>> -> Error(BadFrame)
    126, _ -> Ok(None)
    127, <<0:1, size:63, rest:bits>> if size > 0xFFFF -> Ok(Some(#(size, rest)))
    127, <<_:64, _:bits>> -> Error(BadFrame)
    127, _ -> Ok(None)
    size, _ -> Ok(Some(#(size, rest)))
  }
}

fn decode_opcode(code: Int) -> Result(Opcode, FrameError) {
  case code {
    0 -> Ok(Continuation)
    1 -> Ok(Text)
    2 -> Ok(Binary)
    8 -> Ok(Close)
    9 -> Ok(Ping)
    10 -> Ok(Pong)
    _ -> Error(BadFrame)
  }
}

fn encode_opcode(opcode: Opcode) -> Int {
  case opcode {
    Continuation -> 0
    Text -> 1
    Binary -> 2
    Close -> 8
    Ping -> 9
    Pong -> 10
  }
}

fn is_control(opcode: Opcode) -> Bool {
  case opcode {
    Close | Ping | Pong -> True
    Continuation | Text | Binary -> False
  }
}

@external(erlang, "crypto", "exor")
fn exor(data: BitArray, mask: BitArray) -> BitArray

@external(erlang, "binary", "copy")
fn copy(subject: BitArray, times: Int) -> BitArray

fn unmask(body: BitArray, key: BitArray, size: Int) -> BitArray {
  case size, copy(key, size / 4 + 1) {
    0, _ -> body
    _, <<mask:bytes-size(size), _:bits>> -> exor(body, mask)
    _, _ -> body
  }
}

pub fn encode(opcode: Opcode, payload: BytesTree) -> BytesTree {
  let code = encode_opcode(opcode)
  let header = case bytes_tree.byte_size(payload) {
    size if size < 126 -> <<1:1, 0:3, code:4, 0:1, size:7>>
    size if size < 65_536 -> <<1:1, 0:3, code:4, 0:1, 126:7, size:16>>
    size -> <<1:1, 0:3, code:4, 0:1, 127:7, size:64>>
  }
  bytes_tree.prepend(payload, header)
}

pub fn close_code(payload: BitArray) -> Result(Option(Int), FrameError) {
  case payload {
    <<>> -> Ok(None)
    <<code:16, reason:bits>> ->
      case valid_close_code(code), codec.valid_text(reason) {
        False, _ -> Error(BadFrame)
        True, False -> Error(BadEncoding)
        True, True -> Ok(Some(code))
      }
    _ -> Error(BadFrame)
  }
}

fn valid_close_code(code: Int) -> Bool {
  { code >= 1000 && code <= 1003 }
  || { code >= 1007 && code <= 1014 }
  || { code >= 3000 && code <= 4999 }
}
