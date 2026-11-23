// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/ws/http
import gleam/bit_array
import gleam/string

fn crlf(count: Int) -> BitArray {
  bit_array.from_string(string.repeat("\r\n", count))
}

pub fn only_empty_lines_up_to_limit_are_incomplete_test() {
  assert http.parse(crlf(5), Nil) == Ok(http.Incomplete)
}

pub fn more_than_five_leading_empty_lines_are_rejected_test() {
  assert http.parse(crlf(6), Nil) == Error(http.BadRequest)
  assert http.parse(crlf(100_000), Nil) == Error(http.BadRequest)
}

pub fn request_after_allowed_empty_lines_parses_test() {
  let buffer = <<
    crlf(5):bits,
    "GET /?v=1 HTTP/1.1\r\nHost: example.test\r\n\r\n":utf8,
  >>
  let assert Ok(http.Parsed(request:, version: http.Http11, rest: <<>>, ..)) =
    http.parse(buffer, Nil)
  assert request.path == "/"
  assert request.host == "example.test"
}

pub fn request_after_too_many_empty_lines_is_rejected_test() {
  let buffer = <<crlf(6):bits, "GET / HTTP/1.1\r\nHost: a\r\n\r\n":utf8>>
  assert http.parse(buffer, Nil) == Error(http.BadRequest)
}

pub fn incomplete_head_beyond_max_size_is_rejected_test() {
  let header = "X-Pad: " <> string.repeat("a", 4000) <> "\r\n"
  let buffer =
    bit_array.from_string("GET / HTTP/1.1\r\n" <> string.repeat(header, 108))
  assert http.parse(buffer, Nil) == Error(http.HeadersTooLarge)
}

pub fn long_request_line_without_newline_is_rejected_test() {
  let buffer = bit_array.from_string("GET /" <> string.repeat("a", 8000))
  assert http.parse(buffer, Nil) == Error(http.UriTooLong)
}
