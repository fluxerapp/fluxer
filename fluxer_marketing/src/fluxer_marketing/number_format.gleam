//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import gleam/int
import gleam/string

pub fn format_number(value: Int) -> String {
  let digits = value |> int.max(0) |> int.to_string
  format_number_digits(digits)
}

fn format_number_digits(digits: String) -> String {
  let len = string.length(digits)

  case len <= 3 {
    True -> digits
    False -> {
      let head_len = case len % 3 {
        0 -> 3
        rem -> rem
      }
      let head = string.slice(digits, 0, head_len)
      let tail = string.slice(digits, head_len, len - head_len)
      head <> chunk_digits(tail)
    }
  }
}

fn chunk_digits(digits: String) -> String {
  let len = string.length(digits)

  case len <= 3 {
    True -> digits
    False -> {
      let head = string.slice(digits, 0, 3)
      let tail = string.slice(digits, 3, len - 3)
      head <> "," <> chunk_digits(tail)
    }
  }
}
