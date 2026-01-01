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

import gleam/list
import gleam/option.{type Option}
import gleam/string
import gleam/uri

pub fn build_url(
  base: String,
  params: List(#(String, Option(String))),
) -> String {
  let filtered_params =
    params
    |> list.filter_map(fn(param) {
      let #(key, value_opt) = param
      case value_opt {
        option.Some(value) -> {
          let trimmed = string.trim(value)
          case trimmed {
            "" -> Error(Nil)
            v -> Ok(#(key, v))
          }
        }
        option.None -> Error(Nil)
      }
    })

  case filtered_params {
    [] -> base
    params -> {
      let query_string =
        params
        |> list.map(fn(pair) {
          let #(key, value) = pair
          key <> "=" <> uri.percent_encode(value)
        })
        |> string.join("&")
      base <> "?" <> query_string
    }
  }
}
