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

import birl
import gleam/int
import gleam/list
import gleam/result
import gleam/string

const fluxer_epoch = 1_420_070_400_000

pub fn format_discriminator(discriminator: Int) -> String {
  let disc_str = int.to_string(discriminator)
  let padding = 4 - string.length(disc_str)
  case padding > 0 {
    True -> string.repeat("0", padding) <> disc_str
    False -> disc_str
  }
}

pub fn get_initials(username: String) -> String {
  case username {
    "" -> "?"
    _ -> {
      username
      |> string.to_graphemes
      |> list.first
      |> result.unwrap("?")
      |> string.uppercase
    }
  }
}

pub fn extract_timestamp(snowflake: String) -> Result(String, Nil) {
  case int.parse(snowflake) {
    Ok(id) -> {
      let timestamp_ms = int.bitwise_shift_right(id, 22) + fluxer_epoch

      let time = birl.from_unix_milli(timestamp_ms)

      Ok(format_human_readable(time))
    }
    Error(_) -> Error(Nil)
  }
}

fn format_human_readable(time: birl.Time) -> String {
  let datetime_string = birl.to_iso8601(time)

  case string.split(datetime_string, "T") {
    [date_part, time_part] -> {
      case string.split(date_part, "-") {
        [year, month, day] -> {
          let time_clean = case string.split(time_part, ".") {
            [hms, _] -> hms
            _ -> time_part
          }

          case string.split(time_clean, ":") {
            [hour, minute, _] -> {
              let month_name = case month {
                "01" -> "Jan"
                "02" -> "Feb"
                "03" -> "Mar"
                "04" -> "Apr"
                "05" -> "May"
                "06" -> "Jun"
                "07" -> "Jul"
                "08" -> "Aug"
                "09" -> "Sep"
                "10" -> "Oct"
                "11" -> "Nov"
                "12" -> "Dec"
                _ -> month
              }

              month_name
              <> " "
              <> day
              <> ", "
              <> year
              <> " at "
              <> hour
              <> ":"
              <> minute
            }
            _ -> datetime_string
          }
        }
        _ -> datetime_string
      }
    }
    _ -> datetime_string
  }
}
