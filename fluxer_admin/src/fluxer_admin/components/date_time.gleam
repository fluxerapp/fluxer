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

import gleam/string

pub fn format_timestamp(timestamp: String) -> String {
  case string.split(timestamp, "T") {
    [date_part, time_part] -> {
      let time_clean = case string.split(time_part, ".") {
        [hms, _] -> hms
        _ -> time_part
      }
      let time_clean = string.replace(time_clean, "Z", "")

      case string.split(time_clean, ":") {
        [hour, minute, _] -> date_part <> " " <> hour <> ":" <> minute
        _ -> timestamp
      }
    }
    _ -> timestamp
  }
}

pub fn format_date(timestamp: String) -> String {
  case string.split(timestamp, "T") {
    [date_part, _] -> date_part
    _ -> timestamp
  }
}

pub fn format_time(timestamp: String) -> String {
  case string.split(timestamp, "T") {
    [_, time_part] -> {
      let time_clean = case string.split(time_part, ".") {
        [hms, _] -> hms
        _ -> time_part
      }
      let time_clean = string.replace(time_clean, "Z", "")

      case string.split(time_clean, ":") {
        [hour, minute, _] -> hour <> ":" <> minute
        _ -> timestamp
      }
    }
    _ -> timestamp
  }
}
