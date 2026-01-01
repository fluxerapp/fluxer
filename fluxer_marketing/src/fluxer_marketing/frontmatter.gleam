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

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Frontmatter {
  Frontmatter(data: Dict(String, String), content: String)
}

pub fn parse(markdown: String) -> Frontmatter {
  case string.starts_with(markdown, "---\n") {
    True -> parse_with_frontmatter(markdown)
    False -> Frontmatter(data: dict.new(), content: markdown)
  }
}

fn parse_with_frontmatter(markdown: String) -> Frontmatter {
  let without_first = string.drop_start(markdown, 4)

  case string.split_once(without_first, "\n---\n") {
    Ok(#(frontmatter_text, content)) -> {
      let metadata = parse_frontmatter_text(frontmatter_text)
      Frontmatter(data: metadata, content: string.trim(content))
    }
    Error(_) -> Frontmatter(data: dict.new(), content: markdown)
  }
}

fn parse_frontmatter_text(text: String) -> Dict(String, String) {
  text
  |> string.split("\n")
  |> list.filter(fn(line) { !string.is_empty(string.trim(line)) })
  |> list.filter_map(parse_frontmatter_line)
  |> dict.from_list
}

fn parse_frontmatter_line(line: String) -> Result(#(String, String), Nil) {
  case string.split_once(line, ":") {
    Ok(#(key, value)) -> {
      let clean_key = string.trim(key)
      let clean_value = string.trim(value)
      Ok(#(clean_key, clean_value))
    }
    Error(_) -> Error(Nil)
  }
}

pub fn get_string(frontmatter: Frontmatter, key: String) -> Option(String) {
  dict.get(frontmatter.data, key) |> option.from_result
}

pub fn get_string_or(
  frontmatter: Frontmatter,
  key: String,
  default: String,
) -> String {
  get_string(frontmatter, key) |> option.unwrap(default)
}

pub fn get_int(frontmatter: Frontmatter, key: String) -> Option(Int) {
  case get_string(frontmatter, key) {
    Some(value) -> {
      case int.parse(value) {
        Ok(num) -> Some(num)
        Error(_) -> None
      }
    }
    None -> None
  }
}

pub fn get_int_or(frontmatter: Frontmatter, key: String, default: Int) -> Int {
  get_int(frontmatter, key) |> option.unwrap(default)
}

pub fn get_content(frontmatter: Frontmatter) -> String {
  frontmatter.content
}
