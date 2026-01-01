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

import fluxer_admin/web
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/string

pub fn get_user_avatar_url(
  media_endpoint: String,
  cdn_endpoint: String,
  user_id: String,
  avatar: Option(String),
  animated: Bool,
  asset_version: String,
) -> String {
  case avatar {
    option.Some(hash) -> {
      let is_animated = string.starts_with(hash, "a_")
      let actual_hash = case is_animated {
        True -> string.drop_start(hash, 2)
        False -> hash
      }
      let should_animate = is_animated && animated
      let format = case should_animate {
        True -> "gif"
        False -> "webp"
      }
      media_endpoint
      <> "/avatars/"
      <> user_id
      <> "/"
      <> actual_hash
      <> "."
      <> format
      <> "?size=160"
    }
    option.None -> get_default_avatar(cdn_endpoint, user_id, asset_version)
  }
}

fn get_default_avatar(
  cdn_endpoint: String,
  user_id: String,
  asset_version: String,
) -> String {
  let id = do_parse_bigint(user_id)
  let index = do_rem(id, 6)
  cdn_endpoint
  <> "/avatars/"
  <> int.to_string(index)
  <> ".png"
  |> web.cache_busted_with_version(asset_version)
}

@external(erlang, "erlang", "binary_to_integer")
fn do_parse_bigint(id: String) -> Int

@external(erlang, "erlang", "rem")
fn do_rem(a: Int, b: Int) -> Int

pub fn get_guild_icon_url(
  media_proxy_endpoint: String,
  guild_id: String,
  icon: Option(String),
  animated: Bool,
) -> Option(String) {
  case icon {
    option.Some(hash) -> {
      let is_animated = string.starts_with(hash, "a_")
      let actual_hash = case is_animated {
        True -> string.drop_start(hash, 2)
        False -> hash
      }
      let should_animate = is_animated && animated
      let format = case should_animate {
        True -> "gif"
        False -> "webp"
      }
      option.Some(
        media_proxy_endpoint
        <> "/icons/"
        <> guild_id
        <> "/"
        <> actual_hash
        <> "."
        <> format
        <> "?size=160",
      )
    }
    option.None -> option.None
  }
}

pub fn get_initials_from_name(name: String) -> String {
  name
  |> string.to_graphemes
  |> do_get_initials(True, [])
  |> list.reverse
  |> string.join("")
  |> string.uppercase
}

fn do_get_initials(
  chars: List(String),
  is_start: Bool,
  acc: List(String),
) -> List(String) {
  case chars {
    [] -> acc
    [char, ..rest] -> {
      case char {
        " " -> do_get_initials(rest, True, acc)
        _ -> {
          case is_start {
            True -> do_get_initials(rest, False, [char, ..acc])
            False -> do_get_initials(rest, False, acc)
          }
        }
      }
    }
  }
}
