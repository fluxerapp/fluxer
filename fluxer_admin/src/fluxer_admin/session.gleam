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

import fluxer_admin/web.{type Context, type Session, Session}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/json

fn system_time_seconds() -> Int {
  do_system_time(atom.create("second"))
}

@external(erlang, "erlang", "system_time")
fn do_system_time(unit: Atom) -> Int

pub fn create(
  _ctx: Context,
  user_id: String,
  access_token: String,
) -> Result(String, Nil) {
  let now = system_time_seconds()
  let session =
    Session(user_id: user_id, access_token: access_token, created_at: now)

  let cookie_data =
    json.object([
      #("user_id", json.string(session.user_id)),
      #("access_token", json.string(session.access_token)),
      #("created_at", json.int(session.created_at)),
    ])
    |> json.to_string

  Ok(cookie_data)
}

pub fn get(_ctx: Context, cookie: String) -> Result(Session, Nil) {
  let decoder = {
    use user_id <- decode.field("user_id", decode.string)
    use access_token <- decode.field("access_token", decode.string)
    use created_at <- decode.field("created_at", decode.int)
    decode.success(Session(
      user_id: user_id,
      access_token: access_token,
      created_at: created_at,
    ))
  }

  case json.parse(cookie, decoder) {
    Ok(session) -> {
      case session.user_id == "" || session.access_token == "" {
        True -> Error(Nil)
        False -> {
          let now = system_time_seconds()
          let age = now - session.created_at
          let max_age = 60 * 60 * 24 * 7

          case age < max_age {
            True -> Ok(session)
            False -> Error(Nil)
          }
        }
      }
    }
    Error(_) -> Error(Nil)
  }
}
