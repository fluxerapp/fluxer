// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/member
import fluxer_gateway/guild/dispatch/event
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import gleam/bool
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn extract_session(
  state: GuildState,
  event_name: String,
  data: JsonTree,
) -> #(Option(String), JsonTree) {
  use <- bool.guard(!event.is_reaction(event_name), #(None, data))
  case json_tree.field(data, "session_id") {
    Ok(json_tree.String(key)) -> {
      let clean = json_tree.remove(data, "session_id") |> result.unwrap(data)
      let owned = case
        table.get_key(guild_state.sessions(state), key),
        json_tree.snowflake_field(data, "user_id")
      {
        Some(found), Ok(user) -> entry.user_int(found) == user
        _, _ -> False
      }
      case owned {
        True -> #(Some(key), clean)
        False -> #(None, clean)
      }
    }
    _ -> #(None, data)
  }
}

pub fn member(
  state: GuildState,
  event_name: String,
  data: JsonTree,
) -> JsonTree {
  {
    use user_int <- result.try(
      event.member_user(event_name, data) |> option.to_result(Nil),
    )
    use user <- result.try(id.user_id(user_int) |> result.replace_error(Nil))
    use found <- result.try(
      guild_state.member(state, user) |> option.to_result(Nil),
    )
    let wire = json_tree.Raw(member.wire(found))
    case event.member_placement(event_name) {
      event.MessagePlacement ->
        json_tree.remove(wire, "user")
        |> result.try(json_tree.set(data, "member", _))
      event.UserPlacement -> json_tree.set(data, "member", wire)
      event.NoPlacement -> Ok(data)
    }
  }
  |> result.unwrap(data)
}
