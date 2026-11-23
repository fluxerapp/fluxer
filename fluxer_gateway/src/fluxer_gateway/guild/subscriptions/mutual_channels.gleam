// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import gleam/int
import gleam/list
import gleam/result
import gleam/set

pub fn filter(
  state: GuildState,
  viewer: UserId,
  members: List(Int),
) -> List(Int) {
  let own = id.user_id_to_int(viewer)
  let wanted = list.filter(members, fn(user) { user != own })
  case wanted {
    [] -> []
    _ -> {
      let candidates =
        wanted
        |> list.unique
        |> list.sort(int.compare)
        |> packed_ids.from_list
      perms.shares_view_many(
        guild_state.perms_reader(state),
        viewer,
        candidates,
      )
      |> result.map(fn(shared) {
        let kept = set.from_list(packed_ids.to_list(shared))
        list.filter(wanted, set.contains(kept, _))
      })
      |> result.unwrap([])
    }
  }
}
