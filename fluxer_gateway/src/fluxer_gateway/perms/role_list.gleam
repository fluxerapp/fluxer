// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/member.{type Member}
import gleam/list

pub type RoleList {
  Roles(ids: List(Int))
  Malformed
}

pub fn of_member(source: Member) -> RoleList {
  Roles(list.map(member.roles(source), id.role_id_to_int))
}
