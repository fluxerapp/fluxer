// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/settings.{type ThreadMember}
import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option}

const max_role_mention_thread_adds = 250

const max_thread_members = 1000

pub type ThreadScope {
  ThreadScope(
    thread: Int,
    parent: Int,
    name: Option(String),
    private: Bool,
    archived: Bool,
    members: Dict(Int, ThreadMember),
  )
}

pub fn is_member(scope: ThreadScope, user: Int) -> Bool {
  dict.has_key(scope.members, user)
}

pub fn role_add_room(scope: ThreadScope) -> Int {
  case scope.private || scope.archived {
    True -> 0
    False ->
      int.clamp(
        max_thread_members - dict.size(scope.members),
        min: 0,
        max: max_role_mention_thread_adds,
      )
  }
}
