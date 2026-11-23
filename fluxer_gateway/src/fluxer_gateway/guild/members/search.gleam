// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type RoleId, type UserId}
import fluxer_gateway/core/member
import fluxer_gateway/core/user
import fluxer_gateway/guild/inbox.{type MentionPage}
import fluxer_gateway/guild/members.{type MembersTable}
import gleam/bool
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type Entry {
  Entry(user: Int, direct: Bool, role: Bool, everyone: Bool)
}

pub type Page {
  Page(entries: List(Entry), next_cursor: Option(Int))
}

pub fn page(
  table: MembersTable,
  request: MentionPage,
  connected: Set(Int),
  universe: Option(Set(Int)),
  can_view: fn(UserId, ChannelId) -> Bool,
) -> Page {
  let roles = request.roles
  let direct =
    request.users
    |> list.map(id.user_id_to_int)
    |> set.from_list
  let here = case request.mention_here, universe {
    True, Some(thread_members) -> set.intersection(connected, thread_members)
    True, None -> connected
    False, _ -> set.new()
  }
  let mentioned =
    here
    |> set.union(set.from_list(members.with_any_role(table, roles)))
    |> set.union(direct)
  let candidates = case request.mention_everyone, universe {
    True, None -> members.sorted_user_ints(table)
    True, Some(thread_members) ->
      thread_members
      |> set.filter(fn(user) {
        id.user_id(user)
        |> result.map(members.has(table, _))
        |> result.unwrap(False)
      })
      |> set.union(mentioned)
      |> set.to_list
      |> list.sort(int.compare)
    False, _ -> mentioned |> set.to_list |> list.sort(int.compare)
  }
  let after_cursor = case request.cursor {
    Some(cursor) -> list.drop_while(candidates, fn(user) { user <= cursor })
    None -> candidates
  }
  take(
    after_cursor,
    request.limit,
    fn(user_int) {
      entry(table, request, user_int, direct, connected, can_view)
    },
    [],
  )
}

fn take(
  candidates: List(Int),
  limit: Int,
  build: fn(Int) -> Option(Entry),
  acc: List(Entry),
) -> Page {
  case candidates, limit {
    [], _ -> Page(list.reverse(acc), None)
    _, 0 -> Page(list.reverse(acc), None)
    [user, ..rest], _ ->
      case build(user) {
        None -> take(rest, limit, build, acc)
        Some(found) ->
          case limit {
            1 ->
              Page(list.reverse([found, ..acc]), case rest {
                [] -> None
                _ -> Some(user)
              })
            _ -> take(rest, limit - 1, build, [found, ..acc])
          }
      }
  }
}

fn entry(
  table: MembersTable,
  request: MentionPage,
  user_int: Int,
  direct: Set(Int),
  connected: Set(Int),
  can_view: fn(UserId, ChannelId) -> Bool,
) -> Option(Entry) {
  {
    use user_id <- result.try(id.user_id(user_int) |> result.replace_error(Nil))
    use <- bool.guard(user_id == request.author, Error(Nil))
    use row <- result.try(members.get(table, user_id) |> option.to_result(Nil))
    use <- bool.guard(
      user.is_bot(member.user(row)) || !can_view(user_id, request.channel),
      Error(Nil),
    )
    let is_direct = set.contains(direct, user_int)
    let by_role = has_any_role(member.roles(row), request.roles)
    let everyone =
      request.mention_everyone
      || { request.mention_here && set.contains(connected, user_int) }
    case is_direct || by_role || everyone {
      True ->
        Ok(Entry(user: user_int, direct: is_direct, role: by_role, everyone:))
      False -> Error(Nil)
    }
  }
  |> option.from_result
}

fn has_any_role(held: List(RoleId), wanted: List(RoleId)) -> Bool {
  list.any(held, fn(role_id) { list.contains(wanted, role_id) })
}

pub fn to_json(result: Page) -> Json {
  json.object([
    #(
      "mentions",
      json.array(result.entries, fn(found) {
        json.object([
          #("user_id", id.json(found.user)),
          #("direct", json.bool(found.direct)),
          #("role", json.bool(found.role)),
          #("everyone", json.bool(found.everyone)),
        ])
      }),
    ),
    #("next_cursor", case result.next_cursor {
      Some(cursor) -> id.json(cursor)
      None -> json.null()
    }),
  ])
}
