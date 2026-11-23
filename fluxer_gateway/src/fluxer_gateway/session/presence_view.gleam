// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{
  type DispatchFacts, Guild, MemberRemove, Presence,
}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}

const direct = 0

pub opaque type PresenceView {
  PresenceView(known: Dict(Int, Known))
}

type Known {
  Known(content: BitArray, scopes: Set(Int))
}

type Update {
  Update(user: Int, scope: Int, content: BitArray, online: Bool)
}

pub fn new() -> PresenceView {
  PresenceView(known: dict.new())
}

pub fn from_ready(
  presences: List(RawJson),
  guild_members: List(#(Int, List(Int))),
  me_context: Set(Int),
) -> PresenceView {
  let in_guild =
    list.fold(guild_members, set.new(), fn(acc, entry) {
      set.union(acc, set.from_list(entry.1))
    })
  let view =
    list.fold(presences, new(), fn(view, payload) {
      case update(dispatch_facts.scan("PRESENCE_UPDATE", payload)) {
        Ok(update) -> {
          let scopes = case
            set.contains(me_context, update.user)
            || !set.contains(in_guild, update.user)
          {
            True -> set.from_list([direct])
            False -> set.new()
          }
          PresenceView(known: dict.insert(
            view.known,
            update.user,
            Known(content: update.content, scopes:),
          ))
        }
        Error(Nil) -> view
      }
    })
  list.fold(guild_members, view, fn(view, entry) {
    add_scope(view, entry.0, entry.1)
  })
}

pub fn admit(
  view: PresenceView,
  facts: DispatchFacts,
) -> Result(PresenceView, Nil) {
  case update(facts) {
    Error(Nil) -> Ok(view)
    Ok(update) ->
      case dict.get(view.known, update.user) {
        Ok(known) ->
          case already_shown(known, update) {
            True -> Error(Nil)
            False -> Ok(apply_known(view, known, update))
          }
        Error(Nil) ->
          Ok(
            PresenceView(known: dict.insert(
              view.known,
              update.user,
              Known(
                content: update.content,
                scopes: set.from_list([update.scope]),
              ),
            )),
          )
      }
  }
}

fn already_shown(known: Known, update: Update) -> Bool {
  known.content == update.content
  && set.contains(known.scopes, update.scope)
  && { update.online || update.scope != direct }
}

fn apply_known(
  view: PresenceView,
  known: Known,
  update: Update,
) -> PresenceView {
  let scopes = set.insert(known.scopes, update.scope)
  let scopes = case update.online, update.scope == direct {
    False, True -> set.delete(scopes, direct)
    _, _ -> scopes
  }
  case set.is_empty(scopes) {
    True -> PresenceView(known: dict.delete(view.known, update.user))
    False ->
      PresenceView(known: dict.insert(
        view.known,
        update.user,
        Known(content: update.content, scopes:),
      ))
  }
}

pub fn observe(
  view: PresenceView,
  event: String,
  facts: DispatchFacts,
) -> PresenceView {
  case dict.is_empty(view.known), event, facts.id, facts.detail {
    True, _, _, _ -> view
    False, "GUILD_CREATE", Some(guild), Guild(unavailable: True, ..) ->
      drop_scope(view, guild)
    False, "GUILD_CREATE", Some(guild), Guild(unavailable: False, members:) ->
      add_scope(
        view,
        guild,
        option.map(members, dispatch_facts.member_ids) |> option.unwrap([]),
      )
    False, "GUILD_DELETE", Some(guild), _ -> drop_scope(view, guild)
    False, "GUILD_MEMBER_REMOVE", _, MemberRemove(user: Some(user)) ->
      case facts.guild_id {
        Some(guild) -> drop_user_scope(view, user, guild)
        None -> view
      }
    False, _, _, _ -> view
  }
}

fn add_scope(
  view: PresenceView,
  guild: Int,
  members: List(Int),
) -> PresenceView {
  list.fold(members, view, fn(view, user) {
    case dict.get(view.known, user) {
      Ok(known) ->
        PresenceView(known: dict.insert(
          view.known,
          user,
          Known(..known, scopes: set.insert(known.scopes, guild)),
        ))
      Error(Nil) -> view
    }
  })
}

fn drop_scope(view: PresenceView, guild: Int) -> PresenceView {
  PresenceView(
    known: dict.fold(view.known, dict.new(), fn(acc, user, known) {
      let scopes = set.delete(known.scopes, guild)
      case set.is_empty(scopes) {
        True -> acc
        False -> dict.insert(acc, user, Known(..known, scopes:))
      }
    }),
  )
}

fn drop_user_scope(view: PresenceView, user: Int, guild: Int) -> PresenceView {
  case dict.get(view.known, user) {
    Error(Nil) -> view
    Ok(known) -> {
      let scopes = set.delete(known.scopes, guild)
      case set.is_empty(scopes) {
        True -> PresenceView(known: dict.delete(view.known, user))
        False ->
          PresenceView(known: dict.insert(
            view.known,
            user,
            Known(..known, scopes:),
          ))
      }
    }
  }
}

fn update(facts: DispatchFacts) -> Result(Update, Nil) {
  case facts.detail {
    Presence(user: Some(user), online:, content:) ->
      Ok(Update(
        user:,
        scope: option.unwrap(facts.guild_id, direct),
        content:,
        online:,
      ))
    _ -> Error(Nil)
  }
}
