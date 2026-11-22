// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/session/presence_view.{type PresenceView}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleeunit/should

fn wire(guild: Option(Int), user: Int, status: String, afk: Bool) -> RawJson {
  let scope = case guild {
    Some(guild) -> "\"guild_id\":\"" <> int.to_string(guild) <> "\","
    None -> ""
  }
  let afk_text = case afk {
    True -> "true"
    False -> "false"
  }
  raw_json.wrap(<<
    "{":utf8,
    scope:utf8,
    "\"user\":{\"id\":\"":utf8,
    int.to_string(user):utf8,
    "\"},\"status\":\"":utf8,
    status:utf8,
    "\",\"afk\":":utf8,
    afk_text:utf8,
    ",\"mobile\":false,\"custom_status\":null}":utf8,
  >>)
}

fn reordered(user: Int, status: String) -> RawJson {
  raw_json.wrap(<<
    "{\"custom_status\":null,\"mobile\":false,\"afk\":false,\"status\":\"":utf8,
    status:utf8,
    "\",\"user\":{\"id\":\"":utf8,
    int.to_string(user):utf8,
    "\"}}":utf8,
  >>)
}

fn shown(view: PresenceView, payload: RawJson) -> #(PresenceView, Bool) {
  case
    presence_view.admit(view, dispatch_facts.scan("PRESENCE_UPDATE", payload))
  {
    Ok(next) -> #(next, True)
    Error(Nil) -> #(view, False)
  }
}

fn observe(
  view: PresenceView,
  event: String,
  payload: RawJson,
) -> PresenceView {
  presence_view.observe(view, event, dispatch_facts.scan(event, payload))
}

fn guild_event(id: Int, unavailable: Bool, members: List(Int)) -> RawJson {
  let members_text =
    members
    |> list_text(fn(user) {
      "{\"user\":{\"id\":\"" <> int.to_string(user) <> "\"}}"
    })
  raw_json.wrap(<<
    "{\"id\":\"":utf8,
    int.to_string(id):utf8,
    "\",\"unavailable\":":utf8,
    bool_text(unavailable):utf8,
    ",\"members\":[":utf8,
    members_text:utf8,
    "]}":utf8,
  >>)
}

fn bool_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn list_text(items: List(Int), render: fn(Int) -> String) -> String {
  case items {
    [] -> ""
    [only] -> render(only)
    [first, ..rest] -> render(first) <> "," <> list_text(rest, render)
  }
}

pub fn ready_content_is_not_repeated_in_any_key_order_test() {
  let view =
    presence_view.from_ready([wire(None, 7, "online", False)], [], set.new())
  shown(view, reordered(7, "online")).1 |> should.be_false
  shown(view, wire(None, 7, "idle", False)).1 |> should.be_true
  shown(view, wire(None, 7, "online", True)).1 |> should.be_true
  shown(view, wire(None, 8, "online", False)).1 |> should.be_true
}

pub fn a_new_guild_scope_is_shown_once_test() {
  let view =
    presence_view.from_ready([wire(None, 7, "online", False)], [], set.new())
  let #(view, first) = shown(view, wire(Some(40), 7, "online", False))
  first |> should.be_true
  shown(view, wire(Some(40), 7, "online", False)).1 |> should.be_false
  shown(view, wire(None, 7, "online", False)).1 |> should.be_false
}

pub fn ready_guild_members_already_carry_the_guild_scope_test() {
  let view =
    presence_view.from_ready(
      [wire(None, 7, "online", False)],
      [#(40, [7, 9])],
      set.from_list([7]),
    )
  shown(view, wire(Some(40), 7, "online", False)).1 |> should.be_false
  shown(view, wire(Some(41), 7, "online", False)).1 |> should.be_true
  shown(view, wire(Some(40), 9, "online", False)).1 |> should.be_true
}

pub fn the_last_shown_content_is_what_counts_test() {
  let view =
    presence_view.from_ready([wire(None, 7, "online", False)], [], set.new())
  let #(view, _) = shown(view, wire(None, 7, "idle", False))
  let #(view, back) = shown(view, wire(None, 7, "online", False))
  back |> should.be_true
  shown(view, wire(None, 7, "online", False)).1 |> should.be_false
}

pub fn a_direct_offline_update_is_never_swallowed_test() {
  let view = presence_view.new()
  let #(view, first) = shown(view, wire(None, 7, "offline", False))
  first |> should.be_true
  shown(view, wire(None, 7, "offline", False)).1 |> should.be_true
  let #(view, _) = shown(view, wire(Some(40), 7, "offline", False))
  shown(view, wire(Some(40), 7, "offline", False)).1 |> should.be_false
}

pub fn guild_events_change_the_scopes_the_client_holds_test() {
  let base =
    presence_view.from_ready([wire(None, 7, "online", False)], [], set.new())
  let created = observe(base, "GUILD_CREATE", guild_event(40, False, [7]))
  shown(created, wire(Some(40), 7, "online", False)).1 |> should.be_false
  let deleted =
    observe(
      created,
      "GUILD_DELETE",
      raw_json.wrap(<<"{\"id\":\"40\",\"unavailable\":true}":utf8>>),
    )
  shown(deleted, wire(Some(40), 7, "online", False)).1 |> should.be_true
  shown(deleted, wire(None, 7, "online", False)).1 |> should.be_false
  let unavailable = observe(created, "GUILD_CREATE", guild_event(40, True, []))
  shown(unavailable, wire(Some(40), 7, "online", False)).1 |> should.be_true
  let removed =
    observe(
      created,
      "GUILD_MEMBER_REMOVE",
      raw_json.wrap(<<"{\"guild_id\":\"40\",\"user\":{\"id\":\"7\"}}":utf8>>),
    )
  shown(removed, wire(Some(40), 7, "online", False)).1 |> should.be_true
}

pub fn losing_every_scope_forgets_the_user_test() {
  let view = presence_view.from_ready([], [], set.new())
  let #(view, _) = shown(view, wire(Some(40), 7, "online", False))
  let gone =
    observe(view, "GUILD_DELETE", raw_json.wrap(<<"{\"id\":\"40\"}":utf8>>))
  let #(_, after) = shown(gone, wire(Some(41), 7, "online", False))
  after |> should.be_true
  presence_view.admit(
    gone,
    dispatch_facts.scan("PRESENCE_UPDATE", raw_json.wrap(<<"not json":utf8>>)),
  )
  |> result.is_ok
  |> should.be_true
}

pub fn a_guild_member_outside_me_context_holds_only_guild_scopes_test() {
  let view =
    presence_view.from_ready(
      [wire(None, 7, "online", False)],
      [#(40, [7])],
      set.new(),
    )
  shown(view, wire(Some(40), 7, "online", False)).1 |> should.be_false
  shown(view, wire(None, 7, "online", False)).1 |> should.be_true
  let gone =
    observe(
      view,
      "GUILD_DELETE",
      raw_json.wrap(<<"{\"id\":\"40\",\"unavailable\":true}":utf8>>),
    )
  let back = observe(gone, "GUILD_CREATE", guild_event(40, False, [7]))
  shown(back, wire(None, 7, "online", False)).1 |> should.be_true
  shown(back, wire(Some(40), 7, "online", False)).1 |> should.be_true
  let removed =
    observe(
      view,
      "GUILD_MEMBER_REMOVE",
      raw_json.wrap(<<"{\"guild_id\":\"40\",\"user\":{\"id\":\"7\"}}":utf8>>),
    )
  shown(removed, wire(None, 7, "online", False)).1 |> should.be_true
}

pub fn a_me_context_guild_member_keeps_the_direct_scope_test() {
  let view =
    presence_view.from_ready(
      [wire(None, 7, "online", False)],
      [#(40, [7])],
      set.from_list([7]),
    )
  shown(view, wire(None, 7, "online", False)).1 |> should.be_false
  let gone =
    observe(
      view,
      "GUILD_DELETE",
      raw_json.wrap(<<"{\"id\":\"40\",\"unavailable\":true}":utf8>>),
    )
  shown(gone, wire(None, 7, "online", False)).1 |> should.be_false
}

fn quoted_ids(user: Int, emoji: Int) -> RawJson {
  raw_json.wrap(<<
    "{\"afk\":false,\"custom_status\":{\"emoji_id\":\"":utf8,
    int.to_string(emoji):utf8,
    "\",\"emoji_name\":\"wave\",\"text\":\"hi\"},\"mobile\":false,\"status\":\"online\",\"user\":{\"id\":\"":utf8,
    int.to_string(user):utf8,
    "\",\"username\":\"scenario\"}}":utf8,
  >>)
}

fn bare_ids(user: Int, emoji: Int) -> RawJson {
  raw_json.wrap(<<
    "{\"user\":{\"username\":\"scenario\",\"id\":":utf8,
    int.to_string(user):utf8,
    "},\"status\":\"online\",\"mobile\":false,\"custom_status\":{\"text\":\"hi\",\"emoji_name\":\"wave\",\"emoji_id\":":utf8,
    int.to_string(emoji):utf8,
    "},\"afk\":false}":utf8,
  >>)
}

pub fn the_same_presence_with_bare_or_quoted_ids_is_shown_once_test() {
  let user = 1_234_567_890_123_456_789
  let emoji = 1_111_111_111_111_111_111
  let #(view, first) = shown(presence_view.new(), quoted_ids(user, emoji))
  first |> should.be_true
  shown(view, bare_ids(user, emoji)).1 |> should.be_false
  let #(view, first) = shown(presence_view.new(), bare_ids(user, emoji))
  first |> should.be_true
  shown(view, quoted_ids(user, emoji)).1 |> should.be_false
  let ready = presence_view.from_ready([bare_ids(user, emoji)], [], set.new())
  shown(ready, quoted_ids(user, emoji)).1 |> should.be_false
  shown(ready, quoted_ids(user, emoji + 1)).1 |> should.be_true
}
