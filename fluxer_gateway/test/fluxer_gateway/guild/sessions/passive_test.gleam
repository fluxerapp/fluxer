// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/session.{SessionRef}
import fluxer_gateway/guild/dispatch/event
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/sessions/passive
import gleam/erlang/process
import gleam/option.{None, Some}

fn passive_entry(user: Int, bot: Bool) -> entry.Entry {
  let assert Ok(user_id) = id.user_id(user)
  entry.new(
    ref: SessionRef(
      id: session.new_session_id(),
      user: user_id,
      inbox: process.new_subject(),
    ),
    pid: process.self(),
    monitor: process.monitor(process.self()),
    flags: session.no_flags(),
    bot:,
    staff: None,
    mode: entry.Passive,
  )
}

fn no_roles() -> List(Int) {
  []
}

pub fn passive_session_skips_plain_messages_test() {
  let found = passive_entry(1, False)
  let data = json_tree.Object([#("content", json_tree.String("hi"))])
  assert !passive.should_receive(
    found,
    "MESSAGE_CREATE",
    event.facts("MESSAGE_CREATE", data),
    False,
    no_roles,
  )
  assert passive.should_receive(
    found,
    "MESSAGE_CREATE",
    event.facts("MESSAGE_CREATE", data),
    True,
    no_roles,
  )
  assert passive.should_receive(
    found,
    "CHANNEL_UPDATE",
    event.facts("CHANNEL_UPDATE", data),
    False,
    no_roles,
  )
}

pub fn passive_session_gets_mentions_and_own_messages_test() {
  let found = passive_entry(1, False)
  let mention =
    json_tree.Object([
      #(
        "mentions",
        json_tree.Array([json_tree.Object([#("id", json_tree.String("1"))])]),
      ),
    ])
  assert passive.should_receive(
    found,
    "MESSAGE_CREATE",
    event.facts("MESSAGE_CREATE", mention),
    False,
    no_roles,
  )
  let own =
    json_tree.Object([
      #("author", json_tree.Object([#("id", json_tree.String("1"))])),
    ])
  assert passive.should_receive(
    found,
    "MESSAGE_CREATE",
    event.facts("MESSAGE_CREATE", own),
    False,
    no_roles,
  )
  let role =
    json_tree.Object([
      #("mention_roles", json_tree.Array([json_tree.String("9")])),
    ])
  assert passive.should_receive(
    found,
    "MESSAGE_UPDATE",
    event.facts("MESSAGE_UPDATE", role),
    False,
    fn() { [9] },
  )
  let everyone_here =
    json_tree.Object([
      #("mention_everyone", json_tree.Bool(True)),
      #("mention_here", json_tree.Bool(False)),
    ])
  assert passive.should_receive(
    found,
    "MESSAGE_CREATE",
    event.facts("MESSAGE_CREATE", everyone_here),
    False,
    no_roles,
  )
}

pub fn bots_and_typing_override_test() {
  let bot = passive_entry(1, True)
  let data = json_tree.Object([])
  assert passive.should_receive(
    bot,
    "MESSAGE_CREATE",
    event.facts("MESSAGE_CREATE", data),
    False,
    no_roles,
  )
  let found = passive_entry(1, False)
  assert !passive.should_receive(
    found,
    "TYPING_START",
    event.facts("TYPING_START", data),
    False,
    no_roles,
  )
  let overridden = entry.set_typing(found, Some(True))
  assert passive.should_receive(
    overridden,
    "TYPING_START",
    event.facts("TYPING_START", data),
    False,
    no_roles,
  )
}
