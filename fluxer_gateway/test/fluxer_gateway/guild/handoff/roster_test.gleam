// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/roster
import gleam/erlang/process
import gleam/list
import gleam/string
import support/guild_fixture as fixture

fn guild() -> id.GuildId {
  let assert Ok(guild) = id.guild_id(fixture.unique_guild_id())
  guild
}

fn sorted(sessions: List(session.SessionId)) -> List(String) {
  list.map(sessions, session.session_id_to_string)
  |> list.sort(string.compare)
}

fn put_from_other_owner(
  guild: id.GuildId,
  sessions: List(session.SessionId),
) -> process.Pid {
  let done = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      roster.put_many(guild, sessions)
      process.send(done, Nil)
      process.sleep_forever()
    })
  let assert Ok(Nil) = process.receive(done, 1000)
  pid
}

pub fn take_returns_the_owned_sessions_once_test() {
  let guild = guild()
  let first = session.new_session_id()
  let second = session.new_session_id()
  roster.put(guild, first)
  roster.put_many(guild, [second, first])
  let me = process.self()
  assert sorted(roster.take(guild, me)) == sorted([first, second])
  assert roster.take(guild, me) == []
}

pub fn removed_sessions_are_not_taken_test() {
  let guild = guild()
  let kept = session.new_session_id()
  let gone = session.new_session_id()
  roster.put_many(guild, [kept, gone])
  roster.remove(guild, gone)
  assert roster.take(guild, process.self()) == [kept]
}

pub fn rows_of_another_owner_survive_take_and_clear_test() {
  let guild = guild()
  let mine = session.new_session_id()
  let theirs = session.new_session_id()
  roster.put(guild, mine)
  let other = put_from_other_owner(guild, [theirs])
  roster.remove(guild, theirs)
  roster.clear(guild, process.self())
  assert roster.take(guild, process.self()) == []
  assert roster.take(guild, other) == [theirs]
  process.kill(other)
}

pub fn guilds_do_not_share_rows_test() {
  let one = guild()
  let two = guild()
  let first = session.new_session_id()
  let second = session.new_session_id()
  roster.put(one, first)
  roster.put(two, second)
  assert roster.take(one, process.self()) == [first]
  assert roster.take(two, process.self()) == [second]
}
