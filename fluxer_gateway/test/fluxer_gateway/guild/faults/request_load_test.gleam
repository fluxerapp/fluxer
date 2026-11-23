// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

fn upto(from: Int, count: Int) -> List(Int) {
  int.range(from + count - 1, from - 1, [], fn(acc, value) { [value, ..acc] })
}

fn crowd(size: Int) -> fixture.Guild {
  list.fold(
    upto(2, size - 1),
    fixture.new(1) |> fixture.with_text(10),
    fn(acc, user) { fixture.with_member(acc, user, []) },
  )
}

fn ask_members(
  found: fake.FakeSession,
  handle: inbox.Handle,
  guild: fixture.Guild,
  fields: List(#(String, Json)),
) -> Nil {
  fake.op(
    found,
    handle,
    session.RequestGuildMembers(
      fixture.to_dynamic(
        json.object([
          #("guild_id", json.string(int.to_string(guild.id))),
          ..fields
        ]),
      ),
    ),
  )
}

fn chunks(found: fake.FakeSession, wait_ms: Int) -> List(Dynamic) {
  fake.dispatches(found, wait_ms)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "GUILD_MEMBERS_CHUNK" -> Ok(pair.1)
      _ -> Error(Nil)
    }
  })
}

fn member_ids(chunk: Dynamic) -> List(String) {
  decode.run(
    chunk,
    decode.at(
      ["members"],
      decode.list(decode.at(["user", "id"], decode.string)),
    ),
  )
  |> option.from_result
  |> option.unwrap([])
}

fn int_at(chunk: Dynamic, key: String) -> Option(Int) {
  decode.run(chunk, decode.at([key], decode.int)) |> option.from_result
}

pub fn concurrent_requests_are_answered_up_to_the_guild_window_test() {
  use api <- guild_api.with_api
  let found = crowd(60)
  let handle = fixture.started(api, found)
  let sessions = list.map(upto(2, 50), fake.start)
  list.each(sessions, fn(session) {
    assert fake.is_joined(fake.join(session, handle))
  })
  list.each(sessions, fn(session) { fake.drain(session, 0) })
  let done = process.new_subject()
  list.each(sessions, fn(session) {
    process.spawn_unlinked(fn() {
      let user = int.to_string(id.user_id_to_int(session.ref.user))
      ask_members(session, handle, found, [
        #("user_ids", json.array([user], json.string)),
        #("nonce", json.string("n" <> user)),
      ])
      process.send(done, Nil)
    })
  })
  list.each(sessions, fn(_) {
    let assert Ok(Nil) = process.receive(done, 5000)
  })
  fixture.sync(handle)
  let answered =
    list.map(sessions, fn(session) {
      let user = int.to_string(id.user_id_to_int(session.ref.user))
      let got = chunks(session, 100)
      list.each(got, fn(chunk) {
        assert fake.text_at(chunk, ["nonce"]) == Some("n" <> user)
        assert member_ids(chunk) == [user]
      })
      list.length(got)
    })
  assert list.all(answered, fn(count) { count <= 1 })
  assert int.sum(answered) == 40
  fixture.stop(found)
}

pub fn large_full_list_is_chunked_once_with_every_member_test() {
  use api <- guild_api.with_api
  let found = crowd(2500)
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  let _ = fake.drain(owner, 50)
  ask_members(owner, handle, found, [
    #("query", json.string("")),
    #("limit", json.int(0)),
    #("nonce", json.string("all")),
  ])
  fixture.sync(handle)
  let got = chunks(owner, 300)
  assert list.length(got) == 3
  assert list.map(got, int_at(_, "chunk_index")) == [Some(0), Some(1), Some(2)]
  assert list.all(got, fn(chunk) { int_at(chunk, "chunk_count") == Some(3) })
  assert list.all(got, fn(chunk) {
    fake.text_at(chunk, ["nonce"]) == Some("all")
  })
  let ids = list.flat_map(got, member_ids)
  assert list.length(ids) == 2500
  assert set.size(set.from_list(ids)) == 2500
  fixture.stop(found)
}

fn renamed(found: fixture.Guild, user: Int, nick: Option(String)) -> Json {
  json.object([
    #("guild_id", json.string(int.to_string(found.id))),
    #("user", fixture.user(user)),
    #("nick", json.nullable(nick, json.string)),
    #("roles", json.preprocessed_array([])),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

fn query_ids(
  owner: fake.FakeSession,
  handle: inbox.Handle,
  found: fixture.Guild,
  text: String,
) -> List(String) {
  ask_members(owner, handle, found, [
    #("query", json.string(text)),
    #("limit", json.int(100)),
  ])
  fixture.sync(handle)
  chunks(owner, 100)
  |> list.flat_map(member_ids)
  |> list.sort(fn(a, b) { int.compare(parse(a), parse(b)) })
}

fn parse(text: String) -> Int {
  case int.parse(text) {
    Ok(value) -> value
    Error(Nil) -> 0
  }
}

pub fn search_follows_member_edits_between_requests_test() {
  use api <- guild_api.with_api
  let found = crowd(30)
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  let _ = fake.drain(owner, 50)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    renamed(found, 7, Some("zebra")),
  )
  assert query_ids(owner, handle, found, "zeb") == ["7"]
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    renamed(found, 8, Some("zebu")),
  )
  fixture.dispatch(handle, "GUILD_MEMBER_UPDATE", renamed(found, 7, None))
  assert query_ids(owner, handle, found, "zeb") == ["8"]
  assert query_ids(owner, handle, found, "user7") == ["7"]
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([
      #("guild_id", json.string(int.to_string(found.id))),
      #("user", fixture.user(8)),
    ]),
  )
  assert query_ids(owner, handle, found, "zeb") == []
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_ADD",
    json.object([
      #("guild_id", json.string(int.to_string(found.id))),
      #("user", fixture.user(99)),
      #("nick", json.string("zeppelin")),
      #("roles", json.preprocessed_array([])),
      #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
    ]),
  )
  assert query_ids(owner, handle, found, "ze") == ["99"]
  fixture.stop(found)
}

pub fn channel_counts_requests_under_load_each_get_one_reply_test() {
  use api <- guild_api.with_api
  let found = crowd(40)
  let handle = fixture.started(api, found)
  let sessions = list.map(upto(1, 30), fake.start)
  list.each(sessions, fn(session) {
    assert fake.is_joined(fake.join(session, handle))
  })
  list.each(sessions, fn(session) { fake.drain(session, 0) })
  list.each(sessions, fn(session) {
    process.spawn_unlinked(fn() {
      list.each(upto(1, 5), fn(round) {
        fake.op(
          session,
          handle,
          session.RequestChannelMemberCounts(
            fixture.to_dynamic(
              json.object([
                #("guild_id", json.string(int.to_string(found.id))),
                #("channel_ids", json.array(["10"], json.string)),
                #("nonce", json.string("r" <> int.to_string(round))),
              ]),
            ),
          ),
        )
      })
    })
  })
  process.sleep(300)
  fixture.sync(handle)
  list.each(sessions, fn(session) {
    let replies =
      fake.dispatches(session, 100)
      |> list.filter(fn(pair) { pair.0 == "CHANNEL_MEMBER_COUNTS_UPDATE" })
    let nonces = list.map(replies, fn(pair) { fake.text_at(pair.1, ["nonce"]) })
    assert list.length(replies) == 5
    assert set.size(set.from_list(nonces)) == 5
    list.each(replies, fn(pair) {
      let counts =
        decode.run(
          pair.1,
          decode.at(
            ["counts"],
            decode.list({
              use members <- decode.field("member_count", decode.int)
              use online <- decode.field("online_count", decode.int)
              decode.success(#(members, online))
            }),
          ),
        )
      assert counts == Ok([#(40, 30)])
    })
  })
  fixture.stop(found)
}

pub fn join_storm_answers_every_session_once_test() {
  use api <- guild_api.with_api
  let found = crowd(300)
  let handle = fixture.started(api, found)
  let sessions = list.map(upto(1, 300), fake.start)
  let replies = process.new_subject()
  list.each(sessions, fn(session) {
    guild.send(
      handle.ref,
      guild.Join(fake.request(session, guild.Focused, guild.AsMember), replies),
    )
  })
  let answers =
    list.map(sessions, fn(_) {
      let assert Ok(answer) = process.receive(replies, 10_000)
      answer
    })
  assert process.receive(replies, 200) == Error(Nil)
  assert list.all(answers, fake.is_joined)
  let reply = process.new_subject()
  let assert Ok(owner) = id.user_id(1)
  guild.send(handle.ref, guild.CountsQuery(owner, reply))
  let assert Ok(counts) = process.receive(reply, 2000)
  assert counts.member_count == 300
  assert counts.online_count == 300
  fixture.stop(found)
}
