// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/guild.{
  type JoinReply, AsMember, Focused, JoinRequest, JoinUnavailable, Joined,
}
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/names
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/result
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

const session_grace_ms = 1000

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> decode.Dynamic

fn reductions(pid: Pid) -> Int {
  decode.run(
    process_info(pid, atom.create("reductions")),
    decode.at([1], decode.int),
  )
  |> result.unwrap(0)
}

fn slow_members(api: guild_api.FakeApi, found: fixture.Guild, ms: Int) -> Nil {
  guild_api.override(
    api,
    "guild_collection",
    Some("members"),
    guild_api.Delayed(
      ms,
      guild_api.Data(
        json.object([
          #(
            "members",
            json.preprocessed_array(list.map(found.members, fn(m) { m.1 })),
          ),
          #("has_more", json.bool(False)),
          #("next_after_user_id", json.null()),
        ]),
      ),
    ),
  )
}

fn join(
  found: fixture.Guild,
  session: fake.FakeSession,
  remaining_ms: Int,
  reply: process.Subject(JoinReply),
) -> Nil {
  let assert Ok(Nil) =
    manager.join(
      node.self(),
      fixture.guild_id(found),
      JoinRequest(
        ..fake.request(session, Focused, AsMember),
        remaining_ms:,
        attempt: id.user_id_to_int(session.ref.user),
      ),
      reply,
    )
  Nil
}

pub fn joins_waiting_on_a_slow_load_are_answered_before_the_session_gives_up_test() {
  use api <- guild_api.with_api
  let found =
    int.range(2, 42, fixture.new(1) |> fixture.with_text(10), fn(acc, user) {
      fixture.with_member(acc, user, [])
    })
  fixture.publish(api, found)
  slow_members(api, found, 3000)
  let short = list.map(int.range(2, 22, [], list.prepend), fake.start)
  let long = list.map(int.range(22, 42, [], list.prepend), fake.start)
  let reply = process.new_subject()
  let started = clock.monotonic_ms()
  list.each(short, join(found, _, 1000, reply))
  list.each(long, join(found, _, 10_000, reply))
  let answers =
    list.map(list.append(short, long), fn(_) {
      let assert Ok(answer) = process.receive(reply, 8000)
      #(answer, clock.monotonic_ms() - started)
    })
  assert list.all(answers, fn(pair) {
    case pair {
      #(JoinUnavailable(_, user), elapsed) ->
        user < 22 && elapsed < 1000 + session_grace_ms
      #(Joined(_, _, user), _) -> user >= 22
      _ -> False
    }
  })
  list.each(short, fn(session) {
    let retried = process.new_subject()
    join(found, session, 1000, retried)
    let assert Ok(Joined(..)) = process.receive(retried, 2000)
  })
  fixture.stop(found)
}

pub fn many_joins_waiting_on_a_slow_load_expire_at_linear_cost_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  slow_members(api, found, 2500)
  let session = fake.start(2)
  let reply = process.new_subject()
  let assert Ok(shard) =
    process.named(names.shard(names.shard_of(fixture.guild_id(found))))
  let joins = 3000
  let before = reductions(shard)
  let started = clock.monotonic_ms()
  int.range(0, joins, Nil, fn(_, n) {
    let assert Ok(Nil) =
      manager.join(
        node.self(),
        fixture.guild_id(found),
        JoinRequest(
          ..fake.request(session, Focused, AsMember),
          remaining_ms: 300 + n / 3,
          attempt: n,
        ),
        reply,
      )
    Nil
  })
  let late =
    int.range(0, joins, 0, fn(late, _) {
      let assert Ok(JoinUnavailable(_, n)) = process.receive(reply, 4000)
      case clock.monotonic_ms() - started > 300 + n / 3 + session_grace_ms {
        True -> late + 1
        False -> late
      }
    })
  let spent = reductions(shard) - before
  assert late == 0
  assert spent / joins < 1000
  fake.stop(session)
  fixture.stop(found)
}
