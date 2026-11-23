// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/gateway/abuse_protection
import fluxer_gateway/session/control
import fluxer_gateway/session/manager
import fluxer_gateway/session/manager/registry
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleeunit/should
import support/session_fake_guild as fake_guild
import support/session_harness.{Frame} as h

pub fn identify_reaches_ready_test() {
  let user = h.unique_user()
  use <- h.with_api(h.session_api(h.session_data(user, False, [])))
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-start"), socket)
  let assert Ok(Frame(_, "READY", payload)) =
    h.await_event(socket, "READY", 8000)
  decode.run(payload, decode.at(["session_id"], decode.string))
  |> should.equal(Ok(session.session_id_to_string(ref.id)))
  decode.run(payload, decode.at(["user", "id"], decode.string))
  |> should.equal(Ok(int.to_string(user)))
  registry.lookup(session.session_id_to_string(ref.id)) |> should.equal(Ok(ref))
  h.terminate(ref)
}

pub fn identify_with_guild_collects_it_into_ready_test() {
  let user = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  use <- h.with_api(h.session_api(h.session_data(user, False, [guild])))
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-guild"), socket)
  let assert Ok(Frame(_, "READY", payload)) =
    h.await_event(socket, "READY", 8000)
  decode.run(
    payload,
    decode.at(["guilds"], decode.list(decode.at(["id"], decode.string))),
  )
  |> should.equal(Ok([int.to_string(guild)]))
  h.terminate(ref)
  fake_guild.crash(fake)
}

pub fn rejected_token_fails_start_test() {
  use <- h.with_api(fn(kind, _) {
    case kind {
      "session" -> h.Reply(401, "{}")
      _ -> h.ok_data(json.object([]))
    }
  })
  let socket = h.fake_socket()
  h.start_session(h.identify("bad"), socket)
  |> should.equal(Error(session.StartAuthFailed))
}

fn crowd(user: Int, count: Int, run: fn() -> a) -> a {
  let assert Ok(user_id) = id.user_id(user)
  list.each(list.repeat(Nil, count), fn(_) {
    abuse_protection.increment_user_sessions(user_id)
  })
  let outcome = run()
  list.each(list.repeat(Nil, count), fn(_) {
    abuse_protection.decrement_user_sessions(user_id)
  })
  outcome
}

pub fn session_cap_is_freed_by_ended_and_crashed_sessions_test() {
  let user = h.unique_user()
  use <- crowd(user, 99)
  use <- h.with_api(h.session_api(h.session_data(user, False, [])))
  let #(first, _, _) = h.ready_session("tok-cap")
  h.start_session(h.identify("tok-cap"), h.fake_socket())
  |> should.equal(Error(session.StartTooManySessions))
  h.terminate(first)
  let #(second, _, _) = h.ready_session("tok-cap")
  let pid = h.session_pid(second)
  process.kill(pid)
  h.await_exit(pid, 1000) |> should.be_true
  h.wait_until(
    fn() {
      registry.lookup(session.session_id_to_string(second.id)) == Error(Nil)
    },
    1000,
  )
  |> should.be_true
  let #(third, _, _) = h.ready_session("tok-cap")
  h.terminate(third)
}

pub fn socket_gone_during_fetch_starts_nothing_test() {
  let user = h.unique_user()
  use <- crowd(user, 99)
  use <- h.with_api(fn(kind, body) {
    case kind {
      "session" -> process.sleep(300)
      _ -> Nil
    }
    h.session_api(h.session_data(user, False, []))(kind, body)
  })
  let socket = h.fake_socket()
  let reply = process.new_subject()
  manager.start(
    h.identify("tok-gone"),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    socket.ref,
    reply,
  )
  process.sleep(50)
  h.drop_socket(socket)
  process.receive(reply, 800) |> should.equal(Error(Nil))
  let #(ref, _, _) = h.ready_session("tok-gone")
  h.terminate(ref)
}

pub fn api_outage_reports_unavailable_test() {
  use <- h.with_api(fn(_, _) { h.Reply(503, "{}") })
  let assert Error(session.StartUnavailable(_)) =
    h.start_session(h.identify("tok-503"), h.fake_socket())
}

pub fn terminate_matching_ends_only_the_matching_session_test() {
  let user = h.unique_user()
  use <- h.with_api(
    h.session_api(
      h.session_data_with(user, False, [], [
        #("auth_session_id_hash", json.string("YWJj")),
      ]),
    ),
  )
  let #(ref, _, _) = h.ready_session("tok-term")
  let pid = h.session_pid(ref)
  let reply = process.new_subject()
  h.control(ref, control.TerminateMatching(["eHl6", ""], reply))
  process.receive(reply, 1000) |> should.equal(Ok(False))
  process.is_alive(pid) |> should.be_true
  h.control(ref, control.TerminateMatching(["YWJj"], reply))
  process.receive(reply, 1000) |> should.equal(Ok(True))
  h.await_exit(pid, 1000) |> should.be_true
}

pub fn terminate_matching_spares_session_without_hash_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, _, _) = h.ready_session("tok-nohash")
  let reply = process.new_subject()
  h.control(ref, control.TerminateMatching([""], reply))
  process.receive(reply, 1000) |> should.equal(Ok(False))
  process.is_alive(h.session_pid(ref)) |> should.be_true
  h.terminate(ref)
}
