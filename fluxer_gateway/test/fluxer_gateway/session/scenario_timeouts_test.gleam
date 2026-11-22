// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, type SessionDurations, Durations, GatewayDurations,
  SessionDurations,
}
import fluxer_gateway/core/presence
import fluxer_gateway/core/session.{
  ResumeUnavailable, SessionGone, StartUnavailable,
}
import fluxer_gateway/session/manager/registry
import gleam/erlang/process
import gleam/json
import gleeunit/should
import support/isolation
import support/session_fake_guild as fake_guild
import support/session_harness.{Frame} as h

fn timers(resume_ms: Int, change: fn(SessionDurations) -> SessionDurations) {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      gateway: GatewayDurations(
        ..d.gateway,
        limits_resume_timeout_ms: resume_ms,
      ),
      session: change(d.session),
    )
  })
}

fn api(user: Int) -> h.Responder {
  h.session_api(h.session_data(user, False, []))
}

pub fn detached_session_goes_offline_then_expires_on_its_own_test() {
  timers(400, fn(s) { SessionDurations(..s, server_offline_grace_ms: 100) })
  let user = h.unique_user()
  use <- h.with_api(api(user))
  let #(ref, socket, ready_seq) = h.ready_session("tok-t1")
  let pid = h.session_pid(ref)
  let dropped = h.now_ms()
  h.drop_socket(socket)
  h.wait_until(fn() { h.state_of(ref).status == presence.Offline }, 1000)
  |> should.be_true
  h.await_exit(pid, 2000) |> should.be_true
  { h.now_ms() - dropped >= 350 } |> should.be_true
  h.wait_until(fn() { h.user_sessions(user) == 0 }, 1000) |> should.be_true
  h.resume(ref, "tok-t1", ready_seq, h.fake_socket())
  |> should.equal(Error(SessionGone))
}

pub fn resume_inside_the_window_cancels_the_expiry_test() {
  timers(500, fn(s) { SessionDurations(..s, server_offline_grace_ms: 100) })
  use <- h.with_api(api(h.unique_user()))
  let #(ref, socket, ready_seq) = h.ready_session("tok-t2")
  let pid = h.session_pid(ref)
  h.drop_socket(socket)
  h.wait_until(fn() { h.state_of(ref).status == presence.Offline }, 1000)
  |> should.be_true
  let fresh = h.fake_socket()
  h.resume(ref, "tok-t2", ready_seq, fresh) |> should.equal(Ok(ref))
  h.state_of(ref).status |> should.equal(presence.Online)
  process.sleep(700)
  process.is_alive(pid) |> should.be_true
  h.terminate(ref)
}

pub fn hung_session_api_fails_the_start_at_the_fetch_deadline_test() {
  timers(60_000, fn(s) { SessionDurations(..s, shard_fetch_timeout_ms: 300) })
  let user = h.unique_user()
  let responder = fn(kind, _body) {
    case kind {
      "session" -> {
        process.sleep(3000)
        h.ok_data(h.session_data(user, False, []))
      }
      _ -> h.ok_data(json.object([]))
    }
  }
  use <- h.with_api(responder)
  let started = h.now_ms()
  let assert Error(StartUnavailable(_)) =
    h.start_session(h.identify("tok-t3"), h.fake_socket())
  { h.now_ms() - started < 2000 } |> should.be_true
  h.user_sessions(user) |> should.equal(0)
}

pub fn busy_session_answers_resume_unavailable_at_the_deadline_test() {
  timers(60_000, fn(s) {
    SessionDurations(..s, shard_remote_resume_timeout_ms: 300)
  })
  use <- h.with_api(api(h.unique_user()))
  let #(ref, socket, ready_seq) = h.ready_session("tok-t4")
  let pid = h.session_pid(ref)
  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  h.suspend(pid)
  let started = h.now_ms()
  let assert Error(ResumeUnavailable(_)) =
    h.resume(ref, "tok-t4", ready_seq, h.fake_socket())
  { h.now_ms() - started < 2000 } |> should.be_true
  h.resume_process(pid)
  let fresh = h.fake_socket()
  h.resume(ref, "tok-t4", ready_seq, fresh) |> should.equal(Ok(ref))
  h.await_event(fresh, "RESUMED", 2000) |> should.be_ok
  h.terminate(ref)
}

pub fn slow_guild_misses_ready_and_arrives_as_guild_create_test() {
  timers(60_000, fn(s) {
    SessionDurations(
      ..s,
      server_ready_deadline_ms: 300,
      guilds_first_join_floor_ms: 300,
      guilds_join_grace_ms: 200,
    )
  })
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Flaky(700, 1))
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let started = h.now_ms()
  let assert Ok(ref) = h.start_session(h.identify("tok-t5"), socket)
  let assert Ok(Frame(ready, "READY", _)) = h.await_event(socket, "READY", 3000)
  { h.now_ms() - started < 1500 } |> should.be_true
  let assert Ok(Frame(created, "GUILD_CREATE", _)) =
    h.await_event(socket, "GUILD_CREATE", 3000)
  { created > ready } |> should.be_true
  h.join_quota(guild) |> should.equal(0)
  h.terminate(ref)
  h.wait_until(
    fn() { registry.lookup(session.session_id_to_string(ref.id)) == Error(Nil) },
    2000,
  )
  |> should.be_true
  fake_guild.crash(fake)
}
