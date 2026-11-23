// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{
  type JoinClass, type JoinRequest, Background, Focused, JoinOverloaded,
  JoinRequest, Resume,
}
import fluxer_gateway/core/id
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/guild/join/admission
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}

fn request(class: JoinClass, remaining_ms: Int, attempt: Int) -> JoinRequest {
  let assert Ok(user) = id.user_id(42)
  JoinRequest(
    session: session.SessionRef(
      id: session.new_session_id(),
      user:,
      inbox: process.new_subject(),
    ),
    flags: session.no_flags(),
    class:,
    mode: guild.AsMember,
    presence: presence.offline(user, raw_json.null()),
    bot: False,
    staff: False,
    remaining_ms:,
    attempt:,
  )
}

fn no_jitter(value: Int, _percent: Int) -> Int {
  value
}

pub fn expired_request_is_dropped_and_answered_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let reply = process.new_subject()
  let #(gate, effects) =
    admission.offer(gate, request(Focused, 10, 1), reply, 1000)
  let assert [
    admission.Dropped(_, admission.Expired),
    admission.Reply(to, JoinOverloaded(_, 1)),
  ] = effects
  assert to == reply
  let assert 0 = admission.queued(gate)
}

pub fn request_that_expires_in_the_queue_is_answered_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let reply = process.new_subject()
  let assert #(gate, []) =
    admission.offer(gate, request(Focused, 1000, 7), reply, 0)
  let #(gate, batch, effects) = admission.next_batch(gate, 2000)
  let assert None = batch
  let assert [
    admission.Dropped(_, admission.Expired),
    admission.Reply(to, JoinOverloaded(_, 7)),
  ] = effects
  assert to == reply
  let assert 0 = admission.queued(gate)
}

pub fn undeliverable_member_of_a_batch_is_answered_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let reply = process.new_subject()
  let assert #(gate, []) =
    admission.offer(gate, request(Focused, 1000, 3), reply, 0)
  let assert #(gate, batch, []) = admission.next_batch(gate, 10)
  let assert Some(found) = batch
  let assert [waiting] = found.members
  let assert False = admission.deliverable(waiting, 2000)
  let #(_, effects) = admission.expire(gate, waiting, 2000)
  let assert [
    admission.Dropped(_, admission.Expired),
    admission.Reply(to, JoinOverloaded(_, 3)),
  ] = effects
  assert to == reply
}

pub fn classes_are_served_in_order_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let reply = process.new_subject()
  let #(gate, _) = admission.offer(gate, request(Background, 5000, 1), reply, 0)
  let #(gate, _) = admission.offer(gate, request(Resume, 5000, 1), reply, 0)
  let #(gate, _) = admission.offer(gate, request(Focused, 5000, 1), reply, 0)
  let #(gate, first, _) = admission.next_batch(gate, 1)
  let assert Some(batch) = first
  let assert Resume = batch.class
  let gate = admission.batch_done(gate, batch, 2)
  let #(gate, second, _) = admission.next_batch(gate, 2)
  let assert Some(next) = second
  let assert Focused = next.class
  let gate = admission.batch_done(gate, next, 3)
  let #(_, third, _) = admission.next_batch(gate, 3)
  let assert Some(last) = third
  let assert Background = last.class
}

pub fn stale_attempt_is_dropped_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let reply = process.new_subject()
  let old = request(Focused, 5000, 1)
  let newer = JoinRequest(..old, attempt: 2)
  let #(gate, _) = admission.offer(gate, old, reply, 0)
  let #(gate, _) = admission.offer(gate, newer, reply, 0)
  let #(_, batch, effects) = admission.next_batch(gate, 1)
  let assert Some(found) = batch
  let assert [served] = found.members
  let assert 2 = served.request.attempt
  let assert True =
    list.any(effects, fn(effect) {
      case effect {
        admission.Dropped(_, admission.Stale) -> True
        _ -> False
      }
    })
}

pub fn join_from_a_moved_session_with_a_lower_attempt_is_answered_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let old_reply = process.new_subject()
  let moved_reply = process.new_subject()
  let old = request(Focused, 5000, 50)
  let moved =
    JoinRequest(
      ..old,
      session: session.SessionRef(..old.session, inbox: process.new_subject()),
      attempt: 3,
    )
  let #(gate, _) = admission.offer(gate, old, old_reply, 0)
  let #(gate, _) = admission.offer(gate, moved, moved_reply, 0)
  let #(_, batch, effects) = admission.next_batch(gate, 1)
  let served =
    batch
    |> option.map(fn(found) {
      list.map(found.members, fn(waiting) { waiting.request.attempt })
    })
    |> option.unwrap([])
  let answered =
    list.any(effects, fn(effect) {
      case effect {
        admission.Reply(to, JoinOverloaded(_, 3)) -> to == moved_reply
        _ -> False
      }
    })
  assert list.contains(served, 3) || answered
}

pub fn hard_cap_rejects_with_rising_slots_test() {
  let config = admission.Config(..admission.default_config(), initial_limit: 2)
  let gate = admission.with_jitter(config, 0, no_jitter)
  let cap = admission.hard_cap(gate)
  let reply = process.new_subject()
  let gate =
    int.range(1, cap + 1, gate, fn(acc, _) {
      admission.offer(acc, request(Focused, 60_000, 1), reply, 0).0
    })
  let #(gate, first) =
    admission.offer(gate, request(Focused, 60_000, 1), reply, 0)
  let #(_, second) =
    admission.offer(gate, request(Focused, 60_000, 1), reply, 0)
  let assert [admission.Reply(_, JoinOverloaded(a, _))] = first
  let assert [admission.Reply(_, JoinOverloaded(b, _))] = second
  let assert True = a >= 250
  let assert True = b >= a
}

pub fn limit_grows_while_latency_holds_test() {
  let gate = admission.with_jitter(admission.default_config(), 0, no_jitter)
  let reply = process.new_subject()
  let #(gate, _) = admission.offer(gate, request(Focused, 5000, 1), reply, 0)
  let #(gate, batch, _) = admission.next_batch(gate, 0)
  let assert Some(found) = batch
  let gate = admission.batch_done(gate, found, 5)
  let assert 9 = admission.limit(gate)
  let assert None = admission.next_batch(gate, 6).1
}
