// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, Durations, SessionDurations,
}
import fluxer_gateway/core/guild
import fluxer_gateway/core/node
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/arrival_log.{
  Arrival, NoMoveState, NotAdopted, UnlinkedAtSource,
}
import fluxer_gateway/session/control
import fluxer_gateway/session/links
import fluxer_gateway/session/seed.{type Seed, Seed}
import fluxer_gateway/session/server
import fluxer_gateway/session/state
import fluxer_gateway/telemetry/rollout_metrics
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/isolation
import support/session_fake_guild as fake_guild
import support/session_harness as h
import support/session_transfer_tap as tap

fn fast_joins() -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      session: SessionDurations(
        ..d.session,
        server_ready_deadline_ms: 300,
        guilds_first_join_floor_ms: 300,
        guilds_join_grace_ms: 200,
        guilds_after_ready_join_ms: 300,
        guilds_down_reconnect_ms: 60_000,
        links_base_sleep_ms: 50,
        links_cap_sleep_ms: 200,
      ),
    )
  })
}

type Counts {
  Counts(guild: Int, presence: Int, guild_fallback: Int)
}

fn counts() -> Counts {
  Counts(
    guild: rollout_metrics.total(rollout_metrics.rebind_fallbacks, [
      #("kind", "guild"),
      #("reason", "not_adopted"),
    ]),
    presence: rollout_metrics.total(rollout_metrics.rebind_fallbacks, [
      #("kind", "presence"),
      #("reason", "not_adopted"),
    ]),
    guild_fallback: rollout_metrics.total(rollout_metrics.session_rebinds, [
      #("kind", "guild"),
      #("outcome", "fallback"),
    ]),
  )
}

fn since(before: Counts) -> Counts {
  let now = counts()
  Counts(
    guild: now.guild - before.guild,
    presence: now.presence - before.presence,
    guild_fallback: now.guild_fallback - before.guild_fallback,
  )
}

fn is_linked(ref: SessionRef, guild: Int) -> Bool {
  case links.get(h.state_of(ref).links, h.guild_id(guild)) {
    Some(links.Linked(..)) -> True
    _ -> False
  }
}

fn presence_linked(ref: SessionRef) -> Bool {
  case h.state_of(ref).presence {
    state.PresenceUp(..) -> True
    _ -> False
  }
}

fn arrive(found: Seed) -> SessionRef {
  let assert Ok(started) =
    server.start(server.Start(seed: found, socket: None, reply: None))
  started.data
}

fn move_live(
  ref: SessionRef,
  change: fn(Seed) -> Seed,
) -> #(SessionRef, Counts) {
  let transfer = tap.start()
  let answer = process.new_subject()
  control.send(ref, control.HandOff(node.self(), answer))
  let assert Ok(#(exported, reply)) = tap.committing(transfer, 2000)
  let before = counts()
  let target = arrive(change(exported))
  let moved = since(before)
  process.send(reply, Ok(target))
  process.receive(answer, 3000) |> should.equal(Ok(True))
  tap.release(transfer)
  #(target, moved)
}

fn ready_with(guilds: List(Int)) -> SessionRef {
  let #(ref, _, _) = h.ready_session("tok-arrival")
  h.wait_until(
    fn() { list.all(guilds, is_linked(ref, _)) && presence_linked(ref) },
    3000,
  )
  |> should.be_true
  ref
}

pub fn guilds_unlinked_on_the_source_are_not_counted_as_not_adopted_test() {
  fast_joins()
  let linked = h.unique_user()
  let crashing = h.unique_user()
  let ineligible = h.unique_user()
  let alive = fake_guild.start(linked, fake_guild.Accept)
  let down = fake_guild.start(crashing, fake_guild.Accept)
  let refused =
    fake_guild.start(
      ineligible,
      fake_guild.Answer(fn(join) { guild.JoinNotEligible(join.attempt) }),
    )
  use <- h.with_api(
    h.session_api(
      h.session_data(h.unique_user(), False, [linked, crashing, ineligible]),
    ),
  )
  let ref = ready_with([linked, crashing])
  fake_guild.crash(down)
  h.wait_until(fn() { !is_linked(ref, crashing) }, 2000) |> should.be_true

  let #(target, moved) = move_live(ref, fn(found) { found })
  moved |> should.equal(Counts(guild: 0, presence: 0, guild_fallback: 0))
  is_linked(target, linked) |> should.be_true

  let restarted = fake_guild.start(crashing, fake_guild.Accept)
  h.wait_until(fn() { is_linked(target, crashing) }, 4000) |> should.be_true
  h.terminate(target)
  fake_guild.crash(alive)
  fake_guild.crash(restarted)
  fake_guild.crash(refused)
}

pub fn a_handed_over_guild_the_target_cannot_adopt_is_still_counted_test() {
  fast_joins()
  let kept = h.unique_user()
  let dropped = h.unique_user()
  let first = fake_guild.start(kept, fake_guild.Accept)
  let second = fake_guild.start(dropped, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [kept, dropped])),
  )
  let ref = ready_with([kept, dropped])
  let #(target, moved) =
    move_live(ref, fn(found) {
      Seed(
        ..found,
        guilds: list.filter(found.guilds, fn(guild) {
          guild != h.guild_id(dropped)
        }),
      )
    })
  moved |> should.equal(Counts(guild: 1, presence: 0, guild_fallback: 1))
  h.terminate(target)
  fake_guild.crash(first)
  fake_guild.crash(second)
}

pub fn an_arrival_without_move_state_counts_every_guild_test() {
  fast_joins()
  let one = h.unique_user()
  let two = h.unique_user()
  let first = fake_guild.start(one, fake_guild.Accept)
  let second = fake_guild.start(two, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [one, two])),
  )
  let ref = ready_with([one, two])
  let #(target, moved) =
    move_live(ref, fn(found) { Seed(..found, moved: None) })
  moved |> should.equal(Counts(guild: 2, presence: 1, guild_fallback: 2))
  h.terminate(target)
  fake_guild.crash(first)
  fake_guild.crash(second)
}

pub fn classify_names_why_an_arrival_rejoins_test() {
  arrival_log.classify(guilds: 3, expected: 3, adopted: 3, moved: True)
  |> should.equal(Error(Nil))
  arrival_log.classify(guilds: 3, expected: 2, adopted: 2, moved: True)
  |> should.equal(Ok(UnlinkedAtSource))
  arrival_log.classify(guilds: 3, expected: 3, adopted: 1, moved: True)
  |> should.equal(Ok(NotAdopted))
  arrival_log.classify(guilds: 3, expected: 3, adopted: 0, moved: False)
  |> should.equal(Ok(NoMoveState))
  arrival_log.classify(guilds: 0, expected: 0, adopted: 0, moved: False)
  |> should.equal(Error(Nil))
}

pub fn line_carries_session_source_counts_and_reason_test() {
  arrival_log.line(Arrival(
    session_id: "s1",
    source_node: "gw@a",
    guilds: 5,
    expected: 4,
    adopted: 2,
    reason: NotAdopted,
  ))
  |> should.equal(
    "session_arrival_rejoining: session_id=s1 source_node=gw@a guilds=5 expected=4 adopted=2 unlinked_at_source=1 reason=not_adopted",
  )
}

pub fn admit_allows_a_fixed_number_of_warnings_per_window_test() {
  let window = 9_000_000_000 * arrival_log.window_ms
  let admitted =
    list.repeat(Nil, arrival_log.per_window + 5)
    |> list.count(fn(_) { arrival_log.admit(window) })
  admitted |> should.equal(arrival_log.per_window)
  arrival_log.admit(window + arrival_log.window_ms) |> should.be_true
}
