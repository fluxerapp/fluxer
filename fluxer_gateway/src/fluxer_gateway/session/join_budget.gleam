// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list

fn rate_sample_ms() -> Int {
  durations.session().join_budget_rate_sample_ms
}

fn retry_min_ms() -> Int {
  durations.session().join_budget_retry_min_ms
}

fn retry_max_ms() -> Int {
  durations.session().join_budget_retry_max_ms
}

const quota_per_guild = 32

const rate_floor_per_s = 500

type TableOption {
  Set
  OrderedSet
  Public
  NamedTable
  WriteConcurrency(Bool)
}

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

@external(erlang, "ets", "update_counter")
fn update_counter(
  table: Atom,
  key: Int,
  op: #(Int, Int),
  default: #(Int, Int),
) -> Int

@external(erlang, "ets", "update_counter")
fn update_counter_floor(
  table: Atom,
  key: Int,
  op: #(Int, Int, Int, Int),
  default: #(Int, Int),
) -> Int

@external(erlang, "ets", "insert_new")
fn insert_hold(table: Atom, row: #(#(Pid, Int))) -> Bool

@external(erlang, "ets", "take")
fn take_hold(table: Atom, key: #(Pid, Int)) -> List(#(#(Pid, Int)))

type UniqueOption {
  Monotonic
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

@external(erlang, "erlang", "is_process_alive")
fn is_process_alive(pid: Pid) -> Bool

@external(erlang, "ets", "insert_new")
fn insert_queued(table: Atom, row: #(#(Pid, Int), Int)) -> Bool

@external(erlang, "ets", "take")
fn take_queued(table: Atom, key: #(Pid, Int)) -> List(#(#(Pid, Int), Int))

@external(erlang, "ets", "select")
fn select_queued(
  table: Atom,
  spec: List(#(#(#(Pid, Atom), Atom), List(Atom), List(Atom))),
) -> List(Int)

@external(erlang, "ets", "insert")
fn insert_waiter(table: Atom, row: #(#(Int, Int), Pid, fn() -> Nil)) -> Bool

@external(erlang, "ets", "delete")
fn delete_waiter(table: Atom, key: #(Int, Int)) -> Bool

@external(erlang, "ets", "take")
fn take_waiter(
  table: Atom,
  key: #(Int, Int),
) -> List(#(#(Int, Int), Pid, fn() -> Nil))

@external(erlang, "ets", "next")
fn next_waiter(table: Atom, key: #(Int, Int)) -> #(Int, Int)

@external(erlang, "ets", "member")
fn is_member(table: Atom, key: #(Int, Int)) -> Bool

@external(erlang, "ets", "lookup")
fn lookup_queued(table: Atom, key: #(Pid, Int)) -> List(#(#(Pid, Int), Int))

@external(erlang, "ets", "select")
fn select_held(
  table: Atom,
  spec: List(#(#(#(Pid, Atom)), List(Atom), List(Atom))),
) -> List(Int)

@external(erlang, "ets", "insert_new")
fn insert_cursor(table: Atom, row: #(Int, Int)) -> Bool

@external(erlang, "ets", "select_replace")
fn catch_up_cursor(
  table: Atom,
  spec: List(#(#(Int, Atom), List(#(Atom, Atom, Int)), List(#(#(Int, Int))))),
) -> Int

@external(erlang, "ets", "update_counter")
fn advance_cursor(table: Atom, key: Int, op: #(Int, Int)) -> Int

@external(erlang, "ets", "lookup")
fn lookup_rate(table: Atom, key: Int) -> List(#(Int, Int, Int, Int))

@external(erlang, "ets", "select_replace")
fn resample_rate(
  table: Atom,
  spec: List(
    #(
      #(Int, Int, Atom, Atom),
      List(Nil),
      List(#(#(Int, Int, #(Atom, Atom, Int), Int))),
    ),
  ),
) -> Int

@external(erlang, "ets", "update_counter")
fn count_served(
  table: Atom,
  key: Int,
  op: #(Int, Int),
  default: #(Int, Int, Int, Int),
) -> Int

fn quota_table() -> Atom {
  atom.create("fluxer_gateway_session_join_quota")
}

fn holds_table() -> Atom {
  atom.create("fluxer_gateway_session_join_holds")
}

fn pace_table() -> Atom {
  atom.create("fluxer_gateway_session_join_pace")
}

fn rate_table() -> Atom {
  atom.create("fluxer_gateway_session_join_rate")
}

fn queue_table() -> Atom {
  atom.create("fluxer_gateway_session_join_queue")
}

fn queued_table() -> Atom {
  atom.create("fluxer_gateway_session_join_queued")
}

pub fn create_tables() -> Nil {
  let options = [Set, Public, NamedTable, WriteConcurrency(True)]
  ets_new(quota_table(), options)
  ets_new(holds_table(), [
    OrderedSet,
    Public,
    NamedTable,
    WriteConcurrency(True),
  ])
  ets_new(pace_table(), options)
  ets_new(rate_table(), options)
  ets_new(queue_table(), [
    OrderedSet,
    Public,
    NamedTable,
    WriteConcurrency(True),
  ])
  insert_waiter(queue_table(), #(queue_end(), process.self(), fn() { Nil }))
  ets_new(queued_table(), [
    OrderedSet,
    Public,
    NamedTable,
    WriteConcurrency(True),
  ])
  Nil
}

pub type Admit {
  Send
  OverQuota(retry_after_ms: Int)
}

pub fn try_send(guild: Int, owner: Pid, now_ms: Int) -> Admit {
  let in_flight = update_counter(quota_table(), guild, #(2, 1), #(guild, 0))
  case in_flight > quota_per_guild {
    False -> {
      case insert_hold(holds_table(), #(#(owner, guild))) {
        True -> Nil
        False -> give_back(guild)
      }
      leave_queue(owner, guild)
      Send
    }
    True -> {
      give_back(guild)
      OverQuota(slot(guild, now_ms))
    }
  }
}

pub fn wait(guild: Int, owner: Pid, wake: fn() -> Nil) -> Nil {
  case lookup_queued(queued_table(), #(owner, guild)) {
    [#(_, seq)] ->
      case is_member(queue_table(), #(guild, seq)) {
        True -> Nil
        False -> {
          insert_waiter(queue_table(), #(#(guild, seq), owner, wake))
          Nil
        }
      }
    _ -> {
      let seq = unique_integer([Monotonic, Positive])
      insert_queued(queued_table(), #(#(owner, guild), seq))
      insert_waiter(queue_table(), #(#(guild, seq), owner, wake))
      Nil
    }
  }
}

fn queue_end() -> #(Int, Int) {
  #(int.bitwise_shift_left(1, 80), 0)
}

fn leave_queue(owner: Pid, guild: Int) -> Nil {
  case take_queued(queued_table(), #(owner, guild)) {
    [#(_, seq)] -> {
      delete_waiter(queue_table(), #(guild, seq))
      Nil
    }
    _ -> Nil
  }
}

fn wake_next(guild: Int) -> Nil {
  let #(found, seq) = next_waiter(queue_table(), #(guild, 0))
  case found == guild {
    True ->
      case take_waiter(queue_table(), #(guild, seq)) {
        [#(_, owner, wake)] ->
          case is_process_alive(owner) {
            True -> wake()
            False -> {
              let _ = take_queued(queued_table(), #(owner, guild))
              wake_next(guild)
            }
          }
        _ -> wake_next(guild)
      }
    False -> Nil
  }
}

pub fn settle(guild: Int, owner: Pid) -> Nil {
  case take_hold(holds_table(), #(owner, guild)) {
    [] -> Nil
    _ -> {
      let now_ms = clock.monotonic_ms()
      let _ = count_served(rate_table(), guild, #(3, 1), #(guild, now_ms, 0, 0))
      give_back(guild)
      wake_next(guild)
    }
  }
}

pub fn release(owner: Pid) -> Nil {
  let held = atom.create("$1")
  select_held(holds_table(), [#(#(#(owner, held)), [], [held])])
  |> list.each(settle(_, owner))
  select_queued(queued_table(), [
    #(#(#(owner, held), atom.create("_")), [], [held]),
  ])
  |> list.each(leave_queue(owner, _))
}

fn give_back(guild: Int) -> Nil {
  update_counter_floor(quota_table(), guild, #(2, -1, 0, 0), #(guild, 0))
  Nil
}

fn slot(guild: Int, now_ms: Int) -> Int {
  let now_us = now_ms * 1000
  let step_us =
    1_000_000 / int.max(measured_rate(guild, now_ms), rate_floor_per_s)
  let cursor_us = case insert_cursor(pace_table(), #(guild, now_us + step_us)) {
    True -> now_us + step_us
    False -> {
      let held = atom.create("$1")
      let _ =
        catch_up_cursor(pace_table(), [
          #(#(guild, held), [#(atom.create("<"), held, now_us)], [
            #(#(guild, now_us)),
          ]),
        ])
      advance_cursor(pace_table(), guild, #(2, step_us))
    }
  }
  { cursor_us - now_us } / 1000
  |> jitter
  |> int.clamp(retry_min_ms(), retry_max_ms())
}

fn measured_rate(guild: Int, now_ms: Int) -> Int {
  case lookup_rate(rate_table(), guild) {
    [#(_, sampled_at, served, rate)] -> {
      let elapsed = now_ms - sampled_at
      case elapsed >= rate_sample_ms() {
        True -> {
          let fresh = served * 1000 / elapsed
          let _ =
            resample_rate(rate_table(), [
              #(#(guild, sampled_at, atom.create("$1"), atom.create("_")), [], [
                #(
                  #(
                    guild,
                    now_ms,
                    #(atom.create("-"), atom.create("$1"), served),
                    fresh,
                  ),
                ),
              ]),
            ])
          fresh
        }
        False -> rate
      }
    }
    _ -> 0
  }
}

fn jitter(value: Int) -> Int {
  let spread = value / 10
  case spread > 0 {
    True -> value - spread + int.random(2 * spread + 1)
    False -> value
  }
}
