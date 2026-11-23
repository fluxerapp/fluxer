// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/throttle
import fluxer_gateway/util/decode as edge
import fluxer_gateway/util/spawn
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result

fn fetch_timeout_ms() -> Int {
  durations.push().read_state_fetch_timeout_ms
}

const max_fetches = 64

fn outbox_max_age_ms() -> Int {
  durations.push().read_state_outbox_max_age_ms
}

pub type Buffered {
  Buffered(create: MessageCreate, buffered_at_ms: Int)
}

type Stale {
  Stale(user: Int, channel: Int, message: Int, create: MessageCreate)
}

type AtomicsRef

type AtomicsOption {
  Signed(Bool)
}

type Key {
  FluxerGatewayPushReadStateSlots
}

@external(erlang, "atomics", "new")
fn atomics_new(arity: Int, options: List(AtomicsOption)) -> AtomicsRef

@external(erlang, "atomics", "add_get")
fn atomics_add_get(ref: AtomicsRef, index: Int, value: Int) -> Int

@external(erlang, "atomics", "sub")
fn atomics_sub(ref: AtomicsRef, index: Int, value: Int) -> Atom

@external(erlang, "persistent_term", "get")
fn term_get(key: Key, default: List(AtomicsRef)) -> List(AtomicsRef)

@external(erlang, "persistent_term", "put")
fn term_put(key: Key, value: List(AtomicsRef)) -> Atom

pub fn setup() -> Nil {
  let _ = slots()
  Nil
}

fn slots() -> AtomicsRef {
  case term_get(FluxerGatewayPushReadStateSlots, []) {
    [ref, ..] -> ref
    [] -> {
      let ref = atomics_new(1, [Signed(True)])
      let _ = term_put(FluxerGatewayPushReadStateSlots, [ref])
      ref
    }
  }
}

pub fn handle_buffered(
  buffered: List(Buffered),
  publish: fn(MessageCreate) -> Nil,
) -> Nil {
  let now = clock.system_ms()
  let #(stale, fresh) =
    list.fold_right(buffered, #([], []), fn(acc, entry) {
      let #(stale, fresh) = acc
      case
        now - entry.buffered_at_ms >= outbox_max_age_ms(),
        entry.create.recipients
      {
        True, [user] -> #(
          [
            Stale(
              user,
              entry.create.channel,
              entry.create.message,
              entry.create,
            ),
            ..stale
          ],
          fresh,
        )
        _, _ -> #(stale, [entry.create, ..fresh])
      }
    })
  list.each(fresh, publish)
  check_stale(stale, publish)
}

fn check_stale(stale: List(Stale), publish: fn(MessageCreate) -> Nil) -> Nil {
  case stale {
    [] -> Nil
    _ -> {
      let channels =
        stale
        |> list.map(fn(entry) { #(entry.user, entry.channel) })
        |> list.unique
        |> list.sort(fn(a, b) {
          case int.compare(a.0, b.0) {
            order.Eq -> int.compare(a.1, b.1)
            other -> other
          }
        })
      case acquire_slot() {
        True -> {
          let _ =
            process.spawn_unlinked(fn() {
              let marks = guarded_marks(channels)
              release_slot()
              list.each(stale, fn(entry) {
                publish_unless_read(marks, entry, publish)
              })
            })
          Nil
        }
        False -> {
          metrics.count("push_read_state_fetch_skipped", list.length(channels))
          list.each(stale, fn(entry) {
            publish_unless_read(dict.new(), entry, publish)
          })
        }
      }
    }
  }
}

fn guarded_marks(channels: List(#(Int, Int))) -> Dict(#(Int, Int), Int) {
  let fetched = process.new_subject()
  let #(_, monitor) =
    spawn.monitored(fn() {
      process.send(fetched, fetch_marks(channels, dict.new()))
    })
  let selector =
    process.new_selector()
    |> process.select_map(fetched, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
  case process.selector_receive_forever(selector) {
    Ok(marks) -> {
      process.demonitor_process(monitor)
      marks
    }
    Error(Nil) ->
      case process.receive(fetched, 0) {
        Ok(marks) -> marks
        Error(Nil) -> {
          metrics.count("push_read_state_fetch_failures", 1)
          dict.new()
        }
      }
  }
}

fn acquire_slot() -> Bool {
  case atomics_add_get(slots(), 1, 1) > max_fetches {
    True -> {
      release_slot()
      False
    }
    False -> True
  }
}

fn release_slot() -> Nil {
  let _ = atomics_sub(slots(), 1, 1)
  Nil
}

fn publish_unless_read(
  marks: Dict(#(Int, Int), Int),
  entry: Stale,
  publish: fn(MessageCreate) -> Nil,
) -> Nil {
  let read = dict.get(marks, #(entry.user, entry.channel)) |> result.unwrap(0)
  case entry.message <= read {
    True -> metrics.count("push_read_state_suppressed", 1)
    False -> publish(entry.create)
  }
}

fn fetch_marks(
  channels: List(#(Int, Int)),
  marks: Dict(#(Int, Int), Int),
) -> Dict(#(Int, Int), Int) {
  case channels {
    [] -> marks
    [#(user, channel) as key, ..rest] ->
      case fetch_mark(user, channel) {
        Ok(mark) -> fetch_marks(rest, dict.insert(marks, key, mark))
        Error(Nil) -> {
          metrics.count("push_read_state_fetch_skipped", list.length(rest))
          marks
        }
      }
  }
}

fn fetch_mark(user: Int, channel: Int) -> Result(Int, Nil) {
  case throttle.offer(throttle.ReadState) {
    throttle.Rejected -> {
      metrics.count("push_read_state_fetch_failures", 1)
      Error(Nil)
    }
    throttle.Admitted ->
      case
        api.call(
          "get_read_state",
          [
            #("user_id", id.json(user)),
            #("channel_id", id.json(channel)),
          ],
          clock.deadline_in(fetch_timeout_ms()),
        )
      {
        Ok(data) -> {
          throttle.record_accept(throttle.ReadState)
          let last =
            decode.run(
              data,
              decode.optional_field(
                "last_message_id",
                None,
                decode.one_of(decode.map(edge.snowflake(), Some), [
                  decode.success(None),
                ]),
                decode.success,
              ),
            )
          case last {
            Ok(Some(mark)) if mark > 0 -> Ok(mark)
            _ -> Ok(0)
          }
        }
        Error(_) -> {
          metrics.count("push_read_state_fetch_failures", 1)
          Error(Nil)
        }
      }
  }
}
