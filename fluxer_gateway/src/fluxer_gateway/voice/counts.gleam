// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/telemetry/metrics.{type Sample, Sample}
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/metrics as voice_metrics
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/set

fn init_timeout_ms() -> Int {
  durations.voice().counts_init_timeout_ms
}

fn sync_interval_ms() -> Int {
  durations.voice().counts_sync_interval_ms
}

fn collect_timeout_ms() -> Int {
  durations.voice().counts_collect_timeout_ms
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(named) {
      metrics.register_collector("fluxer_gateway_voice", samples)
      let ticks = process.new_subject()
      list.each(count_table.claim_owners(), fn(owner) {
        process.monitor(owner)
        Nil
      })
      process.send_after(ticks, sync_interval_ms(), count_table.RunSync)
      actor.initialised(ticks)
      |> actor.selecting(
        process.new_selector()
        |> process.select(named)
        |> process.select(ticks)
        |> process.select_monitors(count_table.OwnerDown),
      )
      |> actor.returning(Nil)
      |> Ok
    })
    |> actor.on_message(fn(ticks, message) {
      case message {
        count_table.RunSync -> {
          let started = clock.monotonic_ms()
          rebuild()
          voice_metrics.counts_synced(clock.monotonic_ms() - started)
          process.send_after(ticks, sync_interval_ms(), count_table.RunSync)
          actor.continue(ticks)
        }
        count_table.OwnerSeen(owner) -> {
          process.monitor(owner)
          actor.continue(ticks)
        }
        count_table.OwnerDown(process.ProcessDown(pid:, ..)) -> {
          count_table.drop_owner(pid)
          actor.continue(ticks)
        }
        count_table.OwnerDown(process.PortDown(..)) -> actor.continue(ticks)
      }
    })
    |> actor.named(count_table.janitor())
    |> actor.start
  })
}

fn samples() -> List(Sample) {
  let dimension = fn(rows: List(#(String, Int)), label: String) {
    list.map(rows, fn(row) {
      let #(key, count) = row
      Sample("fluxer_gateway_voice_connections", [#(label, key)], count)
    })
  }
  [
    Sample("fluxer_gateway_voice_connections_total", [], count_table.total()),
    Sample("fluxer_gateway_calls_total", [], list.length(manager.local_calls())),
    ..list.append(
      dimension(count_table.regions(), "region"),
      dimension(count_table.servers(), "server"),
    )
  ]
}

type Report {
  Report(owner: Pid, entries: Result(List(Entry), Nil))
}

fn reports() -> List(Report) {
  let guilds =
    guild_handler.collect_all(collect_timeout_ms())
    |> list.map(fn(pair) { Report(pair.0, pair.1) })
  let calls =
    manager.local_calls()
    |> list.filter_map(fn(pair) {
      use owner <- result.map(process.subject_owner(pair.1))
      Report(owner, call_server.connections(pair.1, collect_timeout_ms()))
    })
  list.append(guilds, calls)
}

fn rebuild() -> Nil {
  let known = count_table.rows()
  let reports = reports()
  let answered =
    list.filter_map(reports, fn(report) {
      report.entries
      |> result.map(fn(entries) {
        #(
          report.owner,
          list.filter_map(entries, fn(value) {
            entry.key(value) |> result.map(fn(key) { #(key, value) })
          }),
        )
      })
    })
  let complete = list.length(answered) == list.length(reports)
  let live =
    list.flat_map(answered, fn(pair) {
      list.map(pair.1, fn(value) { #(pair.0, value) })
    })
  let live_keys = live |> list.map(fn(pair) { pair.1.0 }) |> set.from_list
  let collected =
    reports |> list.map(fn(report) { report.owner }) |> set.from_list
  let answered_owners =
    answered |> list.map(fn(pair) { pair.0 }) |> set.from_list
  let stale = fn(row: count_table.Row) {
    case process.is_alive(row.owner), set.contains(live_keys, row.key) {
      False, _ -> True
      True, True -> False
      True, False ->
        set.contains(answered_owners, row.owner)
        || { complete && !set.contains(collected, row.owner) }
    }
  }
  let #(removed, kept) = list.partition(known, stale)
  list.each(removed, count_table.remove_row)
  let present = kept |> list.map(fn(row) { row.key }) |> set.from_list
  live
  |> list.filter(fn(pair) { !set.contains(present, pair.1.0) })
  |> list.each(fn(pair) { count_table.adopt_missing(pair.1.1, pair.0) })
}
