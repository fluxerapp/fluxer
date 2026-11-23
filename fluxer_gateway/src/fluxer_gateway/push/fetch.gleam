// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock.{type Deadline}
import fluxer_gateway/core/id
import fluxer_gateway/push/chunks
import fluxer_gateway/push/metrics
import fluxer_gateway/push/permit
import fluxer_gateway/push/settings.{type Settings}
import fluxer_gateway/push/settings_cache
import fluxer_gateway/push/throttle
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set

fn result_slack_ms() -> Int {
  durations.push().fetch_result_slack_ms
}

const chunk_size = 200

const max_in_flight = 4

fn max_chunk_ms() -> Int {
  durations.push().fetch_max_chunk_ms
}

pub type Unresolved {
  ApiError
  Timeout
  Throttled
  ApiOpen
  FailedEntryReason
  ShapeMismatch
  MalformedRow
  NodeQueueFull
  WorkerLost
}

pub type Row {
  Resolved(Settings)
  Unresolved(Unresolved)
}

fn reason_label(reason: Unresolved) -> String {
  case reason {
    ApiError -> "api_error"
    Timeout -> "timeout"
    Throttled -> "throttled"
    ApiOpen -> "api_open"
    FailedEntryReason -> "failed_entry"
    ShapeMismatch -> "shape_mismatch"
    MalformedRow -> "malformed_row"
    NodeQueueFull -> "node_queue_full"
    WorkerLost -> "worker_lost"
  }
}

pub fn resolve(
  guild guild: Int,
  users users: List(Int),
  author author: Int,
  deadline deadline: Deadline,
) -> Dict(Int, Row) {
  let #(known, missing) =
    list.fold(users, #(dict.new(), set.new()), fn(acc, user) {
      let #(known, missing) = acc
      case user == author || dict.has_key(known, user) {
        True -> acc
        False ->
          case settings_cache.lookup_settings(user, guild) {
            settings_cache.Cached(found) -> #(
              dict.insert(known, user, Resolved(found)),
              missing,
            )
            settings_cache.FailedEntry -> #(
              dict.insert(known, user, Unresolved(FailedEntryReason)),
              missing,
            )
            settings_cache.Uncached -> #(known, set.insert(missing, user))
          }
      }
    })
  let fill = settings_cache.new_fill()
  let #(rows, _) =
    chunks.run(
      chunks: missing
        |> set.to_list
        |> list.sort(int.compare)
        |> list.sized_chunk(chunk_size),
      limit: max_in_flight,
      wait: fn() { clock.remaining_ms(deadline) + result_slack_ms() },
      work: fn(chunk) { fetch_chunk(fill, guild, chunk, deadline) },
      state: known,
      done: fn(rows, chunk, outcome) {
        case outcome {
          Ok(chunk_rows) -> dict.merge(rows, dict.from_list(chunk_rows))
          Error(lost) -> {
            settings_cache.release_settings(fill, chunk, guild)
            let reason = case lost {
              chunks.Crashed -> WorkerLost
              chunks.TimedOut -> Timeout
            }
            dict.merge(rows, dict.from_list(unresolved(chunk, reason)))
          }
        }
      },
    )
  rows
}

fn unresolved(users: List(Int), reason: Unresolved) -> List(#(Int, Row)) {
  metrics.skipped(reason_label(reason), list.length(users))
  list.map(users, fn(user) { #(user, Unresolved(reason)) })
}

fn fetch_chunk(
  fill: settings_cache.Fill,
  guild: Int,
  users: List(Int),
  deadline: Deadline,
) -> List(#(Int, Row)) {
  case permit.acquire(permit.SettingsFetch, clock.remaining_ms(deadline)) {
    Error(_) -> unresolved(users, NodeQueueFull)
    Ok(held) -> {
      let rows = case throttle.offer(throttle.UserGuildSettings) {
        throttle.Rejected -> unresolved(users, Throttled)
        throttle.Admitted -> request_chunk(fill, guild, users, deadline)
      }
      permit.release(held)
      rows
    }
  }
}

fn request_chunk(
  fill: settings_cache.Fill,
  guild: Int,
  users: List(Int),
  deadline: Deadline,
) -> List(#(Int, Row)) {
  settings_cache.reserve_settings(fill, users, guild)
  let chunk_deadline =
    clock.earliest(deadline, clock.deadline_in(max_chunk_ms()))
  let reply =
    api.call(
      "get_user_guild_settings",
      [
        #("user_ids", json.array(users, id.json)),
        #("guild_id", id.json(guild)),
      ],
      chunk_deadline,
    )
  case reply {
    Ok(data) -> {
      throttle.record_accept(throttle.UserGuildSettings)
      install(fill, guild, users, data)
    }
    Error(error) -> {
      settings_cache.fail_settings(fill, users, guild)
      let reason = case error {
        api.ApiTimeout -> Timeout
        api.ApiUnavailable -> ApiOpen
        api.ApiHttp(_) | api.ApiRejected(..) | api.ApiBadReply -> ApiError
      }
      unresolved(users, reason)
    }
  }
}

fn install(
  fill: settings_cache.Fill,
  guild: Int,
  users: List(Int),
  data: Dynamic,
) -> List(#(Int, Row)) {
  let rows =
    decode.run(
      data,
      decode.at(["user_guild_settings"], decode.list(decode.dynamic)),
    )
  case rows {
    Ok(rows) ->
      case list.length(rows) == list.length(users) {
        True ->
          list.zip(users, rows)
          |> list.map(fn(pair) { install_row(fill, guild, pair.0, pair.1) })
        False -> {
          settings_cache.release_settings(fill, users, guild)
          unresolved(users, ShapeMismatch)
        }
      }
    Error(_) -> {
      settings_cache.release_settings(fill, users, guild)
      unresolved(users, ShapeMismatch)
    }
  }
}

fn install_row(
  fill: settings_cache.Fill,
  guild: Int,
  user: Int,
  raw: Dynamic,
) -> #(Int, Row) {
  let parsed = {
    use row <- result.try(decode.run(raw, decode.optional(decode.dynamic)))
    case row {
      None -> Ok(settings.defaults())
      Some(row) -> decode.run(row, settings.decoder())
    }
  }
  case parsed {
    Ok(found) -> {
      settings_cache.fill_settings(fill, user, guild, found)
      #(user, Resolved(found))
    }
    Error(_) -> {
      settings_cache.release_settings(fill, [user], guild)
      metrics.skipped(reason_label(MalformedRow), 1)
      #(user, Unresolved(MalformedRow))
    }
  }
}
