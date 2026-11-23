// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock.{type Deadline}
import fluxer_gateway/core/id
import fluxer_gateway/push/chunks
import fluxer_gateway/push/fetch
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/rule
import fluxer_gateway/push/settings_cache
import fluxer_gateway/push/throttle
import fluxer_gateway/util/decode as edge
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set

fn result_slack_ms() -> Int {
  durations.push().recipients_result_slack_ms
}

const max_fetch_rpcs = 8

const fetch_chunk = 500

fn blocked_timeout_ms() -> Int {
  durations.push().recipients_blocked_timeout_ms
}

fn known_blocked_deadline_ms() -> Int {
  durations.push().recipients_known_blocked_deadline_ms
}

pub type Recipient {
  Recipient(user: Int, class: rule.Class)
}

pub fn eligible(create: MessageCreate, deadline: Deadline) -> List(Recipient) {
  let candidates = drop_blocked(create.recipients, create.author, dict.new())
  let evaluated = case create.prefilter {
    message.FilteredDirect -> list.map(candidates, Recipient(_, rule.Direct))
    message.FilteredBulk -> list.map(candidates, Recipient(_, rule.Bulk))
    message.NotFiltered -> evaluate(create, candidates, deadline)
  }
  let users = list.map(evaluated, fn(recipient) { recipient.user })
  let fetched = fetch_missing_blocked(users, deadline)
  let kept =
    drop_blocked(users, create.author, fetched)
    |> keep_known(fetched)
    |> set.from_list
  let unblocked =
    list.filter(evaluated, fn(recipient) { set.contains(kept, recipient.user) })
  case create.data.age_restricted {
    True -> adult_recipients(unblocked)
    False -> unblocked
  }
}

fn evaluate(
  create: MessageCreate,
  candidates: List(Int),
  deadline: Deadline,
) -> List(Recipient) {
  let rows =
    fetch.resolve(
      guild: create.guild,
      users: candidates,
      author: create.author,
      deadline:,
    )
  let shape =
    rule.shape(
      data: create.data,
      guild: create.guild,
      channel: create.channel,
      author: create.author,
      guild_default_level: create.guild_default_level,
      large: create.large,
    )
  let now_ms = clock.system_ms()
  list.filter_map(candidates, fn(user) {
    case user == create.author {
      True -> Error(Nil)
      False ->
        case dict.get(rows, user) {
          Ok(fetch.Resolved(found)) -> {
            let facts =
              rule.Facts(
                user:,
                settings: found,
                connected: set.contains(create.connected, user),
                roles: dict.get(create.user_roles, user) |> result.unwrap([]),
              )
            case rule.evaluate(shape, facts, now_ms:) {
              rule.Eligible(reason) -> Ok(Recipient(user, rule.class(reason)))
              rule.Ineligible(_) -> Error(Nil)
            }
          }
          Ok(fetch.Unresolved(_)) | Error(Nil) -> Error(Nil)
        }
    }
  })
}

pub fn known_blocked(users: List(Int)) -> Dict(Int, List(Int)) {
  let fetched =
    fetch_missing_blocked(users, clock.deadline_in(known_blocked_deadline_ms()))
  list.fold(users, dict.new(), fn(acc, user) {
    case dict.get(fetched, user), settings_cache.blocked_ids(user) {
      Ok(blocked), _ | Error(Nil), Some(blocked) ->
        dict.insert(acc, user, blocked)
      Error(Nil), None -> acc
    }
  })
}

fn drop_blocked(
  users: List(Int),
  author: Int,
  fetched: Dict(Int, List(Int)),
) -> List(Int) {
  let kept =
    list.filter(users, fn(user) {
      let from_fetch = case dict.get(fetched, user) {
        Ok(blocked) -> list.contains(blocked, author)
        Error(Nil) -> False
      }
      let from_cache = case settings_cache.blocked_ids(user) {
        Some(blocked) -> list.contains(blocked, author)
        None -> False
      }
      !from_fetch && !from_cache
    })
  metrics.count(
    "push_blocked_ids_suppressed",
    list.length(users) - list.length(kept),
  )
  kept
}

fn keep_known(users: List(Int), fetched: Dict(Int, List(Int))) -> List(Int) {
  let known =
    list.filter(users, fn(user) {
      dict.has_key(fetched, user) || settings_cache.blocked_ids(user) != None
    })
  metrics.count(
    "push_blocked_ids_unknown_skipped",
    list.length(users) - list.length(known),
  )
  known
}

pub fn blocked_fetch_chunks(
  users: List(Int),
  cached: fn(Int) -> Bool,
) -> List(List(Int)) {
  users
  |> list.filter(fn(user) { !cached(user) })
  |> list.unique
  |> list.sort(int.compare)
  |> list.sized_chunk(fetch_chunk)
}

fn fetch_missing_blocked(
  users: List(Int),
  deadline: Deadline,
) -> Dict(Int, List(Int)) {
  let pending =
    blocked_fetch_chunks(users, fn(user) {
      settings_cache.blocked_ids(user) != None
    })
  let fill = settings_cache.new_fill()
  let #(fetched, timed_out) =
    chunks.run(
      chunks: pending,
      limit: max_fetch_rpcs,
      wait: fn() { clock.remaining_ms(deadline) + result_slack_ms() },
      work: fn(chunk) {
        settings_cache.reserve_blocked(fill, chunk)
        let rows = fetch_blocked_chunk(chunk, fill, deadline)
        settings_cache.release_blocked(fill, chunk)
        rows
      },
      state: dict.new(),
      done: fn(fetched, chunk, outcome) {
        case outcome {
          Ok(Ok(rows)) -> dict.merge(fetched, rows)
          Ok(Error(Nil)) -> fetched
          Error(_) -> {
            settings_cache.release_blocked(fill, chunk)
            fetched
          }
        }
      },
    )
  case timed_out {
    True -> metrics.count("push_blocked_ids_fetch_timeouts", 1)
    False -> Nil
  }
  fetched
}

fn fetch_blocked_chunk(
  users: List(Int),
  fill: settings_cache.Fill,
  deadline: Deadline,
) -> Result(Dict(Int, List(Int)), Nil) {
  case throttle.offer(throttle.BlockedIds) {
    throttle.Rejected -> {
      metrics.count("push_blocked_ids_fetch_failures", 1)
      Error(Nil)
    }
    throttle.Admitted -> {
      metrics.count("push_blocked_ids_fetch_attempts", 1)
      case
        api.call(
          "get_user_blocked_ids",
          [#("user_ids", id_strings(users))],
          clock.earliest(deadline, clock.deadline_in(blocked_timeout_ms())),
        )
      {
        Ok(data) ->
          case decode.run(data, decode.dict(decode.string, decode.dynamic)) {
            Ok(raw_rows) -> {
              let rows = by_user(raw_rows)
              throttle.record_accept(throttle.BlockedIds)
              Ok(
                list.fold(users, dict.new(), fn(acc, user) {
                  let blocked =
                    dict.get(rows, user)
                    |> result.try(fn(raw) {
                      decode.run(raw, lenient_snowflakes())
                      |> result.replace_error(Nil)
                    })
                    |> result.unwrap([])
                  settings_cache.fill_blocked(fill, user, blocked)
                  dict.insert(acc, user, blocked)
                }),
              )
            }
            Error(_) -> {
              metrics.count("push_blocked_ids_fetch_failures", 1)
              Error(Nil)
            }
          }
        Error(_) -> {
          metrics.count("push_blocked_ids_fetch_failures", 1)
          Error(Nil)
        }
      }
    }
  }
}

fn adult_recipients(recipients: List(Recipient)) -> List(Recipient) {
  recipients
  |> list.sized_chunk(fetch_chunk)
  |> list.flat_map(adult_chunk)
}

fn adult_chunk(recipients: List(Recipient)) -> List(Recipient) {
  let users = list.map(recipients, fn(recipient) { recipient.user })
  case throttle.offer(throttle.AdultContent) {
    throttle.Rejected -> []
    throttle.Admitted ->
      case
        api.call(
          "get_users_adult_content_allowed",
          [#("user_ids", id_strings(users))],
          clock.deadline_in(blocked_timeout_ms()),
        )
      {
        Ok(data) ->
          case decode.run(data, decode.at(["user_ids"], lenient_snowflakes())) {
            Ok(allowed) -> {
              throttle.record_accept(throttle.AdultContent)
              let allowed = set.from_list(allowed)
              list.filter(recipients, fn(recipient) {
                set.contains(allowed, recipient.user)
              })
            }
            Error(_) -> []
          }
        Error(_) -> []
      }
  }
}

fn id_strings(users: List(Int)) -> json.Json {
  json.array(users, id.json)
}

fn by_user(rows: Dict(String, a)) -> Dict(Int, a) {
  dict.fold(rows, dict.new(), fn(acc, key, row) {
    case id.parse(key) {
      Ok(user) -> dict.insert(acc, user, row)
      Error(Nil) -> acc
    }
  })
}

fn lenient_snowflakes() -> decode.Decoder(List(Int)) {
  decode.one_of(
    decode.list(
      decode.one_of(decode.map(edge.snowflake(), option.Some), [
        decode.success(option.None),
      ]),
    )
      |> decode.map(option.values),
    [decode.success([])],
  )
}
