// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/voice_key.{type VoiceKey}
import fluxer_gateway/telemetry/fixed_counters.{Series}
import fluxer_gateway/telemetry/metrics
import gleam/dict.{type Dict}
import gleam/list
import gleam/set

pub type Scope {
  GuildScope
  CallScope
}

pub type GuildRequest {
  ClientUpdateRequest
  SignalRequest
  TokenReadyRequest
  RpcRequest
  CollectRequest
  SweepRequest
  P2pCheckRequest
  CleanupAccessRequest
}

pub type Job {
  JoinJob
  MoveJob
  ReissueJob
}

pub type CallLookup {
  CallFound
  CallNotFound
  CallUnavailable
}

pub type CallCreate {
  CallCreated
  CallExists
  CallFailed
  CallCreateUnavailable
}

const throughput = "fluxer_gateway_voice_throughput"

pub const transitions_metric = "fluxer_gateway_voice_state_transitions_total"

pub const guild_requests_metric = "fluxer_gateway_voice_guild_requests_total"

pub const server_updates_metric = "fluxer_gateway_voice_server_updates_total"

pub const guild_errors_metric = "fluxer_gateway_voice_guild_errors_total"

pub const calls_started_metric = "fluxer_gateway_voice_calls_started_total"

pub const calls_ended_metric = "fluxer_gateway_voice_calls_ended_total"

pub const token_results_metric = "fluxer_gateway_voice_token_results_total"

pub const token_turnaround_ms = "fluxer_gateway_voice_token_turnaround_ms"

pub const call_create_ms = "fluxer_gateway_voice_call_create_ms"

pub const call_lookup_ms = "fluxer_gateway_voice_call_lookup_ms"

pub const counts_sync_ms = "fluxer_gateway_voice_counts_sync_ms"

pub const scopes = ["guild", "call"]

pub const transition_kinds = ["join", "leave", "move", "update"]

pub const guild_requests = [
  "client_update", "signal", "token_ready", "rpc", "collect", "sweep",
  "p2p_check", "cleanup_access",
]

pub const jobs = ["join", "move", "reissue"]

pub const token_outcomes = ["ok", "error"]

const guild_requests_index = 9

const server_updates_index = 17

const guild_errors_index = 19

const calls_started_index = 20

const calls_ended_index = 21

const token_results_index = 22

fn throughput_series() -> List(fixed_counters.Series) {
  list.flatten([
    list.flat_map(scopes, fn(scope) {
      list.map(transition_kinds, fn(kind) {
        Series(transitions_metric, [#("scope", scope), #("kind", kind)])
      })
    }),
    list.map(guild_requests, fn(request) {
      Series(guild_requests_metric, [#("request", request)])
    }),
    list.map(scopes, fn(scope) {
      Series(server_updates_metric, [#("scope", scope)])
    }),
    [
      Series(guild_errors_metric, []),
      Series(calls_started_metric, []),
      Series(calls_ended_metric, []),
    ],
    list.flat_map(jobs, fn(job) {
      list.map(token_outcomes, fn(outcome) {
        Series(token_results_metric, [#("job", job), #("outcome", outcome)])
      })
    }),
  ])
}

fn scope_offset(scope: Scope) -> Int {
  case scope {
    GuildScope -> 0
    CallScope -> 1
  }
}

fn job_offset(job: Job) -> Int {
  case job {
    JoinJob -> 0
    MoveJob -> 1
    ReissueJob -> 2
  }
}

fn job_label(job: Job) -> String {
  case job {
    JoinJob -> "join"
    MoveJob -> "move"
    ReissueJob -> "reissue"
  }
}

fn count(index: Int, by: Int) -> Nil {
  fixed_counters.add(throughput, throughput_series, index, by)
}

pub fn transitions(
  scope: Scope,
  before: Dict(VoiceKey, value),
  after: Dict(VoiceKey, value),
) -> Nil {
  let removed =
    dict.keys(before) |> list.filter(fn(key) { !dict.has_key(after, key) })
  let added =
    dict.keys(after) |> list.filter(fn(key) { !dict.has_key(before, key) })
  let updated =
    dict.fold(after, 0, fn(total, key, value) {
      case dict.get(before, key) {
        Ok(old) if old != value -> total + 1
        _ -> total
      }
    })
  let leaving = removed |> list.map(voice_key.connection) |> set.from_list
  let moves =
    list.count(added, fn(key) {
      set.contains(leaving, voice_key.connection(key))
    })
  let base = scope_offset(scope) * 4
  count(base + 1, list.length(added) - moves)
  count(base + 2, list.length(removed) - moves)
  count(base + 3, moves)
  count(base + 4, updated)
}

pub fn guild_request(request: GuildRequest) -> Nil {
  let offset = case request {
    ClientUpdateRequest -> 0
    SignalRequest -> 1
    TokenReadyRequest -> 2
    RpcRequest -> 3
    CollectRequest -> 4
    SweepRequest -> 5
    P2pCheckRequest -> 6
    CleanupAccessRequest -> 7
  }
  count(guild_requests_index + offset, 1)
}

pub fn server_updates(scope: Scope, sent: Int) -> Nil {
  count(server_updates_index + scope_offset(scope), sent)
}

pub fn guild_errors(sent: Int) -> Nil {
  count(guild_errors_index, sent)
}

pub fn call_started() -> Nil {
  count(calls_started_index, 1)
}

pub fn call_ended() -> Nil {
  count(calls_ended_index, 1)
}

pub fn token_result(job: Job, ok: Bool, elapsed_ms: Int) -> Nil {
  let outcome = case ok {
    True -> 0
    False -> 1
  }
  count(token_results_index + job_offset(job) * 2 + outcome, 1)
  metrics.observe_ms(
    token_turnaround_ms,
    [#("job", job_label(job))],
    elapsed_ms,
  )
}

pub fn call_lookup(outcome: CallLookup, elapsed_ms: Int) -> Nil {
  let label = case outcome {
    CallFound -> "found"
    CallNotFound -> "not_found"
    CallUnavailable -> "unavailable"
  }
  metrics.observe_ms(call_lookup_ms, [#("outcome", label)], elapsed_ms)
}

pub fn call_create(outcome: CallCreate, elapsed_ms: Int) -> Nil {
  let label = case outcome {
    CallCreated -> "created"
    CallExists -> "exists"
    CallFailed -> "failed"
    CallCreateUnavailable -> "unavailable"
  }
  metrics.observe_ms(call_create_ms, [#("outcome", label)], elapsed_ms)
}

pub fn counts_synced(elapsed_ms: Int) -> Nil {
  metrics.observe_ms(counts_sync_ms, [], elapsed_ms)
}
