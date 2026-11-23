// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client.{type ApiError}
import fluxer_gateway/config/durations
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/threads/context.{type Context}
import fluxer_gateway/threads/load.{type Payload}
import fluxer_gateway/threads/message.{
  type ThreadMsg, ListFlush, LoadResult, LoadRetry, SyncDrain, SyncTick,
}
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/state.{Loading, Ready}
import fluxer_gateway/threads/store.{type Handoff, type ThreadStore}
import fluxer_gateway/threads/subscriptions
import fluxer_gateway/threads/value
import fluxer_gateway/util/spawn
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}

fn first_retry_ms() -> Int {
  durations.threads().runtime_first_retry_ms
}

fn second_retry_ms() -> Int {
  durations.threads().runtime_second_retry_ms
}

fn later_retry_ms() -> Int {
  durations.threads().runtime_later_retry_ms
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

pub fn handle(
  threads: ThreadStore,
  ctx: Context,
  message: ThreadMsg,
) -> ThreadStore {
  case message {
    LoadRetry -> handle_retry(threads, ctx)
    LoadResult(request, result) ->
      handle_load_result(threads, ctx, request, result)
    SyncTick(session) -> subscriptions.handle_tick(threads, ctx, session)
    SyncDrain -> subscriptions.handle_drain(threads, ctx)
    ListFlush -> subscriptions.handle_list_flush(threads, ctx)
  }
}

pub fn start(threads: ThreadStore, ctx: Context) -> ThreadStore {
  case store.load_state(threads) {
    Loading(request: None, timer: None, ..) -> start_retry(threads, ctx)
    _ -> threads
  }
}

fn start_retry(threads: ThreadStore, ctx: Context) -> ThreadStore {
  case store.load_state(threads) {
    Loading(queue:, queued:, attempt:, timer:, ..) -> {
      store.cancel(timer)
      let timer = process.send_after(ctx.self, retry_delay(attempt), LoadRetry)
      store.set_load(
        threads,
        Loading(queue:, queued:, attempt:, request: None, timer: Some(timer)),
      )
    }
    Ready -> threads
  }
}

pub fn retry_delay(attempt: Int) -> Int {
  case attempt {
    0 -> 0
    1 -> first_retry_ms()
    2 -> second_retry_ms()
    _ -> later_retry_ms()
  }
}

fn handle_retry(threads: ThreadStore, ctx: Context) -> ThreadStore {
  case store.load_state(threads) {
    Loading(queue:, queued:, attempt:, ..) -> {
      let request = unique_integer()
      let reply = ctx.self
      let guild = ctx.guild
      process.spawn_unlinked(fn() { watch_fetch(reply, request, guild) })
      store.set_load(
        threads,
        Loading(queue:, queued:, attempt:, request: Some(request), timer: None),
      )
    }
    Ready -> threads
  }
}

fn watch_fetch(
  reply: process.Subject(ThreadMsg),
  request: Int,
  guild: GuildId,
) -> Nil {
  let #(_, monitor) =
    spawn.monitored(fn() {
      process.send(reply, LoadResult(request, load.fetch(guild)))
    })
  let reason =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) { down.reason })
    |> process.selector_receive_forever
  case reason {
    process.Normal -> Nil
    _ -> process.send(reply, LoadResult(request, Error(client.ApiUnavailable)))
  }
}

fn handle_load_result(
  threads: ThreadStore,
  ctx: Context,
  request: Int,
  result: Result(Payload, ApiError),
) -> ThreadStore {
  case store.load_state(threads), result {
    Loading(request: Some(pending), queue:, ..), Ok(payload)
      if pending == request
    -> {
      metrics.fetch("ok")
      threads
      |> store.replace_rows(rows.empty())
      |> store.set_load(Ready)
      |> store.install_payload(payload)
      |> replay(list.reverse(queue))
      |> subscriptions.resync_after_load(ctx)
    }
    Loading(request: Some(pending), attempt:, timer:, ..), Error(_)
      if pending == request
    -> {
      metrics.fetch("error")
      store.set_load(
        threads,
        Loading(
          queue: [],
          queued: 0,
          attempt: attempt + 1,
          request: None,
          timer:,
        ),
      )
      |> start_retry(ctx)
    }
    _, _ -> threads
  }
}

fn replay(
  threads: ThreadStore,
  events: List(#(String, value.Object)),
) -> ThreadStore {
  list.fold(events, threads, fn(acc, queued) {
    case queued {
      #("MESSAGE_DELETE", _) | #("MESSAGE_DELETE_BULK", _) -> acc
      #(event, data) -> store.apply(acc, event, data, store.Replayed)
    }
  })
}

pub fn restore(handoff: Handoff, ctx: Context) -> ThreadStore {
  let #(threads, syncs, dirty) = store.restore_handoff(handoff)
  let threads =
    list.fold(syncs, threads, fn(acc, session) {
      subscriptions.schedule_sync(acc, ctx, session)
    })
  case dirty {
    [] -> threads
    _ ->
      store.mark_dirty(threads, dirty)
      |> store.request_list_refresh
      |> subscriptions.arm(ctx)
  }
}
