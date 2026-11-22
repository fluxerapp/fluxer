// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/message
import fluxer_gateway/push/server as push_server
import fluxer_gateway/push/throttle
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string
import support/push_job_fixture as job
import support/push_throttle
import support/session_harness as h

fn slow_user_lookups(kind: String, body: Dynamic) -> h.Reply {
  case string.starts_with(kind, "get_user") {
    True -> process.sleep(500)
    False -> Nil
  }
  h.empty_ok(kind, body)
}

pub fn api_scope_ends_only_after_its_push_jobs_settle_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  push_throttle.calm(throttle.BlockedIds)
  let base = job.guild_message()
  let create =
    message.MessageCreate(..base, recipients: [h.unique_user(), h.unique_user()])
  use <- h.with_api(slow_user_lookups)
  push_server.handle_message_create(create)
  assert !push_server.is_idle(push_server.name())
}

pub fn push_work_is_settled_once_the_api_scope_ends_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  push_throttle.calm(throttle.BlockedIds)
  let base = job.guild_message()
  let create =
    message.MessageCreate(..base, recipients: [h.unique_user(), h.unique_user()])
  h.with_api(slow_user_lookups, fn() {
    push_server.handle_message_create(create)
  })
  assert push_server.is_idle(push_server.name())
}
