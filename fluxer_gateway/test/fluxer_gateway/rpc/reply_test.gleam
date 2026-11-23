// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/node
import fluxer_gateway/rpc/reply
import gleam/json

pub fn subjects_for_role_test() {
  assert reply.subjects_for_role(node.AllRoles) == ["rpc.gateway.>"]
  assert reply.subjects_for_role(node.Guilds)
    == ["rpc.gateway.guild.>", "rpc.gateway.voice.>", "rpc.gateway.process.>"]
  assert reply.subjects_for_role(node.Presence) == ["rpc.gateway.presence.>"]
  assert reply.subjects_for_role(node.Calls)
    == ["rpc.gateway.call.>", "rpc.gateway.voice.>"]
  assert reply.subjects_for_role(node.Push) == ["rpc.gateway.push.>"]
  assert reply.subjects_for_role(node.Sessions) == []
  assert reply.subjects_for_role(node.Websocket) == []
  assert reply.subjects_for_role(node.WebsocketSessions) == []
}

pub fn method_test() {
  assert reply.method("rpc.gateway.guild.get_counts") == "guild.get_counts"
  assert reply.method("other.subject") == "other.subject"
  assert reply.is_rpc_subject("rpc.gateway.push.x")
  assert !reply.is_rpc_subject("config.gateway.rollout")
}

pub fn ok_reply_test() {
  assert reply.ok(json.object([#("a", json.int(1))]))
    == "{\"ok\":true,\"result\":{\"a\":1}}"
}

pub fn failure_reply_test() {
  assert reply.failure(error.rpc_error_message(error.UnknownMethod("x.y")))
    == "{\"ok\":false,\"error\":\"Unknown method: x.y\"}"
  assert reply.failure(error.rpc_error_message(error.InvalidParams))
    == "{\"ok\":false,\"error\":\"invalid_params\"}"
  assert reply.overloaded() == "{\"ok\":false,\"error\":\"overloaded\"}"
  assert reply.internal_error() == "{\"ok\":false,\"error\":\"internal_error\"}"
}

pub fn quiet_errors_test() {
  assert reply.is_quiet("guild_not_found")
  assert reply.is_quiet("forbidden")
  assert !reply.is_quiet("timeout")
}
