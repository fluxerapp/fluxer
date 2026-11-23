// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node.{type Role}
import gleam/json.{type Json}
import gleam/string

pub const queue_group = "gateway"

pub fn subjects_for_role(role: Role) -> List(String) {
  case role {
    node.AllRoles -> ["rpc.gateway.>"]
    node.Guilds -> [
      "rpc.gateway.guild.>",
      "rpc.gateway.voice.>",
      "rpc.gateway.process.>",
    ]
    node.Presence -> ["rpc.gateway.presence.>"]
    node.Calls -> ["rpc.gateway.call.>", "rpc.gateway.voice.>"]
    node.Push -> ["rpc.gateway.push.>"]
    node.Sessions | node.Websocket | node.WebsocketSessions -> []
  }
}

pub fn is_rpc_subject(subject: String) -> Bool {
  case subject {
    "rpc.gateway." <> _ -> True
    _ -> False
  }
}

pub fn method(subject: String) -> String {
  case subject {
    "rpc.gateway." <> method -> method
    _ -> subject
  }
}

pub fn ok(result: Json) -> String {
  json.object([#("ok", json.bool(True)), #("result", result)])
  |> json.to_string
}

pub fn succeeded(body: String) -> Bool {
  string.starts_with(body, "{\"ok\":true")
}

pub fn failure(message: String) -> String {
  json.object([#("ok", json.bool(False)), #("error", json.string(message))])
  |> json.to_string
}

pub fn overloaded() -> String {
  failure("overloaded")
}

pub fn internal_error() -> String {
  failure("internal_error")
}

pub fn is_quiet(message: String) -> Bool {
  message == "guild_not_found" || message == "forbidden"
}
