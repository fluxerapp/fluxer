// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/list

pub opaque type Node {
  Node(name: Atom)
}

pub type Role {
  AllRoles
  Websocket
  Sessions
  WebsocketSessions
  Presence
  Guilds
  Calls
  Push
}

@external(erlang, "erlang", "node")
fn self_name() -> Atom

@external(erlang, "erlang", "nodes")
fn connected_names() -> List(Atom)

pub fn self() -> Node {
  Node(self_name())
}

pub fn connected() -> List(Node) {
  connected_names() |> list.map(Node)
}

pub fn from_atom(name: Atom) -> Node {
  Node(name)
}

pub fn to_atom(node: Node) -> Atom {
  node.name
}

pub fn to_string(node: Node) -> String {
  atom.to_string(node.name)
}

pub fn runs(node_role: Role, wanted: Role) -> Bool {
  case node_role {
    AllRoles -> True
    WebsocketSessions ->
      wanted == Websocket || wanted == Sessions || wanted == WebsocketSessions
    _ -> node_role == wanted
  }
}

pub fn role_name(role: Role) -> String {
  case role {
    AllRoles -> "all"
    Websocket -> "websocket"
    Sessions -> "sessions"
    WebsocketSessions -> "websocket+sessions"
    Presence -> "presence"
    Guilds -> "guilds"
    Calls -> "calls"
    Push -> "push"
  }
}

pub fn role_from_name(name: String) -> Result(Role, Nil) {
  case name {
    "all" -> Ok(AllRoles)
    "websocket" -> Ok(Websocket)
    "sessions" -> Ok(Sessions)
    "websocket+sessions" -> Ok(WebsocketSessions)
    "presence" -> Ok(Presence)
    "guilds" -> Ok(Guilds)
    "calls" -> Ok(Calls)
    "push" -> Ok(Push)
    _ -> Error(Nil)
  }
}
