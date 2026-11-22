// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/telemetry/metrics.{type Sample, Sample}
import fluxer_gateway/util/liveness
import fluxer_gateway/util/table
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

pub type Kind {
  GuildProcess
  SessionProcess
  PresenceProcess
  CallProcess
  VoiceProcess
}

pub opaque type Scope(id) {
  Scope(kind: Kind)
}

pub const guilds: Scope(Int) = Scope(GuildProcess)

pub const sessions: Scope(SessionId) = Scope(SessionProcess)

pub const presences: Scope(Int) = Scope(PresenceProcess)

pub const calls: Scope(Int) = Scope(CallProcess)

pub const voices: Scope(Int) = Scope(VoiceProcess)

type ProcessKey(id) {
  ProcessKey(kind: Kind, id: id)
}

type MatchHead =
  #(#(Atom, Atom, Atom), Atom)

type PidHead =
  #(#(Atom, Atom, Atom), Pid)

pub const kinds = [
  GuildProcess,
  SessionProcess,
  PresenceProcess,
  CallProcess,
  VoiceProcess,
]

fn registry() -> table.Table {
  table.named("fluxer_gateway_process_registry")
}

pub fn kind_name(kind: Kind) -> String {
  case kind {
    GuildProcess -> "guild"
    SessionProcess -> "session"
    PresenceProcess -> "presence"
    CallProcess -> "call"
    VoiceProcess -> "voice"
  }
}

fn kind_atom(kind: Kind) -> Atom {
  case kind {
    GuildProcess -> atom.create("guild_process")
    SessionProcess -> atom.create("session_process")
    PresenceProcess -> atom.create("presence_process")
    CallProcess -> atom.create("call_process")
    VoiceProcess -> atom.create("voice_process")
  }
}

pub fn register(scope: Scope(id), id: id, pid: Pid) -> Result(Nil, Pid) {
  let key = ProcessKey(scope.kind, id)
  case table.ensure_public(registry()) {
    False -> Ok(Nil)
    True ->
      case table.insert_new(registry(), #(key, pid)) {
        True -> Ok(Nil)
        False ->
          case whereis(scope, id) {
            Ok(existing) -> Error(existing)
            Error(Nil) ->
              case table.insert_new(registry(), #(key, pid)) {
                True -> Ok(Nil)
                False -> whereis(scope, id) |> existing_or_ok
              }
          }
      }
  }
}

fn existing_or_ok(found: Result(Pid, Nil)) -> Result(Nil, Pid) {
  case found {
    Ok(existing) -> Error(existing)
    Error(Nil) -> Ok(Nil)
  }
}

pub fn whereis(scope: Scope(id), id: id) -> Result(Pid, Nil) {
  let key = ProcessKey(scope.kind, id)
  let rows: List(#(ProcessKey(id), Pid)) = table.lookup(registry(), key)
  case rows {
    [] -> Error(Nil)
    [#(_, pid), ..] ->
      case liveness.is_alive(pid) {
        True -> Ok(pid)
        False -> {
          table.delete_object(registry(), #(key, pid))
          Error(Nil)
        }
      }
  }
}

pub fn unregister_pid(scope: Scope(id), id: id, pid: Pid) -> Nil {
  table.delete_object(registry(), #(ProcessKey(scope.kind, id), pid))
}

pub fn clear(scope: Scope(id)) -> Int {
  let wild = atom.create("_")
  let head: MatchHead = #(
    #(atom.create("process_key"), kind_atom(scope.kind), wild),
    wild,
  )
  table.select_delete(registry(), [#(head, [], [True])])
}

pub fn sweep_dead() -> Int {
  let rows: List(#(ProcessKey(any), Pid)) = table.to_list(registry())
  list.fold(rows, 0, fn(swept, row) {
    let #(_, pid) = row
    case liveness.is_local(pid) && !process.is_alive(pid) {
      True -> {
        table.delete_object(registry(), row)
        swept + 1
      }
      False -> swept
    }
  })
}

fn count(kind: Kind) -> Int {
  let wild = atom.create("_")
  let head: MatchHead = #(
    #(atom.create("process_key"), kind_atom(kind), wild),
    wild,
  )
  table.select_count(registry(), [#(head, [], [True])])
}

pub fn entries(scope: Scope(id)) -> List(#(id, Pid)) {
  let head: MatchHead = #(
    #(atom.create("process_key"), kind_atom(scope.kind), atom.create("$1")),
    atom.create("$2"),
  )
  let body = #(#(atom.create("$1"), atom.create("$2")))
  table.select(registry(), [#(head, [], [body])])
}

pub fn id_of(scope: Scope(id), pid: Pid) -> Result(id, Nil) {
  let head: PidHead = #(
    #(atom.create("process_key"), kind_atom(scope.kind), atom.create("$1")),
    pid,
  )
  table.select_first(registry(), [#(head, [], [atom.create("$1")])])
}

pub fn samples() -> List(Sample) {
  list.map(kinds, fn(kind) {
    Sample(
      "fluxer_gateway_processes",
      [#("type", kind_name(kind))],
      count(kind),
    )
  })
}
