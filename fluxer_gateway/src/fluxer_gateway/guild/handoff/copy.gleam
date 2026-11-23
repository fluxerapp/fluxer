// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/message.{type MoveId}
import fluxer_gateway/guild/handoff/codec.{type Tail}
import fluxer_gateway/guild/inbox.{type Handle}
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}

pub type Request {
  Snapshot(move: MoveId, version: Int, reply: Subject(Result(BitArray, String)))
  Changes(
    move: MoveId,
    version: Int,
    round: Int,
    reply: Subject(Result(#(BitArray, Int), String)),
  )
  Cancel(move: MoveId)
  Restage(round: Int, bytes: BitArray, reply: Subject(Result(Nil, String)))
  Activate(tail: Tail, reply: Subject(Result(Handle, String)))
  Discard
  Expire
}

pub fn subject(pid: Pid) -> Subject(Request) {
  process.unsafely_create_subject(
    pid,
    atom.to_dynamic(atom.create("fluxer_gateway_guild_handoff_copy")),
  )
}
