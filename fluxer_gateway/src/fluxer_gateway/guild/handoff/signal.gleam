// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/message.{type MoveId}
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/guild/inbox.{type Handle}
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}

pub type Signal {
  Freeze(
    move: MoveId,
    version: Int,
    epoch: Epoch,
    reply: Subject(Result(BitArray, String)),
  )
  Commit(target: Handle, reply: Subject(Bool))
  Thaw(reply: Subject(Bool))
  Release(seq: Int, bytes: BitArray)
  Seal(last_seq: Int, reply: Subject(Bool))
  AbortArrival
  Repointed(session: SessionId)
  SealDue
  ForwardGraceOver
  Probe(reply: Subject(Bool))
}

pub fn subject(pid: Pid) -> Subject(Signal) {
  process.unsafely_create_subject(
    pid,
    atom.to_dynamic(atom.create("fluxer_gateway_guild_handoff")),
  )
}

pub fn repointed(stub: Pid, session: SessionId) -> Nil {
  process.send(subject(stub), Repointed(session))
}
