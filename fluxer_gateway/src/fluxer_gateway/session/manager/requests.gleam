// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/session.{
  type Identify, type Peer, type ResumeError, type SessionId, type SessionRef,
  type SocketRef, type StartError, ResumeUnavailable, SessionGone,
  StartUnavailable,
}
import fluxer_gateway/session/manager/shards.{
  NoShard, ResumeSession, Settled, ShardDown, StartSession, Unanswered,
}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}

pub fn start(
  session_id: SessionId,
  identify: Identify,
  peer: Peer,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, StartError)),
  upstream: Option(Subject(Nil)),
) -> Nil {
  process.spawn_unlinked(fn() {
    let delivery =
      shards.deliver(session.session_id_to_string(session_id), fn(settled) {
        StartSession(session_id, identify, peer, socket, reply, settled)
      })
    case delivery {
      Settled -> notify(upstream)
      NoShard | ShardDown -> {
        process.send(
          reply,
          Error(StartUnavailable(
            durations.session().manager_unavailable_retry_ms,
          )),
        )
        notify(upstream)
      }
      Unanswered -> Nil
    }
  })
  Nil
}

pub fn resume(
  session_id: String,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
) -> Nil {
  process.spawn_unlinked(fn() {
    let delivery =
      shards.deliver(session_id, fn(settled) {
        ResumeSession(session_id, token, seq, socket, reply, settled)
      })
    case delivery {
      NoShard ->
        process.send(
          reply,
          Error(ResumeUnavailable(
            durations.session().manager_unavailable_retry_ms,
          )),
        )
      ShardDown -> process.send(reply, Error(SessionGone))
      Settled | Unanswered -> Nil
    }
  })
  Nil
}

fn notify(upstream: Option(Subject(Nil))) -> Nil {
  case upstream {
    Some(subject) -> process.send(subject, Nil)
    None -> Nil
  }
}
