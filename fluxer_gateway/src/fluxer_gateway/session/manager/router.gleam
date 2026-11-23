// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/session.{
  type Identify, type Peer, type ResumeError, type SessionId, type SessionRef,
  type SocketRef, type StartError,
}
import fluxer_gateway/session/join_budget
import fluxer_gateway/session/manager/leases
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/requests
import fluxer_gateway/session/session_metrics
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process.{type Subject}
import gleam/option.{Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

fn init_timeout_ms() -> Int {
  durations.session().router_init_timeout_ms
}

pub type Message {
  RemoteStart(
    session_id: SessionId,
    identify: Identify,
    peer: Peer,
    socket: SocketRef,
    reply: Subject(Result(SessionRef, StartError)),
    settled: Subject(Nil),
  )
  RemoteResume(
    session_id: String,
    token: String,
    seq: Int,
    socket: SocketRef,
    reply: Subject(Result(SessionRef, ResumeError)),
  )
}

pub fn supervised() -> ChildSpecification(Nil) {
  supervision.worker(start)
}

fn start() -> actor.StartResult(Nil) {
  actor.new_with_initialiser(init_timeout_ms(), fn(_self) {
    registry.create_tables()
    join_budget.create_tables()
    leases.create_table()
    metrics.register_collector("fluxer_gateway_session_manager", fn() {
      [metrics.Sample("fluxer_gateway_sessions_total", [], registry.owned())]
    })
    session_metrics.register()
    actor.initialised(Nil)
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(names.router())
  |> actor.start
}

fn handle(state: Nil, message: Message) -> actor.Next(Nil, Message) {
  case message {
    RemoteStart(session_id, identify, peer, socket, reply, settled) ->
      requests.start(session_id, identify, peer, socket, reply, Some(settled))
    RemoteResume(session_id, token, seq, socket, reply) ->
      requests.resume(session_id, token, seq, socket, reply)
  }
  actor.continue(state)
}
