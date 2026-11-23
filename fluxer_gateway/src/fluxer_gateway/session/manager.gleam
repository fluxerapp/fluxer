// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router as cluster_router
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/node
import fluxer_gateway/core/session.{
  type Identify, type Peer, type ResumeError, type SessionId, type SessionRef,
  type SocketRef, type StartError, ResumeUnavailable, StartUnavailable,
}
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/requests
import fluxer_gateway/session/manager/router.{RemoteResume, RemoteStart}
import fluxer_gateway/session/manager/shard
import fluxer_gateway/session/manager/shards
import fluxer_gateway/session/manager/transfer
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/option.{None}
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn unavailable_retry_ms() -> Int {
  durations.session().manager_unavailable_retry_ms
}

fn start_timeout() -> Int {
  durations.session().manager_start_timeout
}

fn resume_wait_ms() -> Int {
  durations.gateway().socket_resume_reply_timeout_ms
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  add_shards(
    static_supervisor.new(static_supervisor.RestForOne)
      |> static_supervisor.restart_tolerance(intensity: 5, period: 10)
      |> static_supervisor.add(router.supervised()),
    0,
    shards.count(),
  )
  |> static_supervisor.supervised
  |> supervision.map_data(fn(_) { Nil })
}

fn add_shards(
  builder: static_supervisor.Builder,
  index: Int,
  count: Int,
) -> static_supervisor.Builder {
  case index < count {
    False -> builder
    True ->
      add_shards(
        static_supervisor.add(builder, shard.supervised(index)),
        index + 1,
        count,
      )
  }
}

type Owner {
  Here
  There(node.Node)
  Nowhere(retry_after_ms: Int)
}

fn owner(session_id: String) -> Owner {
  case
    cluster_router.owner(cluster_router.SessionKey(session_id), node.Sessions)
  {
    cluster_router.Local -> Here
    cluster_router.Remote(target) -> There(target)
    cluster_router.OwnerUnavailable(retry_after_ms) -> Nowhere(retry_after_ms)
  }
}

pub fn start(
  identify: Identify,
  peer: Peer,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, StartError)),
) -> Nil {
  start_as(session.new_session_id(), identify, peer, socket, reply)
}

pub fn start_as(
  session_id: SessionId,
  identify: Identify,
  peer: Peer,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, StartError)),
) -> Nil {
  case owner(session.session_id_to_string(session_id)) {
    Here -> requests.start(session_id, identify, peer, socket, reply, None)
    There(target) -> {
      process.spawn_unlinked(fn() {
        watch_remote(target, session_id, identify, peer, socket, reply)
      })
      Nil
    }
    Nowhere(retry_after_ms) ->
      process.send(reply, Error(StartUnavailable(retry_after_ms)))
  }
}

type RemoteOutcome {
  RemoteSettled
  OwnerLost
}

@external(erlang, "erlang", "monitor_node")
fn monitor_node(node: Atom, flag: Bool) -> Bool

fn watch_remote(
  target: node.Node,
  session_id: SessionId,
  identify: Identify,
  peer: Peer,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, StartError)),
) -> Nil {
  let target_atom = node.to_atom(target)
  monitor_node(target_atom, True)
  let settled = process.new_subject()
  let sent =
    remote.send_named(
      target,
      names.router(),
      RemoteStart(session_id, identify, peer, socket, reply, settled),
    )
  let outcome = case sent {
    Error(_) -> Ok(OwnerLost)
    Ok(Nil) ->
      process.new_selector()
      |> process.select_map(settled, fn(_) { RemoteSettled })
      |> process.select_record(atom.create("nodedown"), 1, fn(_) { OwnerLost })
      |> process.selector_receive(start_timeout())
  }
  case outcome {
    Ok(OwnerLost) ->
      process.send(reply, Error(StartUnavailable(unavailable_retry_ms())))
    Ok(RemoteSettled) | Error(Nil) -> Nil
  }
  monitor_node(target_atom, False)
  Nil
}

pub fn resume(
  session_id: SessionId,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
) -> Nil {
  let key = session.session_id_to_string(session_id)
  case owner(key) {
    Here -> requests.resume(key, token, seq, socket, reply)
    There(target) -> {
      process.spawn_unlinked(fn() {
        watch_resume(target, key, token, seq, socket, reply)
      })
      Nil
    }
    Nowhere(retry_after_ms) -> {
      process.spawn_unlinked(fn() {
        resume_located(key, token, seq, socket, reply, retry_after_ms)
      })
      Nil
    }
  }
}

fn resume_located(
  key: String,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
  retry_after_ms: Int,
) -> Nil {
  let host =
    transfer.locate_live(key)
    |> result.try(fn(ref) { process.subject_owner(ref.inbox) })
    |> result.map(node_of)
  case host {
    Error(Nil) -> process.send(reply, Error(ResumeUnavailable(retry_after_ms)))
    Ok(found) ->
      case found == node.self() {
        True -> requests.resume(key, token, seq, socket, reply)
        False -> watch_resume(found, key, token, seq, socket, reply)
      }
  }
}

type ResumeOutcome {
  Answered(Result(SessionRef, ResumeError))
  HostLost
  SocketGone
}

fn watch_resume(
  target: node.Node,
  key: String,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
) -> Nil {
  let target_atom = node.to_atom(target)
  monitor_node(target_atom, True)
  let socket_monitor = process.monitor(socket.pid)
  let answer = process.new_subject()
  let sent =
    remote.send_named(
      target,
      names.router(),
      RemoteResume(key, token, seq, socket, answer),
    )
  let outcome = case sent {
    Error(_) -> Ok(HostLost)
    Ok(Nil) ->
      process.new_selector()
      |> process.select_map(answer, Answered)
      |> process.select_record(atom.create("nodedown"), 1, fn(_) { HostLost })
      |> process.select_specific_monitor(socket_monitor, fn(_) { SocketGone })
      |> process.selector_receive(resume_wait_ms())
  }
  case outcome {
    Ok(Answered(result)) -> process.send(reply, result)
    Ok(HostLost) ->
      process.send(reply, Error(ResumeUnavailable(unavailable_retry_ms())))
    Ok(SocketGone) | Error(Nil) -> Nil
  }
  process.demonitor_process(socket_monitor)
  monitor_node(target_atom, False)
  Nil
}

@external(erlang, "erlang", "node")
fn pid_node(pid: process.Pid) -> Atom

fn node_of(pid: process.Pid) -> node.Node {
  node.from_atom(pid_node(pid))
}

pub fn start_timeout_ms() -> Int {
  start_timeout()
}
