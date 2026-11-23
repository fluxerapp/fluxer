// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/application
import fluxer_gateway/app/drain
import fluxer_gateway/cluster/handoff
import fluxer_gateway/cluster/rendezvous
import fluxer_gateway/cluster/router as cluster_router
import fluxer_gateway/cluster/topology
import fluxer_gateway/config/env
import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{
  type SocketMsg, type SocketRef, Close, DispatchFrame, ReconnectFrame, Send,
  SocketRef,
}
import fluxer_gateway/session/control
import fluxer_gateway/session/manager
import fluxer_gateway/session/manager/registry
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import support/session_harness as h

pub type Sock {
  Sock(ref: SocketRef, query: Subject(Subject(List(String))))
}

type Input {
  Outbound(SocketMsg)
  Query(Subject(List(String)))
}

pub fn socket() -> Sock {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outbox = process.new_subject()
      let query = process.new_subject()
      process.send(ready, #(outbox, query))
      let selector =
        process.new_selector()
        |> process.select_map(outbox, Outbound)
        |> process.select_map(query, Query)
      collect(selector, [])
    })
  let assert Ok(#(outbox, query)) = process.receive(ready, 1000)
  Sock(ref: SocketRef(pid:, outbox:), query:)
}

fn collect(selector: process.Selector(Input), seen: List(String)) -> Nil {
  case process.selector_receive_forever(selector) {
    Query(reply) -> {
      process.send(reply, list.reverse(seen))
      collect(selector, seen)
    }
    Outbound(Send(_, frames)) ->
      collect(
        selector,
        list.fold(frames, seen, fn(acc, frame) {
          case frame {
            DispatchFrame(seq, event, _) -> [
              event <> ":" <> int.to_string(seq),
              ..acc
            ]
            ReconnectFrame -> ["RECONNECT", ..acc]
            _ -> acc
          }
        }),
      )
    Outbound(Close(..)) -> collect(selector, ["CLOSE", ..seen])
    Outbound(session.Moved(from:, seq:, ..)) -> {
      session.send(from, session.SocketDown(process.self()))
      collect(selector, ["MOVED:" <> int.to_string(seq), ..seen])
    }
    Outbound(session.Reattached(..)) ->
      collect(selector, ["REATTACHED", ..seen])
  }
}

pub fn events(sock: Sock) -> List(String) {
  let reply = process.new_subject()
  process.send(sock.query, reply)
  case process.receive(reply, 2000) {
    Ok(seen) -> seen
    Error(Nil) -> []
  }
}

pub fn drop(sock: Sock) -> Nil {
  process.kill(sock.ref.pid)
}

pub fn start(sock: Sock, token: String) -> #(String, Int) {
  let reply = process.new_subject()
  manager.start(
    h.identify(token),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    sock.ref,
    reply,
  )
  case process.receive(reply, 15_000) {
    Ok(Ok(ref)) -> #(
      session.session_id_to_string(ref.id),
      await_ready(sock, 80),
    )
    _ -> #("", -1)
  }
}

fn await_ready(sock: Sock, tries: Int) -> Int {
  let ready =
    list.find_map(events(sock), fn(event) {
      case string.split_once(event, ":") {
        Ok(#("READY", seq)) -> int.parse(seq)
        _ -> Error(Nil)
      }
    })
  case ready, tries {
    Ok(seq), _ -> seq
    Error(Nil), 0 -> -1
    Error(Nil), _ -> {
      process.sleep(100)
      await_ready(sock, tries - 1)
    }
  }
}

pub fn hosts(key: String) -> Bool {
  case registry.lookup(key) {
    Ok(_) -> True
    Error(Nil) -> False
  }
}

pub fn hand_off(key: String, target: String) -> Bool {
  case registry.lookup(key) {
    Ok(ref) -> control.hand_off(ref, node.from_atom(atom.create(target)), 8000)
    Error(Nil) -> False
  }
}

pub fn dispatch(key: String, content: String) -> Bool {
  case registry.lookup(key) {
    Ok(ref) -> {
      session.send(
        ref,
        session.Dispatch(
          "MESSAGE_CREATE",
          raw_json.from_json(json.object([#("content", json.string(content))])),
        ),
      )
      True
    }
    Error(Nil) -> False
  }
}

pub fn resume(sock: Sock, key: String, token: String, seq: Int) -> String {
  let assert Ok(session_id) = session.session_id_from_string(key)
  let reply = process.new_subject()
  manager.resume(session_id, token, seq, sock.ref, reply)
  case process.receive(reply, 15_000) {
    Ok(Ok(_)) -> "ok"
    Ok(Error(error)) -> string.inspect(error)
    Error(Nil) -> "no_reply"
  }
}

pub fn hand_off_racing_resume(
  key: String,
  target: String,
  sock: Sock,
  token: String,
  seq: Int,
) -> #(Bool, String) {
  let handed = process.new_subject()
  process.spawn_unlinked(fn() { process.send(handed, hand_off(key, target)) })
  let resumed = resume(sock, key, token, seq)
  case process.receive(handed, 15_000) {
    Ok(moved) -> #(moved, resumed)
    Error(Nil) -> #(False, resumed)
  }
}

@external(erlang, "erlang", "phash2")
fn phash2(term: String) -> Int

fn owned_by(target: String) -> String {
  pick_owned(node.from_atom(atom.create(target)))
}

fn pick_owned(target: node.Node) -> String {
  let id = session.new_session_id()
  let key = session.session_id_to_string(id)
  case
    cluster_router.owner_node(cluster_router.SessionKey(key), node.Sessions)
  {
    Ok(found) if found == target -> key
    _ -> pick_owned(target)
  }
}

pub fn shifting_key(then: String) -> String {
  let assert Some(published) = cluster_router.published()
  let others =
    topology.role_nodes(published, node.Sessions)
    |> list.filter(fn(peer) { peer != node.self() })
  pick_shifting(others, node.from_atom(atom.create(then)), 10_000)
}

fn pick_shifting(
  others: List(node.Node),
  then: node.Node,
  tries: Int,
) -> String {
  let key = pick_owned(node.self())
  case rendezvous.select(phash2(key), others), tries {
    Ok(found), _ if found == then -> key
    _, 0 -> ""
    _, _ -> pick_shifting(others, then, tries - 1)
  }
}

pub fn start_key(sock: Sock, token: String, key: String) -> Int {
  let assert Ok(session_id) = session.session_id_from_string(key)
  let reply = process.new_subject()
  manager.start_as(
    session_id,
    h.identify(token),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    sock.ref,
    reply,
  )
  case process.receive(reply, 12_000) {
    Ok(Ok(_)) -> await_ready(sock, 80)
    _ -> -1
  }
}

pub fn start_remote(
  sock: Sock,
  token: String,
  owner: String,
  wait_ms: Int,
) -> String {
  let key = owned_by(owner)
  let assert Ok(session_id) = session.session_id_from_string(key)
  let reply = process.new_subject()
  manager.start_as(
    session_id,
    h.identify(token),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    sock.ref,
    reply,
  )
  case process.receive(reply, wait_ms) {
    Ok(Ok(_)) -> "ok"
    Ok(Error(error)) -> string.inspect(error)
    Error(Nil) -> "no_reply"
  }
}

pub fn resume_within(
  sock: Sock,
  key: String,
  token: String,
  seq: Int,
  wait_ms: Int,
) -> String {
  let assert Ok(session_id) = session.session_id_from_string(key)
  let reply = process.new_subject()
  manager.resume(session_id, token, seq, sock.ref, reply)
  case process.receive(reply, wait_ms) {
    Ok(Ok(_)) -> "ok"
    Ok(Error(error)) -> string.inspect(error)
    Error(Nil) -> "no_reply"
  }
}

pub fn suspend_session(key: String) -> Bool {
  case registry.lookup(key) {
    Ok(ref) ->
      case process.subject_owner(ref.inbox) {
        Ok(pid) -> {
          h.suspend(pid)
          True
        }
        Error(Nil) -> False
      }
    Error(Nil) -> False
  }
}

pub fn slow_settings(gone_after_ms: Int) -> Nil {
  let current = settings.get()
  let d = current.durations
  let _ =
    settings.store(
      settings.Settings(
        ..current,
        durations: settings.Durations(
          ..d,
          cluster: settings.ClusterDurations(
            ..d.cluster,
            membership_state_gone_after_ms: gone_after_ms,
          ),
        ),
      ),
    )
  Nil
}

@external(erlang, "sys", "get_state")
fn sys_state(pid: process.Pid, timeout: Int) -> Dynamic

pub fn rebalance_settled() -> Bool {
  case process.named(env.process_name("fluxer_gateway_cluster_membership")) {
    Ok(membership) -> {
      let _ = sys_state(membership, 5000)
      handoff.settled(5000)
    }
    Error(Nil) -> False
  }
}

pub fn start_owned_async(sock: Sock, token: String) -> String {
  let key = pick_owned(node.self())
  let assert Ok(session_id) = session.session_id_from_string(key)
  process.spawn_unlinked(fn() {
    let reply = process.new_subject()
    manager.start_as(
      session_id,
      h.identify(token),
      session.Peer(ip: "127.0.0.1", api_version: 1),
      sock.ref,
      reply,
    )
    let _ = process.receive(reply, 20_000)
    Nil
  })
  key
}

pub fn ready_seq(sock: Sock) -> Int {
  await_ready(sock, 100)
}

pub fn drain_start() -> Nil {
  drain.start()
}

pub fn await_drained(timeout_ms: Int) -> Bool {
  handoff.await_drained(timeout_ms)
}

pub fn prep_stop() -> Nil {
  application.prep_stop(Nil)
}

pub fn slow_handoff_debounce(debounce_ms: Int) -> Nil {
  let current = settings.get()
  let d = current.durations
  let _ =
    settings.store(
      settings.Settings(
        ..current,
        durations: settings.Durations(
          ..d,
          cluster: settings.ClusterDurations(
            ..d.cluster,
            handoff_debounce_ms: debounce_ms,
          ),
        ),
      ),
    )
  Nil
}

pub fn serving() -> List(String) {
  cluster_router.serving()
  |> list.map(node.to_string)
  |> list.sort(string.compare)
}

pub fn leaving() -> List(String) {
  case cluster_router.published() {
    Some(published) ->
      topology.leaving(published)
      |> set.to_list
      |> list.map(node.to_string)
      |> list.sort(string.compare)
    None -> []
  }
}
