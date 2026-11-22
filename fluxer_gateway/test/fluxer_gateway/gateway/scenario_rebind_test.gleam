// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/control
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/transfer
import fluxer_gateway/session/seed.{type Seed}
import fluxer_gateway/session/server
import fluxer_gateway/session/state
import gleam/dict
import gleam/erlang/process.{type Name, type Subject}
import gleam/json
import gleam/option.{None, Some}
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/session_harness as sh

fn adopt_on_commit(ready: Subject(Nil), adopted: Subject(SessionRef)) -> Nil {
  let name: Name(transfer.Message) = names.transfer()
  let assert Ok(original) = process.named(name)
  let assert Ok(Nil) = process.unregister(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  process.send(ready, Nil)
  let inbox = process.named_subject(name)
  case process.receive(inbox, 5000) {
    Ok(transfer.Receive(found, _, reply)) -> {
      process.send(reply, True)
      case process.receive(inbox, 5000) {
        Ok(transfer.Commit(_, _, committed)) -> {
          let assert Ok(started) =
            server.start(server.Start(seed: found, socket: None, reply: None))
          process.unlink(started.pid)
          process.send(committed, Ok(started.data))
          process.send(adopted, started.data)
        }
        _ -> Nil
      }
    }
    _ -> Nil
  }
  let _ = process.unregister(name)
  let _ = process.register(original, name)
  Nil
}

fn message(content: String) -> raw_json.RawJson {
  raw_json.from_json(json.object([#("content", json.string(content))]))
}

fn dispatched(client: ws.Client) -> #(ws.Client, ws.Payload) {
  let #(client, payload) = ws.expect_payload(client, 5000)
  case payload.op, payload.t {
    0, Some("MESSAGE_CREATE") -> #(client, payload)
    11, _ -> dispatched(client)
    _, _ -> #(client, payload)
  }
}

pub fn session_hand_off_rebinds_the_socket_without_a_reconnect_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000851"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("t-r851"))
  let #(client, ready) =
    ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
  let key = h.session_id(ready)
  let assert Ok(source) = registry.lookup(key)
  let source_pid = sh.session_pid(source)
  session.send(source, session.Dispatch("MESSAGE_CREATE", message("before")))
  let #(client, before) = dispatched(client)
  assert before.t == Some("MESSAGE_CREATE")
  let assert Some(before_seq) = before.s

  let ready_transfer = process.new_subject()
  let adopted = process.new_subject()
  process.spawn_unlinked(fn() { adopt_on_commit(ready_transfer, adopted) })
  let assert Ok(Nil) = process.receive(ready_transfer, 1000)
  let answer = process.new_subject()
  control.send(source, control.HandOff(node.self(), answer))
  let assert Ok(True) = process.receive(answer, 5000)
  let assert Ok(target) = process.receive(adopted, 5000)
  assert sh.session_pid(target) != source_pid
  assert sh.await_exit(source_pid, 3000)
  assert sh.wait_until(
    fn() {
      case sh.state_of(target).socket {
        state.Attached(..) -> True
        state.Detached(..) -> False
      }
    },
    3000,
  )

  session.send(target, session.Dispatch("MESSAGE_CREATE", message("after")))
  let #(client, after) = dispatched(client)
  assert after.op == 0
  assert after.t == Some("MESSAGE_CREATE")
  assert after.s == Some(before_seq + 1)
  ws.send_json(client, h.heartbeat_payload(json.int(before_seq + 1)))
  let #(client, ack) = ws.wait_for(client, 5000, fn(p) { p.op == 11 })
  assert ack.op == 11
  let #(_, quiet) = ws.silent(client, 300)
  assert quiet
  sh.terminate(target)
}

fn adopt_every_commit(
  ready: Subject(Nil),
  adopted: Subject(#(String, SessionRef)),
) -> Nil {
  let name: Name(transfer.Message) = names.transfer()
  let assert Ok(original) = process.named(name)
  let assert Ok(Nil) = process.unregister(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  process.send(ready, Nil)
  adopt_loop(process.named_subject(name), dict.new(), adopted)
  let _ = process.unregister(name)
  let _ = process.register(original, name)
  Nil
}

fn adopt_loop(
  inbox: Subject(transfer.Message),
  seeds: dict.Dict(String, Seed),
  adopted: Subject(#(String, SessionRef)),
) -> Nil {
  case process.receive(inbox, 1000) {
    Ok(transfer.Receive(found, _, reply)) -> {
      process.send(reply, True)
      let key = session.session_id_to_string(found.id)
      adopt_loop(inbox, dict.insert(seeds, key, found), adopted)
    }
    Ok(transfer.Commit(key, _, committed)) ->
      case dict.get(seeds, key) {
        Ok(found) -> {
          let assert Ok(started) =
            server.start(server.Start(seed: found, socket: None, reply: None))
          process.unlink(started.pid)
          process.send(committed, Ok(started.data))
          process.send(adopted, #(key, started.data))
          adopt_loop(inbox, dict.delete(seeds, key), adopted)
        }
        Error(Nil) -> {
          process.send(committed, Error(Nil))
          adopt_loop(inbox, seeds, adopted)
        }
      }
    Ok(_) -> adopt_loop(inbox, seeds, adopted)
    Error(Nil) -> Nil
  }
}

fn adopted_as(
  adopted: Subject(#(String, SessionRef)),
  key: String,
) -> Result(SessionRef, Nil) {
  case process.receive(adopted, 5000) {
    Ok(#(found, ref)) if found == key -> Ok(ref)
    Ok(#(_, other)) -> {
      sh.terminate(other)
      adopted_as(adopted, key)
    }
    Error(Nil) -> Error(Nil)
  }
}

pub fn export_spread_hands_every_session_to_a_target_without_a_reconnect_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000852"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("t-r852"))
  let #(client, ready) =
    ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
  let key = h.session_id(ready)
  let assert Ok(source) = registry.lookup(key)
  let source_pid = sh.session_pid(source)
  assert transfer.export_spread([]) == 0
  assert registry.lookup(key) == Ok(source)

  let ready_transfer = process.new_subject()
  let adopted = process.new_subject()
  process.spawn_unlinked(fn() { adopt_every_commit(ready_transfer, adopted) })
  let assert Ok(Nil) = process.receive(ready_transfer, 1000)
  assert transfer.export_spread([node.self()]) >= 1
  let assert Ok(target) = adopted_as(adopted, key)
  assert sh.await_exit(source_pid, 3000)
  assert sh.wait_until(
    fn() {
      case sh.state_of(target).socket {
        state.Attached(..) -> True
        state.Detached(..) -> False
      }
    },
    3000,
  )
  session.send(target, session.Dispatch("MESSAGE_CREATE", message("moved")))
  let #(client, after) = dispatched(client)
  assert after.op == 0
  assert after.t == Some("MESSAGE_CREATE")
  let #(_, quiet) = ws.silent(client, 300)
  assert quiet
  sh.terminate(target)
}
