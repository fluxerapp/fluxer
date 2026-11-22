// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{Dispatch, ReconnectFrame}
import fluxer_gateway/core/user
import fluxer_gateway/session/control
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards
import fluxer_gateway/session/manager/transfer
import fluxer_gateway/session/seed.{type Seed}
import gleam/erlang/atom
import gleam/erlang/process.{type Name, type Subject}
import gleam/json
import gleam/list
import gleeunit/should
import support/session_fake_guild as fake_guild
import support/session_harness.{Frame, Other} as h

type Captured {
  Captured(seed: Seed, committed: Bool)
}

fn capture_transfer(ready: Subject(Nil), captured: Subject(Captured)) -> Nil {
  let name: Name(transfer.Message) = names.transfer()
  let assert Ok(original) = process.named(name)
  let assert Ok(Nil) = process.unregister(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  process.send(ready, Nil)
  let inbox = process.named_subject(name)
  case process.receive(inbox, 5000) {
    Ok(transfer.Receive(found, nonce, reply)) -> {
      process.send(reply, True)
      let committed = case process.receive(inbox, 5000) {
        Ok(transfer.Commit(_, committed_nonce, adopted)) -> {
          process.send(
            adopted,
            Ok(session.SessionRef(
              id: found.id,
              user: user.id(found.user),
              inbox: process.new_subject(),
            )),
          )
          committed_nonce == nonce
        }
        _ -> False
      }
      process.send(captured, Captured(found, committed))
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

pub fn hand_off_moves_session_and_resume_finds_it_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let #(ref, socket, ready_seq) = h.ready_session("tok-h1")
  let source = h.session_pid(ref)
  let assert Ok(_) = fake_guild.joins(fake, 1000)
  session.send(ref, Dispatch("MESSAGE_CREATE", message("before")))
  let assert Ok(Frame(before, _, _)) =
    h.await_event(socket, "MESSAGE_CREATE", 2000)

  let ready = process.new_subject()
  let captured = process.new_subject()
  process.spawn_unlinked(fn() { capture_transfer(ready, captured) })
  let assert Ok(Nil) = process.receive(ready, 1000)
  let answer = process.new_subject()
  control.send(ref, control.HandOff(node.self(), answer))
  process.receive(answer, 5000) |> should.equal(Ok(True))
  let assert Ok(Captured(exported, True)) = process.receive(captured, 5000)
  h.await_frame(socket, 2000, fn(frame) { frame == h.Rebound(before) })
  |> should.be_ok
  h.collect(socket, 50)
  |> list.contains(Other(ReconnectFrame))
  |> should.be_false
  h.await_exit(source, 2000) |> should.be_true
  process.sleep(50)
  list.any(drain(fake.seen), fn(request) {
    case request {
      guild.Leave(..) -> True
      _ -> False
    }
  })
  |> should.be_false
  exported.seq |> should.equal(before)
  exported.transferred |> should.be_true

  let key = session.session_id_to_string(ref.id)
  h.wait_until(fn() { registry.lookup(key) == Error(Nil) }, 2000)
  |> should.be_true
  let adopted_reply = process.new_subject()
  shards.send(key, shards.Adopt(exported, adopted_reply))
  |> should.equal(Ok(Nil))
  let assert Ok(Ok(_)) = process.receive(adopted_reply, 2000)
  h.wait_until(fn() { registry.lookup(key) |> is_ok }, 2000) |> should.be_true
  let assert Ok(adopted) = registry.lookup(key)
  let target = h.session_pid(adopted)
  { target != source } |> should.be_true
  let assert Ok(_) = fake_guild.joins(fake, 2000)

  let fresh = h.fake_socket()
  h.resume_id(key, "tok-h1", ready_seq, fresh) |> should.equal(Ok(adopted))
  let assert Ok(Frame(replayed, "MESSAGE_CREATE", _)) =
    h.await_event(fresh, "MESSAGE_CREATE", 2000)
  replayed |> should.equal(before)
  h.await_event(fresh, "RESUMED", 2000) |> should.be_ok
  h.terminate(adopted)
  fake_guild.crash(fake)
}

pub fn failed_push_keeps_session_serving_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, socket, _) = h.ready_session("tok-h2")
  let source = h.session_pid(ref)
  let nowhere = node.from_atom(atom.create("nowhere@unreachable.invalid"))
  let answer = process.new_subject()
  control.send(ref, control.HandOff(nowhere, answer))
  process.receive(answer, 9000) |> should.equal(Ok(False))
  process.is_alive(source) |> should.be_true
  session.send(ref, Dispatch("MESSAGE_CREATE", message("after")))
  let assert Ok(Frame(_, "MESSAGE_CREATE", _)) =
    h.await_event(socket, "MESSAGE_CREATE", 2000)
  h.terminate(ref)
}

fn is_ok(result: Result(a, b)) -> Bool {
  case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn drain(subject: Subject(a)) -> List(a) {
  case process.receive(subject, 0) {
    Ok(item) -> [item, ..drain(subject)]
    Error(Nil) -> []
  }
}
