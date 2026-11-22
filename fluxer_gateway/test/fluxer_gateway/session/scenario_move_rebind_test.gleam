// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/control
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/seed.{type Seed}
import fluxer_gateway/session/server
import fluxer_gateway/session/state
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/guild_fixture as fixture
import support/session_harness as h
import support/session_move as move
import support/session_transfer_tap as tap
import support/wait

fn adopt(found: Seed) -> SessionRef {
  let reply = process.new_subject()
  process.spawn_unlinked(fn() {
    let assert Ok(started) =
      server.start(server.Start(seed: found, socket: None, reply: None))
    process.send(reply, started.data)
  })
  let assert Ok(ref) = process.receive(reply, 5000)
  ref
}

fn ready(sock: move.Sock) -> Int {
  assert wait.until(8000, fn() { move.ready_seq(move.seen(sock)) != None })
  let assert Some(seq) = move.ready_seq(move.seen(sock))
  seq
}

fn in_background(run: fn() -> Bool) -> process.Subject(Bool) {
  let done = process.new_subject()
  process.spawn_unlinked(fn() { process.send(done, run()) })
  done
}

fn move_here(ref: SessionRef) -> SessionRef {
  let transfer = tap.start()
  let answer = process.new_subject()
  control.send(ref, control.HandOff(node.self(), answer))
  let assert Ok(#(exported, commit)) = tap.committing(transfer, 5000)
  let target = adopt(exported)
  process.send(commit, Ok(target))
  assert process.receive(answer, 5000) == Ok(True)
  tap.release(transfer)
  target
}

fn delivered(sock: move.Sock, event: String, total: Int) -> Bool {
  wait.until(15_000, fn() { move.count(move.seen(sock), event) >= total })
}

pub fn a_moved_session_keeps_its_guild_and_loses_no_event_test_() {
  use <- fixture.slow(90)
  let user = h.unique_user()
  let found = move.guild(user)
  use <- h.with_api(move.responder(user, [found]))
  let sock = move.socket()
  let key = move.start(sock, "tok-mv1")
  let ready_seq = ready(sock)
  let assert Ok(ref) = registry.lookup(key)
  let source = h.session_pid(ref)
  assert move.hosts_guild(found.id)
  assert move.send_messages(found.id, 1, 5, 0)
  assert delivered(sock, "MESSAGE_CREATE", 5)
  assert move.send_notes(user, 1, 5, 0)
  assert delivered(sock, "USER_NOTE_UPDATE", 5)

  let messages = in_background(fn() { move.send_messages(found.id, 6, 400, 2) })
  let notes = in_background(fn() { move.send_notes(user, 6, 200, 4) })
  process.sleep(200)
  let target = move_here(ref)
  assert process.receive(messages, 20_000) == Ok(True)
  assert process.receive(notes, 20_000) == Ok(True)
  assert h.await_exit(source, 15_000)
  assert delivered(sock, "MESSAGE_CREATE", 405)
  assert delivered(sock, "USER_NOTE_UPDATE", 205)
  process.sleep(300)

  let seen = move.seen(sock)
  assert move.count(seen, "GUILD_CREATE") == 0 as move.describe(seen)
  assert move.count(seen, "GUILD_DELETE") == 0
  assert move.named(seen, "MESSAGE_CREATE") == move.numbers(1, 405)
  assert move.named(seen, "USER_NOTE_UPDATE") == move.numbers(1, 205)
  assert move.seqs_follow(seen)
  assert move.ready_seq(seen) == Some(ready_seq)
  assert !list.contains(seen, move.Reconnect)
  assert list.any(seen, fn(item) {
    case item {
      move.Moved(_) -> True
      _ -> False
    }
  })
  assert h.state_of(target).arrival == state.Settled
  assert registry.lookup(key) == Ok(target)

  assert move.send_messages(found.id, 406, 3, 0)
  assert delivered(sock, "MESSAGE_CREATE", 408)
  assert move.named(move.seen(sock), "MESSAGE_CREATE") == move.numbers(1, 408)
  assert move.count(move.seen(sock), "GUILD_CREATE") == 0
  h.terminate(target)
  fixture.stop(found)
}

pub fn a_session_moved_twice_under_traffic_keeps_every_guild_test_() {
  use <- fixture.slow(90)
  let user = h.unique_user()
  let first = move.guild(user)
  let second = move.guild(user)
  use <- h.with_api(move.responder(user, [first, second]))
  let sock = move.socket()
  let key = move.start(sock, "tok-mv2")
  let _ = ready(sock)
  let assert Ok(ref) = registry.lookup(key)
  assert move.hosts_guild(first.id) && move.hosts_guild(second.id)

  let a = in_background(fn() { move.send_messages(first.id, 1, 300, 2) })
  let b = in_background(fn() { move.send_messages(second.id, 1001, 300, 2) })
  process.sleep(100)
  let middle = move_here(ref)
  process.sleep(150)
  let last = move_here(middle)
  assert process.receive(a, 20_000) == Ok(True)
  assert process.receive(b, 20_000) == Ok(True)
  assert delivered(sock, "MESSAGE_CREATE", 600)
  process.sleep(300)

  let seen = move.seen(sock)
  assert move.count(seen, "GUILD_CREATE") == 0 as move.describe(seen)
  assert move.count(seen, "GUILD_DELETE") == 0
  let ids = move.named(seen, "MESSAGE_CREATE")
  assert list.filter(ids, fn(found) { string.length(found) < 4 })
    == move.numbers(1, 300)
  assert list.filter(ids, fn(found) { string.length(found) == 4 })
    == move.numbers(1001, 300)
  assert move.seqs_follow(seen)
  assert !list.contains(seen, move.Reconnect)
  assert h.state_of(last).arrival == state.Settled
  h.terminate(last)
  fixture.stop(first)
  fixture.stop(second)
}

fn slow_guild(user: Int, found: fixture.Guild, delay_ms: Int) -> h.Responder {
  let base = move.responder(user, [found])
  fn(kind, body) {
    case kind, decode.run(body, decode.at(["collection"], decode.string)) {
      "guild_collection", Ok("guild") -> process.sleep(delay_ms)
      _, _ -> Nil
    }
    base(kind, body)
  }
}

pub fn a_session_moved_before_ready_still_sends_one_ready_on_the_same_socket_test_() {
  use <- fixture.slow(60)
  let user = h.unique_user()
  let found = move.guild(user)
  use <- h.with_api(slow_guild(user, found, 2000))
  let sock = move.socket()
  let key = move.start(sock, "tok-mv-early")
  assert wait.until(5000, fn() { registry.lookup(key) != Error(Nil) })
  let assert Ok(ref) = registry.lookup(key)
  let source = h.session_pid(ref)
  assert move.ready_seq(move.seen(sock)) == None
  let target = move_here(ref)
  assert h.await_exit(source, 15_000)
  let _ = ready(sock)
  process.sleep(300)

  let seen = move.seen(sock)
  assert move.count(seen, "READY") == 1 as move.describe(seen)
  assert !list.contains(seen, move.Reconnect)
  assert !list.contains(seen, move.Closed)
  assert registry.lookup(key) == Ok(target)
  h.terminate(target)
  fixture.stop(found)
}
