// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{
  type Identify, type Outbound, type SessionRef, type SocketMsg, type SocketRef,
  type StartError, Close, DispatchFrame, Send, SocketRef,
}
import fluxer_gateway/push/server as push_server
import fluxer_gateway/session/control
import fluxer_gateway/session/manager
import fluxer_gateway/session/state.{type State}
import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glisten
import support/api_breaker
import support/isolation
import support/wait

pub type Reply {
  Reply(status: Int, body: String)
}

pub type Responder =
  fn(String, Dynamic) -> Reply

pub type Frame {
  Frame(seq: Int, event: String, payload: Dynamic)
  Other(Outbound)
  Closed(code: String)
  Rebound(seq: Int)
  Reattached
}

pub type FakeSocket {
  FakeSocket(ref: SocketRef, frames: Subject(Frame))
}

@external(erlang, "persistent_term", "put")
fn put_term(key: Atom, value: a) -> Atom

@external(erlang, "persistent_term", "get")
fn get_term(key: Atom, default: a) -> a

@external(erlang, "sys", "get_state")
fn sys_get_state(pid: Pid) -> State

@external(erlang, "erlang", "exit")
fn exit_term(pid: a, reason: b) -> Bool

fn responder_key() -> Atom {
  atom.create("fluxer_gateway_session_harness_responder")
}

fn port_key() -> Atom {
  atom.create("fluxer_gateway_session_harness_port")
}

pub fn ok_data(data: Json) -> Reply {
  Reply(200, json.to_string(json.object([#("data", data)])))
}

pub fn empty_ok(_kind: String, _body: Dynamic) -> Reply {
  ok_data(json.object([]))
}

fn respond(kind: String, body: Dynamic) -> Reply {
  let responder: Responder = get_term(responder_key(), empty_ok)
  responder(kind, body)
}

pub fn with_api(responder: Responder, run: fn() -> a) -> a {
  let port = ensure_stub()
  let _ = api_breaker.reset()
  put_term(responder_key(), responder)
  let previous = settings.get()
  let _ =
    settings.store(
      settings.Settings(
        ..previous,
        api_rpc_endpoint: Some(
          "http://127.0.0.1:" <> int.to_string(port) <> "/internal/rpc",
        ),
      ),
    )
  let outcome = run()
  let _ = wait.until(15_000, fn() { push_server.is_idle(push_server.name()) })
  let _ = settings.store(previous)
  put_term(responder_key(), empty_ok)
  outcome
}

fn ensure_stub() -> Int {
  let running = case get_term(port_key(), None) {
    Some(listener) ->
      case process.named(listener) {
        Ok(_) -> Some(glisten.get_server_info(listener, 5000).port)
        Error(Nil) -> None
      }
    None -> None
  }
  case running {
    Some(port) -> port
    None -> {
      let ready = process.new_subject()
      process.spawn_unlinked(fn() { hold_stub(ready) })
      let assert Ok(listener) = process.receive(ready, 5000)
      put_term(port_key(), Some(listener))
      glisten.get_server_info(listener, 5000).port
    }
  }
}

fn hold_stub(ready) -> Nil {
  isolation.keep_across_tests()
  let listener = process.new_name("session_harness_listener")
  let assert Ok(_) =
    glisten.new(fn(_) { #(<<>>, None) }, serve)
    |> glisten.with_listener_name(listener)
    |> glisten.start(0)
  process.send(ready, listener)
  process.sleep_forever()
}

fn serve(
  buffer: BitArray,
  message: glisten.Message(Nil),
  connection: glisten.Connection(Nil),
) -> glisten.Next(BitArray, glisten.Message(Nil)) {
  case message {
    glisten.User(Nil) -> glisten.continue(buffer)
    glisten.Packet(data) -> {
      let buffer = bit_array.append(buffer, data)
      case complete_body(buffer) {
        Error(Nil) -> glisten.continue(buffer)
        Ok(body) -> {
          let reply = case json.parse(body, decode.dynamic) {
            Ok(decoded) ->
              case decode.run(decoded, decode.at(["type"], decode.string)) {
                Ok(kind) -> respond(kind, decoded)
                Error(_) -> Reply(400, "{}")
              }
            Error(_) -> Reply(400, "{}")
          }
          let head =
            "HTTP/1.1 "
            <> int.to_string(reply.status)
            <> " X\r\ncontent-type: application/json\r\ncontent-length: "
            <> int.to_string(string.byte_size(reply.body))
            <> "\r\n\r\n"
          case
            glisten.send(connection, bytes_tree.from_string(head <> reply.body))
          {
            Ok(Nil) -> glisten.continue(<<>>)
            Error(_) -> glisten.stop()
          }
        }
      }
    }
  }
}

fn complete_body(buffer: BitArray) -> Result(String, Nil) {
  use text <- result.try(bit_array.to_string(buffer))
  use #(head, body) <- result.try(string.split_once(text, "\r\n\r\n"))
  let length =
    head
    |> string.lowercase
    |> string.split("\r\n")
    |> list.find_map(fn(line) {
      case string.split_once(line, ":") {
        Ok(#("content-length", value)) -> int.parse(string.trim(value))
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(0)
  case string.byte_size(body) >= length {
    True -> Ok(body)
    False -> Error(Nil)
  }
}

pub fn fake_socket() -> FakeSocket {
  let frames = process.new_subject()
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outbox = process.new_subject()
      process.send(ready, outbox)
      forward(outbox, frames)
    })
  let assert Ok(outbox) = process.receive(ready, 1000)
  FakeSocket(ref: SocketRef(pid:, outbox:), frames:)
}

fn forward(outbox: Subject(SocketMsg), frames: Subject(Frame)) -> Nil {
  let message = process.receive_forever(outbox)
  list.each(flatten(message), process.send(frames, _))
  forward(outbox, frames)
}

pub fn drop_socket(socket: FakeSocket) -> Nil {
  process.kill(socket.ref.pid)
}

fn flatten(message: SocketMsg) -> List(Frame) {
  case message {
    Send(_, outbound) ->
      list.map(outbound, fn(frame) {
        case frame {
          DispatchFrame(seq, event, payload) ->
            Frame(
              seq:,
              event:,
              payload: json.parse_bits(
                raw_json.to_bits(payload),
                decode.dynamic,
              )
                |> result.unwrap(dynamic.nil()),
            )
          other -> Other(other)
        }
      })
    Close(_, code, _) -> [Closed(string.inspect(code))]
    session.Moved(from:, seq:, ..) -> {
      session.send(from, session.SocketDown(process.self()))
      [Rebound(seq)]
    }
    session.Reattached(..) -> [Reattached]
  }
}

pub fn await_event(
  socket: FakeSocket,
  event: String,
  timeout_ms: Int,
) -> Result(Frame, List(Frame)) {
  await_frame(socket, timeout_ms, fn(frame) {
    case frame {
      Frame(_, found, _) -> found == event
      _ -> False
    }
  })
}

pub fn await_frame(
  socket: FakeSocket,
  timeout_ms: Int,
  matches: fn(Frame) -> Bool,
) -> Result(Frame, List(Frame)) {
  await_loop(socket, now_ms() + timeout_ms, [], matches)
}

fn await_loop(
  socket: FakeSocket,
  deadline: Int,
  seen: List(Frame),
  matches: fn(Frame) -> Bool,
) -> Result(Frame, List(Frame)) {
  case process.receive(socket.frames, int.max(deadline - now_ms(), 0)) {
    Error(Nil) -> Error(list.reverse(seen))
    Ok(frame) ->
      case matches(frame) {
        True -> Ok(frame)
        False -> await_loop(socket, deadline, [frame, ..seen], matches)
      }
  }
}

pub fn collect(socket: FakeSocket, quiet_ms: Int) -> List(Frame) {
  collect_loop(socket, quiet_ms, [])
}

fn collect_loop(
  socket: FakeSocket,
  quiet_ms: Int,
  acc: List(Frame),
) -> List(Frame) {
  case process.receive(socket.frames, quiet_ms) {
    Ok(frame) -> collect_loop(socket, quiet_ms, [frame, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

pub fn events(frames: List(Frame)) -> List(String) {
  list.filter_map(frames, fn(frame) {
    case frame {
      Frame(_, event, _) -> Ok(event)
      _ -> Error(Nil)
    }
  })
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: Atom) -> Int

pub fn now_ms() -> Int {
  monotonic(atom.create("millisecond"))
}

pub fn wait_until(check: fn() -> Bool, timeout_ms: Int) -> Bool {
  wait.until(timeout_ms, check)
}

pub fn identify(token: String) -> Identify {
  session.Identify(
    token:,
    properties: session.ClientProperties(os: "linux", browser: "", device: ""),
    shard: None,
    presence: None,
    flags: session.no_flags(),
    ignored_events: [],
    initial_guild_id: None,
    e2ee_capable: False,
    mobile: False,
    latitude: None,
    longitude: None,
  )
}

pub fn unique_user() -> Int {
  100_000_000_000_000_000 + int.random(800_000_000_000_000_000)
}

pub fn user_json(user: Int, bot: Bool) -> Json {
  json.object([
    #("id", json.string(int.to_string(user))),
    #("username", json.string("u" <> int.to_string(user))),
    #("bot", json.bool(bot)),
  ])
}

pub fn session_data(user: Int, bot: Bool, guilds: List(Int)) -> Json {
  session_data_with(user, bot, guilds, [])
}

pub fn session_data_with(
  user: Int,
  bot: Bool,
  guilds: List(Int),
  extra: List(#(String, Json)),
) -> Json {
  json.object([
    #("user", user_json(user, bot)),
    #(
      "guilds",
      json.array(guilds, fn(guild) {
        json.object([#("id", json.string(int.to_string(guild)))])
      }),
    ),
    ..extra
  ])
}

pub fn session_api(data: Json) -> Responder {
  fn(kind, _body) {
    case kind {
      "session" -> ok_data(data)
      _ -> ok_data(json.object([]))
    }
  }
}

pub fn start_session(
  identify: Identify,
  socket: FakeSocket,
) -> Result(SessionRef, StartError) {
  let reply = process.new_subject()
  manager.start(
    identify,
    session.Peer(ip: "127.0.0.1", api_version: 1),
    socket.ref,
    reply,
  )
  case process.receive(reply, 15_000) {
    Ok(result) -> result
    Error(Nil) -> Error(session.StartUnknownError)
  }
}

pub fn session_pid(ref: SessionRef) -> Pid {
  let assert Ok(pid) = process.subject_owner(ref.inbox)
  pid
}

pub fn state_of(ref: SessionRef) -> State {
  sys_get_state(session_pid(ref))
}

pub fn guild_id(value: Int) -> id.GuildId {
  let assert Ok(guild) = id.guild_id(value)
  guild
}

pub fn resume(
  ref: SessionRef,
  token: String,
  seq: Int,
  socket: FakeSocket,
) -> Result(SessionRef, session.ResumeError) {
  resume_id(session.session_id_to_string(ref.id), token, seq, socket)
}

pub fn resume_id(
  key: String,
  token: String,
  seq: Int,
  socket: FakeSocket,
) -> Result(SessionRef, session.ResumeError) {
  let assert Ok(session_id) = session.session_id_from_string(key)
  let reply = process.new_subject()
  manager.resume(session_id, token, seq, socket.ref, reply)
  case process.receive(reply, 15_000) {
    Ok(result) -> result
    Error(Nil) -> Error(session.ResumeUnavailable(-1))
  }
}

pub fn terminate(ref: SessionRef) -> Nil {
  case process.subject_owner(ref.inbox) {
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      session.send(ref, session.Terminate(session.TerminatedByManager))
      let _ =
        process.new_selector()
        |> process.select_specific_monitor(monitor, fn(_) { Nil })
        |> process.selector_receive(5000)
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn await_exit(pid: Pid, timeout_ms: Int) -> Bool {
  let monitor = process.monitor(pid)
  case
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(timeout_ms)
  {
    Ok(Nil) -> True
    Error(Nil) -> {
      process.demonitor_process(monitor)
      False
    }
  }
}

pub fn is_detached(ref: SessionRef) -> Bool {
  case state_of(ref).socket {
    state.Detached(_) -> True
    state.Attached(_, _) -> False
  }
}

pub fn ready_session(token: String) -> #(SessionRef, FakeSocket, Int) {
  let socket = fake_socket()
  let assert Ok(ref) = start_session(identify(token), socket)
  let assert Ok(Frame(seq, "READY", _)) = await_event(socket, "READY", 8000)
  #(ref, socket, seq)
}

pub fn offline() -> presence.Status {
  presence.Offline
}

pub fn close_socket_by_client(socket: FakeSocket) -> Nil {
  let _ =
    exit_term(socket.ref.pid, #(
      atom.create("shutdown"),
      atom.create("client_closed"),
    ))
  Nil
}

pub fn control(ref: SessionRef, message: control.Control) -> Nil {
  control.send(ref, message)
}

pub fn leave(guild: Int) -> control.Control {
  control.GuildLeave(guild_id(guild))
}

pub fn call_unmonitor(channel: id.ChannelId) -> control.Control {
  control.CallUnmonitor(channel)
}

pub fn exit_handoff(pid: Pid) -> Nil {
  let _ = exit_term(pid, #(atom.create("shutdown"), atom.create("handoff")))
  Nil
}

pub fn resume_class() -> guild.JoinClass {
  guild.Resume
}

@external(erlang, "sys", "suspend")
fn sys_suspend(pid: Pid) -> Dynamic

@external(erlang, "sys", "resume")
fn sys_resume(pid: Pid) -> Dynamic

pub fn suspend(pid: Pid) -> Nil {
  let _ = sys_suspend(pid)
  Nil
}

pub fn resume_process(pid: Pid) -> Nil {
  let _ = sys_resume(pid)
  Nil
}

pub fn stub_port() -> Int {
  ensure_stub()
}

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: Int) -> List(#(Int, Int))

@external(erlang, "ets", "whereis")
fn ets_whereis(table: Atom) -> Dynamic

fn counter(table: String, key: Int) -> Int {
  let name = atom.create(table)
  case ets_whereis(name) == atom.to_dynamic(atom.create("undefined")) {
    True -> 0
    False ->
      case ets_lookup(name, key) {
        [#(_, count), ..] -> count
        [] -> 0
      }
  }
}

pub fn join_quota(guild: Int) -> Int {
  counter("fluxer_gateway_session_join_quota", guild)
}

pub fn user_sessions(user: Int) -> Int {
  counter("session_user_counts", user)
}
