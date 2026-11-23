// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/build_info
import fluxer_gateway/config/durations
import fluxer_gateway/gateway/codec
import fluxer_gateway/gateway/limits
import fluxer_gateway/gateway/socket
import fluxer_gateway/gateway/socket_metrics
import fluxer_gateway/gateway/ws/fragment.{type Fragment}
import fluxer_gateway/gateway/ws/frame.{type Frame, type Opcode, Frame}
import fluxer_gateway/gateway/ws/handshake
import fluxer_gateway/gateway/ws/http.{type HttpError, type Version}
import fluxer_gateway/gateway/ws/link.{type Link}
import gleam/bytes_tree.{type BytesTree}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/static_supervisor.{type Supervisor}
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import glisten.{type IpAddress}
import glisten/socket/options

fn init_timeout_ms() -> Int {
  durations.gateway().server_init_timeout_ms
}

fn request_timeout_ms() -> Int {
  durations.gateway().server_request_timeout_ms
}

fn send_timeout_ms() -> Int {
  durations.gateway().server_send_timeout_ms
}

const max_keepalive = 1000

fn registry_sweep_interval_ms() -> Int {
  durations.gateway().server_registry_sweep_interval_ms
}

pub type Route {
  Respond(Response(BytesTree))
  Websocket(socket.Upgrade)
}

pub type Router =
  fn(Request(IpAddress)) -> Route

pub opaque type Message {
  SocketMessage(socket.Message)
  RequestTimeout(token: Int)
  Passive
}

type Ws {
  Ws(
    socket: socket.State,
    link: Link,
    buffer: BitArray,
    fragment: Option(Fragment),
    paused: Bool,
  )
}

type Http {
  Http(
    peer: IpAddress,
    mailbox: socket.Mailbox,
    timers: Subject(Message),
    buffer: BitArray,
    served: Int,
    timer: Int,
  )
}

type State {
  Reading(Http)
  Open(Ws)
  Unknown
}

type Step {
  Next(Ws)
  Done
}

pub fn supervised(
  interface: String,
  port: Int,
  listener_name,
  router: Router,
  on_start: fn(Int) -> Nil,
) -> ChildSpecification(Nil) {
  let builder =
    glisten.new(init, fn(state, message, connection) {
      loop(router, state, message, connection)
    })
    |> glisten.with_close(on_close)
    |> glisten.with_active_state(options.Count(link.receive_window))
    |> glisten.bind(interface)
    |> glisten.with_listener_name(listener_name)
  let listener =
    supervision.supervisor(fn() {
      start(builder, port, listener_name, on_start)
    })
  supervision.supervisor(fn() {
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(supervision.worker(start_sweeper))
    |> static_supervisor.add(listener)
    |> static_supervisor.start
  })
  |> supervision.map_data(fn(_) { Nil })
}

fn start_sweeper() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    process.send_after(self, registry_sweep_interval_ms(), Nil)
    Ok(actor.initialised(self))
  })
  |> actor.on_message(sweep)
  |> actor.start
}

fn sweep(self: Subject(Nil), _tick: Nil) -> actor.Next(Subject(Nil), Nil) {
  socket.prune_dead()
  process.send_after(self, registry_sweep_interval_ms(), Nil)
  actor.continue(self)
}

fn start(
  builder: glisten.Builder(State, Message),
  port: Int,
  listener_name,
  on_start: fn(Int) -> Nil,
) -> Result(actor.Started(Supervisor), actor.StartError) {
  use started <- result.map(glisten.start(builder, port))
  on_start(glisten.get_server_info(listener_name, init_timeout_ms()).port)
  started
}

fn init(
  connection: glisten.Connection(Message),
) -> #(State, Option(process.Selector(Message))) {
  let #(mailbox, socket_selector) = socket.mailbox()
  let timers = process.new_subject()
  let selector =
    process.new_selector()
    |> process.select(timers)
    |> process.select_record(atom.create("tcp_passive"), 1, fn(_) { Passive })
    |> process.select_record(atom.create("ssl_passive"), 1, fn(_) { Passive })
    |> process.merge_selector(process.map_selector(
      socket_selector,
      SocketMessage,
    ))
  let state = case glisten.get_connection_info(connection) {
    Ok(info) ->
      Reading(
        arm_timer(Http(
          peer: info.ip_address,
          mailbox:,
          timers:,
          buffer: <<>>,
          served: 0,
          timer: 0,
        )),
      )
    Error(Nil) -> Unknown
  }
  #(state, Some(selector))
}

fn on_close(state: State) -> Nil {
  case state {
    Open(ws) -> {
      socket_metrics.disconnected(socket_metrics.TransportLost)
      socket.terminate(ws.socket, False)
    }
    Reading(_) | Unknown -> Nil
  }
}

fn loop(
  router: Router,
  state: State,
  message: glisten.Message(Message),
  connection: glisten.Connection(Message),
) -> glisten.Next(State, glisten.Message(Message)) {
  case state, message {
    Reading(reading), glisten.Packet(data) ->
      read(router, reading, connect(connection), data)
    Reading(reading), glisten.User(RequestTimeout(token))
      if token == reading.timer
    -> glisten.stop()
    Reading(_), glisten.User(Passive) -> {
      link.set_receiving(connect(connection), True)
      glisten.continue(state)
    }
    Reading(_), glisten.User(_) -> glisten.continue(state)
    Open(ws), glisten.Packet(data) ->
      frames(Ws(..ws, buffer: <<ws.buffer:bits, data:bits>>))
    Open(ws), glisten.User(SocketMessage(message)) ->
      case live(Ws(..ws, socket: socket.handle(ws.socket, message))) {
        Next(ws) -> settle(ws)
        Done -> glisten.stop()
      }
    Open(ws), glisten.User(Passive) -> {
      case ws.paused {
        True -> Nil
        False -> link.set_receiving(ws.link, True)
      }
      glisten.continue(state)
    }
    Open(_), glisten.User(RequestTimeout(_)) -> glisten.continue(state)
    Unknown, _ -> glisten.stop()
  }
}

fn connect(connection: glisten.Connection(Message)) -> Link {
  link.new(connection.socket, connection.transport)
}

fn arm_timer(reading: Http) -> Http {
  let timer = reading.timer + 1
  process.send_after(
    reading.timers,
    request_timeout_ms(),
    RequestTimeout(timer),
  )
  Http(..reading, timer:)
}

fn read(
  router: Router,
  reading: Http,
  channel: Link,
  data: BitArray,
) -> glisten.Next(State, glisten.Message(Message)) {
  let buffer = <<reading.buffer:bits, data:bits>>
  case http.parse(buffer, reading.peer) {
    Ok(http.Incomplete) -> glisten.continue(Reading(Http(..reading, buffer:)))
    Ok(http.Parsed(request: req, version:, keep_alive:, rest:)) ->
      serve(router, Http(..reading, buffer: <<>>), channel, req, version, {
        keep_alive && reading.served + 1 < max_keepalive
      })
      |> continue_reading(router, channel, rest)
    Error(error) -> {
      reject(channel, error)
      glisten.stop()
    }
  }
}

type Served {
  KeepReading(Http)
  Closed
  Upgraded(Ws)
}

fn continue_reading(
  served: Served,
  router: Router,
  channel: Link,
  rest: BitArray,
) -> glisten.Next(State, glisten.Message(Message)) {
  case served {
    KeepReading(reading) -> read(router, reading, channel, rest)
    Closed -> glisten.stop()
    Upgraded(ws) ->
      case live(ws) {
        Next(ws) -> frames(Ws(..ws, buffer: rest))
        Done -> glisten.stop()
      }
  }
}

fn serve(
  router: Router,
  reading: Http,
  channel: Link,
  req: Request(IpAddress),
  version: Version,
  keep_alive: Bool,
) -> Served {
  case router(req) {
    Respond(reply) -> {
      let sent =
        link.send(channel, http.encode_reply(reply, req.method, !keep_alive))
      case keep_alive, sent {
        True, Ok(Nil) ->
          KeepReading(arm_timer(Http(..reading, served: reading.served + 1)))
        _, _ -> Closed
      }
    }
    Websocket(upgrade) ->
      case handshake.accept(req, version) {
        Error(handshake.UpgradeRequired) -> {
          let reply = versioned(handshake.upgrade_required(), bytes_tree.new())
          let sent =
            link.send(
              channel,
              http.encode_reply(reply, req.method, !keep_alive),
            )
          case keep_alive, sent {
            True, Ok(Nil) ->
              KeepReading(arm_timer(Http(..reading, served: reading.served + 1)))
            _, _ -> Closed
          }
        }
        Error(handshake.BadHandshake) -> {
          let _ =
            link.send(
              channel,
              http.encode(versioned(response.new(400), bytes_tree.new()), True),
            )
          Closed
        }
        Ok(accept) -> {
          let switching =
            versioned(handshake.switching(accept), bytes_tree.new())
          case link.send(channel, http.encode(switching, False)) {
            Error(Nil) -> Closed
            Ok(Nil) -> {
              link.set_send_timeout(channel, send_timeout_ms())
              Upgraded(Ws(
                socket: socket.open(upgrade, reading.mailbox, channel),
                link: channel,
                buffer: <<>>,
                fragment: None,
                paused: False,
              ))
            }
          }
        }
      }
  }
}

fn versioned(reply: Response(a), body: BytesTree) -> Response(BytesTree) {
  let #(name, value) = build_info.version_header()
  reply
  |> response.set_header(name, value)
  |> response.set_body(body)
}

fn reject(channel: Link, error: HttpError) -> Nil {
  let reply =
    response.new(http.error_status(error))
    |> response.set_body(bytes_tree.new())
  let _ = link.send(channel, http.encode(reply, True))
  Nil
}

fn frames(ws: Ws) -> glisten.Next(State, glisten.Message(Message)) {
  case frame.parse(ws.buffer, limits.max_payload_size()) {
    Error(error) -> {
      fail(ws, frame.error_code(error))
      glisten.stop()
    }
    Ok(frame.Incomplete) -> settle(ws)
    Ok(frame.Parsed(frame: received, rest:)) ->
      case on_frame(Ws(..ws, buffer: rest), received) {
        Next(ws) -> frames(ws)
        Done -> glisten.stop()
      }
  }
}

fn on_frame(ws: Ws, received: Frame) -> Step {
  let Frame(fin:, opcode:, payload:) = received
  case opcode, ws.fragment {
    frame.Ping, _ -> {
      link.pong(ws.link, payload)
      Next(ws)
    }
    frame.Pong, _ -> Next(ws)
    frame.Close, _ -> remote_close(ws, payload)
    frame.Continuation, None -> fail(ws, 1002)
    frame.Continuation, Some(partial) -> extend(ws, partial, fin, payload)
    _, Some(_) -> fail(ws, 1002)
    _, None ->
      case fin {
        True -> deliver(ws, opcode, payload)
        False -> Next(Ws(..ws, fragment: Some(fragment.start(opcode, payload))))
      }
  }
}

fn extend(ws: Ws, partial: Fragment, fin: Bool, payload: BitArray) -> Step {
  case fragment.extend(partial, fin, payload, limits.max_payload_size()) {
    fragment.TooBig -> fail(ws, 1009)
    fragment.Complete(opcode:, payload:) ->
      deliver(Ws(..ws, fragment: None), opcode, payload)
    fragment.Partial(partial) -> Next(Ws(..ws, fragment: Some(partial)))
  }
}

fn deliver(ws: Ws, opcode: Opcode, payload: BitArray) -> Step {
  case opcode == frame.Text && !codec.valid_text(payload) {
    True -> fail(ws, 1007)
    False -> live(Ws(..ws, socket: socket.receive(ws.socket, payload)))
  }
}

fn live(ws: Ws) -> Step {
  case socket.closing(ws.socket) {
    True -> {
      socket_metrics.disconnected(socket_metrics.ServerClosed)
      socket.terminate(ws.socket, False)
      Done
    }
    False -> Next(ws)
  }
}

fn remote_close(ws: Ws, payload: BitArray) -> Step {
  case frame.close_code(payload) {
    Error(error) -> fail(ws, frame.error_code(error))
    Ok(code) -> {
      case socket.close_sent(ws.socket), code {
        True, _ -> Nil
        False, Some(code) -> link.close(ws.link, code, "")
        False, None -> link.close_without_code(ws.link)
      }
      let client_closed =
        !socket.close_sent(ws.socket)
        && { code == Some(1000) || code == Some(1001) }
      socket_metrics.disconnected(
        case socket.close_sent(ws.socket), client_closed {
          True, _ -> socket_metrics.ServerClosed
          False, True -> socket_metrics.ClientNormal
          False, False -> socket_metrics.ClientCode
        },
      )
      socket.terminate(ws.socket, client_closed)
      case client_closed {
        True -> exit(#(atom.create("shutdown"), atom.create("client_closed")))
        False -> Nil
      }
      Done
    }
  }
}

fn fail(ws: Ws, code: Int) -> Step {
  socket_metrics.disconnected(socket_metrics.ProtocolError)
  case socket.close_sent(ws.socket) {
    True -> Nil
    False -> link.close(ws.link, code, "")
  }
  socket.terminate(ws.socket, False)
  Done
}

fn settle(ws: Ws) -> glisten.Next(State, glisten.Message(Message)) {
  let paused = socket.paused(ws.socket)
  case paused == ws.paused {
    True -> Nil
    False -> link.set_receiving(ws.link, !paused)
  }
  glisten.continue(Open(Ws(..ws, paused:)))
}

@external(erlang, "erlang", "exit")
fn exit(reason: #(Atom, Atom)) -> Nil
