// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{type Settings}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import support/api_breaker
import support/fake_http
import support/wait

pub type Response {
  Data(Json)
  Status(code: Int, body: String)
}

pub type Rule {
  Reply(Response)
  Respond(fn(Dynamic) -> Response)
  Hold
}

pub type Call {
  Call(kind: String, body: Dynamic)
}

pub opaque type Api {
  Api(subject: Subject(Message), route: fake_http.Route, previous: Settings)
}

pub opaque type Message {
  Incoming(call: Call, reply: Subject(Response))
  SetRule(kind: String, rule: Rule)
  Calls(reply: Subject(List(Call)))
  Release(kind: String, respond: fn(Call) -> Response, reply: Subject(Bool))
  ReleaseWhere(
    kind: String,
    matches: fn(Call) -> Bool,
    respond: fn(Call) -> Response,
    reply: Subject(Bool),
  )
  HeldCount(kind: String, reply: Subject(Int))
  Stop
}

type State {
  State(
    rules: Dict(String, Rule),
    log: List(Call),
    held: List(#(Call, Subject(Response))),
    counter: Int,
  )
}

pub fn start() -> Api {
  let assert Ok(started) =
    actor.new(State(rules: dict.new(), log: [], held: [], counter: 0))
    |> actor.on_message(handle)
    |> actor.start
  process.unlink(started.pid)
  let api = started.data
  let route =
    fake_http.route(fn(request) {
      let #(status, body) =
        process.call_forever(api, Incoming(decode_call(request.body), _))
        |> encode
      fake_http.Answer(status, body)
    })
  let previous = settings.get()
  settings.store(
    settings.Settings(
      ..previous,
      api_rpc_endpoint: Some(fake_http.url(route) <> "/internal/rpc"),
    ),
  )
  let _ = api_breaker.reset()
  Api(subject: api, route:, previous:)
}

pub fn stop(api: Api) -> Nil {
  settings.store(api.previous)
  fake_http.unroute(api.route)
  process.send(api.subject, Stop)
}

pub fn set(api: Api, kind: String, rule: Rule) -> Nil {
  process.send(api.subject, SetRule(kind, rule))
}

pub fn calls(api: Api) -> List(Call) {
  process.call(api.subject, 5000, Calls)
}

pub fn calls_of(api: Api, kind: String) -> List(Call) {
  calls(api) |> list.filter(fn(call) { call.kind == kind })
}

pub fn release(api: Api, kind: String, response: Response) -> Bool {
  process.call(api.subject, 5000, Release(kind, fn(_) { response }, _))
}

pub fn release_token(api: Api) -> Bool {
  process.call(api.subject, 5000, Release(
    "voice_get_token",
    fn(call) { token(option.unwrap(field(call, "connection_id"), "fresh")) },
    _,
  ))
}

pub fn release_where(
  api: Api,
  kind: String,
  matches: fn(Call) -> Bool,
  respond: fn(Call) -> Response,
) -> Bool {
  process.call(api.subject, 5000, ReleaseWhere(kind, matches, respond, _))
}

pub fn held(api: Api, kind: String) -> Int {
  process.call(api.subject, 5000, HeldCount(kind, _))
}

pub fn await_held(api: Api, kind: String, count: Int) -> Bool {
  eventually(fn() { held(api, kind) >= count })
}

pub fn eventually(check: fn() -> Bool) -> Bool {
  wait.until(5000, check)
}

pub fn settle() -> Nil {
  let _ = wait.until(1500, fn() { False })
  Nil
}

pub fn field(call: Call, key: String) -> Option(String) {
  decode.run(
    call.body,
    decode.at(
      [key],
      decode.one_of(decode.string, [decode.map(decode.int, int.to_string)]),
    ),
  )
  |> option.from_result
}

pub fn bool_field(call: Call, key: String) -> Option(Bool) {
  decode.run(call.body, decode.at([key], decode.bool)) |> option.from_result
}

pub fn token(connection_id: String) -> Response {
  Data(
    json.object([
      #("token", json.string("token-" <> connection_id)),
      #("endpoint", json.string("wss://sfu.test")),
      #("connectionId", json.string(connection_id)),
      #("regionId", json.string("eu")),
      #("serverId", json.string("sfu-1")),
    ]),
  )
}

pub fn p2p_grant(connection_id: String) -> Response {
  Data(
    json.object([
      #("p2p", json.bool(True)),
      #("connectionId", json.string(connection_id)),
      #(
        "iceServers",
        json.preprocessed_array([
          json.object([#("urls", json.string("stun:stun.test"))]),
        ]),
      ),
      #("maxParticipants", json.int(4)),
    ]),
  )
}

pub fn success() -> Response {
  Data(json.object([#("success", json.bool(True))]))
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Incoming(call, reply) -> {
      let state = State(..state, log: [call, ..state.log])
      case dict.get(state.rules, call.kind) {
        Ok(Hold) ->
          actor.continue(
            State(..state, held: list.append(state.held, [#(call, reply)])),
          )
        Ok(Reply(response)) -> {
          process.send(reply, response)
          actor.continue(state)
        }
        Ok(Respond(make)) -> {
          process.send(reply, make(call.body))
          actor.continue(state)
        }
        Error(Nil) -> {
          let #(response, counter) = default_response(call, state.counter)
          process.send(reply, response)
          actor.continue(State(..state, counter:))
        }
      }
    }
    SetRule(kind, rule) ->
      actor.continue(
        State(..state, rules: dict.insert(state.rules, kind, rule)),
      )
    Calls(reply) -> {
      process.send(reply, list.reverse(state.log))
      actor.continue(state)
    }
    Release(kind, respond, reply) ->
      case list.split_while(state.held, fn(pair) { { pair.0 }.kind != kind }) {
        #(before, [#(call, waiting), ..after]) -> {
          process.send(waiting, respond(call))
          process.send(reply, True)
          actor.continue(State(..state, held: list.append(before, after)))
        }
        _ -> {
          process.send(reply, False)
          actor.continue(state)
        }
      }
    ReleaseWhere(kind, matches, respond, reply) ->
      case
        list.split_while(state.held, fn(pair) {
          { pair.0 }.kind != kind || !matches(pair.0)
        })
      {
        #(before, [#(call, waiting), ..after]) -> {
          process.send(waiting, respond(call))
          process.send(reply, True)
          actor.continue(State(..state, held: list.append(before, after)))
        }
        _ -> {
          process.send(reply, False)
          actor.continue(state)
        }
      }
    HeldCount(kind, reply) -> {
      process.send(
        reply,
        list.count(state.held, fn(pair) { { pair.0 }.kind == kind }),
      )
      actor.continue(state)
    }
    Stop -> {
      list.each(state.held, fn(pair) {
        process.send(pair.1, Status(503, "stopped"))
      })
      actor.stop()
    }
  }
}

fn default_response(call: Call, counter: Int) -> #(Response, Int) {
  case call.kind {
    "voice_get_token" -> {
      let conn = case field(call, "connection_id") {
        Some(conn) -> conn
        None -> "conn-" <> int.to_string(counter + 1)
      }
      #(token(conn), counter + 1)
    }
    "voice_state_upsert"
    | "voice_state_remove"
    | "voice_force_disconnect_participant"
    | "voice_update_participant"
    | "voice_update_participant_permissions"
    | "call_ended" -> #(success(), counter)
    _ -> #(Status(404, "{\"code\":\"UNKNOWN\"}"), counter)
  }
}

fn decode_call(body: BitArray) -> Call {
  let parsed = json.parse_bits(body, decode.dynamic)
  let body = result.unwrap(parsed, dynamic.nil())
  let kind =
    decode.run(body, decode.at(["type"], decode.string))
    |> result.unwrap("")
  Call(kind:, body:)
}

fn encode(response: Response) -> #(Int, String) {
  case response {
    Data(data) -> #(200, json.to_string(json.object([#("data", data)])))
    Status(code, body) -> #(code, body)
  }
}
