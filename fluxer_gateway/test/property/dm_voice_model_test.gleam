// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client

import fluxer_gateway/voice/rpc
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import property/gen.{type Rng}
import support/geo_probe as probe
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake

type Model {
  Model(
    channel: Int,
    devices: Dict(Int, fake.Fake),
    conns: Dict(String, #(Int, Int)),
    live: Bool,
    next: Int,
    log: List(String),
  )
}

fn dm_channel(recipients: List(Int)) -> api.Rule {
  api.Reply(
    api.Data(
      json.object([
        #(
          "channel",
          json.object([
            #("type", json.int(1)),
            #(
              "recipients",
              json.array(recipients, fn(user) {
                json.object([#("id", json.string(int.to_string(user)))])
              }),
            ),
          ]),
        ),
      ]),
    ),
  )
}

fn handle(
  method: String,
  fields: List(#(String, Json)),
) -> Result(String, String) {
  rpc.handle(method, raw_json.from_json(json.object(fields)))
  |> result.map(json.to_string)
  |> result.map_error(string.inspect)
}

fn create_call(channel: Int) -> Result(Nil, String) {
  handle("call.create", [
    #("channel_id", json.string(int.to_string(channel))),
    #("message_id", json.string(int.to_string(channel + 1000))),
    #("region", json.null()),
    #("ringing", json.array([], json.string)),
    #("recipients", json.array(["1", "2"], json.string)),
  ])
  |> result.replace(Nil)
}

fn connection_ids(channel: Int) -> List(String) {
  case manager.lookup(g.channel_id(channel)) {
    Ok(call) ->
      case call_server.get_state(call, 2000) {
        Ok(view) -> list.map(view.connections, voice_ids.of)
        Error(Nil) -> []
      }
    Error(_) -> []
  }
  |> list.sort(string.compare)
}

fn call_gone(channel: Int) -> Bool {
  manager.lookup(g.channel_id(channel)) == Error(manager.NotFound)
}

fn updates(device: fake.Fake) -> List(Dynamic) {
  fake.dispatched(device, "VOICE_SERVER_UPDATE")
}

fn log(model: Model, line: String) -> Model {
  Model(..model, log: [line, ..model.log])
}

fn join(model: Model, rng: Rng) -> Result(#(Model, Rng), String) {
  let #(fresh, rng) = gen.chance(rng, 35)
  let #(user, rng) = gen.pick(rng, [1, 2], 1)
  let #(model, key) = case fresh || dict.is_empty(model.devices) {
    True -> #(
      Model(
        ..model,
        devices: dict.insert(model.devices, model.next, fake.start(user)),
        next: model.next + 1,
      ),
      model.next,
    )
    False -> {
      let #(key, _) = gen.pick(rng, dict.keys(model.devices), 0)
      #(model, key)
    }
  }
  let assert Ok(device) = dict.get(model.devices, key)
  let user = case device.ref.user == g.user_id(1) {
    True -> 1
    False -> 2
  }
  let mine = dict.filter(model.conns, fn(_, owner) { owner.1 == user })
  case dict.size(mine) >= 3 {
    True -> Ok(#(model, rng))
    False -> {
      use model <- result.try(case model.live {
        True -> Ok(model)
        False ->
          create_call(model.channel)
          |> result.replace(Model(..model, live: True))
      })
      let before = list.length(updates(device))
      client.dm_voice_update(
        device.ref,
        g.client_update(g.join_with(model.channel, probe.fields())),
        calls: [],
      )
      use _ <- result.try(
        case api.eventually(fn() { list.length(updates(device)) > before }) {
          True -> Ok(Nil)
          False -> Error("no VOICE_SERVER_UPDATE for a DM join")
        },
      )
      let conn =
        updates(device)
        |> list.last
        |> result.map(g.text_of(_, "connection_id"))
        |> result.unwrap(None)
      case conn {
        None -> Error("DM join gave no connection id")
        Some(conn) ->
          Ok(#(
            log(
              Model(
                ..model,
                conns: dict.insert(model.conns, conn, #(key, user)),
              ),
              "join user "
                <> int.to_string(user)
                <> " device "
                <> int.to_string(key)
                <> " as "
                <> conn,
            ),
            rng,
          ))
      }
    }
  }
}

fn pick_conn(model: Model, rng: Rng) -> #(Option(#(String, #(Int, Int))), Rng) {
  case dict.to_list(model.conns) {
    [] -> #(None, rng)
    all -> {
      let #(found, rng) = gen.pick(rng, all, #("", #(0, 0)))
      #(Some(found), rng)
    }
  }
}

fn leave_conn(model: Model, rng: Rng) -> Result(#(Model, Rng), String) {
  let #(picked, rng) = pick_conn(model, rng)
  case picked {
    None -> Ok(#(model, rng))
    Some(#(conn, #(key, _))) -> {
      let assert Ok(device) = dict.get(model.devices, key)
      client.dm_voice_update(
        device.ref,
        g.client_update(g.update(None, Some(conn), [])),
        calls: [],
      )
      Ok(#(
        log(
          Model(..model, conns: dict.delete(model.conns, conn)),
          "leave " <> conn,
        ),
        rng,
      ))
    }
  }
}

fn leave_device(model: Model, rng: Rng) -> Result(#(Model, Rng), String) {
  case dict.keys(model.devices) {
    [] -> Ok(#(model, rng))
    keys -> {
      let #(key, rng) = gen.pick(rng, keys, 0)
      let assert Ok(device) = dict.get(model.devices, key)
      client.dm_voice_update(
        device.ref,
        g.client_update(g.update(None, None, [])),
        calls: [],
      )
      Ok(#(
        log(
          Model(
            ..model,
            conns: dict.filter(model.conns, fn(_, owner) { owner.0 != key }),
          ),
          "leave all of device " <> int.to_string(key),
        ),
        rng,
      ))
    }
  }
}

fn disconnect(model: Model, rng: Rng) -> Result(#(Model, Rng), String) {
  let #(user, rng) = gen.pick(rng, [1, 2], 1)
  let #(targeted, rng) = gen.chance(rng, 50)
  let mine =
    dict.to_list(model.conns) |> list.filter(fn(pair) { pair.1.1 == user })
  let #(conn, rng) = case targeted, mine {
    True, [_, ..] -> {
      let #(pair, rng) = gen.pick(rng, mine, #("", #(0, 0)))
      #(Some(pair.0), rng)
    }
    _, _ -> #(None, rng)
  }
  let fields = [
    #("channel_id", json.string(int.to_string(model.channel))),
    #("user_id", json.string(int.to_string(user))),
    ..case conn {
      Some(conn) -> [#("connection_id", json.string(conn))]
      None -> []
    }
  ]
  let _ = handle("call.disconnect_user_if_in_channel", fields)
  let gone = case conn {
    Some(conn) -> [conn]
    None -> list.map(mine, fn(pair) { pair.0 })
  }
  Ok(#(
    log(
      Model(..model, conns: dict.drop(model.conns, gone)),
      "rpc disconnect user "
        <> int.to_string(user)
        <> " conn "
        <> string.inspect(conn),
    ),
    rng,
  ))
}

fn check(model: Model) -> Result(Model, String) {
  let want = dict.keys(model.conns) |> list.sort(string.compare)
  case want, model.live {
    [], True -> {
      use _ <- result.try(
        case api.eventually(fn() { call_gone(model.channel) }) {
          True -> Ok(Nil)
          False ->
            Error(
              "the call outlived its last connection, still holding "
              <> string.inspect(connection_ids(model.channel)),
            )
        },
      )
      Ok(Model(..model, live: False))
    }
    _, _ -> {
      use _ <- result.try(
        case api.eventually(fn() { connection_ids(model.channel) == want }) {
          True -> Ok(Nil)
          False ->
            Error(
              "call connections "
              <> string.inspect(connection_ids(model.channel))
              <> " want "
              <> string.inspect(want),
            )
        },
      )
      let leaked =
        dict.values(model.devices)
        |> list.flat_map(fn(device) {
          fake.seen(device).dispatches
          |> list.map(fn(pair) { string.inspect(pair.1) })
        })
        |> probe.leaking
      case leaked {
        [] -> Ok(model)
        found -> Error("coordinates reached a frame: " <> string.inspect(found))
      }
    }
  }
}

fn step(model: Model, rng: Rng) -> Result(#(Model, Rng), String) {
  let #(kind, rng) = gen.int(rng, 6)
  case kind {
    0 | 1 | 2 -> join(model, rng)
    3 -> leave_conn(model, rng)
    4 -> leave_device(model, rng)
    _ -> disconnect(model, rng)
  }
}

fn run(model: Model, rng: Rng, steps: Int) -> Result(Model, String) {
  case steps <= 0 {
    True -> Ok(model)
    False ->
      case step(model, rng) {
        Error(reason) -> Error(reason <> trail(model))
        Ok(#(next, rng)) ->
          case check(next) {
            Ok(next) -> run(next, rng, steps - 1)
            Error(reason) -> Error(reason <> trail(next))
          }
      }
  }
}

fn trail(model: Model) -> String {
  "\n  after: " <> string.join(list.reverse(list.take(model.log, 14)), "\n    ")
}

fn dm_case(rng: Rng) -> Result(Nil, String) {
  let #(slot, rng) = gen.int(rng, 1_000_000)
  let channel = 7_000_000 + slot
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  let model =
    Model(
      channel:,
      devices: dict.new(),
      conns: dict.new(),
      live: False,
      next: 0,
      log: [],
    )
  let outcome = run(model, rng, 20)
  case outcome {
    Ok(model) -> dict.each(model.devices, fn(_, device) { fake.stop(device) })
    Error(_) -> Nil
  }
  api.stop(stub)
  outcome |> result.replace(Nil)
}

pub fn dm_call_connections_follow_the_model_test() {
  gen.check("dm voice model", 8, dm_case)
}

pub fn one_device_keeps_two_connections_in_a_call_test() {
  let assert Ok(Nil) = dm_case(gen.new(812_473_366))
}
