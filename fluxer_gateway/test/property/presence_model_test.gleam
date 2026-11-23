// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{type Status, Dnd, Idle, Invisible, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/client.{type PresenceLink}
import fluxer_gateway/presence/status
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import property/gen.{type Rng}
import support/presence_scenario as scn
import support/session_harness.{type Frame, Frame} as h

const quiet_ms = 120

type Device {
  Device(friend: Int, link: PresenceLink, status: Status)
}

type Model {
  Model(
    friends: List(Int),
    pool: List(Int),
    devices: Dict(Int, Device),
    next: Int,
    seen: Dict(Int, String),
    log: List(String),
  )
}

fn id_text() -> decode.Decoder(String) {
  decode.one_of(decode.string, [decode.int |> decode.map(int.to_string)])
}

fn shown(model: Model, friend: Int) -> String {
  dict.values(model.devices)
  |> list.filter(fn(device) { device.friend == friend })
  |> list.map(fn(device) { device.status })
  |> status.precedence
  |> status.to_wire
}

fn updates(frames: List(Frame)) -> List(#(Int, String, String)) {
  list.filter_map(frames, fn(frame) {
    case frame {
      Frame(_, "PRESENCE_UPDATE", payload) -> {
        let user =
          decode.run(payload, decode.at(["user", "id"], id_text()))
          |> result.replace_error(Nil)
          |> result.try(int.parse)
        let found =
          decode.run(payload, decode.at(["status"], decode.string))
          |> result.unwrap("")
        let scope =
          decode.run(payload, decode.at(["guild_id"], decode.string))
          |> result.unwrap("direct")
        result.map(user, fn(user) { #(user, found, scope) })
      }
      _ -> Error(Nil)
    }
  })
}

fn ready_statuses(ready: Dynamic) -> Dict(Int, String) {
  let item = {
    use user <- decode.subfield(["user", "id"], id_text())
    use found <- decode.field("status", decode.string)
    decode.success(#(user, found))
  }
  decode.run(ready, decode.at(["presences"], decode.list(item)))
  |> result.unwrap([])
  |> list.filter_map(fn(pair) {
    int.parse(pair.0) |> result.map(fn(user) { #(user, pair.1) })
  })
  |> dict.from_list
}

fn absorb(model: Model, frames: List(Frame)) -> Result(Model, String) {
  list.try_fold(updates(frames), model, fn(model, update) {
    let #(user, found, scope) = update
    let before = dict.get(model.seen, user) |> result.unwrap("offline")
    use _ <- result.try(
      gen.expect(list.contains(model.friends, user), fn() {
        "PRESENCE_UPDATE about a stranger " <> int.to_string(user)
      }),
    )
    use _ <- result.try(
      gen.expect(scope == "direct", fn() {
        "friend presence arrived with guild scope " <> scope
      }),
    )
    use _ <- result.try(
      gen.expect(found != before || found == "offline", fn() {
        "duplicate PRESENCE_UPDATE for "
        <> int.to_string(user)
        <> " repeating "
        <> found
      }),
    )
    Ok(Model(..model, seen: dict.insert(model.seen, user, found)))
  })
}

fn expected(model: Model) -> Dict(Int, String) {
  model.pool
  |> list.map(fn(friend) {
    case list.contains(model.friends, friend) {
      True -> #(friend, shown(model, friend))
      False -> #(friend, "offline")
    }
  })
  |> dict.from_list
}

fn current(model: Model) -> Dict(Int, String) {
  model.pool
  |> list.map(fn(friend) {
    #(friend, dict.get(model.seen, friend) |> result.unwrap("offline"))
  })
  |> dict.from_list
}

fn settle(socket: h.FakeSocket, model: Model) -> Result(Model, String) {
  settle_loop(socket, model, h.now_ms() + 3000)
}

fn settle_loop(
  socket: h.FakeSocket,
  model: Model,
  deadline: Int,
) -> Result(Model, String) {
  use model <- result.try(absorb(model, h.collect(socket, quiet_ms)))
  case current(model) == expected(model), h.now_ms() > deadline {
    True, _ -> Ok(model)
    False, True ->
      Error(
        "client shows "
        <> string.inspect(dict.to_list(current(model)))
        <> " want "
        <> string.inspect(dict.to_list(expected(model))),
      )
    False, False -> settle_loop(socket, model, deadline)
  }
}

fn status_choice(rng: Rng) -> #(Status, Rng) {
  gen.pick(rng, [Online, Idle, Dnd, Invisible], Online)
}

fn connect(model: Model, friend: Int, found: Status, me: Int) -> Model {
  let fake = scn.fake_session(friend, int.random(1_000_000_000))
  let link = scn.connect(fake, found, [me])
  Model(
    ..model,
    devices: dict.insert(model.devices, model.next, Device(friend, link, found)),
    next: model.next + 1,
    log: [
      "connect "
        <> int.to_string(friend)
        <> " device "
        <> int.to_string(model.next)
        <> " "
        <> presence.status_to_wire(found),
      ..model.log
    ],
  )
}

fn befriend(model: Model, friend: Int, me: Int) -> Model {
  let assert Ok(user) = id.user_id(me)
  case list.contains(model.friends, friend) {
    True -> {
      let _ =
        client.dispatch_to_user(
          user,
          "RELATIONSHIP_REMOVE",
          raw_json.from_json(
            json.object([
              #("id", json.string(int.to_string(friend))),
              #("type", json.int(1)),
            ]),
          ),
        )
      Model(
        ..model,
        friends: list.filter(model.friends, fn(f) { f != friend }),
        seen: dict.insert(model.seen, friend, "offline"),
        log: ["unfriend " <> int.to_string(friend), ..model.log],
      )
    }
    False -> {
      let _ =
        client.dispatch_to_user(
          user,
          "RELATIONSHIP_ADD",
          raw_json.from_json(friend_row(friend)),
        )
      Model(..model, friends: [friend, ..model.friends], log: [
        "befriend " <> int.to_string(friend),
        ..model.log
      ])
    }
  }
}

fn step(model: Model, rng: Rng, me: Int) -> #(Model, Rng) {
  let #(kind, rng) = gen.int(rng, 4)
  let devices = dict.keys(model.devices)
  case kind, devices {
    3, _ -> {
      let #(friend, rng) = gen.pick(rng, model.pool, 0)
      #(befriend(model, friend, me), rng)
    }
    0, _ | _, [] -> {
      let #(friend, rng) = gen.pick(rng, model.pool, 0)
      let #(found, rng) = status_choice(rng)
      case
        list.count(dict.values(model.devices), fn(d) { d.friend == friend })
        >= 3
      {
        True -> #(model, rng)
        False -> #(connect(model, friend, found, me), rng)
      }
    }
    1, _ -> {
      let #(key, rng) = gen.pick(rng, devices, 0)
      let #(found, rng) = status_choice(rng)
      let assert Ok(device) = dict.get(model.devices, key)
      client.session_update(device.link, found, False, False, None)
      #(
        Model(
          ..model,
          devices: dict.insert(
            model.devices,
            key,
            Device(..device, status: found),
          ),
          log: [
            "update device "
              <> int.to_string(key)
              <> " "
              <> presence.status_to_wire(found),
            ..model.log
          ],
        ),
        rng,
      )
    }
    _, _ -> {
      let #(key, rng) = gen.pick(rng, devices, 0)
      let assert Ok(device) = dict.get(model.devices, key)
      client.session_disconnect(device.link)
      #(
        Model(..model, devices: dict.delete(model.devices, key), log: [
          "disconnect device " <> int.to_string(key),
          ..model.log
        ]),
        rng,
      )
    }
  }
}

fn run(
  socket: h.FakeSocket,
  model: Model,
  rng: Rng,
  me: Int,
  steps: Int,
) -> Result(Model, String) {
  case steps <= 0 {
    True -> Ok(model)
    False -> {
      let #(model, rng) = step(model, rng, me)
      case settle(socket, model) {
        Ok(model) -> run(socket, model, rng, me, steps - 1)
        Error(reason) ->
          Error(
            reason
            <> "\n  after: "
            <> string.join(list.reverse(list.take(model.log, 12)), "\n    "),
          )
      }
    }
  }
}

fn friend_row(user: Int) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(user))),
    #("type", json.int(1)),
    #("user", h.user_json(user, False)),
  ])
}

fn presence_case(rng: Rng) -> Result(Nil, String) {
  let me = h.unique_user()
  let friends = [h.unique_user(), h.unique_user(), h.unique_user()]
  let model =
    Model(
      friends:,
      pool: friends,
      devices: dict.new(),
      next: 0,
      seen: dict.new(),
      log: [],
    )
  let #(count, rng) = gen.int(rng, 4)
  let #(model, rng) =
    list.fold(gen.span(1, count), #(model, rng), fn(state, _) {
      let #(model, rng) = state
      let #(friend, rng) = gen.pick(rng, friends, 0)
      let #(found, rng) = status_choice(rng)
      #(connect(model, friend, found, me), rng)
    })
  list.each(friends, scn.sync)
  let rows = list.map(friends, friend_row)
  let outcome = {
    use <- h.with_api(
      h.session_api(
        h.session_data_with(me, False, [], [
          #("relationships", json.preprocessed_array(rows)),
        ]),
      ),
    )
    let socket = h.fake_socket()
    let assert Ok(ref) =
      h.start_session(h.identify("tok-property-presence"), socket)
    let result = case h.await_event(socket, "READY", 8000) {
      Ok(Frame(_, "READY", ready)) -> {
        let seen = ready_statuses(ready)
        let model = Model(..model, seen:)
        let wanted =
          expected(model) |> dict.filter(fn(_, found) { found != "offline" })
        case seen == wanted {
          False ->
            Error(
              "READY presences "
              <> string.inspect(dict.to_list(seen))
              <> " want "
              <> string.inspect(dict.to_list(wanted)),
            )
          True ->
            settle(socket, model)
            |> result.try(fn(model) { run(socket, model, rng, me, 18) })
            |> result.replace(Nil)
        }
      }
      _ -> Error("no READY")
    }
    h.terminate(ref)
    result
  }
  outcome
}

pub fn friend_presence_reaches_the_client_once_per_change_test() {
  gen.check("presence model", 6, presence_case)
}
