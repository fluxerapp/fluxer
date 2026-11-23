// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import property/gen.{type Rng}
import support/geo_probe as probe
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids

const voice_channels = [10, 20, 30]

const users = [1, 2, 3]

type Conn {
  Conn(user: Int, device: Int, channel: Int, mute: Bool, deaf: Bool)
}

type Model {
  Model(
    devices: Dict(Int, #(Int, SessionRef)),
    conns: Dict(String, Conn),
    denied: List(#(Int, Int)),
    overrides: Dict(Int, #(Bool, Bool)),
    next_device: Int,
    log: List(String),
  )
}

type Seen {
  Seen(
    conn: String,
    user: String,
    channel: Option(String),
    mute: Bool,
    deaf: Bool,
  )
}

fn seen_decoder() -> decode.Decoder(Seen) {
  use conn <- decode.field("connection_id", decode.string)
  use user <- decode.field("user_id", decode.string)
  use channel <- decode.field("channel_id", decode.optional(decode.string))
  use mute <- decode.optional_field("mute", False, decode.bool)
  use deaf <- decode.optional_field("deaf", False, decode.bool)
  decode.success(Seen(conn:, user:, channel:, mute:, deaf:))
}

fn live(host: g.Host) -> List(Seen) {
  let voice = g.inspect(host).0
  guild_handler.voice_states(voice)
  |> list.filter_map(fn(state) {
    json.to_string(guild_handler.voice_state_json(voice, state))
    |> json.parse(seen_decoder())
    |> result.replace_error(Nil)
  })
  |> list.sort(fn(a, b) { string.compare(a.conn, b.conn) })
}

fn expected(model: Model) -> List(Seen) {
  dict.to_list(model.conns)
  |> list.map(fn(pair) {
    let #(conn, found) = pair
    Seen(
      conn:,
      user: int.to_string(found.user),
      channel: Some(int.to_string(found.channel)),
      mute: found.mute,
      deaf: found.deaf,
    )
  })
  |> list.sort(fn(a, b) { string.compare(a.conn, b.conn) })
}

fn server_updates(host: g.Host, ref: SessionRef) -> Int {
  list.length(g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE"))
}

fn last_server_update(host: g.Host, ref: SessionRef) -> Option(String) {
  g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE")
  |> list.last
  |> result.map(fn(payload) { g.text_of(payload, "connection_id") })
  |> result.unwrap(None)
}

fn nonce_for(stub: api.Api, conn: String) -> Option(String) {
  let tokens = api.calls_of(stub, "voice_get_token") |> list.reverse
  let exact =
    list.find(tokens, fn(call) {
      api.field(call, "connection_id") == Some(conn)
    })
  let fresh =
    list.find(tokens, fn(call) { api.field(call, "connection_id") == None })
  case exact, fresh {
    Ok(call), _ -> api.field(call, "token_nonce")
    Error(Nil), Ok(call) -> api.field(call, "token_nonce")
    Error(Nil), Error(Nil) -> None
  }
}

fn confirm(stub: api.Api, host: g.Host, conn: String) -> Result(Nil, String) {
  let reply = g.confirm(host, conn, nonce_for(stub, conn))
  gen.expect(reply == "{\"success\":true}", fn() {
    "confirm of " <> conn <> " answered " <> reply
  })
}

fn await_update(
  host: g.Host,
  ref: SessionRef,
  before: Int,
) -> Result(Option(String), String) {
  case api.eventually(fn() { server_updates(host, ref) > before }) {
    True -> Ok(last_server_update(host, ref))
    False -> Error("no VOICE_SERVER_UPDATE arrived")
  }
}

fn allowed(model: Model, user: Int, channel: Int) -> Bool {
  !list.contains(model.denied, #(channel, user))
}

fn world(model: Model) -> g.World {
  let base = g.world()
  g.World(
    ..base,
    members: users,
    channels: list.map(base.channels, fn(found) {
      g.VoiceChannel(
        ..found,
        denied: list.filter_map(model.denied, fn(pair) {
          case pair.0 == found.id {
            True -> Ok(pair.1)
            False -> Error(Nil)
          }
        }),
      )
    }),
  )
}

fn user_conns(model: Model, user: Int) -> List(#(String, Conn)) {
  dict.to_list(model.conns)
  |> list.filter(fn(pair) { { pair.1 }.user == user })
}

fn device_conns(model: Model, device: Int) -> List(#(String, Conn)) {
  dict.to_list(model.conns)
  |> list.filter(fn(pair) { { pair.1 }.device == device })
}

fn log(model: Model, line: String) -> Model {
  Model(..model, log: [line, ..model.log])
}

fn override_of(model: Model, user: Int) -> #(Bool, Bool) {
  dict.get(model.overrides, user) |> result.unwrap(#(False, False))
}

type Outcome =
  Result(#(Model, Rng), String)

fn join(stub: api.Api, host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(fresh, rng) = gen.chance(rng, 30)
  let #(user, rng) = gen.pick(rng, users, 1)
  let #(model, device) = case fresh || dict.is_empty(model.devices) {
    True -> {
      let ref = g.session(user)
      let device = model.next_device
      #(
        Model(
          ..model,
          devices: dict.insert(model.devices, device, #(user, ref)),
          next_device: device + 1,
        ),
        device,
      )
    }
    False -> {
      let #(device, _) = gen.pick(rng, dict.keys(model.devices), 0)
      #(model, device)
    }
  }
  let assert Ok(#(user, ref)) = dict.get(model.devices, device)
  let channels = list.filter(voice_channels, allowed(model, user, _))
  let #(channel, rng) = gen.pick(rng, channels, 0)
  case
    channel == 0
    || list.length(device_conns(model, device)) >= 2
    || list.length(user_conns(model, user)) >= 4
  {
    True -> Ok(#(model, rng))
    False -> {
      let before = server_updates(host, ref)
      g.client(host, ref, g.join_with(channel, probe.fields()))
      use found <- result.try(await_update(host, ref, before))
      use conn <- result.try(option.to_result(found, "join gave no connection"))
      use _ <- result.try(
        gen.expect(!dict.has_key(model.conns, conn), fn() {
          "join reused live connection " <> conn
        }),
      )
      use _ <- result.try(confirm(stub, host, conn))
      let #(mute, deaf) = override_of(model, user)
      let model =
        Model(
          ..model,
          conns: dict.insert(
            model.conns,
            conn,
            Conn(user:, device:, channel:, mute:, deaf:),
          ),
        )
      Ok(#(
        log(
          model,
          "join user "
            <> int.to_string(user)
            <> " device "
            <> int.to_string(device)
            <> " channel "
            <> int.to_string(channel)
            <> " as "
            <> conn,
        ),
        rng,
      ))
    }
  }
}

fn pick_conn(model: Model, rng: Rng) -> #(Option(#(String, Conn)), Rng) {
  case dict.to_list(model.conns) {
    [] -> #(None, rng)
    all -> {
      let #(found, rng) = gen.pick(rng, all, #("", Conn(0, 0, 0, False, False)))
      #(Some(found), rng)
    }
  }
}

fn client_move(stub: api.Api, host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(picked, rng) = pick_conn(model, rng)
  case picked {
    None -> Ok(#(model, rng))
    Some(#(conn, found)) -> {
      let channels =
        list.filter(voice_channels, fn(channel) {
          channel != found.channel && allowed(model, found.user, channel)
        })
      let #(channel, rng) = gen.pick(rng, channels, 0)
      case channel {
        0 -> Ok(#(model, rng))
        _ -> {
          let assert Ok(#(_, ref)) = dict.get(model.devices, found.device)
          let before = server_updates(host, ref)
          g.client(host, ref, g.update(Some(channel), Some(conn), []))
          use update <- result.try(await_update(host, ref, before))
          use _ <- result.try(
            gen.expect(update == Some(conn), fn() {
              "client move of " <> conn <> " issued " <> string.inspect(update)
            }),
          )
          use _ <- result.try(confirm(stub, host, conn))
          Ok(#(
            log(
              Model(
                ..model,
                conns: dict.insert(model.conns, conn, Conn(..found, channel:)),
              ),
              "client move " <> conn <> " to " <> int.to_string(channel),
            ),
            rng,
          ))
        }
      }
    }
  }
}

fn leave(stub: api.Api, host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(picked, rng) = pick_conn(model, rng)
  case picked {
    None -> Ok(#(model, rng))
    Some(#(conn, found)) -> {
      let assert Ok(#(_, ref)) = dict.get(model.devices, found.device)
      let before = list.length(forced(stub))
      g.client(host, ref, g.update(None, Some(conn), []))
      use _ <- result.try(await_forced(stub, before, [conn], "leave"))
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

fn leave_device(
  stub: api.Api,
  host: g.Host,
  model: Model,
  rng: Rng,
) -> Outcome {
  case dict.keys(model.devices) {
    [] -> Ok(#(model, rng))
    devices -> {
      let #(device, rng) = gen.pick(rng, devices, 0)
      let assert Ok(#(_, ref)) = dict.get(model.devices, device)
      let before = list.length(forced(stub))
      g.client(host, ref, g.update(None, None, []))
      let gone = device_conns(model, device) |> list.map(fn(pair) { pair.0 })
      use _ <- result.try(await_forced(stub, before, gone, "leave device"))
      Ok(#(
        log(
          Model(..model, conns: dict.drop(model.conns, gone)),
          "leave all of device " <> int.to_string(device),
        ),
        rng,
      ))
    }
  }
}

fn rpc_disconnect(host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(user, rng) = gen.pick(rng, users, 1)
  let #(channel, rng) = gen.pick(rng, voice_channels, 10)
  let #(targeted, rng) = gen.chance(rng, 40)
  let mine =
    user_conns(model, user)
    |> list.filter(fn(pair) { { pair.1 }.channel == channel })
  let #(conn, rng) = case targeted, mine {
    True, [_, ..] -> {
      let #(pair, rng) = gen.pick(rng, mine, #("", Conn(0, 0, 0, False, False)))
      #(Some(pair.0), rng)
    }
    _, _ -> #(None, rng)
  }
  let _ =
    g.rpc(
      host,
      guild_state.DisconnectIfInChannel(
        g.user_id(user),
        g.channel_id(channel),
        option.map(conn, voice_ids.conn),
      ),
    )
  let gone = case conn {
    Some(conn) -> [conn]
    None -> list.map(mine, fn(pair) { pair.0 })
  }
  Ok(#(
    log(
      Model(..model, conns: dict.drop(model.conns, gone)),
      "rpc disconnect user "
        <> int.to_string(user)
        <> " channel "
        <> int.to_string(channel)
        <> " conn "
        <> string.inspect(conn),
    ),
    rng,
  ))
}

fn rpc_move(stub: api.Api, host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(user, rng) = gen.pick(rng, users, 1)
  let mine = user_conns(model, user)
  let #(targeted, rng) = gen.chance(rng, 40)
  let #(selected, rng) = case targeted, mine {
    True, [_, ..] -> {
      let #(pair, rng) = gen.pick(rng, mine, #("", Conn(0, 0, 0, False, False)))
      #([pair], rng)
    }
    _, _ -> #(mine, rng)
  }
  let conn = case targeted, selected {
    True, [pair] -> Some(pair.0)
    _, _ -> None
  }
  let #(kick, rng) = gen.chance(rng, 25)
  let channels = list.filter(voice_channels, allowed(model, user, _))
  let #(channel, rng) = gen.pick(rng, channels, 0)
  case selected, kick, channel {
    [], _, _ -> Ok(#(model, rng))
    _, True, _ -> {
      let reply =
        g.rpc_text(
          host,
          guild_state.MoveMember(
            g.user_id(user),
            g.user_id(1),
            None,
            option.map(conn, voice_ids.conn),
          ),
        )
      use _ <- result.try(
        gen.expect(reply == "{\"success\":true}", fn() {
          "moderator disconnect answered " <> reply
        }),
      )
      Ok(#(
        log(
          Model(
            ..model,
            conns: dict.drop(model.conns, list.map(selected, fn(p) { p.0 })),
          ),
          "rpc move user "
            <> int.to_string(user)
            <> " to nowhere conn "
            <> string.inspect(conn),
        ),
        rng,
      ))
    }
    _, False, 0 -> Ok(#(model, rng))
    _, False, _ -> {
      let moving = selected
      let before =
        list.map(moving, fn(pair) {
          let assert Ok(#(_, ref)) = dict.get(model.devices, { pair.1 }.device)
          #(pair, ref, server_updates(host, ref))
        })
      let reply =
        g.rpc_text(
          host,
          guild_state.MoveMember(
            g.user_id(user),
            g.user_id(1),
            Some(g.channel_id(channel)),
            option.map(conn, voice_ids.conn),
          ),
        )
      use _ <- result.try(
        gen.expect(reply == "{\"success\":true}", fn() {
          "moderator move answered " <> reply
        }),
      )
      use _ <- result.try(
        list.try_each(before, fn(item) {
          let #(pair, ref, count) = item
          let device_moves =
            list.count(before, fn(other) {
              { other.0.1 }.device == { pair.1 }.device
            })
          use _ <- result.try(
            case
              api.eventually(fn() {
                server_updates(host, ref) >= count + device_moves
              })
            {
              True -> Ok(Nil)
              False ->
                Error(
                  "moderator move sent no VOICE_SERVER_UPDATE for " <> pair.0,
                )
            },
          )
          confirm(stub, host, pair.0)
        }),
      )
      let conns =
        list.fold(moving, model.conns, fn(acc, pair) {
          dict.insert(acc, pair.0, Conn(..pair.1, channel:))
        })
      Ok(#(
        log(
          Model(..model, conns:),
          "rpc move user "
            <> int.to_string(user)
            <> " to "
            <> int.to_string(channel)
            <> " conn "
            <> string.inspect(conn),
        ),
        rng,
      ))
    }
  }
}

fn rpc_mute(host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(user, rng) = gen.pick(rng, users, 1)
  let #(mute, rng) = gen.bool(rng)
  let #(deaf, rng) = gen.bool(rng)
  let _ = g.rpc(host, guild_state.MemberVoice(g.user_id(user), mute, deaf))
  let conns =
    dict.map_values(model.conns, fn(_, found) {
      case found.user == user {
        True -> Conn(..found, mute:, deaf:)
        False -> found
      }
    })
  Ok(#(
    log(
      Model(
        ..model,
        conns:,
        overrides: dict.insert(model.overrides, user, #(mute, deaf)),
      ),
      "rpc member voice user "
        <> int.to_string(user)
        <> " mute "
        <> string.inspect(mute)
        <> " deaf "
        <> string.inspect(deaf),
    ),
    rng,
  ))
}

fn forced(stub: api.Api) -> List(String) {
  api.calls_of(stub, "voice_force_disconnect_participant")
  |> list.filter_map(fn(call) {
    api.field(call, "connection_id") |> option.to_result(Nil)
  })
}

fn await_forced(
  stub: api.Api,
  before: Int,
  gone: List(String),
  what: String,
) -> Result(Nil, String) {
  let hit = fn() { list.drop(forced(stub), before) }
  gen.expect(
    api.eventually(fn() { list.all(gone, list.contains(hit(), _)) }),
    fn() {
      what
      <> " force disconnected "
      <> string.inspect(hit())
      <> " want "
      <> string.inspect(gone)
    },
  )
}

fn permissions(stub: api.Api, host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(user, rng) = gen.pick(rng, [2, 3], 2)
  let #(channel, rng) = gen.pick(rng, voice_channels, 10)
  let key = #(channel, user)
  case list.contains(model.denied, key) {
    True -> {
      let model =
        Model(..model, denied: list.filter(model.denied, fn(k) { k != key }))
      g.update_world(host, world(model))
      g.run(host, guild_handler.on_permissions_change)
      Ok(#(
        log(
          model,
          "grant user "
            <> int.to_string(user)
            <> " channel "
            <> int.to_string(channel),
        ),
        rng,
      ))
    }
    False -> {
      let gone =
        user_conns(model, user)
        |> list.filter(fn(pair) { { pair.1 }.channel == channel })
        |> list.map(fn(pair) { pair.0 })
      let model =
        Model(
          ..model,
          denied: [key, ..model.denied],
          conns: dict.drop(model.conns, gone),
        )
      let before = list.length(forced(stub))
      g.update_world(host, world(model))
      g.run(host, guild_handler.on_permissions_change)
      let wanted = list.sort(gone, string.compare)
      let hit = fn() {
        list.drop(forced(stub), before) |> list.sort(string.compare)
      }
      use _ <- result.try(
        gen.expect(api.eventually(fn() { hit() == wanted }), fn() {
          "permission loss force disconnected "
          <> string.inspect(hit())
          <> " want "
          <> string.inspect(wanted)
        }),
      )
      list.each(gone, fn(conn) {
        g.rpc(
          host,
          guild_state.DisconnectIfInChannel(
            g.user_id(user),
            g.channel_id(channel),
            Some(voice_ids.conn(conn)),
          ),
        )
      })
      Ok(#(
        log(
          model,
          "deny user "
            <> int.to_string(user)
            <> " channel "
            <> int.to_string(channel),
        ),
        rng,
      ))
    }
  }
}

fn step(stub: api.Api, host: g.Host, model: Model, rng: Rng) -> Outcome {
  let #(kind, rng) = gen.int(rng, 9)
  case kind {
    0 | 1 -> join(stub, host, model, rng)
    2 -> client_move(stub, host, model, rng)
    3 -> leave(stub, host, model, rng)
    4 -> leave_device(stub, host, model, rng)
    5 -> rpc_disconnect(host, model, rng)
    6 -> rpc_move(stub, host, model, rng)
    7 -> rpc_mute(host, model, rng)
    _ -> permissions(stub, host, model, rng)
  }
}

fn frames(host: g.Host) -> List(String) {
  g.effects(host)
  |> list.flat_map(fn(effect) {
    case effect {
      guild_handler.Broadcast(_, payload, _) -> [payload]
      guild_handler.BroadcastMove(_, payload, left, _, _) -> [payload, left]
      guild_handler.SendToSession(_, _, payload) -> [payload]
      _ -> []
    }
  })
  |> list.map(fn(payload) {
    raw_json.to_bits(payload) |> bit_array.to_string |> result.unwrap("")
  })
}

fn per_connection(host: g.Host) -> Result(Nil, String) {
  let found = live(host) |> list.map(fn(seen) { seen.conn })
  gen.expect(list.length(list.unique(found)) == list.length(found), fn() {
    "a connection id holds two voice states: " <> string.inspect(found)
  })
}

fn check(host: g.Host, model: Model) -> Result(Nil, String) {
  let want = expected(model)
  let settled = api.eventually(fn() { live(host) == want })
  use _ <- result.try(
    gen.expect(settled, fn() {
      "voice states\n  got  "
      <> string.inspect(live(host))
      <> "\n  want "
      <> string.inspect(want)
    }),
  )
  use _ <- result.try(per_connection(host))
  use _ <- result.try(
    list.try_each(voice_channels, fn(channel) {
      let reply =
        g.rpc_text(host, guild_state.ChannelStates(g.channel_id(channel)))
      let listed =
        json.parse(
          reply,
          decode.at(
            ["voice_states"],
            decode.list(decode.at(["connection_id"], decode.string)),
          ),
        )
        |> result.unwrap([])
        |> list.sort(string.compare)
      let wanted =
        dict.to_list(model.conns)
        |> list.filter(fn(pair) { { pair.1 }.channel == channel })
        |> list.map(fn(pair) { pair.0 })
        |> list.sort(string.compare)
      gen.expect(listed == wanted, fn() {
        "ChannelStates "
        <> int.to_string(channel)
        <> " gave "
        <> string.inspect(listed)
        <> " want "
        <> string.inspect(wanted)
      })
    }),
  )
  use _ <- result.try(
    list.try_each(users, fn(user) {
      let reply = g.rpc_text(host, guild_state.GetVoiceState(g.user_id(user)))
      let found =
        json.parse(
          reply,
          decode.at(
            ["voice_state"],
            decode.optional(decode.at(["connection_id"], decode.string)),
          ),
        )
        |> result.unwrap(Some("unreadable"))
      let mine = user_conns(model, user) |> list.map(fn(pair) { pair.0 })
      gen.expect(
        case found {
          None -> mine == []
          Some(conn) -> list.contains(mine, conn)
        },
        fn() {
          "GetVoiceState "
          <> int.to_string(user)
          <> " gave "
          <> string.inspect(found)
          <> " but the user holds "
          <> string.inspect(mine)
        },
      )
    }),
  )
  case probe.leaking(frames(host)) {
    [] -> Ok(Nil)
    leaked -> Error("coordinates reached a frame: " <> string.inspect(leaked))
  }
}

fn run(
  stub: api.Api,
  host: g.Host,
  model: Model,
  rng: Rng,
  steps: Int,
) -> Result(Nil, String) {
  case steps <= 0 {
    True -> Ok(Nil)
    False -> {
      case step(stub, host, model, rng) {
        Error(reason) -> Error(reason <> trail(model))
        Ok(#(model, rng)) ->
          case check(host, model) {
            Ok(Nil) -> run(stub, host, model, rng, steps - 1)
            Error(reason) -> Error(reason <> trail(model))
          }
      }
    }
  }
}

fn trail(model: Model) -> String {
  "\n  after: " <> string.join(list.reverse(list.take(model.log, 14)), "\n    ")
}

fn voice_case(rng: Rng) -> Result(Nil, String) {
  let model =
    Model(
      devices: dict.new(),
      conns: dict.new(),
      denied: [],
      overrides: dict.new(),
      next_device: 0,
      log: [],
    )
  let stub = api.start()
  let host = g.start(world(model))
  let outcome = run(stub, host, model, rng, 25)
  g.stop(host)
  api.stop(stub)
  outcome
}

pub fn voice_states_follow_the_connection_model_test() {
  gen.check("voice model", 12, voice_case)
}
