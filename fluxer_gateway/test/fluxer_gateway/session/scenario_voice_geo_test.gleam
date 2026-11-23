// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/state as call_state
import fluxer_gateway/voice/rpc
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import support/geo_probe as probe
import support/session_harness.{Closed, Frame, Other} as h

type Recorded {
  Recorded(kind: String, body: Dynamic)
}

fn id_text(value: Int) -> Json {
  json.string(int.to_string(value))
}

fn api(
  user: Int,
  partner: Int,
  channel: Int,
  log: Subject(Recorded),
) -> h.Responder {
  let private_channel =
    json.object([
      #("id", id_text(channel)),
      #("type", json.int(1)),
      #("recipients", json.preprocessed_array([h.user_json(partner, False)])),
    ])
  fn(kind, body) {
    process.send(log, Recorded(kind, body))
    case kind {
      "session" ->
        h.ok_data(
          h.session_data_with(user, False, [], [
            #("private_channels", json.preprocessed_array([private_channel])),
          ]),
        )
      "get_dm_channel" ->
        h.ok_data(
          json.object([
            #(
              "channel",
              json.object([
                #("type", json.int(1)),
                #(
                  "recipients",
                  json.preprocessed_array([
                    json.object([#("id", id_text(user))]),
                    json.object([#("id", id_text(partner))]),
                  ]),
                ),
              ]),
            ),
          ]),
        )
      "voice_get_token" -> {
        let conn =
          decode.run(body, decode.at(["connection_id"], decode.string))
          |> result.unwrap("dm-geo")
        h.ok_data(
          json.object([
            #("token", json.string("token-" <> conn)),
            #("endpoint", json.string("wss://sfu.test")),
            #("connectionId", json.string(conn)),
            #("regionId", json.string("eu")),
            #("serverId", json.string("sfu-1")),
          ]),
        )
      }
      _ -> h.ok_data(json.object([#("success", json.bool(True))]))
    }
  }
}

fn drain(log: Subject(Recorded), acc: List(Recorded)) -> List(Recorded) {
  case process.receive(log, 0) {
    Ok(found) -> drain(log, [found, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

fn start_call(channel: Int, user: Int) -> process.Pid {
  let assert Ok(channel_id) = id.channel_id(channel)
  let assert Ok(message_id) = id.message_id(channel + 1)
  let assert Ok(user_id) = id.user_id(user)
  let assert Ok(call) =
    manager.create(
      channel_id,
      call_state.Init(
        channel: channel_id,
        message: message_id,
        region: None,
        ringing: [],
        recipients: [user_id],
        caller: None,
      ),
    )
  let assert Ok(pid) = process.subject_owner(call)
  pid
}

fn frame_texts(frames: List(h.Frame)) -> List(String) {
  list.map(frames, fn(frame) {
    case frame {
      Frame(_, event, payload) -> event <> " " <> string.inspect(payload)
      Other(session.GatewayErrorFrame(_, payload)) ->
        raw_json.to_bits(payload) |> bit_array.to_string |> result.unwrap("")
      Other(other) -> string.inspect(other)
      Closed(code) -> code
      h.Rebound(seq) -> "rebound " <> string.inspect(seq)
      h.Reattached -> "reattached"
    }
  })
}

fn voice_update(data: Json) -> session.SessionInbox {
  session.SocketOp(session.VoiceStateUpdate(
    json.to_string(data)
    |> json.parse(decode.dynamic)
    |> result.unwrap(dynamic.nil()),
  ))
}

fn rpc_text(method: String, params: Json) -> String {
  rpc.handle(method, raw_json.from_json(params))
  |> result.map(json.to_string)
  |> result.unwrap("rpc_error")
}

fn carries(found: Recorded) -> Bool {
  decode.run(found.body, decode.at(["latitude"], decode.string))
  == Ok(probe.latitude)
  && decode.run(found.body, decode.at(["longitude"], decode.string))
  == Ok(probe.longitude)
}

pub fn dm_call_with_resume_never_shows_coordinates_test() {
  let user = h.unique_user()
  let partner = h.unique_user()
  let channel = h.unique_user()
  let log = process.new_subject()
  let call = start_call(channel, user)
  use <- h.with_api(api(user, partner, channel, log))
  let logs = probe.capture_logs()
  let socket = h.fake_socket()
  let identify =
    session.Identify(
      ..h.identify("tok-geo"),
      latitude: Some(probe.latitude),
      longitude: Some(probe.longitude),
    )
  let assert Ok(ref) = h.start_session(identify, socket)
  let assert Ok(Frame(ready_seq, "READY", ready)) =
    h.await_event(socket, "READY", 8000)
  session.send(
    ref,
    voice_update(
      json.object([#("channel_id", id_text(channel)), ..probe.fields()]),
    ),
  )
  let assert Ok(Frame(_, "VOICE_SERVER_UPDATE", update)) =
    h.await_event(socket, "VOICE_SERVER_UPDATE", 5000)
  let assert Ok(conn) =
    decode.run(update, decode.at(["connection_id"], decode.string))
  let target =
    json.object([
      #("channel_id", id_text(channel)),
      #("connection_id", json.string(conn)),
    ])
  let replies = [
    rpc_text("call.confirm_connection", target),
    rpc_text("call.get", json.object([#("channel_id", id_text(channel))])),
  ]
  let live = h.collect(socket, 400)
  h.drop_socket(socket)
  let assert True = h.wait_until(fn() { h.is_detached(ref) }, 2000)
  let fresh = h.fake_socket()
  let assert Ok(_) = h.resume(ref, "tok-geo", ready_seq, fresh)
  let replayed = h.collect(fresh, 400)
  assert list.contains(h.events(replayed), "RESUMED")
  session.send(ref, voice_update(json.object([#("channel_id", json.null())])))
  let left = h.collect(fresh, 400)
  h.terminate(ref)
  process.kill(call)
  let frames =
    list.flatten([
      [string.inspect(ready)],
      frame_texts(live),
      frame_texts(replayed),
      frame_texts(left),
    ])
  assert list.any(frames, string.starts_with(_, "CALL_"))
  assert list.any(frames, string.contains(_, conn))
  assert probe.leaking(frames) == []
  assert probe.leaking(replies) == []
  assert list.any(replies, string.contains(_, conn))
  let recorded = drain(log, [])
  let tokens =
    list.filter(recorded, fn(found) { found.kind == "voice_get_token" })
  assert tokens != []
  assert list.all(tokens, carries)
  let others =
    recorded
    |> list.filter(fn(found) {
      found.kind != "voice_get_token" && found.kind != "session"
    })
    |> list.map(fn(found) { found.kind <> " " <> string.inspect(found.body) })
  assert others != []
  assert probe.leaking(others) == []
  assert probe.leaking(probe.finish(logs)) == []
}
