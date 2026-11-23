// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key
import fluxer_gateway/core/raw_json
import fluxer_gateway/session/control
import fluxer_gateway/voice/call/handoff/adapter as call_adapter
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/client
import fluxer_gateway/voice/rpc
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import support/drain_rig
import support/voice_api as api
import support/voice_guild as g
import support/voice_session as fake

const calls = 40

const production_deadline_ms = 57_000

fn channels() -> List(Int) {
  int.range(from: calls, to: 0, with: [], run: fn(acc, n) { [5600 + n, ..acc] })
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

fn create_call(channel: Int, recipients: List(Int)) -> Nil {
  let assert Ok(_) =
    rpc.handle(
      "call.create",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("message_id", json.string(int.to_string(channel + 1000))),
          #("region", json.null()),
          #("ringing", json.array([], json.string)),
          #(
            "recipients",
            json.array(recipients, fn(user) { json.string(int.to_string(user)) }),
          ),
        ]),
      ),
    )
  Nil
}

fn join(session: fake.Fake, channel: Int) -> Nil {
  client.dm_voice_update(
    session.ref,
    g.client_update(g.join(channel)),
    calls: [],
  )
  let assert True =
    api.eventually(fn() {
      fake.dispatched(session, "VOICE_SERVER_UPDATE") != []
    })
  Nil
}

fn call_pid(channel: Int) -> Result(Pid, Nil) {
  manager.lookup(g.channel_id(channel))
  |> result.replace_error(Nil)
  |> result.try(process.subject_owner)
}

fn watches(session: fake.Fake, call: Pid) -> Bool {
  fake.controls(session)
  |> list.any(fn(found) {
    case found {
      control.CallMonitor(call: watched, ..) -> watched == call
      _ -> False
    }
  })
}

pub fn a_drain_moves_every_local_call_well_under_the_deadline_test() {
  let stub = api.start()
  let ids = channels()
  api.set(
    stub,
    "get_dm_channel",
    dm_channel(int.range(
      from: 1,
      to: 2 * calls + 1,
      with: [],
      run: list.prepend,
    )),
  )
  let members =
    list.map(ids, fn(channel) {
      let first = 2 * { channel - 5600 } - 1
      create_call(channel, [first, first + 1])
      let one = fake.start(first)
      let two = fake.start(first + 1)
      join(one, channel)
      join(two, channel)
      #(channel, [one, two])
    })
  api.settle()
  let sources = list.filter_map(ids, call_pid)
  assert list.length(sources) == calls
  let rig =
    drain_rig.start(call_adapter.adapter(), drain_rig.buckets(key.call, ids))
  let drained = drain_rig.drain(rig, production_deadline_ms)
  assert drain_rig.moved_all(drained, ids)
  assert drained.elapsed_ms * 10 < production_deadline_ms
  assert list.all(members, fn(entry) {
    let #(channel, sessions) = entry
    case call_pid(channel) {
      Ok(moved) ->
        !list.contains(sources, moved)
        && api.eventually(fn() { list.all(sessions, watches(_, moved)) })
      Error(Nil) -> False
    }
  })
  drain_rig.stop(rig)
  list.each(members, fn(entry) {
    list.each(entry.1, fn(session) {
      client.dm_voice_update(
        session.ref,
        g.client_update(g.update(None, None, [])),
        calls: [],
      )
    })
  })
  list.each(members, fn(entry) { list.each(entry.1, fake.stop) })
  api.stop(stub)
}
