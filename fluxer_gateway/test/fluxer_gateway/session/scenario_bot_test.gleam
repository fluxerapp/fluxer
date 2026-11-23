// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/session/control
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/string
import gleeunit/should
import support/session_fake_guild as fake_guild
import support/session_harness.{type Frame, Frame} as h

fn guild_ids(frames: List(Frame)) -> List(String) {
  list.filter_map(frames, fn(frame) {
    case frame {
      Frame(_, "GUILD_CREATE", payload) ->
        decode.run(payload, decode.at(["id"], decode.string))
        |> result_to_nil
      _ -> Error(Nil)
    }
  })
}

fn result_to_nil(result: Result(a, b)) -> Result(a, Nil) {
  case result {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
  }
}

pub fn bot_guild_creates_replay_on_resume_test() {
  let first = h.unique_user()
  let second = h.unique_user()
  let a = fake_guild.start(first, fake_guild.Accept)
  let b = fake_guild.start(second, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), True, [first, second])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-bot"), socket)
  let frames = h.collect(socket, 400)
  let assert [Frame(ready_seq, "READY", ready), ..creates] = frames
  decode.run(
    ready,
    decode.at(["guilds"], decode.list(decode.at(["unavailable"], decode.bool))),
  )
  |> should.equal(Ok([True, True]))
  list.sort(guild_ids(creates), string.compare)
  |> should.equal(list.sort(
    [int.to_string(first), int.to_string(second)],
    string.compare,
  ))

  h.drop_socket(socket)
  h.wait_until(fn() { h.is_detached(ref) }, 2000) |> should.be_true
  let fresh = h.fake_socket()
  h.resume(ref, "tok-bot", ready_seq, fresh) |> should.equal(Ok(ref))
  let replayed = h.collect(fresh, 300)
  h.events(replayed)
  |> should.equal(["GUILD_CREATE", "GUILD_CREATE", "RESUMED"])
  guild_ids(replayed) |> should.equal(guild_ids(creates))

  let third = h.unique_user()
  let c = fake_guild.start(third, fake_guild.Accept)
  control.send(ref, control.GuildJoin(h.guild_id(third)))
  let assert Ok(Frame(_, "GUILD_CREATE", joined)) =
    h.await_event(fresh, "GUILD_CREATE", 2000)
  decode.run(joined, decode.at(["id"], decode.string))
  |> should.equal(Ok(int.to_string(third)))
  h.terminate(ref)
  fake_guild.crash(a)
  fake_guild.crash(b)
  fake_guild.crash(c)
}
