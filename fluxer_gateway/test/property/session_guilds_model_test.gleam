// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/control
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import property/gen.{type Rng}
import support/session_fake_guild as fake_guild
import support/session_harness.{type Frame, Frame} as h

type Model {
  Model(
    ref: SessionRef,
    token: String,
    socket: h.FakeSocket,
    attached: Bool,
    last: Int,
    guilds: List(Int),
    joined: Set(Int),
    shown: Set(Int),
    log: List(String),
  )
}

fn guild_of(frame: Frame) -> Result(Int, Nil) {
  case frame {
    Frame(_, _, payload) ->
      decode.run(payload, decode.at(["id"], decode.string))
      |> result.replace_error(Nil)
      |> result.try(int.parse)
    _ -> Error(Nil)
  }
}

fn absorb(model: Model, frames: List(Frame)) -> Result(Model, String) {
  list.try_fold(frames, model, fn(model, frame) {
    let model = case frame {
      Frame(seq, _, _) if seq > model.last -> Model(..model, last: seq)
      _ -> model
    }
    case frame, guild_of(frame) {
      Frame(_, "GUILD_CREATE", _), Ok(guild) ->
        case set.contains(model.shown, guild) {
          True -> Error("duplicate GUILD_CREATE for " <> int.to_string(guild))
          False -> Ok(Model(..model, shown: set.insert(model.shown, guild)))
        }
      Frame(_, "GUILD_DELETE", _), Ok(guild) ->
        case set.contains(model.shown, guild) {
          False ->
            Error(
              "GUILD_DELETE for a guild the client does not hold "
              <> int.to_string(guild),
            )
          True -> Ok(Model(..model, shown: set.delete(model.shown, guild)))
        }
      _, _ -> Ok(model)
    }
  })
}

fn check(model: Model) -> Result(Model, String) {
  case model.attached {
    False -> Ok(model)
    True -> check_loop(model, h.now_ms() + 2000)
  }
}

fn check_loop(model: Model, deadline: Int) -> Result(Model, String) {
  use model <- result.try(absorb(model, h.collect(model.socket, 40)))
  case model.shown == model.joined, h.now_ms() > deadline {
    True, _ -> Ok(model)
    False, True ->
      Error(
        "client holds guilds "
        <> string.inspect(set.to_list(model.shown))
        <> " want "
        <> string.inspect(set.to_list(model.joined)),
      )
    False, False -> check_loop(model, deadline)
  }
}

fn step(model: Model, rng: Rng) -> Result(#(Model, Rng), String) {
  let #(kind, rng) = gen.int(rng, 6)
  let note = fn(model: Model, line: String) {
    Model(..model, log: [line, ..model.log])
  }
  case kind, model.attached {
    0, _ | 1, _ -> {
      let #(guild, rng) = gen.pick(rng, model.guilds, 0)
      h.control(model.ref, control.GuildJoin(h.guild_id(guild)))
      Ok(#(
        note(
          Model(..model, joined: set.insert(model.joined, guild)),
          "join " <> int.to_string(guild),
        ),
        rng,
      ))
    }
    2, _ | 3, _ -> {
      let #(guild, rng) = gen.pick(rng, model.guilds, 0)
      h.control(model.ref, h.leave(guild))
      Ok(#(
        note(
          Model(..model, joined: set.delete(model.joined, guild)),
          "leave " <> int.to_string(guild),
        ),
        rng,
      ))
    }
    4, True -> {
      use model <- result.try(absorb(model, h.collect(model.socket, 40)))
      h.drop_socket(model.socket)
      let _ = h.wait_until(fn() { h.is_detached(model.ref) }, 2000)
      Ok(#(note(Model(..model, attached: False), "drop socket"), rng))
    }
    _, _ -> {
      use model <- result.try(case model.attached {
        True -> absorb(model, h.collect(model.socket, 40))
        False -> Ok(model)
      })
      let fresh = h.fake_socket()
      case h.resume(model.ref, model.token, model.last, fresh) {
        Ok(_) ->
          Ok(#(
            note(
              Model(..model, socket: fresh, attached: True),
              "resume at " <> int.to_string(model.last),
            ),
            rng,
          ))
        Error(reason) -> Error("resume failed: " <> string.inspect(reason))
      }
    }
  }
}

fn run(model: Model, rng: Rng, steps: Int) -> Result(Model, String) {
  case steps <= 0 {
    True -> Ok(model)
    False ->
      case
        step(model, rng)
        |> result.try(fn(pair) {
          check(pair.0) |> result.map(fn(model) { #(model, pair.1) })
        })
      {
        Ok(#(model, rng)) -> run(model, rng, steps - 1)
        Error(reason) ->
          Error(
            reason
            <> "\n  after: "
            <> string.join(list.reverse(list.take(model.log, 12)), "\n    "),
          )
      }
  }
}

fn guilds_case(rng: Rng) -> Result(Nil, String) {
  let guilds = [h.unique_user(), h.unique_user(), h.unique_user()]
  let fakes = list.map(guilds, fake_guild.start(_, fake_guild.Accept))
  let #(count, rng) = gen.int(rng, 3)
  let initial = list.take(guilds, count)
  let #(tag, rng) = gen.int(rng, 1_000_000_000)
  let token = "tok-property-guilds-" <> int.to_string(tag)
  let outcome = {
    use <- h.with_api(
      h.session_api(h.session_data(h.unique_user(), False, initial)),
    )
    let socket = h.fake_socket()
    let assert Ok(ref) = h.start_session(h.identify(token), socket)
    let result = case h.await_event(socket, "READY", 8000) {
      Ok(Frame(seq, "READY", ready)) -> {
        let listed =
          decode.run(
            ready,
            decode.at(
              ["guilds"],
              decode.list({
                use id <- decode.field("id", decode.string)
                use unavailable <- decode.optional_field(
                  "unavailable",
                  False,
                  decode.bool,
                )
                decode.success(#(id, unavailable))
              }),
            ),
          )
          |> result.unwrap([])
          |> list.filter_map(fn(pair) {
            case pair.1 {
              True -> Error(Nil)
              False -> int.parse(pair.0)
            }
          })
        let model =
          Model(
            ref:,
            token:,
            socket:,
            attached: True,
            last: seq,
            guilds:,
            joined: set.from_list(initial),
            shown: set.from_list(listed),
            log: [],
          )
        check(model) |> result.try(run(_, rng, 25))
      }
      _ -> Error("no READY")
    }
    h.terminate(ref)
    result
  }
  list.each(fakes, fake_guild.crash)
  outcome |> result.replace(Nil)
}

pub fn guild_join_and_leave_reach_the_client_once_test() {
  gen.check("session guilds model", 8, guilds_case)
}

pub fn leaving_a_guild_still_joining_sends_no_guild_delete_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, socket, _) = h.ready_session("tok-property-unannounced")
  h.control(ref, control.GuildJoin(h.guild_id(guild)))
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  h.control(ref, h.leave(guild))
  let frames = h.collect(socket, 300)
  assert !list.contains(h.events(frames), "GUILD_DELETE")
  assert !list.contains(h.events(frames), "GUILD_CREATE")
  h.terminate(ref)
  fake_guild.crash(fake)
}
