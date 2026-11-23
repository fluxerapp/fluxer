// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, Dispatch}
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import property/gen.{type Rng}
import support/session_harness.{type Frame, Frame, Other} as h

type Client {
  Client(
    socket: h.FakeSocket,
    attached: Bool,
    last: Int,
    by_seq: Dict(Int, String),
    contents: List(String),
  )
}

type Model {
  Model(
    ref: SessionRef,
    token: String,
    sent: Int,
    client: Client,
    log: List(String),
  )
}

fn content(frame: Frame) -> Result(#(Int, String), Nil) {
  case frame {
    Frame(seq, "MESSAGE_CREATE", payload) ->
      decode.run(payload, decode.at(["content"], decode.string))
      |> result.map(fn(text) { #(seq, text) })
      |> result.replace_error(Nil)
    _ -> Error(Nil)
  }
}

fn absorb(client: Client, frames: List(Frame)) -> Result(Client, String) {
  list.try_fold(frames, client, fn(client, frame) {
    case frame {
      Frame(seq, event, _) -> {
        use _ <- result.try(
          gen.expect(event != "READY", fn() { "a second READY arrived" }),
        )
        case content(frame) {
          Ok(#(seq, text)) ->
            case seq <= client.last {
              True ->
                gen.expect(dict.get(client.by_seq, seq) == Ok(text), fn() {
                  "replayed seq "
                  <> int.to_string(seq)
                  <> " carries "
                  <> text
                  <> " but first carried "
                  <> string.inspect(dict.get(client.by_seq, seq))
                })
                |> result.replace(client)
              False -> {
                use _ <- result.try(
                  gen.expect(seq == client.last + 1, fn() {
                    "seq jumped from "
                    <> int.to_string(client.last)
                    <> " to "
                    <> int.to_string(seq)
                  }),
                )
                Ok(
                  Client(
                    ..client,
                    last: seq,
                    by_seq: dict.insert(client.by_seq, seq, text),
                    contents: [text, ..client.contents],
                  ),
                )
              }
            }
          Error(Nil) ->
            case event {
              "RESUMED" ->
                gen.expect(seq == client.last, fn() {
                  "RESUMED carries seq "
                  <> int.to_string(seq)
                  <> " after "
                  <> int.to_string(client.last)
                })
                |> result.replace(client)
              _ ->
                case seq > client.last {
                  True ->
                    gen.expect(seq == client.last + 1, fn() {
                      "seq jumped to " <> int.to_string(seq) <> " on " <> event
                    })
                    |> result.replace(Client(..client, last: seq))
                  False -> Ok(client)
                }
            }
        }
      }
      Other(_) | h.Closed(_) | h.Rebound(_) | h.Reattached -> Ok(client)
    }
  })
}

fn message(text: String) -> raw_json.RawJson {
  raw_json.from_json(json.object([#("content", json.string(text))]))
}

fn settle(model: Model) -> Result(Model, String) {
  case model.client.attached {
    False -> Ok(model)
    True -> {
      use client <- result.try(absorb(
        model.client,
        h.collect(model.client.socket, 40),
      ))
      Ok(Model(..model, client:))
    }
  }
}

fn expected(model: Model) -> List(String) {
  gen.span(1, model.sent) |> list.map(fn(n) { "m" <> int.to_string(n) })
}

fn check(model: Model) -> Result(Model, String) {
  case model.client.attached {
    False -> Ok(model)
    True -> check_loop(model, h.now_ms() + 2000)
  }
}

fn check_loop(model: Model, deadline: Int) -> Result(Model, String) {
  use model <- result.try(settle(model))
  let got = list.reverse(model.client.contents)
  case got == expected(model), h.now_ms() > deadline {
    True, _ -> Ok(model)
    False, True ->
      Error(
        "client stream "
        <> string.inspect(got)
        <> " want "
        <> string.inspect(expected(model)),
      )
    False, False -> check_loop(model, deadline)
  }
}

fn resume(model: Model, seq: Int, live: Bool) -> Result(Model, String) {
  let fresh = h.fake_socket()
  case h.resume(model.ref, model.token, seq, fresh) {
    Ok(_) -> {
      Ok(
        Model(
          ..model,
          client: Client(..model.client, socket: fresh, attached: True),
        ),
      )
    }
    Error(reason) ->
      Error(
        "resume at seq "
        <> int.to_string(seq)
        <> " live "
        <> string.inspect(live)
        <> " failed: "
        <> string.inspect(reason),
      )
  }
}

fn step(model: Model, rng: Rng) -> #(Result(Model, String), Rng) {
  let #(kind, rng) = gen.int(rng, 8)
  let model_log = fn(model: Model, line: String) {
    Model(..model, log: [line, ..model.log])
  }
  case kind, model.client.attached {
    0, _ | 1, _ | 2, _ -> {
      let #(count, rng) = gen.range(rng, 1, 4)
      let model =
        list.fold(gen.span(1, count), model, fn(model, _) {
          let sent = model.sent + 1
          session.send(
            model.ref,
            Dispatch("MESSAGE_CREATE", message("m" <> int.to_string(sent))),
          )
          Model(..model, sent:)
        })
      #(
        Ok(model_log(model, "dispatch up to m" <> int.to_string(model.sent))),
        rng,
      )
    }
    3, True -> {
      let outcome =
        settle(model)
        |> result.map(fn(model) {
          h.drop_socket(model.client.socket)
          let _ = h.wait_until(fn() { h.is_detached(model.ref) }, 2000)
          model_log(
            Model(..model, client: Client(..model.client, attached: False)),
            "drop socket at seq " <> int.to_string(model.client.last),
          )
        })
      #(outcome, rng)
    }
    4, _ | 5, False -> {
      let #(back, rng) = gen.pick(rng, [0, 0, 0, 1, 3], 0)
      let outcome =
        settle(model)
        |> result.try(fn(model) {
          let first =
            dict.keys(model.client.by_seq)
            |> list.fold(model.client.last, int.min)
          let seq = int.max(model.client.last - back, first)
          let live = model.client.attached
          let model =
            model_log(
              model,
              "resume at seq "
                <> int.to_string(seq)
                <> " of "
                <> int.to_string(model.client.last)
                <> " live "
                <> string.inspect(live),
            )
          resume(model, seq, live)
        })
      #(outcome, rng)
    }
    6, _ -> {
      let outcome = case
        h.resume(
          model.ref,
          "wrong-" <> model.token,
          model.client.last,
          h.fake_socket(),
        )
      {
        Ok(_) -> Error("resume with a wrong token was accepted")
        Error(_) -> Ok(model_log(model, "resume with wrong token"))
      }
      #(outcome, rng)
    }
    _, _ -> {
      let outcome = case
        h.resume(model.ref, model.token, model.sent + 1000, h.fake_socket())
      {
        Ok(_) -> Error("resume from the future was accepted")
        Error(_) -> Ok(model_log(model, "resume from a future seq"))
      }
      #(outcome, rng)
    }
  }
}

fn run(model: Model, rng: Rng, steps: Int) -> Result(Model, String) {
  case steps <= 0 {
    True -> Ok(model)
    False -> {
      let #(outcome, rng) = step(model, rng)
      case outcome |> result.try(check) {
        Ok(model) -> run(model, rng, steps - 1)
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

fn session_case(rng: Rng) -> Result(Nil, String) {
  let #(tag, rng) = gen.int(rng, 1_000_000_000)
  let token = "tok-property-session-" <> int.to_string(tag)
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, socket, ready_seq) = h.ready_session(token)
  let model =
    Model(
      ref:,
      token:,
      sent: 0,
      client: Client(
        socket:,
        attached: True,
        last: ready_seq,
        by_seq: dict.new(),
        contents: [],
      ),
      log: [],
    )
  let outcome = run(model, rng, 30)
  h.terminate(ref)
  outcome |> result.replace(Nil)
}

pub fn resume_keeps_the_dispatch_stream_whole_test() {
  gen.check("session model", 10, session_case)
}
