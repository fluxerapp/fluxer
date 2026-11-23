// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/status
import fluxer_gateway/core/session.{
  type SocketMsg, DispatchFrame, Send, SocketRef,
}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/result
import support/session_peer.{type Sock, Sock}

type Input {
  Outbound(SocketMsg)
  Query(Subject(List(String)))
}

pub fn silent_socket() -> Sock {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outbox = process.new_subject()
      let query = process.new_subject()
      process.send(ready, #(outbox, query))
      process.new_selector()
      |> process.select_map(outbox, Outbound)
      |> process.select_map(query, Query)
      |> collect([])
    })
  let assert Ok(#(outbox, query)) = process.receive(ready, 1000)
  Sock(ref: SocketRef(pid:, outbox:), query:)
}

fn collect(selector: process.Selector(Input), seen: List(String)) -> Nil {
  case process.selector_receive_forever(selector) {
    Query(reply) -> {
      process.send(reply, list.reverse(seen))
      collect(selector, seen)
    }
    Outbound(Send(_, frames)) ->
      collect(
        selector,
        list.fold(frames, seen, fn(acc, frame) {
          case frame {
            DispatchFrame(seq, event, _) -> [
              event <> ":" <> int.to_string(seq),
              ..acc
            ]
            _ -> acc
          }
        }),
      )
    Outbound(session.Moved(seq:, ..)) ->
      collect(selector, ["MOVED:" <> int.to_string(seq), ..seen])
    Outbound(session.Reattached(..)) ->
      collect(selector, ["REATTACHED", ..seen])
    Outbound(_) -> collect(selector, seen)
  }
}

pub fn sessions_gauge() -> Int {
  status.snapshot()
  |> status.numbers
  |> list.key_find("sessions")
  |> result.unwrap(-1)
}
