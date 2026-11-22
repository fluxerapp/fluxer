// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/close_frame
import fluxer_gateway/gateway/ws/frame
import gleam/bytes_tree.{type BytesTree}
import gleam/result
import glisten/socket.{type Socket}
import glisten/socket/options
import glisten/transport.{type Transport}

pub const receive_window = 16

pub opaque type Link {
  Link(socket: Socket, transport: Transport)
}

pub fn new(socket: Socket, transport: Transport) -> Link {
  Link(socket:, transport:)
}

pub fn send(link: Link, data: BytesTree) -> Result(Nil, Nil) {
  transport.send(link.transport, link.socket, data)
  |> result.replace_error(Nil)
}

pub fn text(link: Link, payload: BytesTree) -> Result(Nil, Nil) {
  send(link, frame.encode(frame.Text, payload))
}

pub fn binary(link: Link, payload: BytesTree) -> Result(Nil, Nil) {
  send(link, frame.encode(frame.Binary, payload))
}

pub fn pong(link: Link, payload: BitArray) -> Nil {
  let _ =
    send(link, frame.encode(frame.Pong, bytes_tree.from_bit_array(payload)))
  Nil
}

pub fn close(link: Link, code: Int, reason: String) -> Nil {
  let _ =
    send(link, bytes_tree.from_bit_array(close_frame.encode(code, reason)))
  Nil
}

pub fn close_without_code(link: Link) -> Nil {
  let _ = send(link, frame.encode(frame.Close, bytes_tree.new()))
  Nil
}

pub fn set_receiving(link: Link, receiving: Bool) -> Nil {
  let mode = case receiving {
    True -> options.Count(receive_window)
    False -> options.Passive
  }
  let _ =
    transport.set_opts(link.transport, link.socket, [options.ActiveMode(mode)])
  Nil
}

pub fn set_send_timeout(link: Link, timeout_ms: Int) -> Nil {
  let _ =
    transport.set_opts(link.transport, link.socket, [
      options.SendTimeout(timeout_ms),
    ])
  Nil
}
