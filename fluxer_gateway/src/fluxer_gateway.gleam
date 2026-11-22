// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/string
import logging

type StartReason

type IoReply

type RestartType {
  Permanent
}

type Encoding {
  Utf8
}

type IoOption {
  Binary
  Encoding(Encoding)
}

@external(erlang, "application", "ensure_all_started")
fn ensure_all_started(
  application: Atom,
  restart_type: RestartType,
) -> Result(List(Atom), StartReason)

@external(erlang, "io", "setopts")
fn set_io_options(device: Atom, options: List(IoOption)) -> IoReply

@external(erlang, "init", "stop")
fn stop(status: Int) -> Nil

pub fn main() -> Nil {
  set_io_options(atom.create("standard_io"), [Binary, Encoding(Utf8)])
  set_io_options(atom.create("standard_error"), [Encoding(Utf8)])
  case ensure_all_started(atom.create("fluxer_gateway"), Permanent) {
    Ok(_) -> process.sleep_forever()
    Error(reason) -> {
      logging.log(
        logging.Emergency,
        "Gateway failed to start: " <> string.inspect(reason),
      )
      stop(1)
    }
  }
}
