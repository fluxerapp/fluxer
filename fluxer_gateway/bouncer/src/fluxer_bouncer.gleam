// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_bouncer/config
import fluxer_bouncer/server
import fluxer_bouncer/shutdown
import fluxer_bouncer/status
import gleam/erlang/process
import gleam/result

@external(erlang, "erlang", "halt")
fn halt(status: Int) -> Nil

pub fn main() -> Nil {
  case config.from_env() {
    Error(_) -> halt(2)
    Ok(settings) ->
      case server.start(settings) {
        Error(_) -> halt(1)
        Ok(_) ->
          case serve_status(settings.status_port), shutdown.install() {
            Ok(Nil), Ok(Nil) ->
              shutdown.run(settings.timings.leave_bound_ms, server.leave)
            Error(Nil), _ -> halt(3)
            Ok(Nil), Error(Nil) -> halt(4)
          }
      }
  }
}

fn serve_status(port: Int) -> Result(Nil, Nil) {
  case port {
    0 -> Ok(Nil)
    _ ->
      status.start(
        port,
        process.new_name("fluxer_bouncer_status"),
        status.current,
      )
      |> result.replace(Nil)
      |> result.replace_error(Nil)
  }
}
