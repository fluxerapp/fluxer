// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain
import fluxer_gateway/app/supervisor
import fluxer_gateway/config/draining
import fluxer_gateway/config/env
import fluxer_gateway/config/settings
import fluxer_gateway/nif
import gleam/erlang/application.{type StartType}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/otp/actor
import gleam/result
import gleam/string
import logging

pub type StartFailure {
  InvalidSettings(reason: String)
  NifUnavailable(reason: String)
  SupervisorFailed(actor.StartError)
}

type SystemFlag {
  FullsweepAfter
}

type LoggerKey {
  Level
}

type HandlerId {
  Default
}

type LoggerReply

@external(erlang, "erlang", "system_flag")
fn system_flag(flag: SystemFlag, value: Int) -> Int

@external(erlang, "logger", "set_primary_config")
fn set_primary_config(key: LoggerKey, value: Atom) -> LoggerReply

@external(erlang, "logger", "set_handler_config")
fn set_handler_config(
  handler: HandlerId,
  key: LoggerKey,
  value: Atom,
) -> LoggerReply

pub fn start(
  _start_type: StartType,
  _arguments: List(Nil),
) -> Result(Pid, StartFailure) {
  system_flag(FullsweepAfter, 10)
  use loaded <- result.try(
    settings.read(env.system())
    |> result.map_error(fn(error) {
      let reason = settings.describe_error(error)
      logging.log(logging.Critical, "Gateway settings invalid: " <> reason)
      InvalidSettings(reason)
    }),
  )
  let loaded = settings.store(loaded)
  apply_logger_level(loaded.logger_level)
  use Nil <- result.try(
    nif.load()
    |> result.map_error(fn(error) {
      let reason = case error {
        nif.NoPrivDirectory -> "no priv directory"
        nif.LoadFailed(reason) -> reason
      }
      logging.log(
        logging.Critical,
        "Gateway NIF library failed to load: " <> reason,
      )
      NifUnavailable(reason)
    }),
  )
  case draining.is_draining() {
    True -> draining.clear_draining()
    False -> Nil
  }
  supervisor.start(loaded)
  |> result.map(fn(started) { started.pid })
  |> result.map_error(fn(error) {
    logging.log(
      logging.Critical,
      "Gateway supervisor failed to start: " <> string.inspect(error),
    )
    SupervisorFailed(error)
  })
}

pub fn prep_stop(state: a) -> a {
  drain.run()
  state
}

pub fn stop(_state: a) -> Nil {
  Nil
}

fn apply_logger_level(level: settings.LogLevel) -> Nil {
  let name = atom.create(settings.log_level_name(level))
  set_primary_config(Level, name)
  set_handler_config(Default, Level, name)
  Nil
}
