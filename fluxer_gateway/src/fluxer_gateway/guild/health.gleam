// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/guild/inbox.{type Control}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/table.{type Table}
import fluxer_gateway/session/control
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

fn init_timeout_ms() -> Int {
  durations.guild().health_init_timeout_ms
}

fn interval_ms() -> Int {
  durations.guild().health_interval_ms
}

fn degraded_ms() -> Int {
  durations.guild().health_degraded_ms
}

fn recovered_ms() -> Int {
  durations.guild().health_recovered_ms
}

fn probe_timeout_ms() -> Int {
  durations.guild().health_probe_timeout_ms
}

fn stale_probe_ms() -> Int {
  durations.guild().health_stale_probe_ms
}

type Entry {
  Entry(
    pid: Pid,
    control: Subject(Control),
    degraded: Bool,
    pending: Option(Probe),
    targets: Table(String, SessionRef),
  )
}

type Probe {
  Probe(token: Int, sent_at: Int, lag: Int)
}

pub opaque type Message {
  Tick
  Register(guild: Int, pid: Pid)
  Pong(guild: Int, token: Int, handled_at: Int)
  GuildDown(process.Down)
}

type State {
  State(self: Subject(Message), watched: Watched, next_token: Int)
}

@internal
pub opaque type Watched {
  Watched(guilds: Dict(Int, #(Pid, Monitor)), monitors: Dict(Monitor, Int))
}

@internal
pub fn new_watched() -> Watched {
  Watched(dict.new(), dict.new())
}

@internal
pub fn watch(
  watched: Watched,
  guild: Int,
  pid: Pid,
  monitor: fn(Pid) -> Monitor,
) -> #(Watched, Option(Monitor)) {
  case dict.get(watched.guilds, guild) {
    Ok(#(current, _)) if current == pid -> #(watched, None)
    found -> {
      let fresh = monitor(pid)
      let replaced = case found {
        Ok(#(_, old)) -> Some(old)
        Error(Nil) -> None
      }
      let monitors = case replaced {
        Some(old) -> dict.delete(watched.monitors, old)
        None -> watched.monitors
      }
      #(
        Watched(
          guilds: dict.insert(watched.guilds, guild, #(pid, fresh)),
          monitors: dict.insert(monitors, fresh, guild),
        ),
        replaced,
      )
    }
  }
}

@internal
pub fn down(watched: Watched, monitor: Monitor) -> #(Watched, Option(Int)) {
  case dict.get(watched.monitors, monitor) {
    Ok(guild) -> #(
      Watched(
        guilds: dict.delete(watched.guilds, guild),
        monitors: dict.delete(watched.monitors, monitor),
      ),
      Some(guild),
    )
    Error(Nil) -> #(watched, None)
  }
}

@internal
pub fn watched_guilds(watched: Watched) -> List(Int) {
  dict.keys(watched.guilds)
}

fn rows() -> Table(Int, Entry) {
  table.named(names.health_table)
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(named) {
      let self = process.new_subject()
      process.send_after(self, interval_ms(), Tick)
      let selector =
        process.new_selector()
        |> process.select(named)
        |> process.select(self)
        |> process.select_monitors(GuildDown)
      actor.initialised(State(self, recover_watched(), 1))
      |> actor.selecting(selector)
      |> actor.returning(Nil)
      |> Ok
    })
    |> actor.named(names.health())
    |> actor.on_message(handle)
    |> actor.start
  })
}

fn recover_watched() -> Watched {
  table.fold(rows(), new_watched(), fn(watched, guild, entry: Entry) {
    case process.is_alive(entry.pid) {
      True -> watch(watched, guild, entry.pid, process.monitor).0
      False -> {
        table.delete_row(rows(), guild, entry)
        watched
      }
    }
  })
}

pub fn register(guild: GuildId, control: Subject(Control)) -> Nil {
  let pid = process.self()
  let targets =
    table.new("fluxer_gateway_guild_health_sessions", [
      table.Set,
      table.ReadConcurrency(True),
    ])
  table.insert(
    rows(),
    id.guild_id_to_int(guild),
    Entry(pid, control, False, None, targets),
  )
  case process.named(names.health()) {
    Ok(_) ->
      process.send(
        process.named_subject(names.health()),
        Register(id.guild_id_to_int(guild), pid),
      )
    Error(Nil) -> Nil
  }
}

pub fn unregister(guild: GuildId) -> Nil {
  let me = process.self()
  case table.lookup(rows(), id.guild_id_to_int(guild)) {
    Ok(entry) if entry.pid == me -> {
      table.delete(rows(), id.guild_id_to_int(guild))
      table.drop(entry.targets)
    }
    _ -> Nil
  }
}

pub fn put_session(guild: GuildId, ref: SessionRef) -> Nil {
  case own_entry(guild) {
    Ok(entry) ->
      table.insert(entry.targets, session.session_id_to_string(ref.id), ref)
    Error(Nil) -> Nil
  }
}

pub fn remove_session(guild: GuildId, session_id: SessionId) -> Nil {
  case own_entry(guild) {
    Ok(entry) ->
      table.delete(entry.targets, session.session_id_to_string(session_id))
    Error(Nil) -> Nil
  }
}

pub fn send_current(guild: GuildId, ref: SessionRef) -> Nil {
  case table.lookup(rows(), id.guild_id_to_int(guild)) {
    Ok(entry) -> control.send(ref, control.GuildHealth(guild, entry.degraded))
    Error(Nil) -> Nil
  }
}

fn own_entry(guild: GuildId) -> Result(Entry, Nil) {
  let me = process.self()
  case table.lookup(rows(), id.guild_id_to_int(guild)) {
    Ok(entry) if entry.pid == me -> Ok(entry)
    _ -> Error(Nil)
  }
}

fn update(guild: Int, previous: Entry, next: Entry) -> Nil {
  table.insert(rows(), guild, next)
  case
    previous.degraded == next.degraded || !table.exists(next.targets),
    id.guild_id(guild)
  {
    False, Ok(guild_id) ->
      table.to_list(next.targets)
      |> list.each(fn(row) {
        control.send(row.1, control.GuildHealth(guild_id, next.degraded))
      })
    _, _ -> Nil
  }
}

pub fn is_overloaded(guild: GuildId) -> Bool {
  case table.lookup(rows(), id.guild_id_to_int(guild)) {
    Ok(Entry(pending: Some(Probe(sent_at:, lag:, ..)), ..)) -> {
      let now = clock.monotonic_ms()
      now - sent_at <= stale_probe_ms()
      && int.max(lag, now - sent_at) > degraded_ms()
    }
    _ -> False
  }
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Register(guild, pid) -> {
      let #(watched, replaced) =
        watch(state.watched, guild, pid, process.monitor)
      case replaced {
        Some(old) -> process.demonitor_process(old)
        None -> Nil
      }
      actor.continue(State(..state, watched:))
    }
    GuildDown(process.ProcessDown(monitor:, pid:, ..)) -> {
      let #(watched, gone) = down(state.watched, monitor)
      case gone {
        Some(guild) ->
          case table.lookup(rows(), guild) {
            Ok(entry) if entry.pid == pid -> table.delete(rows(), guild)
            _ -> Nil
          }
        None -> Nil
      }
      actor.continue(State(..state, watched:))
    }
    GuildDown(_) -> actor.continue(state)
    Pong(guild, token, handled_at) -> {
      pong(guild, token, handled_at)
      actor.continue(state)
    }
    Tick -> {
      let now = clock.monotonic_ms()
      let next_token =
        dict.fold(state.watched.guilds, state.next_token, fn(token, guild, _) {
          check(state.self, guild, now, token)
        })
      process.send_after(state.self, interval_ms(), Tick)
      actor.continue(State(..state, next_token:))
    }
  }
}

fn check(self: Subject(Message), guild: Int, now: Int, token: Int) -> Int {
  let timeout_ms = probe_timeout_ms()
  case table.lookup(rows(), guild) {
    Error(Nil) -> token
    Ok(entry) ->
      case entry.pending {
        None ->
          case entry.degraded || busy(entry.pid) {
            True -> {
              probe(self, guild, entry, token, now, 0, entry.degraded)
              token + 1
            }
            False -> token
          }
        Some(Probe(sent_at:, lag:, ..)) if now - sent_at >= timeout_ms -> {
          probe(
            self,
            guild,
            entry,
            token,
            now,
            int.max(lag, now - sent_at),
            True,
          )
          token + 1
        }
        Some(Probe(sent_at:, ..)) -> {
          update(
            guild,
            entry,
            Entry(
              ..entry,
              degraded: entry.degraded || now - sent_at > degraded_ms(),
            ),
          )
          token
        }
      }
  }
}

fn probe(
  self: Subject(Message),
  guild: Int,
  entry: Entry,
  token: Int,
  now: Int,
  lag: Int,
  degraded: Bool,
) -> Nil {
  process.send(
    entry.control,
    inbox.HealthProbe(token, fn(answered, handled_at) {
      process.send(self, Pong(guild, answered, handled_at))
    }),
  )
  update(
    guild,
    entry,
    Entry(..entry, degraded:, pending: Some(Probe(token:, sent_at: now, lag:))),
  )
}

fn pong(guild: Int, token: Int, handled_at: Int) -> Nil {
  case table.lookup(rows(), guild) {
    Ok(Entry(pending: Some(Probe(token: expected, sent_at:, ..)), ..) as entry)
      if expected == token
    -> {
      let delay = int.max(0, handled_at - sent_at)
      let slow_ms = degraded_ms()
      let fast_ms = recovered_ms()
      let degraded = case delay {
        _ if delay > slow_ms -> True
        _ if delay < fast_ms -> False
        _ -> entry.degraded
      }
      update(guild, entry, Entry(..entry, degraded:, pending: None))
    }
    _ -> Nil
  }
}

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, items: List(atom.Atom)) -> Dynamic

fn busy(pid: Pid) -> Bool {
  let info =
    process_info(pid, [atom.create("message_queue_len"), atom.create("status")])
  case decode.run(info, decode.list(decode.dynamic)) {
    Ok([length, status]) -> {
      let queued = decode.run(length, decode.at([1], decode.int))
      let running =
        decode.run(status, decode.at([1], atom.decoder()))
        == Ok(atom.create("running"))
      case queued {
        Ok(count) -> count > 0 || running
        Error(_) -> running
      }
    }
    _ -> False
  }
}
