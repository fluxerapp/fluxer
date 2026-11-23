// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, Dispatch}
import fluxer_gateway/session/control
import fluxer_gateway/session/dispatch
import fluxer_gateway/session/links
import fluxer_gateway/session/relations
import fluxer_gateway/session/state.{
  type CallSlot, type State, CallReconnect, Lost, State, Watched,
}
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/call/state as call_state
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result

const max_retry_attempts = 15

fn get_state_timeout_ms() -> Int {
  durations.session().calls_get_state_timeout_ms
}

fn unavailable_retry_ms() -> Int {
  durations.session().calls_unavailable_retry_ms
}

pub fn watch(state: State, channel: ChannelId, call: Pid) -> State {
  let key = id.channel_id_to_int(channel)
  case dict.get(state.calls, key) {
    Ok(Watched(old, _)) if old == call -> state
    Ok(Watched(_, old_monitor)) -> {
      process.demonitor_process(old_monitor)
      put(state, key, Watched(call, process.monitor(call)))
    }
    _ -> put(state, key, Watched(call, process.monitor(call)))
  }
}

pub fn unwatch(state: State, channel: ChannelId) -> State {
  let key = id.channel_id_to_int(channel)
  case dict.get(state.calls, key) {
    Ok(Watched(_, monitor)) -> {
      process.demonitor_process(monitor)
      State(..state, calls: dict.delete(state.calls, key))
    }
    _ -> state
  }
}

pub fn on_down(
  state: State,
  monitor: Monitor,
  reason: process.ExitReason,
) -> Option(State) {
  find(state, monitor)
  |> option.map(fn(key) {
    case reason {
      process.Normal -> ended(state, key)
      process.Abnormal(term) ->
        case is_handoff(term) {
          True -> after_handoff(state, key)
          False -> unavailable(state, key)
        }
      process.Killed -> unavailable(state, key)
    }
  })
}

pub fn reconnect(state: State, channel: Int, attempt: Int) -> State {
  let state =
    State(..state, call_timers: dict.delete(state.call_timers, channel))
  case dict.get(state.calls, channel) {
    Ok(Watched(_, _)) -> state
    _ ->
      case id.channel_id(channel) {
        Ok(channel_id) -> attempt_reconnect(state, channel_id, attempt)
        Error(_) -> state
      }
  }
}

pub fn announce(state: State) -> Nil {
  let me = state.me
  let channels = relations.private_channel_ids(state.relations)
  process.spawn_unlinked(fn() {
    list.each(channels, fn(channel) { announce_channel(me, channel) })
  })
  Nil
}

fn announce_channel(me: SessionRef, channel: Int) -> Nil {
  let found = {
    use channel_id <- result.try(
      id.channel_id(channel) |> result.replace_error(Nil),
    )
    use call <- result.try(
      manager.lookup(channel_id) |> result.replace_error(Nil),
    )
    use pid <- result.try(process.subject_owner(call))
    use view <- result.map(call_server.get_state(call, get_state_timeout_ms()))
    #(channel_id, pid, view)
  }
  case found {
    Ok(#(channel_id, pid, view)) -> {
      control.send(me, control.CallMonitor(channel_id, pid))
      process.send(
        me.inbox,
        Dispatch("CALL_CREATE", call_state.view_event(view)),
      )
    }
    Error(Nil) -> Nil
  }
}

fn put(state: State, key: Int, slot: CallSlot) -> State {
  State(..state, calls: dict.insert(state.calls, key, slot))
}

fn find(state: State, monitor: Monitor) -> Option(Int) {
  state.calls
  |> dict.to_list
  |> list.find_map(fn(entry) {
    case entry.1 {
      Watched(_, watched) if watched == monitor -> Ok(entry.0)
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

fn is_handoff(term: Dynamic) -> Bool {
  let shutdown_handoff = {
    use tag <- decode.field(0, atom.decoder())
    use detail <- decode.field(1, atom.decoder())
    decode.success(
      atom.to_string(tag) == "shutdown" && atom.to_string(detail) == "handoff",
    )
  }
  let bare_handoff =
    atom.decoder() |> decode.map(fn(tag) { atom.to_string(tag) == "handoff" })
  let text_handoff = decode.string |> decode.map(fn(text) { text == "handoff" })
  decode.run(
    term,
    decode.one_of(shutdown_handoff, [bare_handoff, text_handoff]),
  )
  == Ok(True)
}

fn after_handoff(state: State, key: Int) -> State {
  case rewatch(state, key) {
    Ok(state) -> state
    Error(Nil) -> {
      process.send(state.internal, CallReconnect(key, 0))
      put(state, key, Lost)
    }
  }
}

fn rewatch(state: State, key: Int) -> Result(State, Nil) {
  use channel <- result.try(id.channel_id(key) |> result.replace_error(Nil))
  use call <- result.try(manager.lookup(channel) |> result.replace_error(Nil))
  use pid <- result.map(process.subject_owner(call))
  put(state, key, Watched(pid, process.monitor(pid)))
}

fn ended(state: State, key: Int) -> State {
  cancel_timer(state, key)
  State(
    ..state,
    calls: dict.delete(state.calls, key),
    call_timers: dict.delete(state.call_timers, key),
  )
}

fn cancel_timer(state: State, key: Int) -> Nil {
  case dict.get(state.call_timers, key) {
    Ok(timer) -> {
      process.cancel_timer(timer)
      Nil
    }
    Error(Nil) -> Nil
  }
}

fn unavailable(state: State, key: Int) -> State {
  let payload =
    json.object([
      #("channel_id", id.json(key)),
      #("unavailable", json.bool(True)),
    ])
    |> raw_json.from_json
  let state = dispatch.emit(state, "CALL_DELETE", payload)
  let state = put(state, key, Lost)
  cancel_timer(state, key)
  let timer =
    process.send_after(
      state.internal,
      unavailable_retry_ms(),
      CallReconnect(key, 0),
    )
  State(..state, call_timers: dict.insert(state.call_timers, key, timer))
}

fn attempt_reconnect(state: State, channel: ChannelId, attempt: Int) -> State {
  let key = id.channel_id_to_int(channel)
  let target = {
    use call <- result.try(manager.lookup(channel) |> result.replace_error(Nil))
    use pid <- result.map(process.subject_owner(call))
    #(call, pid)
  }
  case target {
    Error(Nil) -> retry_or_remove(state, key, attempt)
    Ok(#(call, pid)) -> {
      let monitor = process.monitor(pid)
      case call_server.get_state(call, get_state_timeout_ms()) {
        Ok(view) ->
          put(state, key, Watched(pid, monitor))
          |> dispatch.emit("CALL_CREATE", call_state.view_event(view))
        Error(Nil) -> {
          process.demonitor_process(monitor)
          retry_or_remove(state, key, attempt)
        }
      }
    }
  }
}

fn retry_or_remove(state: State, key: Int, attempt: Int) -> State {
  case attempt < max_retry_attempts {
    True -> {
      process.send_after(
        state.internal,
        links.backoff_ms(attempt),
        CallReconnect(key, attempt + 1),
      )
      state
    }
    False -> State(..state, calls: dict.delete(state.calls, key))
  }
}
