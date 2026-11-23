// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/wire
import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/core/voice_state
import fluxer_gateway/presence/client as presence
import fluxer_gateway/session/control
import fluxer_gateway/util/decode as edge
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/call/state as call_state
import fluxer_gateway/voice/client_update.{
  type ViewerKeys, BadKeys, ClearKeys, KeepKeys, Keys,
}
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/e2ee
import fluxer_gateway/voice/entry.{type Entry, type Flags}
import fluxer_gateway/voice/geo.{type Geo}
import fluxer_gateway/voice/guild_state
import fluxer_gateway/voice/metrics as voice_metrics
import fluxer_gateway/voice/p2p
import fluxer_gateway/voice/registry
import fluxer_gateway/voice/store.{type Store}
import fluxer_gateway/voice/stream_key
import fluxer_gateway/voice/token
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/set.{type Set}

fn init_timeout_ms() -> Int {
  durations.voice().dm_server_init_timeout_ms
}

fn call_timeout_ms() -> Int {
  durations.voice().server_call_timeout_ms
}

fn handshake_timeout_ms() -> Int {
  durations.voice().server_handshake_timeout_ms
}

fn retry_sleep_ms() -> Int {
  durations.voice().server_retry_sleep_ms
}

fn legacy_check_interval_ms() -> Int {
  durations.voice().dm_legacy_call_check_interval_ms
}

const connection_limit = 5

const join_retries = 10

const adopt_attempts = 3

pub type DmUpdate {
  DmUpdate(
    channel: Option(ChannelId),
    connection_id: Option(ConnectionId),
    flags: Flags,
    viewer_keys: ViewerKeys,
    geo: Geo,
    e2ee_capable: Bool,
    bot: Bool,
    p2p: Option(Bool),
    country_code: Option(String),
    calls: List(ChannelId),
  )
}

pub opaque type Message {
  Update(DmUpdate)
  ForceDisconnect(channel: ChannelId, connection_id: Option(ConnectionId))
  Converted(connection_id: ConnectionId)
  CallJoined(channel: ChannelId, call: Pid)
  Down(process.Down)
  Adopted(view: call_state.View, session: SessionRef, reply: Subject(Bool))
  Detach(session: SessionRef)
  Close(session: SessionRef)
}

type DmChannel {
  DmChannel(kind: Int, recipients: List(UserId))
}

type Dm {
  Dm(
    self: Subject(Message),
    calls: Subject(CallOp),
    session: SessionRef,
    session_monitor: Option(Monitor),
    states: Store(Entry),
    channels: Dict(Int, List(UserId)),
    watched: Dict(Pid, Watch),
  )
}

type Watch {
  Watch(channel: ChannelId, monitor: Monitor, connections: Set(ConnectionId))
}

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: String) -> List(#(String, Subject(Message)))

@external(erlang, "ets", "insert_new")
fn ets_insert_new(table: Atom, row: #(String, Subject(Message))) -> Bool

@external(erlang, "ets", "delete_object")
fn ets_delete_object(table: Atom, row: #(String, Subject(Message))) -> Bool

fn key(ref: SessionRef) -> String {
  session.session_id_to_string(ref.id)
}

pub fn lookup(session: SessionRef) -> Result(Subject(Message), Nil) {
  let table = registry.dm_sessions_table()
  use <- bool.guard(!registry.exists(table), Error(Nil))
  case ets_lookup(table, key(session)) {
    [#(_, subject)] ->
      case alive(subject) {
        True -> Ok(subject)
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn alive(subject: Subject(Message)) -> Bool {
  case process.subject_owner(subject) {
    Ok(pid) -> process.is_alive(pid)
    Error(Nil) -> False
  }
}

pub fn ensure(session: SessionRef) -> Result(Subject(Message), Nil) {
  use <- result.lazy_or(lookup(session))
  case start(session) {
    Ok(subject) -> Ok(subject)
    Error(Nil) -> lookup(session)
  }
}

fn claim(table: Atom, row: #(String, Subject(Message)), attempts: Int) -> Bool {
  use <- bool.guard(ets_insert_new(table, row), True)
  use <- bool.guard(attempts <= 1, False)
  case ets_lookup(table, row.0) {
    [#(_, existing) as stale] ->
      case alive(existing) {
        True -> False
        False -> {
          ets_delete_object(table, stale)
          claim(table, row, attempts - 1)
        }
      }
    _ -> claim(table, row, attempts - 1)
  }
}

fn release(session: SessionRef, self: Subject(Message)) -> Nil {
  let table = registry.dm_sessions_table()
  case registry.exists(table) {
    True -> {
      ets_delete_object(table, #(key(session), self))
      Nil
    }
    False -> Nil
  }
}

fn start(session: SessionRef) -> Result(Subject(Message), Nil) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    let selector =
      process.new_selector()
      |> process.select(self)
      |> process.select_map(call_state.notes(process.self()), noted)
      |> process.select_monitors(Down)
    let table = registry.dm_sessions_table()
    use Nil <- result.try(case registry.exists(table) {
      True ->
        case claim(table, #(key(session), self), 3) {
          True -> Ok(Nil)
          False -> Error("dm_session_already_registered")
        }
      False -> Ok(Nil)
    })
    let session_monitor =
      process.subject_owner(session.inbox)
      |> result.map(process.monitor)
      |> option.from_result
    use calls <- result.try(case start_call_worker(session, self) {
      Ok(calls) -> Ok(calls)
      Error(Nil) -> {
        release(session, self)
        Error("dm_call_worker_failed")
      }
    })
    actor.initialised(Dm(
      self:,
      calls:,
      session:,
      session_monitor:,
      states: store.new(),
      channels: dict.new(),
      watched: dict.new(),
    ))
    |> actor.selecting(selector)
    |> actor.returning(self)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.start
  |> result.map(fn(started) {
    process.unlink(started.pid)
    started.data
  })
  |> result.replace_error(Nil)
}

pub fn update(subject: Subject(Message), update: DmUpdate) -> Nil {
  process.send(subject, Update(update))
}

pub fn close(subject: Subject(Message), session: SessionRef) -> Nil {
  process.send(subject, Close(session))
}

pub fn detach(subject: Subject(Message), session: SessionRef) -> Nil {
  process.send(subject, Detach(session))
}

pub fn adopt(session: SessionRef, channels: List(ChannelId)) -> Nil {
  process.spawn_unlinked(fn() {
    list.each(channels, fn(channel) {
      case call_view(channel) {
        Some(view) ->
          case owned(session, view.connections) {
            [] -> Nil
            _ -> adopt_into(session, view, adopt_attempts)
          }
        None -> Nil
      }
    })
  })
  Nil
}

fn adopt_into(
  session: SessionRef,
  view: call_state.View,
  attempts: Int,
) -> Nil {
  case ensure(session) {
    Ok(subject) ->
      case hand_adopted(subject, session, view), attempts > 1 {
        True, _ -> Nil
        False, True -> adopt_into(session, view, attempts - 1)
        False, False -> Nil
      }
    Error(Nil) -> Nil
  }
}

fn hand_adopted(
  subject: Subject(Message),
  session: SessionRef,
  view: call_state.View,
) -> Bool {
  case process.subject_owner(subject) {
    Error(Nil) -> False
    Ok(owner) -> {
      let reply = process.new_subject()
      let monitor = process.monitor(owner)
      process.send(subject, Adopted(view:, session:, reply:))
      let selector =
        process.new_selector()
        |> process.select(reply)
        |> process.select_specific_monitor(monitor, fn(_) { False })
      let taken =
        process.selector_receive(selector, call_timeout_ms())
        |> result.unwrap(False)
      process.demonitor_process(monitor)
      taken
    }
  }
}

pub fn owned(session: SessionRef, connections: List(Entry)) -> List(Entry) {
  list.filter(connections, fn(value) {
    entry.user(value) == session.user
    && entry.session_id(value) == Some(key(session))
  })
}

fn handle(dm: Dm, message: Message) -> actor.Next(Dm, Message) {
  case message {
    Update(update) ->
      case update.channel {
        None ->
          case update.connection_id {
            None -> actor.continue(disconnect_all(dm))
            Some(conn) ->
              actor.continue(
                disconnect_one(dm, conn, update.calls) |> report(dm),
              )
          }
        Some(channel) ->
          actor.continue(connect(dm, channel, update) |> report(dm))
      }
    ForceDisconnect(channel, conn) ->
      actor.continue(force_disconnect(dm, channel, conn))
    Converted(conn) -> actor.continue(converted(dm, conn))
    CallJoined(channel, call) -> actor.continue(watch_call(dm, channel, call))
    Down(process.ProcessDown(monitor:, ..))
      if Some(monitor) == dm.session_monitor
    -> {
      let dm = disconnect_all(dm)
      list.each(store.keys(dm.states), count_table.remove)
      unregister(dm)
      actor.stop()
    }
    Down(process.ProcessDown(monitor:, pid:, reason:)) ->
      actor.continue(call_down(dm, pid, monitor, reason))
    Down(process.PortDown(..)) -> actor.continue(dm)
    Adopted(view:, session:, reply:) ->
      case dm.session.inbox == session.inbox {
        True -> {
          let dm = adopt_view(dm, view)
          process.send(reply, True)
          actor.continue(dm)
        }
        False -> {
          list.each(store.keys(dm.states), count_table.remove)
          unregister(dm)
          process.send(reply, False)
          actor.stop()
        }
      }
    Detach(session:) if session.inbox != dm.session.inbox -> actor.continue(dm)
    Detach(..) -> {
      list.each(store.keys(dm.states), count_table.remove)
      unregister(dm)
      actor.stop()
    }
    Close(session:) if session.inbox != dm.session.inbox -> actor.continue(dm)
    Close(..) -> {
      let dm = disconnect_all(dm)
      unregister(dm)
      actor.stop()
    }
  }
}

fn watch_call(dm: Dm, channel: ChannelId, call: Pid) -> Dm {
  let seen = held_by(dm, channel, call)
  case dict.get(dm.watched, call), store.in_channel(dm.states, channel) {
    Ok(watch), _ -> {
      let watch =
        Watch(..watch, connections: set.union(watch.connections, seen))
      Dm(..dm, watched: dict.insert(dm.watched, call, watch))
    }
    Error(Nil), [] -> dm
    Error(Nil), _ -> {
      let watch =
        Watch(channel:, monitor: process.monitor(call), connections: seen)
      Dm(..dm, watched: dict.insert(dm.watched, call, watch))
    }
  }
}

fn held_by(dm: Dm, channel: ChannelId, call: Pid) -> Set(ConnectionId) {
  let mine =
    store.in_channel(dm.states, channel)
    |> list.map(fn(pair) { voice_key.connection(pair.0) })
  use <- bool.guard(mine == [], set.new())
  let view = {
    use found <- result.try(
      manager.lookup(channel) |> result.replace_error(Nil),
    )
    use owner <- result.try(process.subject_owner(found))
    use <- bool.guard(owner != call, Error(Nil))
    call_server.get_state(found, call_timeout_ms())
  }
  case view {
    Ok(view) ->
      view.connections
      |> list.map(entry.connection_id)
      |> list.filter(list.contains(mine, _))
      |> set.from_list
    Error(Nil) -> set.new()
  }
}

fn call_down(
  dm: Dm,
  call: Pid,
  monitor: Monitor,
  reason: process.ExitReason,
) -> Dm {
  case dict.get(dm.watched, call) {
    Ok(watch) if watch.monitor == monitor -> {
      let dm = Dm(..dm, watched: dict.delete(dm.watched, call))
      let exit = case is_handoff(reason), is_noproc(reason) {
        True, _ -> HandedOff
        False, True -> Vanished
        False, False -> Ended(watch.connections)
      }
      process.send(dm.calls, CallExited(watch.channel, call, exit))
      case exit {
        Ended(dropped) -> drop_connections(dm, watch.channel, dropped)
        HandedOff | Vanished -> dm
      }
    }
    _ -> dm
  }
}

fn is_handoff(reason: process.ExitReason) -> Bool {
  case reason {
    process.Abnormal(term) -> {
      let shutdown = {
        use tag <- decode.field(0, atom.decoder())
        use detail <- decode.field(1, atom.decoder())
        decode.success(#(atom.to_string(tag), atom.to_string(detail)))
      }
      case decode.run(term, shutdown), decode.run(term, atom.decoder()) {
        Ok(#("shutdown", "handoff")), _ -> True
        _, Ok(tag) -> atom.to_string(tag) == "handoff"
        _, _ -> False
      }
    }
    process.Normal | process.Killed -> False
  }
}

fn is_noproc(reason: process.ExitReason) -> Bool {
  case reason {
    process.Abnormal(term) ->
      decode.run(term, atom.decoder()) == Ok(atom.create("noproc"))
    process.Normal | process.Killed -> False
  }
}

fn drop_connections(
  dm: Dm,
  channel: ChannelId,
  connections: Set(ConnectionId),
) -> Dm {
  let gone =
    store.in_channel(dm.states, channel)
    |> list.map(fn(pair) { pair.0 })
    |> list.filter(fn(key) {
      set.contains(connections, voice_key.connection(key))
    })
  list.each(gone, count_table.remove)
  Dm(..dm, states: store.drop(dm.states, gone))
}

fn converted(dm: Dm, conn: ConnectionId) -> Dm {
  case store.by_connection(dm.states, conn) {
    Ok(#(key, value)) ->
      case entry.is_p2p(value) {
        True -> {
          let value = entry.to_sfu(value)
          count_table.upsert(value)
          Dm(..dm, states: store.put(dm.states, key, value))
        }
        False -> dm
      }
    Error(Nil) -> dm
  }
}

fn adopt_view(dm: Dm, view: call_state.View) -> Dm {
  let fresh =
    owned(dm.session, view.connections)
    |> list.filter_map(fn(value) {
      case store.has_connection(dm.states, entry.connection_id(value)) {
        True -> Error(Nil)
        False -> entry.key(value) |> result.map(fn(key) { #(key, value) })
      }
    })
  case fresh {
    [] -> dm
    _ -> {
      list.each(fresh, fn(pair) { count_table.upsert(pair.1) })
      let states =
        list.fold(fresh, dm.states, fn(acc, pair) {
          store.put(acc, pair.0, pair.1)
        })
      let recipients =
        list.filter(view.recipients, fn(other) { other != user(dm) })
      let channels =
        dict.insert(dm.channels, id.channel_id_to_int(view.channel), recipients)
      process.send(
        dm.calls,
        RebindCall(
          view.channel,
          session_text(dm),
          member(dm),
          list.map(fresh, fn(pair) { pair.1 }),
        ),
      )
      Dm(..dm, states:, channels:)
    }
  }
}

fn unregister(dm: Dm) -> Nil {
  release(dm.session, dm.self)
}

fn report(outcome: Result(Dm, GatewayError), fallback: Dm) -> Dm {
  case outcome {
    Ok(dm) -> dm
    Error(reason) -> {
      case p2p.reports_rejection(reason) {
        True -> control.send(fallback.session, control.VoiceRejected(reason))
        False -> Nil
      }
      fallback
    }
  }
}

fn user(dm: Dm) -> UserId {
  dm.session.user
}

fn session_text(dm: Dm) -> String {
  key(dm.session)
}

fn disconnect_all(dm: Dm) -> Dm {
  let mine =
    store.of_user(dm.states, user(dm))
    |> list.filter(fn(pair) {
      entry.session_id(pair.1) == None
      || entry.session_id(pair.1) == Some(session_text(dm))
    })
  let states = store.drop(dm.states, list.map(mine, fn(pair) { pair.0 }))
  list.each(mine, fn(pair) {
    count_table.remove(pair.0)
    broadcast(dm, entry.channel(pair.1), entry.disconnected(pair.1))
  })
  mine
  |> list.map(fn(pair) { voice_key.channel(pair.0) })
  |> list.unique
  |> list.each(fn(channel) { leave_call(dm, channel) })
  Dm(..dm, states:)
}

fn leave_call(dm: Dm, channel: ChannelId) -> Nil {
  process.send(dm.calls, LeaveCall(channel))
}

fn release_connection(dm: Dm, key: VoiceKey) -> Nil {
  let channel = voice_key.channel(key)
  case store.in_channel(dm.states, channel) {
    [] -> leave_call(dm, channel)
    _ -> process.send(dm.calls, DropCall(channel, voice_key.connection(key)))
  }
}

fn sync_call(dm: Dm, channel: ChannelId, value: Entry) -> Nil {
  process.send(dm.calls, SyncCall(user(dm), channel, value))
}

fn disconnect_one(
  dm: Dm,
  conn: ConnectionId,
  calls: List(ChannelId),
) -> Result(Dm, GatewayError) {
  case store.by_connection(dm.states, conn) {
    Error(Nil) -> {
      disconnect_remote(dm, conn, calls)
      Ok(dm)
    }
    Ok(#(key, old)) ->
      case voice_key.user(key) == user(dm) {
        False -> Error(error.VoiceUserMismatch)
        True -> {
          let dm = remove_connection(dm, #(key, old))
          release_connection(dm, key)
          spawn_media_disconnect(key)
          Ok(dm)
        }
      }
  }
}

fn spawn_media_disconnect(key: VoiceKey) -> Nil {
  process.spawn_unlinked(fn() { media_disconnect(key) })
  Nil
}

fn media_disconnect(key: VoiceKey) -> Nil {
  let _ =
    guild_state.force_disconnect(
      None,
      voice_key.channel(key),
      voice_key.user(key),
      voice_key.connection(key),
    )
  Nil
}

fn disconnect_remote(
  dm: Dm,
  conn: ConnectionId,
  calls: List(ChannelId),
) -> Nil {
  let me = user(dm)
  let known =
    dict.keys(dm.channels)
    |> list.filter_map(fn(channel) {
      id.channel_id(channel) |> result.replace_error(Nil)
    })
  process.spawn_unlinked(fn() {
    case remote_connection(list.unique(list.append(calls, known)), me, conn) {
      Ok(#(call, key)) -> {
        media_disconnect(key)
        let _ =
          call_server.disconnect_user(
            call,
            me,
            voice_key.channel(key),
            Some(conn),
            call_timeout_ms(),
          )
        Nil
      }
      Error(Nil) -> Nil
    }
  })
  Nil
}

fn remote_connection(
  channels: List(ChannelId),
  me: UserId,
  conn: ConnectionId,
) -> Result(#(manager.CallRef, VoiceKey), Nil) {
  list.find_map(channels, fn(channel) {
    use call <- result.try(manager.lookup(channel) |> result.replace_error(Nil))
    use view <- result.try(call_server.get_state(call, call_timeout_ms()))
    view.connections
    |> list.find(fn(value) { entry.connection_id(value) == conn })
    |> result.try(entry.key)
    |> result.try(fn(key) {
      case voice_key.user(key) == me {
        True -> Ok(#(call, key))
        False -> Error(Nil)
      }
    })
  })
}

fn remove_connection(dm: Dm, pair: #(VoiceKey, Entry)) -> Dm {
  let #(key, old) = pair
  count_table.remove(key)
  broadcast(dm, entry.channel(old), entry.disconnected(old))
  Dm(..dm, states: store.delete(dm.states, key))
}

fn force_disconnect(
  dm: Dm,
  channel: ChannelId,
  conn: Option(ConnectionId),
) -> Dm {
  let dm =
    force_targets(dm.states, user(dm), channel, conn)
    |> list.fold(dm, remove_connection)
  case conn, store.in_channel(dm.states, channel) {
    None, _ -> control.call_unmonitor(dm.session, channel)
    Some(_), [] -> leave_call(dm, channel)
    Some(_), _ -> Nil
  }
  dm
}

pub fn force_targets(
  states: Store(Entry),
  user: UserId,
  channel: ChannelId,
  conn: Option(ConnectionId),
) -> List(#(VoiceKey, Entry)) {
  store.of_user(states, user)
  |> list.filter(fn(pair) {
    voice_key.channel(pair.0) == channel
    && case conn {
      Some(conn) -> conn == voice_key.connection(pair.0)
      None -> True
    }
  })
}

fn broadcast(dm: Dm, channel: Option(ChannelId), value: Entry) -> Nil {
  case channel {
    None -> Nil
    Some(channel) ->
      case dict.get(dm.channels, id.channel_id_to_int(channel)) {
        Error(Nil) -> Nil
        Ok(found) -> {
          let recipients = list.unique([user(dm), ..found])
          let payload = entry.to_raw(value, entry.Broadcast)
          process.spawn_unlinked(fn() {
            list.each(recipients, fn(recipient) {
              presence.dispatch_to_user(
                recipient,
                "VOICE_STATE_UPDATE",
                payload,
              )
            })
          })
          Nil
        }
      }
  }
}

fn fetch_channel(
  dm: Dm,
  channel: ChannelId,
) -> Result(DmChannel, GatewayError) {
  let fields = [
    #("channel_id", id.json(id.channel_id_to_int(channel))),
    #("user_id", id.json(id.user_id_to_int(user(dm)))),
  ]
  let decoder = {
    use found <- decode.field(
      "channel",
      decode.optional({
        use kind <- decode.optional_field("type", -1, decode.int)
        use recipients <- decode.optional_field(
          "recipients",
          [],
          decode.list(recipient_decoder()),
        )
        decode.success(DmChannel(kind:, recipients: option.values(recipients)))
      }),
    )
    decode.success(found)
  }
  let found =
    token.api_read("get_dm_channel", fields, decoder)
    |> result.replace_error(Nil)
    |> result.flatten
  case found {
    Ok(Some(found)) ->
      Ok(
        DmChannel(
          ..found,
          recipients: list.filter(found.recipients, fn(other) {
            other != user(dm)
          }),
        ),
      )
    _ -> Error(error.DmChannelNotFound)
  }
}

fn recipient_decoder() -> decode.Decoder(Option(UserId)) {
  decode.one_of(decode.at(["id"], edge.user_id()) |> decode.map(Some), [
    edge.user_id() |> decode.map(Some),
    decode.success(None),
  ])
}

fn call_view(channel: ChannelId) -> Option(call_state.View) {
  case manager.lookup(channel) {
    Ok(call) ->
      call_server.get_state(call, call_timeout_ms()) |> option.from_result
    Error(_) -> None
  }
}

fn region_for_rpc(view: Option(call_state.View)) -> Option(String) {
  case option.then(view, fn(view) { view.region }) {
    Some("automatic") -> None
    other -> other
  }
}

fn connect(
  dm: Dm,
  channel: ChannelId,
  update: DmUpdate,
) -> Result(Dm, GatewayError) {
  use found <- result.try(fetch_channel(dm, channel))
  let dm =
    Dm(
      ..dm,
      channels: dict.insert(
        dm.channels,
        id.channel_id_to_int(channel),
        found.recipients,
      ),
    )
  case found.kind {
    1 | 3 ->
      case update.connection_id {
        None -> new_connection(dm, channel, update)
        Some(conn) -> existing_connection(dm, channel, conn, update)
      }
    _ -> Error(error.DmInvalidChannelType)
  }
}

fn known_states(dm: Dm, view: Option(call_state.View)) -> Store(Entry) {
  let from_call = case view {
    Some(view) ->
      view.connections
      |> list.filter_map(fn(value) {
        entry.key(value) |> result.map(fn(key) { #(key, value) })
      })
      |> store.from_list
    None -> store.new()
  }
  store.fold(dm.states, from_call, store.put)
}

fn resolve_keys(
  keys: ViewerKeys,
  channel: ChannelId,
  states: Store(Entry),
  existing: List(String),
) -> Result(List(String), GatewayError) {
  case keys {
    KeepKeys -> Ok(existing)
    ClearKeys -> Ok([])
    BadKeys -> Error(error.VoiceInvalidState)
    Keys(keys) -> stream_key.validate_dm(keys, channel, states)
  }
}

fn e2ee_check(
  states: Store(Entry),
  channel: ChannelId,
  update: DmUpdate,
) -> Result(Nil, GatewayError) {
  case e2ee.enabled_for_dm() {
    True ->
      e2ee.check_join(
        store.values(states),
        channel,
        capable: update.e2ee_capable,
        bot: update.bot,
      )
    False -> Ok(Nil)
  }
}

fn new_connection(
  dm: Dm,
  channel: ChannelId,
  update: DmUpdate,
) -> Result(Dm, GatewayError) {
  let own =
    store.keys_of_user(dm.states, user(dm))
    |> list.count(fn(key) { voice_key.channel(key) == channel })
  case own < connection_limit {
    False -> Error(error.VoiceConnectionLimitReached)
    True -> {
      let view = call_view(channel)
      let states = known_states(dm, view)
      let initial_keys = case update.viewer_keys {
        KeepKeys -> ClearKeys
        other -> other
      }
      use keys <- result.try(resolve_keys(initial_keys, channel, states, []))
      use Nil <- result.try(e2ee_check(states, channel, update))
      let decision = join_decision(dm, update, view)
      case decision {
        p2p.Reject(reason) -> Error(reason)
        _ ->
          admit(
            dm,
            channel,
            update,
            decision,
            region_for_rpc(view),
            None,
            fn(conn, capable) {
              build(dm, channel, conn, update, keys, capable)
            },
          )
      }
    }
  }
}

fn join_decision(
  dm: Dm,
  update: DmUpdate,
  view: Option(call_state.View),
) -> p2p.Decision {
  let others = case view {
    Some(view) ->
      list.filter(view.connections, fn(value) { entry.user(value) != user(dm) })
    None -> []
  }
  p2p.join_decision(
    agreed: p2p.agreed(update.p2p, update.bot),
    states: others,
    pending: False,
  )
}

fn admit(
  dm: Dm,
  channel: ChannelId,
  update: DmUpdate,
  decision: p2p.Decision,
  region: Option(String),
  existing: Option(#(VoiceKey, Entry)),
  make: fn(ConnectionId, Bool) -> Entry,
) -> Result(Dm, GatewayError) {
  let request =
    token.Request(
      ..token.request(
        guild: None,
        channel:,
        user: user(dm),
        connection_id: option.map(existing, fn(pair) {
          voice_key.connection(pair.0)
        }),
      ),
      geo: update.geo,
      region:,
      decision:,
      country_code: update.country_code,
    )
  use outcome <- result.try(token.fetch(request))
  let #(admission, granted) = case outcome {
    token.Granted(granted) -> #(
      p2p.admit(decision, p2p.SfuGrant),
      Some(granted),
    )
    token.GrantedP2p(grant) -> #(p2p.admit(decision, grant), None)
  }
  let capable = update.e2ee_capable && e2ee.enabled_for_dm()
  case admission, granted {
    p2p.Refuse(reason), _ -> Error(reason)
    p2p.AdmitP2p(conn, ice, max), _ -> {
      let created = make(conn, capable) |> entry.set_p2p(True)
      let dm = place(dm, channel, created, existing)
      join_call(
        dm,
        channel,
        created,
        Some(max),
        Some(p2p.voice_server_update(conn, channel, None, ice)),
        update.geo,
      )
      Ok(dm)
    }
    p2p.AdmitSfu, Some(granted) -> {
      use room_key <- result.try(case capable {
        True -> shared_room_key(channel, join_retries) |> result.map(Some)
        False -> Ok(None)
      })
      let created =
        make(granted.connection_id, capable)
        |> entry.set_p2p(False)
        |> entry.set_routing(granted.region_id, granted.server_id)
      let dm = place(dm, channel, created, existing)
      join_call(
        dm,
        channel,
        created,
        None,
        Some(server_update(channel, granted, room_key)),
        update.geo,
      )
      Ok(dm)
    }
    p2p.AdmitSfu, None -> Error(error.VoiceTokenFailed)
  }
}

fn shared_room_key(
  channel: ChannelId,
  retries: Int,
) -> Result(String, GatewayError) {
  case manager.lookup(channel), retries > 1 {
    Ok(call), _ ->
      call_server.room_key(call, call_timeout_ms())
      |> result.replace_error(error.VoiceTokenFailed)
    Error(_), True -> {
      process.sleep(retry_sleep_ms())
      shared_room_key(channel, retries - 1)
    }
    Error(_), False -> Error(error.VoiceTokenFailed)
  }
}

fn place(
  dm: Dm,
  channel: ChannelId,
  created: Entry,
  existing: Option(#(VoiceKey, Entry)),
) -> Dm {
  case existing {
    None -> keep(dm, channel, created)
    Some(#(old_key, old)) -> {
      let old_conn = voice_key.connection(old_key)
      let renamed = old_conn != entry.connection_id(created)
      count_table.remove(old_key)
      let states = case renamed {
        True -> store.delete(dm.states, old_key)
        False ->
          case
            store.rekey(dm.states, old_conn, to: channel, with: fn(_) {
              created
            })
          {
            Ok(#(_, moved)) -> moved
            Error(Nil) -> store.delete(dm.states, old_key)
          }
      }
      let dm = keep(Dm(..dm, states:), channel, created)
      case voice_key.channel(old_key) != channel {
        True -> {
          case renamed {
            True -> broadcast(dm, entry.channel(old), entry.disconnected(old))
            False -> Nil
          }
          release_connection(dm, old_key)
          dm
        }
        False -> dm
      }
    }
  }
}

fn server_update(
  channel: ChannelId,
  granted: token.Token,
  room_key: Option(String),
) -> RawJson {
  let base = [
    #("token", json.string(granted.token)),
    #("endpoint", json.string(granted.endpoint)),
    #("channel_id", id.channel_json(channel)),
    #("connection_id", voice_key.connection_json(granted.connection_id)),
  ]
  let fields = case room_key {
    Some(room_key) -> list.append(base, [#("e2ee_key", json.string(room_key))])
    None -> base
  }
  json.object(fields) |> raw_json.from_json
}

fn build(
  dm: Dm,
  channel: ChannelId,
  conn: ConnectionId,
  update: DmUpdate,
  keys: List(String),
  capable: Bool,
) -> Entry {
  entry.new(
    user: user(dm),
    guild: None,
    channel:,
    connection_id: conn,
    session_id: Some(session_text(dm)),
    flags: entry.Flags(..update.flags, suppress: False),
    mute: False,
    deaf: False,
  )
  |> entry.set_viewer_stream_keys(keys)
  |> entry.set_e2ee_capable(capable)
}

fn keep(dm: Dm, channel: ChannelId, value: Entry) -> Dm {
  let states = case entry.key(value) {
    Ok(key) -> store.put(dm.states, key, value)
    Error(Nil) -> dm.states
  }
  let dm = Dm(..dm, states:)
  count_table.upsert(value)
  broadcast(dm, Some(channel), value)
  dm
}

fn existing_connection(
  dm: Dm,
  channel: ChannelId,
  conn: ConnectionId,
  update: DmUpdate,
) -> Result(Dm, GatewayError) {
  use #(dm, #(current_key, current)) <- result.try(known_connection(
    dm,
    channel,
    conn,
  ))
  case voice_key.user(current_key) == user(dm) {
    False -> Error(error.VoiceUserMismatch)
    True -> {
      let moving = !entry.in_channel(current, channel)
      let view = case moving || update.viewer_keys != KeepKeys {
        True -> call_view(channel)
        False -> None
      }
      let states = known_states(dm, view)
      use keys <- result.try(resolve_keys(
        update.viewer_keys,
        channel,
        states,
        entry.viewer_stream_keys(current),
      ))
      case moving {
        False -> Ok(update_in_place(dm, channel, current, update, keys))
        True -> {
          use Nil <- result.try(e2ee_check(states, channel, update))
          move_connection(dm, channel, current_key, current, update, keys, view)
        }
      }
    }
  }
}

fn known_connection(
  dm: Dm,
  channel: ChannelId,
  conn: ConnectionId,
) -> Result(#(Dm, #(VoiceKey, Entry)), GatewayError) {
  case store.by_connection(dm.states, conn) {
    Ok(current) -> Ok(#(dm, current))
    Error(Nil) ->
      case call_view(channel) {
        None -> Error(error.VoiceConnectionNotFound)
        Some(view) -> {
          let dm = adopt_view(dm, view)
          store.by_connection(dm.states, conn)
          |> result.map(fn(current) { #(dm, current) })
          |> result.replace_error(error.VoiceConnectionNotFound)
        }
      }
  }
}

fn refreshed(
  dm: Dm,
  channel: ChannelId,
  current: Entry,
  update: DmUpdate,
  keys: List(String),
) -> Entry {
  let session_id = case entry.session_id(current) {
    None -> Some(session_text(dm))
    other -> other
  }
  current
  |> entry.with_session(session_id)
  |> entry.set_channel(Some(channel))
  |> entry.set_flags(entry.Flags(..update.flags, suppress: False))
  |> entry.set_flag(voice_state.Mute, False)
  |> entry.set_flag(voice_state.Deaf, False)
  |> entry.set_viewer_stream_keys(keys)
  |> entry.bump
}

fn update_in_place(
  dm: Dm,
  channel: ChannelId,
  current: Entry,
  update: DmUpdate,
  keys: List(String),
) -> Dm {
  let updated =
    refreshed(dm, channel, current, update, keys)
    |> entry.set_p2p(entry.is_p2p(current) && !p2p.declined(update.p2p))
  let dm = keep(dm, channel, updated)
  sync_call(dm, channel, updated)
  dm
}

fn move_connection(
  dm: Dm,
  channel: ChannelId,
  current_key: VoiceKey,
  current: Entry,
  update: DmUpdate,
  keys: List(String),
  view: Option(call_state.View),
) -> Result(Dm, GatewayError) {
  let decision = join_decision(dm, update, view)
  case decision {
    p2p.Reject(reason) -> Error(reason)
    _ -> {
      let moved = refreshed(dm, channel, current, update, keys)
      admit(
        dm,
        channel,
        update,
        decision,
        region_for_rpc(view),
        Some(#(current_key, current)),
        fn(granted, capable) {
          case granted == voice_key.connection(current_key) {
            True -> entry.set_e2ee_capable(moved, capable)
            False -> build(dm, channel, granted, update, keys, capable)
          }
        },
      )
    }
  }
}

type CallOp {
  JoinCall(JoinAttempt)
  LeaveCall(channel: ChannelId)
  DropCall(channel: ChannelId, connection: ConnectionId)
  SyncCall(user: UserId, channel: ChannelId, value: Entry)
  RebindCall(
    channel: ChannelId,
    session_id: String,
    member: call_state.Member,
    connections: List(Entry),
  )
  CallExited(channel: ChannelId, call: Pid, exit: Exit)
  CheckLegacyCalls
  LegacyView(
    key: Int,
    call: manager.CallRef,
    snapshot: Dict(ConnectionId, Bool),
    result: Result(call_state.View, call_server.AskError),
  )
  OwnerGone
}

type Exit {
  Ended(dropped: Set(ConnectionId))
  HandedOff
  Vanished
}

type Worker {
  Worker(
    session: SessionRef,
    dm: Subject(Message),
    ops: Subject(CallOp),
    selector: process.Selector(CallOp),
    legacy: Dict(Int, LegacyCall),
    checking: Bool,
    joined: Dict(Pid, Joined),
  )
}

type Joined {
  Joined(channel: ChannelId, connections: Set(ConnectionId))
}

type LegacyCall {
  LegacyCall(
    channel: ChannelId,
    call: manager.CallRef,
    held: Dict(ConnectionId, Bool),
    fetching: Bool,
  )
}

fn start_call_worker(
  session: SessionRef,
  dm: Subject(Message),
) -> Result(Subject(CallOp), Nil) {
  let owner = process.self()
  let handshake = process.new_subject()
  process.spawn_unlinked(fn() {
    let ops = process.new_subject()
    let monitor = process.monitor(owner)
    process.send(handshake, ops)
    let selector =
      process.new_selector()
      |> process.select(ops)
      |> process.select_specific_monitor(monitor, fn(_down) { OwnerGone })
    call_worker(Worker(
      session:,
      dm:,
      ops:,
      selector:,
      legacy: dict.new(),
      checking: False,
      joined: dict.new(),
    ))
  })
  process.receive(handshake, handshake_timeout_ms())
}

fn call_worker(worker: Worker) -> Nil {
  case process.selector_receive_forever(worker.selector) {
    OwnerGone -> Nil
    op -> call_worker(run_op(worker, op))
  }
}

fn run_op(worker: Worker, op: CallOp) -> Worker {
  let session = worker.session
  case op {
    OwnerGone -> worker
    JoinCall(attempt) ->
      case join_loop(attempt, join_retries) {
        Ok(#(call, pid)) -> {
          process.send(worker.dm, CallJoined(attempt.channel, pid))
          worker
          |> record(attempt.channel, pid, [entry.connection_id(attempt.value)])
          |> track_legacy(attempt.channel, call, [attempt.value])
        }
        Error(Nil) -> worker
      }
    CallExited(channel, call, exit) -> settle(worker, channel, call, exit)
    LeaveCall(channel) -> {
      control.call_unmonitor(session, channel)
      case lookup_call(channel, join_retries) {
        Ok(call) -> call_server.leave(call, key(session))
        Error(Nil) -> Nil
      }
      Worker(
        ..worker,
        legacy: dict.delete(worker.legacy, id.channel_id_to_int(channel)),
        joined: dict.filter(worker.joined, fn(_call, found) {
          found.channel != channel
        }),
      )
    }
    DropCall(channel, conn) -> {
      case lookup_call(channel, join_retries) {
        Ok(call) -> call_server.drop_connection(call, key(session), conn)
        Error(Nil) -> Nil
      }
      let joined =
        dict.map_values(worker.joined, fn(_call, found) {
          Joined(..found, connections: set.delete(found.connections, conn))
        })
      release_legacy(Worker(..worker, joined:), channel, conn)
    }
    SyncCall(user, channel, value) -> {
      case lookup_call(channel, join_retries) {
        Ok(call) -> {
          let _ =
            call_server.update_voice_state(call, user, value, call_timeout_ms())
          Nil
        }
        Error(Nil) -> Nil
      }
      sync_legacy(worker, channel, value)
    }
    RebindCall(channel, session_id, member, connections) ->
      case lookup_call(channel, join_retries) {
        Ok(call) -> {
          call_server.rebind(call, session_id, member)
          case process.subject_owner(call) {
            Ok(pid) -> {
              control.call_monitor(session, channel, pid)
              process.send(worker.dm, CallJoined(channel, pid))
              worker
              |> record(
                channel,
                pid,
                list.map(connections, entry.connection_id),
              )
              |> track_legacy(channel, call, connections)
            }
            Error(Nil) -> worker
          }
        }
        Error(Nil) -> worker
      }
    CheckLegacyCalls -> check_legacy(Worker(..worker, checking: False))
    LegacyView(key, call, snapshot, result) ->
      legacy_viewed(worker, key, call, snapshot, result)
  }
}

fn record(
  worker: Worker,
  channel: ChannelId,
  call: Pid,
  connections: List(ConnectionId),
) -> Worker {
  let held = case dict.get(worker.joined, call) {
    Ok(found) -> found.connections
    Error(Nil) -> set.new()
  }
  let found =
    Joined(channel:, connections: set.union(held, set.from_list(connections)))
  Worker(..worker, joined: dict.insert(worker.joined, call, found))
}

fn settle(worker: Worker, channel: ChannelId, call: Pid, exit: Exit) -> Worker {
  let recorded = case dict.get(worker.joined, call) {
    Ok(found) -> found.connections
    Error(Nil) -> set.new()
  }
  let worker = Worker(..worker, joined: dict.delete(worker.joined, call))
  case exit {
    Ended(dropped) -> {
      evict(worker, channel, set.difference(recorded, dropped))
      worker
    }
    HandedOff ->
      case successor(channel, call) {
        Ok(#(next, pid)) -> rehome(worker, channel, next, pid, recorded)
        Error(Nil) -> worker
      }
    Vanished ->
      case successor(channel, call) {
        Ok(#(next, pid)) -> {
          let kept = still_held(next, recorded)
          evict(worker, channel, set.difference(recorded, kept))
          rehome(worker, channel, next, pid, kept)
        }
        Error(Nil) -> {
          evict(worker, channel, recorded)
          worker
        }
      }
  }
}

fn still_held(
  call: manager.CallRef,
  connections: Set(ConnectionId),
) -> Set(ConnectionId) {
  case call_server.get_state(call, call_timeout_ms()) {
    Ok(view) ->
      view.connections
      |> list.map(entry.connection_id)
      |> set.from_list
      |> set.intersection(connections)
    Error(Nil) -> connections
  }
}

fn successor(
  channel: ChannelId,
  gone: Pid,
) -> Result(#(manager.CallRef, Pid), Nil) {
  use call <- result.try(lookup_call(channel, join_retries))
  use pid <- result.try(process.subject_owner(call))
  use <- bool.guard(pid == gone, Error(Nil))
  Ok(#(call, pid))
}

fn rehome(
  worker: Worker,
  channel: ChannelId,
  call: manager.CallRef,
  pid: Pid,
  connections: Set(ConnectionId),
) -> Worker {
  process.send(worker.dm, CallJoined(channel, pid))
  worker
  |> record(channel, pid, set.to_list(connections))
  |> retarget_legacy(channel, call)
}

fn evict(
  worker: Worker,
  channel: ChannelId,
  connections: Set(ConnectionId),
) -> Nil {
  set.each(connections, fn(conn) {
    process.send(worker.dm, ForceDisconnect(channel, Some(conn)))
  })
}

fn is_legacy(call: manager.CallRef) -> Bool {
  case process.subject_owner(call) {
    Ok(pid) -> !wire.pid_speaks(pid, wire.move_forms_epoch)
    Error(Nil) -> False
  }
}

fn track_legacy(
  worker: Worker,
  channel: ChannelId,
  call: manager.CallRef,
  values: List(Entry),
) -> Worker {
  let key = id.channel_id_to_int(channel)
  case is_legacy(call) {
    False -> Worker(..worker, legacy: dict.delete(worker.legacy, key))
    True -> {
      let held = case dict.get(worker.legacy, key) {
        Ok(found) if found.call == call -> found.held
        _ -> dict.new()
      }
      let held =
        list.fold(values, held, fn(acc, value) {
          dict.insert(acc, entry.connection_id(value), entry.is_p2p(value))
        })
      let fetching = case dict.get(worker.legacy, key) {
        Ok(found) -> found.fetching
        Error(Nil) -> False
      }
      let legacy =
        dict.insert(
          worker.legacy,
          key,
          LegacyCall(channel:, call:, held:, fetching:),
        )
      schedule_legacy_check(Worker(..worker, legacy:))
    }
  }
}

fn retarget_legacy(
  worker: Worker,
  channel: ChannelId,
  call: manager.CallRef,
) -> Worker {
  let key = id.channel_id_to_int(channel)
  case dict.get(worker.legacy, key), is_legacy(call) {
    Ok(found), True ->
      Worker(
        ..worker,
        legacy: dict.insert(worker.legacy, key, LegacyCall(..found, call:)),
      )
    Ok(_), False -> Worker(..worker, legacy: dict.delete(worker.legacy, key))
    Error(Nil), _ -> worker
  }
}

fn release_legacy(
  worker: Worker,
  channel: ChannelId,
  conn: ConnectionId,
) -> Worker {
  let key = id.channel_id_to_int(channel)
  case dict.get(worker.legacy, key) {
    Ok(found) ->
      Worker(
        ..worker,
        legacy: dict.insert(
          worker.legacy,
          key,
          LegacyCall(..found, held: dict.delete(found.held, conn)),
        ),
      )
    Error(Nil) -> worker
  }
}

fn schedule_legacy_check(worker: Worker) -> Worker {
  case worker.checking || dict.is_empty(worker.legacy) {
    True -> worker
    False -> {
      process.send_after(
        worker.ops,
        legacy_check_interval_ms(),
        CheckLegacyCalls,
      )
      Worker(..worker, checking: True)
    }
  }
}

fn sync_legacy(worker: Worker, channel: ChannelId, value: Entry) -> Worker {
  let key = id.channel_id_to_int(channel)
  let conn = entry.connection_id(value)
  case dict.get(worker.legacy, key) {
    Ok(found) ->
      case dict.has_key(found.held, conn) {
        True ->
          Worker(
            ..worker,
            legacy: dict.insert(
              worker.legacy,
              key,
              LegacyCall(
                ..found,
                held: dict.insert(found.held, conn, entry.is_p2p(value)),
              ),
            ),
          )
        False -> worker
      }
    Error(Nil) -> worker
  }
}

fn check_legacy(worker: Worker) -> Worker {
  let legacy =
    dict.map_values(worker.legacy, fn(key, found) {
      case found.fetching {
        True -> found
        False -> {
          let ops = worker.ops
          let call = found.call
          let snapshot = found.held
          process.spawn_unlinked(fn() {
            let result = call_server.fetch_state(call, call_timeout_ms())
            process.send(ops, LegacyView(key:, call:, snapshot:, result:))
          })
          LegacyCall(..found, fetching: True)
        }
      }
    })
  schedule_legacy_check(Worker(..worker, legacy:))
}

fn legacy_viewed(
  worker: Worker,
  key: Int,
  call: manager.CallRef,
  snapshot: Dict(ConnectionId, Bool),
  result: Result(call_state.View, call_server.AskError),
) -> Worker {
  case dict.get(worker.legacy, key) {
    Ok(found) if found.call == call -> {
      let found = LegacyCall(..found, fetching: False)
      case result {
        Ok(view) -> {
          let #(notes, held) =
            legacy_notes(worker.session, found, snapshot, view)
          list.each(notes, fn(note) { process.send(worker.dm, noted(note)) })
          Worker(
            ..worker,
            legacy: dict.insert(worker.legacy, key, LegacyCall(..found, held:)),
          )
        }
        Error(call_server.CallGone) ->
          Worker(..worker, legacy: dict.delete(worker.legacy, key))
        Error(call_server.CallFailed) ->
          Worker(..worker, legacy: dict.insert(worker.legacy, key, found))
      }
    }
    Ok(found) ->
      Worker(
        ..worker,
        legacy: dict.insert(
          worker.legacy,
          key,
          LegacyCall(..found, fetching: False),
        ),
      )
    Error(Nil) -> worker
  }
}

fn legacy_notes(
  session: SessionRef,
  found: LegacyCall,
  snapshot: Dict(ConnectionId, Bool),
  view: call_state.View,
) -> #(List(call_state.MemberNote), Dict(ConnectionId, Bool)) {
  let seen =
    owned(session, view.connections)
    |> list.map(fn(value) { #(entry.connection_id(value), entry.is_p2p(value)) })
    |> dict.from_list
  dict.fold(found.held, #([], dict.new()), fn(acc, conn, was_p2p) {
    let #(notes, held) = acc
    case dict.get(snapshot, conn) == Ok(was_p2p), dict.get(seen, conn) {
      False, _ -> #(notes, dict.insert(held, conn, was_p2p))
      True, Error(Nil) -> #(
        [call_state.ForceDisconnect(found.channel, Some(conn)), ..notes],
        held,
      )
      True, Ok(False) if was_p2p -> #(
        [call_state.Converted(conn), ..notes],
        dict.insert(held, conn, False),
      )
      True, Ok(now_p2p) -> #(notes, dict.insert(held, conn, now_p2p))
    }
  })
}

fn lookup_call(
  channel: ChannelId,
  retries: Int,
) -> Result(manager.CallRef, Nil) {
  case manager.lookup(channel) {
    Ok(call) -> Ok(call)
    Error(manager.NotFound) -> Error(Nil)
    Error(manager.Unavailable) ->
      case retries > 1 {
        True -> {
          process.sleep(retry_sleep_ms())
          lookup_call(channel, retries - 1)
        }
        False -> Error(Nil)
      }
  }
}

type JoinAttempt {
  JoinAttempt(
    target: SessionRef,
    channel: ChannelId,
    value: Entry,
    session_id: String,
    member: call_state.Member,
    connection: Option(ConnectionId),
    max: Option(Int),
    server_update: Option(RawJson),
    geo: Geo,
  )
}

fn join_call(
  dm: Dm,
  channel: ChannelId,
  value: Entry,
  max: Option(Int),
  server_update: Option(RawJson),
  geo: Geo,
) -> Nil {
  let member = member(dm)
  let attempt =
    JoinAttempt(
      target: dm.session,
      channel:,
      value:,
      session_id: entry.session_id(value) |> option.unwrap(session_text(dm)),
      member:,
      connection: Some(entry.connection_id(value)),
      max:,
      server_update:,
      geo:,
    )
  process.send(dm.calls, JoinCall(attempt))
}

fn member(dm: Dm) -> call_state.Member {
  call_state.Member(
    user: user(dm),
    inbox: dm.session.inbox,
    monitor: None,
    owner: process.subject_owner(dm.self) |> option.from_result,
    owner_monitor: None,
  )
}

fn noted(note: call_state.MemberNote) -> Message {
  case note {
    call_state.ForceDisconnect(channel:, connection:) ->
      ForceDisconnect(channel, connection)
    call_state.Converted(connection:) -> Converted(connection)
  }
}

fn join_loop(
  attempt: JoinAttempt,
  retries: Int,
) -> Result(#(manager.CallRef, Pid), Nil) {
  case retries {
    0 -> {
      rollback(attempt)
      Error(Nil)
    }
    _ ->
      case manager.lookup(attempt.channel) {
        Ok(call) ->
          case
            call_server.join(
              call,
              attempt.member.user,
              attempt.value,
              attempt.session_id,
              attempt.member,
              attempt.connection,
              attempt.max,
              attempt.geo,
            )
          {
            Ok(Ok(Nil)) -> {
              option.map(attempt.server_update, fn(payload) {
                voice_metrics.server_updates(voice_metrics.CallScope, 1)
                session.send(
                  attempt.target,
                  session.Dispatch("VOICE_SERVER_UPDATE", payload),
                )
              })
              use pid <- result.map(process.subject_owner(call))
              control.call_monitor(attempt.target, attempt.channel, pid)
              #(call, pid)
            }
            Ok(Error(reason)) -> {
              case p2p.reports_rejection(reason) {
                True ->
                  control.send(attempt.target, control.VoiceRejected(reason))
                False -> Nil
              }
              rollback(attempt)
              Error(Nil)
            }
            Error(Nil) -> retry(attempt, retries)
          }
        Error(manager.NotFound) -> retry(announce(attempt), retries)
        Error(manager.Unavailable) -> retry(attempt, retries)
      }
  }
}

fn announce(attempt: JoinAttempt) -> JoinAttempt {
  case attempt.server_update {
    Some(payload) -> {
      voice_metrics.server_updates(voice_metrics.CallScope, 1)
      session.send(
        attempt.target,
        session.Dispatch("VOICE_SERVER_UPDATE", payload),
      )
      JoinAttempt(..attempt, server_update: None)
    }
    None -> attempt
  }
}

fn retry(
  attempt: JoinAttempt,
  retries: Int,
) -> Result(#(manager.CallRef, Pid), Nil) {
  process.sleep(retry_sleep_ms())
  join_loop(attempt, retries - 1)
}

fn rollback(attempt: JoinAttempt) -> Nil {
  case entry.key(attempt.value) {
    Ok(key) -> count_table.remove(key)
    Error(Nil) -> Nil
  }
  call_state.force_disconnect(
    attempt.member,
    attempt.session_id,
    attempt.channel,
    Some(entry.connection_id(attempt.value)),
  )
}
