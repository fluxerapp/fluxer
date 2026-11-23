// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/wire
import fluxer_gateway/config/durations
import fluxer_gateway/core/id.{type ChannelId, type MessageId, type UserId}
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionInbox}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/guild/remote
import fluxer_gateway/telemetry/process_info
import fluxer_gateway/voice/e2ee
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/registry
import fluxer_gateway/voice/store.{type Store}
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Pid, type Subject, type Timer}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type Caller {
  Caller(id: UserId, name: String, avatar: Option(String))
}

pub type Init {
  Init(
    channel: ChannelId,
    message: MessageId,
    region: Option(String),
    ringing: List(UserId),
    recipients: List(UserId),
    caller: Option(Caller),
  )
}

pub type Member {
  Member(
    user: UserId,
    inbox: Subject(SessionInbox),
    monitor: Option(Monitor),
    owner: Option(Pid),
    owner_monitor: Option(Monitor),
  )
}

const epoch_four_member_size = 8

@external(erlang, "erlang", "tuple_size")
fn member_size(member: Member) -> Int

@external(erlang, "erlang", "append_element")
fn append_disconnect(
  member: Member,
  notify: fn(ChannelId, Option(ConnectionId)) -> ChannelId,
) -> Member

@external(erlang, "erlang", "append_element")
fn append_converted(member: Member, notify: fn(ConnectionId) -> Bool) -> Member

@external(erlang, "erlang", "max")
fn ignore_disconnect(
  channel: ChannelId,
  connection: Option(ConnectionId),
) -> ChannelId

@external(erlang, "erlang", "is_atom")
fn ignore_converted(connection: ConnectionId) -> Bool

pub fn from_wire(member: Member) -> Member {
  case member_size(member) == epoch_four_member_size {
    True ->
      Member(
        user: member.user,
        inbox: member.inbox,
        monitor: member.monitor,
        owner: member.owner,
        owner_monitor: member.owner_monitor,
      )
    False -> member
  }
}

pub fn for_peer(member: Member, speaks_moves: Bool) -> Member {
  case speaks_moves {
    True -> member
    False ->
      member
      |> append_disconnect(ignore_disconnect)
      |> append_converted(ignore_converted)
  }
}

pub type MemberNote {
  ForceDisconnect(channel: ChannelId, connection: Option(ConnectionId))
  Converted(connection: ConnectionId)
}

pub fn notes(owner: Pid) -> Subject(MemberNote) {
  process.unsafely_create_subject(
    owner,
    atom.to_dynamic(atom.create("fluxer_gateway_call_member_note")),
  )
}

pub fn force_disconnect(
  member: Member,
  session_id: String,
  channel: ChannelId,
  connection: Option(ConnectionId),
) -> Nil {
  note(member, session_id, ForceDisconnect(channel, connection))
}

pub fn converted(
  member: Member,
  session_id: String,
  connection: ConnectionId,
) -> Nil {
  note(member, session_id, Converted(connection))
}

fn note(member: Member, session_id: String, found: MemberNote) -> Nil {
  case member.owner {
    Some(owner) ->
      case wire.pid_speaks(owner, wire.move_forms_epoch) {
        True -> process.send(notes(owner), found)
        False -> {
          process.spawn_unlinked(fn() {
            deliver_registered(owner, session_id, found)
          })
          Nil
        }
      }
    None -> Nil
  }
}

@external(erlang, "erlang", "node")
fn pid_node(pid: Pid) -> Atom

fn deliver_registered(
  owner: Pid,
  session_id: String,
  found: MemberNote,
) -> Nil {
  let rows =
    remote.call(
      node.from_atom(pid_node(owner)),
      "ets",
      "lookup",
      [remote.arg(registry.dm_sessions_table()), remote.arg(session_id)],
      durations.voice().rpc_call_timeout_ms,
      expecting: decode.list(decode.at([1], subject_decoder())),
    )
  case rows {
    Ok([subject]) ->
      case process.subject_owner(subject) == Ok(owner) {
        True -> process.send(subject, found)
        False -> Nil
      }
    _ -> Nil
  }
}

fn subject_decoder() -> Decoder(Subject(MemberNote)) {
  use tag <- decode.field(0, atom.decoder())
  use owner <- decode.field(1, process_info.pid_decoder())
  use reference <- decode.field(2, decode.dynamic)
  case atom.to_string(tag) {
    "subject" ->
      decode.success(process.unsafely_create_subject(owner, reference))
    _ ->
      decode.failure(
        process.unsafely_create_subject(owner, reference),
        "Subject",
      )
  }
}

pub type Slot {
  Slot(entry: Entry, seq: Int)
}

pub type PendingJoin {
  PendingJoin(user: UserId, session_id: String, joined_at: Int)
}

pub type Call {
  Call(
    channel: ChannelId,
    message: MessageId,
    region: Option(String),
    ringing: List(UserId),
    pending_ringing: List(UserId),
    recipients: List(UserId),
    caller: Option(Caller),
    voice_states: Store(Slot),
    next_seq: Int,
    sessions: Dict(String, Member),
    detached: Dict(String, Int),
    pending: Dict(ConnectionId, PendingJoin),
    initiator_ready: Bool,
    ring_timers: Dict(Int, Timer),
    idle_timer: Option(Timer),
    created_at: Int,
    history: Set(UserId),
    last_event: Option(EventKey),
    room_key: Option(String),
  )
}

pub opaque type EventKey {
  EventKey(
    region: Option(String),
    ringing: List(UserId),
    voice_states: List(entry.BroadcastView),
  )
}

pub type View {
  View(
    channel: ChannelId,
    message: MessageId,
    region: Option(String),
    ringing: List(UserId),
    recipients: List(UserId),
    voice_states: List(Entry),
    connections: List(Entry),
    created_at: Int,
  )
}

pub fn new(init: Init, now: Int) -> Call {
  Call(
    channel: init.channel,
    message: init.message,
    region: init.region,
    ringing: [],
    pending_ringing: init.ringing,
    recipients: init.recipients,
    caller: init.caller,
    voice_states: store.new(),
    next_seq: 0,
    sessions: dict.new(),
    detached: dict.new(),
    pending: dict.new(),
    initiator_ready: False,
    ring_timers: dict.new(),
    idle_timer: None,
    created_at: now,
    history: set.new(),
    last_event: None,
    room_key: None,
  )
}

pub fn has_connection(call: Call, conn: ConnectionId) -> Bool {
  store.has_connection(call.voice_states, conn)
}

pub fn put_caller(call: Call, caller: Option(Caller)) -> Call {
  case caller {
    Some(Caller(name: name, ..)) if name != "" -> Call(..call, caller:)
    _ -> call
  }
}

fn sorted_states(call: Call) -> List(Entry) {
  call.voice_states
  |> store.to_list
  |> list.sort(fn(a, b) { voice_key.compare(a.0, b.0) })
  |> list.map(fn(pair) { { pair.1 }.entry })
}

pub fn room_key(call: Call) -> #(String, Call) {
  case call.room_key {
    Some(key) -> #(key, call)
    None -> {
      let key = e2ee.new_key()
      #(key, Call(..call, room_key: Some(key)))
    }
  }
}

pub fn session_held(call: Call, pending: PendingJoin) -> Bool {
  case dict.get(call.sessions, pending.session_id) {
    Ok(member) if member.user == pending.user ->
      option.is_some(member.monitor)
      || dict.has_key(call.detached, pending.session_id)
    _ -> False
  }
}

pub fn detach(call: Call, session_id: String, at: Int) -> Call {
  case dict.get(call.sessions, session_id) {
    Ok(member) ->
      Call(
        ..call,
        sessions: dict.insert(
          call.sessions,
          session_id,
          Member(..member, monitor: None),
        ),
        detached: dict.insert(call.detached, session_id, at),
      )
    Error(Nil) -> call
  }
}

pub fn rebind(
  call: Call,
  session_id: String,
  member: Member,
) -> Result(#(Member, Call), Nil) {
  case dict.get(call.sessions, session_id) {
    Ok(existing) if existing.user == member.user ->
      Ok(#(
        existing,
        Call(
          ..call,
          sessions: dict.insert(call.sessions, session_id, member),
          detached: dict.delete(call.detached, session_id),
        ),
      ))
    _ -> Error(Nil)
  }
}

pub fn detach_expired(call: Call, session_id: String, at: Int) -> Bool {
  dict.get(call.detached, session_id) == Ok(at)
  && dict.has_key(call.sessions, session_id)
}

pub fn entries(call: Call) -> List(Entry) {
  call.voice_states
  |> store.values
  |> list.map(fn(slot) { slot.entry })
}

pub fn keyed_entries(call: Call) -> List(#(VoiceKey, Entry)) {
  call.voice_states
  |> store.to_list
  |> list.map(fn(pair) { #(pair.0, { pair.1 }.entry) })
}

pub fn entry_at(
  call: Call,
  conn: ConnectionId,
) -> Result(#(VoiceKey, Entry), Nil) {
  store.by_connection(call.voice_states, conn)
  |> result.map(fn(pair) { #(pair.0, { pair.1 }.entry) })
}

pub fn session_entries(
  call: Call,
  session_id: String,
) -> List(#(VoiceKey, Entry)) {
  keyed_entries(call)
  |> list.filter(fn(pair) { entry.session_id(pair.1) == Some(session_id) })
}

pub fn has_session_entries(call: Call, session_id: String) -> Bool {
  session_entries(call, session_id) != []
}

pub fn connected_users(call: Call) -> List(UserId) {
  store.users(call.voice_states)
}

pub fn has_user(call: Call, user: UserId) -> Bool {
  store.has_user(call.voice_states, user)
}

pub fn put_entry(call: Call, value: Entry) -> Call {
  case entry.key(value) {
    Error(Nil) -> call
    Ok(key) ->
      Call(
        ..call,
        voice_states: store.put(
          call.voice_states,
          key,
          Slot(value, call.next_seq),
        ),
        next_seq: call.next_seq + 1,
      )
  }
}

pub fn replace_entry(call: Call, value: Entry) -> Call {
  case entry.key(value) {
    Error(Nil) -> call
    Ok(key) ->
      case store.by_connection(call.voice_states, voice_key.connection(key)) {
        Ok(#(_, slot)) ->
          Call(
            ..call,
            voice_states: store.put(
              call.voice_states,
              key,
              Slot(..slot, entry: value),
            ),
          )
        Error(Nil) -> call
      }
  }
}

pub fn drop_entries(call: Call, keys: List(VoiceKey)) -> Call {
  Call(..call, voice_states: store.drop(call.voice_states, keys))
}

pub fn drop_sessions(call: Call, session_ids: List(String)) -> Call {
  Call(
    ..call,
    voice_states: store.filter(call.voice_states, fn(_key, slot) {
      case entry.session_id(slot.entry) {
        Some(session_id) -> !list.contains(session_ids, session_id)
        None -> True
      }
    }),
  )
}

pub fn view(call: Call) -> View {
  View(
    channel: call.channel,
    message: call.message,
    region: call.region,
    ringing: call.ringing,
    recipients: call.recipients,
    voice_states: sorted_states(call),
    connections: entries(call),
    created_at: call.created_at,
  )
}

fn ids(users: List(UserId)) -> Json {
  json.array(users, fn(user) { id.user_json(user) })
}

pub fn view_json(view: View) -> Json {
  json.object([
    #("channel_id", id.channel_json(view.channel)),
    #("message_id", id.message_json(view.message)),
    #("region", json.nullable(view.region, json.string)),
    #("ringing", ids(view.ringing)),
    #("recipients", ids(view.recipients)),
    #(
      "voice_states",
      json.array(view.voice_states, entry.to_json(_, entry.External)),
    ),
    #("created_at", json.int(view.created_at)),
  ])
}

pub fn event_states(call: Call) -> List(Entry) {
  sorted_states(call)
}

pub fn event_key(call: Call, states: List(Entry)) -> EventKey {
  EventKey(
    region: call.region,
    ringing: call.ringing,
    voice_states: list.map(states, entry.broadcast_view),
  )
}

pub fn event(call: Call) -> RawJson {
  event_of(call, sorted_states(call))
}

pub fn event_of(call: Call, states: List(Entry)) -> RawJson {
  broadcast_event(call.channel, call.message, call.region, call.ringing, states)
}

pub fn view_event(view: View) -> RawJson {
  broadcast_event(
    view.channel,
    view.message,
    view.region,
    view.ringing,
    view.voice_states,
  )
}

fn broadcast_event(
  channel: ChannelId,
  message: MessageId,
  region: Option(String),
  ringing: List(UserId),
  states: List(Entry),
) -> RawJson {
  json.object([
    #("channel_id", id.channel_json(channel)),
    #("message_id", id.message_json(message)),
    #("region", json.nullable(region, json.string)),
    #("ringing", ids(ringing)),
    #("voice_states", json.array(states, entry.to_json(_, entry.Broadcast))),
  ])
  |> raw_json.from_json
}

pub fn remove_users_from_ringing(call: Call, users: List(UserId)) -> Call {
  Call(
    ..call,
    ringing: list.filter(call.ringing, fn(user) { !list.contains(users, user) }),
    pending_ringing: list.filter(call.pending_ringing, fn(user) {
      !list.contains(users, user)
    }),
  )
}

pub fn unique_users(users: List(UserId)) -> List(UserId) {
  users
  |> list.unique
  |> list.sort(id.compare_user_id)
}
