// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type Unavailable}
import fluxer_gateway/core/guild.{type GuildRequest, MemberPresence}
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{type Presence, type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type SessionInbox, type SessionRef, SessionRef,
}
import fluxer_gateway/core/user
import fluxer_gateway/presence/client.{type PresenceLink, SessionConnect}
import fluxer_gateway/presence/handoff/signal as presence_signal
import fluxer_gateway/presence/pids
import fluxer_gateway/presence/protocol.{type Input}
import fluxer_gateway/presence/route
import fluxer_gateway/session/control.{type Control}
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None}
import gleam/result
import gleam/string
import support/wait

pub type Seen {
  Frame(SessionInbox)
  Notice(Control)
}

pub type FakeSession {
  FakeSession(ref: SessionRef, pid: Pid, seen: Subject(Seen))
}

type FakeInput {
  Inbound(SessionInbox)
  Controlled(Control)
}

@external(erlang, "erlang", "exit")
fn exit_pid(pid: Pid, reason: Dynamic) -> Bool

pub fn session_id(tag: Int) -> session.SessionId {
  let text = string.pad_start(int.to_string(tag), to: 32, with: "0")
  case session.session_id_from_string(text) {
    Ok(found) -> found
    Error(Nil) -> session.new_session_id()
  }
}

pub fn fake_session(user: Int, tag: Int) -> FakeSession {
  fake_session_with(user, tag, True)
}

pub fn fake_session_with(user: Int, tag: Int, terminates: Bool) -> FakeSession {
  spawn_fake(user, tag, terminates, False)
}

pub fn rebinding_session(user: Int, tag: Int) -> FakeSession {
  spawn_fake(user, tag, True, True)
}

fn spawn_fake(
  user: Int,
  tag: Int,
  terminates: Bool,
  rebinds: Bool,
) -> FakeSession {
  let seen = process.new_subject()
  let ready = process.new_subject()
  let assert Ok(user_id) = id.user_id(user)
  let id = session_id(tag)
  let pid =
    process.spawn_unlinked(fn() {
      let inbox = process.new_subject()
      let controls = control.subject(process.self())
      process.send(ready, inbox)
      let selector =
        process.new_selector()
        |> process.select_map(inbox, Inbound)
        |> process.select_map(controls, Controlled)
      fake_loop(selector, seen, Behaviour(terminates:, rebinds:, id:))
    })
  let assert Ok(inbox) = process.receive(ready, 1000)
  FakeSession(ref: SessionRef(id:, user: user_id, inbox:), pid:, seen:)
}

type Behaviour {
  Behaviour(terminates: Bool, rebinds: Bool, id: session.SessionId)
}

fn fake_loop(
  selector: process.Selector(FakeInput),
  seen: Subject(Seen),
  behaviour: Behaviour,
) -> Nil {
  case process.selector_receive_forever(selector) {
    Inbound(message) -> process.send(seen, Frame(message))
    Controlled(control.TerminateMatching(_, reply)) -> {
      process.send(reply, behaviour.terminates)
      process.send(seen, Notice(control.TerminateMatching([], reply)))
    }
    Controlled(control.PresenceRebind(_, stub) as message)
      if behaviour.rebinds
    -> {
      presence_signal.repointed(stub, behaviour.id)
      process.send(seen, Notice(message))
    }
    Controlled(message) -> process.send(seen, Notice(message))
  }
  fake_loop(selector, seen, behaviour)
}

@external(erlang, "erlang", "process_info")
fn monitored_by(pid: Pid, item: atom.Atom) -> #(atom.Atom, List(Pid))

fn watched_by_presence(fake: FakeSession) -> Bool {
  case
    presence_pid(id.user_id_to_int(fake.ref.user)),
    process.is_alive(fake.pid)
  {
    Ok(presence), True ->
      list.contains(
        monitored_by(fake.pid, atom.create("monitored_by")).1,
        presence,
      )
    _, _ -> True
  }
}

pub fn exit_session(fake: FakeSession, reason: Dynamic) -> Nil {
  let _ = wait_until(fn() { watched_by_presence(fake) }, 1000)
  let monitor = process.monitor(fake.pid)
  let _ = exit_pid(fake.pid, reason)
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
  let _ = process.selector_receive(selector, 1000)
  Nil
}

pub fn wire_for(user: Int) -> RawJson {
  raw_json.wrap(<<
    "{\"id\":\"":utf8,
    int.to_string(user):utf8,
    "\",\"username\":\"scenario\"}":utf8,
  >>)
}

pub fn connect_request(
  fake: FakeSession,
  status: Status,
  friends: List(Int),
  invisible: Bool,
  custom_status: Option(RawJson),
) -> client.SessionConnect {
  let user_id = fake.ref.user
  SessionConnect(
    session: fake.ref,
    status:,
    afk: False,
    mobile: False,
    custom_status:,
    settings_invisible: invisible,
    user: user.new(
      id: user_id,
      bot: False,
      flags: 0,
      restricted: False,
      wire: wire_for(id.user_id_to_int(user_id)),
    ),
    friends: packed_ids.from_list(list.sort(friends, int.compare)),
    group_dm_recipients: packed_ids.empty(),
    guilds: [],
  )
}

pub fn connect_with(
  request: client.SessionConnect,
) -> Result(PresenceLink, Unavailable) {
  let reply = process.new_subject()
  client.session_connect(request, reply)
  let assert Ok(result) = process.receive(reply, 15_000)
  result
}

pub fn connect(
  fake: FakeSession,
  status: Status,
  friends: List(Int),
) -> PresenceLink {
  let assert Ok(link) =
    connect_with(connect_request(fake, status, friends, False, None))
  link
}

pub fn presence_inbox(user: Int) -> Result(Subject(Input(Monitor)), Nil) {
  pids.lookup(user)
}

pub fn presence_pid(user: Int) -> Result(Pid, Nil) {
  case pids.lookup(user) {
    Ok(inbox) -> process.subject_owner(inbox)
    Error(Nil) -> Error(Nil)
  }
}

@external(erlang, "sys", "get_state")
fn get_state(pid: Pid) -> Dynamic

pub fn sync(user: Int) -> Nil {
  case presence_pid(user) {
    Ok(pid) ->
      case process.is_alive(pid) {
        True -> {
          let _ = get_state(pid)
          Nil
        }
        False -> Nil
      }
    Error(Nil) -> Nil
  }
}

pub fn wait_until(check: fn() -> Bool, timeout_ms: Int) -> Bool {
  wait.until(timeout_ms, check)
}

pub fn wait_down(pid: Pid, timeout_ms: Int) -> Bool {
  wait_until(fn() { !process.is_alive(pid) }, timeout_ms)
}

pub fn observe(guild: Int, members: List(Int)) -> Subject(GuildRequest) {
  let assert Ok(guild_id) = id.guild_id(guild)
  let inbox = process.new_subject()
  client.register_guild_interest(
    guild_id,
    packed_ids.from_list(list.sort(members, int.compare)),
    inbox,
  )
  list.each(members, sync_registry)
  inbox
}

pub fn stop_observing(guild: Int) -> Nil {
  let assert Ok(guild_id) = id.guild_id(guild)
  client.drop_guild_interest(guild_id)
}

pub fn next_presence(
  observer: Subject(GuildRequest),
  user: Int,
  timeout_ms: Int,
) -> Result(Presence, Nil) {
  case process.receive(observer, timeout_ms) {
    Ok(MemberPresence(found)) ->
      case id.user_id_to_int(presence.user(found)) == user {
        True -> Ok(found)
        False -> next_presence(observer, user, timeout_ms)
      }
    Ok(_) -> next_presence(observer, user, timeout_ms)
    Error(Nil) -> Error(Nil)
  }
}

pub fn presences(
  observer: Subject(GuildRequest),
  user: Int,
  quiet_ms: Int,
) -> List(Presence) {
  case next_presence(observer, user, quiet_ms) {
    Ok(found) -> [found, ..presences(observer, user, quiet_ms)]
    Error(Nil) -> []
  }
}

pub fn next_seen(fake: FakeSession, timeout_ms: Int) -> Result(Seen, Nil) {
  process.receive(fake.seen, timeout_ms)
}

pub fn drain_seen(fake: FakeSession, quiet_ms: Int) -> List(Seen) {
  case process.receive(fake.seen, quiet_ms) {
    Ok(found) -> [found, ..drain_seen(fake, quiet_ms)]
    Error(Nil) -> []
  }
}

pub fn frames_about(seen: List(Seen), user: Int) -> List(RawJson) {
  list.filter_map(seen, fn(found) {
    case found {
      Frame(session.PresenceFrame(about, payload, _)) ->
        case id.user_id_to_int(about) == user {
          True -> Ok(payload)
          False -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
}

pub fn dispatches(seen: List(Seen), name: String) -> List(RawJson) {
  list.filter_map(seen, fn(found) {
    case found {
      Frame(session.Dispatch(event, payload)) if event == name -> Ok(payload)
      _ -> Error(Nil)
    }
  })
}

pub fn wire_status(payload: RawJson) -> String {
  let text = bit_array.to_string(raw_json.to_bits(payload)) |> result.unwrap("")
  case string.split(text, "\"status\":\"") {
    [_, rest, ..] ->
      case string.split(rest, "\"") {
        [found, ..] -> found
        [] -> ""
      }
    _ -> ""
  }
}

pub fn bits_to_string(bits: BitArray) -> Result(String, Nil) {
  bit_array.to_string(bits)
}

pub fn wait_frames(
  fake: FakeSession,
  about: Int,
  timeout_ms: Int,
) -> List(RawJson) {
  case process.receive(fake.seen, timeout_ms) {
    Error(Nil) -> []
    Ok(found) ->
      case frames_about([found], about) {
        [] -> wait_frames(fake, about, timeout_ms)
        frames -> list.append(frames, frames_about(drain_seen(fake, 50), about))
      }
  }
}

pub fn closed_reason() -> Dynamic {
  atom.to_dynamic(atom.create("shutdown"))
}

pub fn guild_join(guild: Int) -> Control {
  let assert Ok(guild_id) = id.guild_id(guild)
  control.GuildJoin(guild_id)
}

pub fn user_updates(
  observer: Subject(GuildRequest),
  quiet_ms: Int,
) -> List(String) {
  case process.receive(observer, quiet_ms) {
    Ok(guild.MemberUserUpdate(wire)) -> [
      result.unwrap(bit_array.to_string(wire), ""),
      ..user_updates(observer, quiet_ms)
    ]
    Ok(_) -> user_updates(observer, quiet_ms)
    Error(Nil) -> []
  }
}

pub fn sync_registry(user: Int) -> Nil {
  sync_registry_shard(route.shard(user))
  list.each(route.shards(), sync_registry_shard)
  sync_registry_shard(route.shard(user))
}

fn sync_registry_shard(shard: Int) -> Nil {
  case process.named(route.name(route.Registry, shard)) {
    Ok(pid) -> {
      let _ = get_state(pid)
      Nil
    }
    Error(Nil) -> Nil
  }
}
