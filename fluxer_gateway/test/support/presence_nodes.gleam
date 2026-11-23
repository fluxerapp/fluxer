// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, PresenceDurations, Settings}
import fluxer_gateway/core/guild.{type GuildRequest, MemberPresence}
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Online}
import fluxer_gateway/core/session.{type SessionInbox}
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client.{type PresenceLink}
import fluxer_gateway/presence/pids
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/rpc
import fluxer_gateway/session/control.{type Control}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import support/presence_scenario as scn
import support/push_crash
import support/tick_trace

pub type Report {
  Report(node: String, terminated: Bool, connects: Int)
}

type Msg {
  Frame(SessionInbox)
  Ctl(Control)
  PresenceGone(process.Down)
  Retry
  Query(Subject(Report))
  Die(Dynamic)
  Detach(reply: Subject(Nil))
}

type Live {
  Live(
    fake: scn.FakeSession,
    self: Subject(Msg),
    link: Option(PresenceLink),
    watch: Option(Monitor),
    terminated: Bool,
    connects: Int,
  )
}

@external(erlang, "erlang", "exit")
fn exit_self(pid: process.Pid, reason: Dynamic) -> Bool

pub fn tune() -> Nil {
  let current = settings.get()
  let d = current.durations
  settings.store(
    Settings(
      ..current,
      durations: Durations(
        ..d,
        presence: PresenceDurations(
          ..d.presence,
          manager_topology_settle_ms: 100,
          manager_move_spread_ms: 1500,
          manager_check_interval_ms: 1000,
          route_rebuild_interval_ms: 100,
          server_handoff_timeout_ms: 1000,
          state_max_move_backoff_ms: 1000,
          registry_lost_owner_grace_ms: 500,
          registry_digest_interval_ms: 500,
          state_noconnection_grace_ms: 30_000,
        ),
      ),
    ),
  )
  Nil
}

fn name(user: Int, tag: Int) {
  route.local_name(
    "scn_live_" <> int.to_string(user) <> "_" <> int.to_string(tag),
  )
}

pub fn start_sessions(users: List(Int), tag: Int) -> Int {
  list.map(users, fn(user) {
    let ready = process.new_subject()
    let _ =
      process.spawn_unlinked(fn() {
        let _ = process.register(process.self(), name(user, tag))
        let self = process.named_subject(name(user, tag))
        let fake = own_fake(user, tag)
        let live =
          connect(Live(
            fake:,
            self:,
            link: None,
            watch: None,
            terminated: False,
            connects: 0,
          ))
        process.send(ready, option.is_some(live.link))
        loop(live)
      })
    process.receive(ready, 20_000) |> result.unwrap(False)
  })
  |> list.count(fn(ok) { ok })
}

fn own_fake(user: Int, tag: Int) -> scn.FakeSession {
  let inbox = process.new_subject()
  let assert Ok(user_id) = id.user_id(user)
  scn.FakeSession(
    ref: session.SessionRef(id: scn.session_id(tag), user: user_id, inbox:),
    pid: process.self(),
    seen: process.new_subject(),
  )
}

fn selector(live: Live) -> process.Selector(Msg) {
  process.new_selector()
  |> process.select(live.self)
  |> process.select_map(live.fake.ref.inbox, Frame)
  |> process.select_map(control.subject(process.self()), Ctl)
  |> process.select_monitors(PresenceGone)
}

fn connect(live: Live) -> Live {
  case live.terminated {
    True -> live
    False ->
      case
        scn.connect_with(scn.connect_request(live.fake, Online, [], False, None))
      {
        Ok(link) ->
          Live(
            ..live,
            link: Some(link),
            watch: client.link_pid(link)
              |> result.map(process.monitor)
              |> option.from_result,
            connects: live.connects + 1,
          )
        Error(_) -> {
          let _ = process.send_after(live.self, 100, Retry)
          Live(..live, link: None, watch: None)
        }
      }
  }
}

fn drop_link(live: Live) -> Live {
  case live.watch {
    Some(monitor) -> process.demonitor_process(monitor)
    None -> Nil
  }
  case live.link {
    Some(link) -> client.session_disconnect(link)
    None -> Nil
  }
  Live(..live, link: None, watch: None)
}

fn loop(live: Live) -> Nil {
  let live = case process.selector_receive_forever(selector(live)) {
    Frame(session.Terminate(_)) -> Live(..drop_link(live), terminated: True)
    Frame(_) -> live
    Ctl(control.PresenceMoved) -> connect(drop_link(live))
    Ctl(control.TerminateMatching(_, reply)) -> {
      process.send(reply, True)
      Live(..drop_link(live), terminated: True)
    }
    Ctl(_) -> live
    PresenceGone(process.ProcessDown(monitor:, ..)) ->
      case live.watch == Some(monitor) {
        True -> {
          let _ = process.send_after(live.self, 50, Retry)
          Live(..live, link: None, watch: None)
        }
        False -> live
      }
    PresenceGone(_) -> live
    Retry ->
      case live.link {
        None -> connect(live)
        Some(_) -> live
      }
    Query(reply) -> {
      let node = case live.link {
        Some(link) ->
          case client.link_pid(link) {
            Ok(pid) -> atom.to_string(pid_node(pid))
            Error(Nil) -> ""
          }
        None -> ""
      }
      process.send(
        reply,
        Report(node:, terminated: live.terminated, connects: live.connects),
      )
      live
    }
    Detach(reply) -> {
      case live.watch {
        Some(monitor) -> process.demonitor_process(monitor)
        None -> Nil
      }
      process.send(reply, Nil)
      Live(..live, watch: None, terminated: True)
    }
    Die(reason) -> {
      let _ = exit_self(process.self(), reason)
      live
    }
  }
  loop(live)
}

@external(erlang, "erlang", "node")
fn pid_node(pid: process.Pid) -> atom.Atom

pub fn report(user: Int, tag: Int) -> Report {
  case process.named(name(user, tag)) {
    Ok(_) -> {
      let reply = process.new_subject()
      process.send(process.named_subject(name(user, tag)), Query(reply))
      process.receive(reply, 2000)
      |> result.unwrap(Report(node: "", terminated: False, connects: 0))
    }
    Error(Nil) -> Report(node: "gone", terminated: False, connects: 0)
  }
}

pub fn session_nodes(users: List(Int), tag: Int) -> List(String) {
  list.map(users, fn(user) { report(user, tag).node })
}

pub fn exit_session(user: Int, tag: Int, reason: Dynamic) -> Nil {
  case process.named(name(user, tag)) {
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      process.send(process.named_subject(name(user, tag)), Die(reason))
      let _ =
        process.new_selector()
        |> process.select_specific_monitor(monitor, fn(_) { Nil })
        |> process.selector_receive(2000)
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn handoff_exit() -> Dynamic {
  protocol.handoff_exit()
}

pub fn hosted(users: List(Int)) -> List(Int) {
  list.filter(users, fn(user) {
    case pids.lookup(user) {
      Ok(inbox) ->
        case process.subject_owner(inbox) {
          Ok(pid) -> process.is_alive(pid)
          Error(Nil) -> False
        }
      Error(Nil) -> False
    }
  })
}

pub fn owners(users: List(Int)) -> List(String) {
  list.map(users, fn(user) {
    case route.owner(user) {
      Ok(node) -> atom.to_string(node)
      Error(_) -> ""
    }
  })
}

pub fn self_name() -> String {
  atom.to_string(remote.self_atom())
}

pub fn cached(users: List(Int)) -> List(Int) {
  cache.lookup_many(packed_ids.from_list(list.sort(users, int.compare)))
  |> list.map(fn(found) { id.user_id_to_int(presence.user(found)) })
  |> list.sort(int.compare)
}

pub fn terminate(user: Int, hashes: List(String)) -> Bool {
  rpc.handle(
    "presence.terminate_sessions",
    dynamic.properties([
      #(dynamic.string("user_id"), dynamic.string(int.to_string(user))),
      #(
        dynamic.string("session_id_hashes"),
        dynamic.list(list.map(hashes, dynamic.string)),
      ),
    ]),
  )
  |> result.is_ok
}

type Watch {
  WatchPresence(GuildRequest)
  WatchDo(Command)
}

pub type Command {
  Register(members: List(Int))
  Unregister(members: List(Int))
  DropAll
  Latest(reply: Subject(List(#(Int, String))))
}

fn observer_name(guild: Int) {
  route.local_name("scn_observer_" <> int.to_string(guild))
}

pub fn start_observer(guild: Int, members: List(Int)) -> Bool {
  let ready = process.new_subject()
  let _ =
    process.spawn_unlinked(fn() {
      let _ = process.register(process.self(), observer_name(guild))
      let commands = process.named_subject(observer_name(guild))
      let inbox = process.new_subject()
      let assert Ok(guild_id) = id.guild_id(guild)
      client.register_guild_interest(
        guild_id,
        packed_ids.from_list(list.sort(members, int.compare)),
        inbox,
      )
      process.send(ready, True)
      let selector =
        process.new_selector()
        |> process.select_map(inbox, WatchPresence)
        |> process.select_map(commands, WatchDo)
      watch_loop(selector, guild_id, inbox, dict.new())
    })
  process.receive(ready, 5000) |> result.unwrap(False)
}

fn watch_loop(selector, guild_id, inbox, latest: Dict(Int, String)) -> Nil {
  let latest = case process.selector_receive_forever(selector) {
    WatchPresence(MemberPresence(found)) ->
      dict.insert(
        latest,
        id.user_id_to_int(presence.user(found)),
        status_name(presence.status(found)),
      )
    WatchPresence(_) -> latest
    WatchDo(Register(members)) -> {
      client.register_guild_interest(
        guild_id,
        packed_ids.from_list(list.sort(members, int.compare)),
        inbox,
      )
      latest
    }
    WatchDo(Unregister(members)) -> {
      client.unregister_guild_interest(
        guild_id,
        packed_ids.from_list(list.sort(members, int.compare)),
      )
      latest
    }
    WatchDo(DropAll) -> {
      client.drop_guild_interest(guild_id)
      latest
    }
    WatchDo(Latest(reply)) -> {
      process.send(reply, dict.to_list(latest))
      latest
    }
  }
  watch_loop(selector, guild_id, inbox, latest)
}

fn status_name(status: presence.Status) -> String {
  case status {
    presence.Online -> "online"
    presence.Offline -> "offline"
    presence.Idle -> "idle"
    presence.Dnd -> "dnd"
    presence.Invisible -> "invisible"
  }
}

fn command(guild: Int, message: Command) -> Nil {
  process.send(process.named_subject(observer_name(guild)), message)
}

pub fn observer_register(guild: Int, members: List(Int)) -> Nil {
  command(guild, Register(members))
}

pub fn observer_unregister(guild: Int, members: List(Int)) -> Nil {
  command(guild, Unregister(members))
}

pub fn observer_drop(guild: Int) -> Nil {
  command(guild, DropAll)
}

pub fn observer_statuses(guild: Int, users: List(Int)) -> List(String) {
  let reply = process.new_subject()
  command(guild, Latest(reply))
  let latest = process.receive(reply, 2000) |> result.unwrap([])
  list.map(users, fn(user) { list.key_find(latest, user) |> result.unwrap("") })
}

pub fn interested(users: List(Int)) -> List(List(String)) {
  list.map(users, fn(user) {
    registry.interest(user) |> list.map(atom.to_string)
  })
}

pub fn subscribed(users: List(Int)) -> List(Int) {
  list.filter(users, fn(user) { registry.local_subscribers(user) != [] })
}

fn registry_name(user: Int) -> String {
  "fluxer_presence_registry_" <> int.to_string(route.shard(user))
}

pub fn freeze_registries(users: List(Int)) -> Bool {
  users
  |> list.map(registry_name)
  |> list.unique
  |> list.all(fn(name) {
    case tick_trace.whereis(name) {
      Ok(pid) -> {
        push_crash.suspend(pid)
        True
      }
      Error(Nil) -> False
    }
  })
}

pub fn thaw_registries(users: List(Int)) -> Bool {
  users
  |> list.map(registry_name)
  |> list.unique
  |> list.all(fn(name) {
    case tick_trace.whereis(name) {
      Ok(pid) -> {
        tick_trace.resume(pid)
        True
      }
      Error(Nil) -> False
    }
  })
}

pub fn closed_reason() -> Dynamic {
  scn.closed_reason()
}

pub fn kill_presence(user: Int, tag: Int) -> Bool {
  case process.named(name(user, tag)) {
    Ok(_) -> {
      let _ = process.call(process.named_subject(name(user, tag)), 2000, Detach)
      Nil
    }
    Error(Nil) -> Nil
  }
  case scn.presence_pid(user) {
    Ok(pid) -> {
      process.kill(pid)
      True
    }
    Error(Nil) -> False
  }
}
