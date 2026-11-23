// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/push/state.{
  type GuildMessage, type Holds, HeldSessions, Holds,
}
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/push/message.{type MessageCreate, MessageCreate}
import fluxer_gateway/push/server
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/int
import gleam/list
import gleam/result
import gleam/set

fn recheck_ms() -> Int {
  durations.guild().grace_recheck_ms
}

fn hold_call_timeout_ms() -> Int {
  durations.guild().grace_hold_call_timeout_ms
}

type Held {
  Held(user: Int, session: SessionRef)
}

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Atom) -> Atom

pub fn hold(
  create: MessageCreate,
  held: List(#(Int, List(SessionRef))),
  guild: Subject(GuildMessage),
) -> Nil {
  case held {
    [] -> Nil
    _ -> {
      let recheck_at = clock.monotonic_ms() + recheck_ms()
      let users = list.map(held, fn(entry) { entry.0 })
      let user_set = set.from_list(users)
      let held_create =
        MessageCreate(
          ..create,
          user_roles: dict.filter(create.user_roles, fn(user, _) {
            set.contains(user_set, user)
          }),
          connected: set.intersection(create.connected, user_set),
        )
      let _ =
        process.spawn_unlinked(fn() {
          deliver_after_grace(held_create, held, users, recheck_at, guild)
        })
      Nil
    }
  }
}

fn deliver_after_grace(
  create: MessageCreate,
  held: List(#(Int, List(SessionRef))),
  users: List(Int),
  recheck_at: Int,
  guild: Subject(GuildMessage),
) -> Nil {
  let _ = process_flag(atom.create("priority"), atom.create("low"))
  let already_offline = offline(users)
  let monitored =
    list.flat_map(held, fn(entry) {
      list.filter_map(entry.1, fn(ref) {
        process.subject_owner(ref.inbox)
        |> result.map(fn(pid) { #(process.monitor(pid), Held(entry.0, ref)) })
      })
    })
  let #(alive, gone) = await_recheck(monitored, [], recheck_at)
  let holding = case
    held_sessions(current_inbox(create.guild, guild), alive, gone)
  {
    Ok(Holds(present:, released:)) -> {
      let present = set.from_list(present)
      let released = set.from_list(released)
      list.append(
        list.filter(alive, fn(item) { !set.contains(released, key(item)) }),
        list.filter(gone, fn(item) {
          set.contains(present, key(item)) && !set.contains(released, key(item))
        }),
      )
    }
    Error(Nil) -> alive
  }
  let recipients =
    grace_recipients(
      users,
      list.map(holding, fn(item) { item.user }),
      already_offline,
    )
  case recipients {
    [] -> Nil
    _ ->
      server.handle_message_create(message.with_recipients(create, recipients))
  }
}

fn key(item: Held) -> String {
  session.session_id_to_string(item.session.id)
}

fn await_recheck(
  monitored: List(#(Monitor, Held)),
  gone: List(Held),
  recheck_at: Int,
) -> #(List(Held), List(Held)) {
  let remaining = int.max(0, recheck_at - clock.monotonic_ms())
  case monitored {
    [] -> #([], gone)
    _ -> {
      let selector =
        process.new_selector()
        |> process.select_monitors(fn(down) { down })
      case process.selector_receive(selector, remaining) {
        Ok(process.ProcessDown(monitor:, ..)) -> {
          let #(down, up) =
            list.partition(monitored, fn(entry) { entry.0 == monitor })
          await_recheck(
            up,
            list.append(list.map(down, fn(entry) { entry.1 }), gone),
            recheck_at,
          )
        }
        Ok(_) -> await_recheck(monitored, gone, recheck_at)
        Error(Nil) -> #(list.map(monitored, fn(entry) { entry.1 }), gone)
      }
    }
  }
}

fn current_inbox(
  guild: Int,
  captured: Subject(GuildMessage),
) -> Subject(GuildMessage) {
  case
    id.guild_id(guild)
    |> result.replace_error(Nil)
    |> result.try(registry.lookup)
  {
    Ok(handle) -> state.inbox_of(handle.pid)
    Error(Nil) -> captured
  }
}

fn held_sessions(
  guild: Subject(GuildMessage),
  alive: List(Held),
  gone: List(Held),
) -> Result(Holds, Nil) {
  use owner <- result.try(process.subject_owner(guild))
  let monitor = process.monitor(owner)
  let reply = process.new_subject()
  let ids = list.map(list.append(alive, gone), key)
  process.send(guild, HeldSessions(ids, reply))
  let answer =
    process.new_selector()
    |> process.select_map(reply, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
    |> process.selector_receive(hold_call_timeout_ms())
  process.demonitor_process(monitor)
  result.flatten(answer)
}

fn offline(users: List(Int)) -> List(Int) {
  users
  |> list.sort(int.compare)
  |> packed_ids.from_list
  |> presence_client.lookup_many
  |> list.filter_map(fn(found) {
    case presence.status(found) {
      presence.Offline -> Ok(id.user_id_to_int(presence.user(found)))
      _ -> Error(Nil)
    }
  })
}

fn grace_recipients(
  held: List(Int),
  live: List(Int),
  already_offline: List(Int),
) -> List(Int) {
  let live_set = set.from_list(live)
  let gone = list.filter(held, fn(user) { !set.contains(live_set, user) })
  let newly_offline = offline_among(live, already_offline)
  list.append(gone, newly_offline)
  |> list.unique
  |> list.sort(int.compare)
}

fn offline_among(live: List(Int), already_offline: List(Int)) -> List(Int) {
  let before = set.from_list(already_offline)
  offline(list.unique(live))
  |> list.filter(fn(user) { !set.contains(before, user) })
}
