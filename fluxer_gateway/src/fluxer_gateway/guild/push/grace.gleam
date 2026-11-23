// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/push/state.{type GuildMessage, ReleasedHolds}
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
  let live = await_recheck(monitored, recheck_at)
  let released = released_holds(guild, live)
  let holding =
    live
    |> list.filter(fn(item) {
      !set.contains(released, session.session_id_to_string(item.session.id))
    })
    |> list.map(fn(item) { item.user })
  let recipients = grace_recipients(users, holding, already_offline)
  case recipients {
    [] -> Nil
    _ ->
      server.handle_message_create(message.with_recipients(create, recipients))
  }
}

fn await_recheck(
  monitored: List(#(Monitor, Held)),
  recheck_at: Int,
) -> List(Held) {
  let remaining = int.max(0, recheck_at - clock.monotonic_ms())
  case monitored {
    [] -> []
    _ -> {
      let selector =
        process.new_selector()
        |> process.select_monitors(fn(down) { down })
      case process.selector_receive(selector, remaining) {
        Ok(process.ProcessDown(monitor:, ..)) ->
          await_recheck(
            list.filter(monitored, fn(entry) { entry.0 != monitor }),
            recheck_at,
          )
        Ok(_) -> await_recheck(monitored, recheck_at)
        Error(Nil) -> list.map(monitored, fn(entry) { entry.1 })
      }
    }
  }
}

fn released_holds(
  guild: Subject(GuildMessage),
  live: List(Held),
) -> set.Set(String) {
  case live {
    [] -> set.new()
    _ -> {
      let reply = process.new_subject()
      let ids =
        list.map(live, fn(item) {
          session.session_id_to_string(item.session.id)
        })
      process.send(guild, ReleasedHolds(ids, reply))
      process.receive(reply, hold_call_timeout_ms())
      |> result.map(set.from_list)
      |> result.unwrap(set.new())
    }
  }
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
