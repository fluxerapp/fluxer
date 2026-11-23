// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/init
import fluxer_gateway/guild/members
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/member_list
import fluxer_gateway/member_list/effects
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/perms
import fluxer_gateway/perms/event.{type Effects} as perm_event
import fluxer_gateway/voice/guild_handler
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import logging

pub fn apply_perms(
  state: GuildState,
  name: String,
  events: List(perm_event.Event),
) -> #(GuildState, Option(Effects)) {
  case events {
    [] -> #(state, None)
    _ ->
      case perms.apply(guild_state.perms_handle(state), events) {
        Ok(effects) -> #(state, Some(effects))
        Error(_) -> {
          logging.log(
            logging.Error,
            "guild permission apply failed for event "
              <> name
              <> ", rebuilding from guild data",
          )
          #(rebuild_perms(state), None)
        }
      }
  }
}

pub fn apply_member_list(
  state: GuildState,
  events: List(list_event.Event),
  delta: Option(perms.Delta),
) -> #(GuildState, List(effects.Dispatch)) {
  case events, delta {
    [], None -> #(state, [])
    _, _ ->
      case
        member_list.apply_timed(
          guild_state.member_list(state),
          events,
          delta,
          rows.lookup(state),
        )
      {
        Ok(#(dispatches, timer)) -> #(rows.list_timer(state, timer), dispatches)
        Error(_) -> {
          logging.log(
            logging.Error,
            "guild member list apply failed, rebuilding from guild data",
          )
          case rebuild_list(state, guild_state.perms_handle(state)) {
            Ok(rebuilt) -> rebuilt
            Error(Nil) -> #(guild_state.mark_unrepairable(state), [])
          }
        }
      }
  }
}

fn rebuild_perms(state: GuildState) -> GuildState {
  let #(fresh, reader) = perms.new(guild_state.guild_id(state), process.self())
  let snapshot =
    init.reload_snapshot(
      guild_state.guild_data(state),
      guild_state.members_table(state),
      guild_handler.virtual_grants(guild_state.voice(state)),
    )
  let rebuilt = case perms.load(fresh, snapshot) {
    Ok(_) -> rebuild_list(state, fresh)
    Error(_) -> Error(Nil)
  }
  case rebuilt {
    Ok(#(state, dispatches)) -> {
      case member_list_updates_enabled(state) {
        True -> send.list_dispatches(state, dispatches)
        False -> Nil
      }
      guild_state.replace_perms(state, fresh, reader)
    }
    Error(Nil) -> {
      logging.log(logging.Error, "guild permission rebuild failed")
      perms.release(fresh)
      guild_state.mark_unrepairable(state)
    }
  }
}

fn rebuild_list(
  state: GuildState,
  handle: perms.Perms,
) -> Result(#(GuildState, List(effects.Dispatch)), Nil) {
  let source =
    data.Loaded(
      data: guild_state.guild_data(state),
      members: members.to_list(guild_state.members_table(state)),
    )
  case
    member_list.rebuild(
      guild_state.member_list(state),
      handle,
      init.list_snapshot(source, guild_state.sessions(state)),
      rows.lookup(state),
    )
  {
    Ok(#(list, counts, dispatches)) ->
      Ok(#(guild_state.set_member_list(state, list, counts), dispatches))
    Error(_) -> Error(Nil)
  }
}

pub fn set_virtual_access(
  state: GuildState,
  user: UserId,
  channels: List(ChannelId),
) -> GuildState {
  case
    apply_perms(state, "VIRTUAL_ACCESS", [
      perm_event.virtual_access(user, channels),
    ])
  {
    #(state, None) -> state
    #(state, Some(found)) -> {
      let #(state, dispatches) =
        apply_member_list(state, [], Some(perms.delta(found)))
      case member_list_updates_enabled(state) {
        True -> send.list_dispatches(state, dispatches)
        False -> Nil
      }
      state
    }
  }
}

pub fn member_list_updates_enabled(state: GuildState) -> Bool {
  data.updates_enabled(guild_state.guild_data(state))
}
