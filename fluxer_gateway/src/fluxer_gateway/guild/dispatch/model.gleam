// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/member_list
import fluxer_gateway/member_list/effects
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/perms
import fluxer_gateway/perms/event.{type Effects} as perm_event
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import logging

const disabled_member_list_updates = 64

pub fn apply_perms(
  state: GuildState,
  name: String,
  events: List(perm_event.Event),
) -> Option(Effects) {
  case events {
    [] -> None
    _ ->
      case perms.apply(guild_state.perms_handle(state), events) {
        Ok(effects) -> Some(effects)
        Error(_) -> {
          logging.log(
            logging.Error,
            "guild permission apply failed for event " <> name,
          )
          None
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
          logging.log(logging.Error, "guild member list apply failed")
          #(state, [])
        }
      }
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
    None -> state
    Some(found) -> {
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
  updates_enabled(guild_state.guild_data(state))
}

pub fn updates_enabled(guild_data: GuildData) -> Bool {
  !operation_disabled(guild_data, disabled_member_list_updates)
}

pub fn operation_disabled(guild_data: GuildData, bit: Int) -> Bool {
  let bits =
    data.decoded_property(guild_data, "disabled_operations", operations())
    |> option.unwrap(0)
  int.bitwise_and(bits, bit) == bit
}

fn operations() -> decode.Decoder(Int) {
  decode.one_of(decode.int, [
    decode.map(decode.string, fn(text) {
      id.parse_decimal(text) |> result.unwrap(0)
    }),
    decode.success(0),
  ])
}
