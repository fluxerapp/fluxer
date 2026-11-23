// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/voice_key.{type VoiceKey}
import fluxer_gateway/core/voice_state.{type VoiceState}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/table.{type Table}
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/guild_state as voice_guild_state
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/option.{Some}

pub opaque type Remains {
  Remains(connections: List(VoiceKey), frozen: Bool)
}

type Row {
  Row(owner: Pid, remains: Remains)
}

fn rows() -> Table(Int, Row) {
  table.named(names.remains_table)
}

pub fn new() -> Remains {
  Remains(connections: [], frozen: False)
}

pub fn connections_of(states: List(VoiceState)) -> List(VoiceKey) {
  states
  |> list.filter_map(voice_state.key)
  |> list.sort(fn(a, b) {
    voice_key.compare_connection(
      voice_key.connection(a),
      voice_key.connection(b),
    )
  })
  |> list.fold([], fn(acc: List(VoiceKey), found: VoiceKey) {
    case acc {
      [previous, ..] ->
        case voice_key.connection(previous) == voice_key.connection(found) {
          True -> acc
          False -> [found, ..acc]
        }
      [] -> [found]
    }
  })
  |> list.reverse
}

pub fn with_connections(
  guild: GuildId,
  remains: Remains,
  connections: List(VoiceKey),
  frozen frozen: Bool,
) -> Remains {
  let next = Remains(connections:, frozen:)
  case next == remains {
    True -> remains
    False -> store(guild, next)
  }
}

fn store(guild: GuildId, remains: Remains) -> Remains {
  case remains {
    Remains(connections: [], ..) -> forget(guild, process.self())
    _ ->
      case table.exists(rows()) {
        True ->
          table.insert(
            rows(),
            id.guild_id_to_int(guild),
            Row(process.self(), remains),
          )
        False -> Nil
      }
  }
  remains
}

pub fn forget(guild: GuildId, owner: Pid) -> Nil {
  let _ = take(guild, owner)
  Nil
}

fn take(guild: GuildId, owner: Pid) -> Result(Remains, Nil) {
  let key = id.guild_id_to_int(guild)
  case table.lookup(rows(), key) {
    Ok(Row(owner: found, ..) as row) if found == owner ->
      case table.take_row(rows(), key, row) {
        True -> Ok(row.remains)
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

pub fn settle(guild: GuildId, owner: Pid) -> Nil {
  presence_client.drop_guild_interest_of(guild, owner)
  case held_frozen(guild, owner) {
    Ok(_) -> Nil
    Error(Nil) -> disconnect(guild, take(guild, owner))
  }
}

pub fn release(guild: GuildId, owner: Pid) -> Nil {
  case held_frozen(guild, owner) {
    Error(Nil) -> Nil
    Ok(row) -> {
      let thawed = Row(..row, remains: Remains(..row.remains, frozen: False))
      case table.swap_row(rows(), id.guild_id_to_int(guild), row, thawed) {
        False -> release(guild, owner)
        True ->
          case process.is_alive(owner) {
            True -> Nil
            False -> settle(guild, owner)
          }
      }
    }
  }
}

fn held_frozen(guild: GuildId, owner: Pid) -> Result(Row, Nil) {
  case table.lookup(rows(), id.guild_id_to_int(guild)) {
    Ok(Row(owner: found, remains: Remains(frozen: True, ..)) as row)
      if found == owner
    -> Ok(row)
    _ -> Error(Nil)
  }
}

fn disconnect(guild: GuildId, taken: Result(Remains, Nil)) -> Nil {
  case taken {
    Error(Nil) -> Nil
    Ok(remains) -> {
      list.each(remains.connections, count_table.remove)
      case remains.connections {
        [] -> Nil
        connections -> {
          process.spawn_unlinked(fn() {
            list.each(connections, fn(found) {
              let _ =
                voice_guild_state.force_disconnect(
                  Some(guild),
                  voice_key.channel(found),
                  voice_key.user(found),
                  voice_key.connection(found),
                )
              Nil
            })
          })
          Nil
        }
      }
    }
  }
}
