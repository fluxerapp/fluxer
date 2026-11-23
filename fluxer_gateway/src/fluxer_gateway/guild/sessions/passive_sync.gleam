// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/channel
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/core/voice_key.{type VoiceKey}
import fluxer_gateway/core/voice_state.{type VoiceState}
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/perms
import fluxer_gateway/voice/guild_handler
import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}

pub fn interval_ms() -> Int {
  durations.guild().passive_sync_interval_ms
}

const large_guild_members = 250

pub fn tick(state: GuildState) -> GuildState {
  case core_guild.member_count(guild_state.facts(state)) > large_guild_members {
    False -> state
    True -> {
      let facts = guild_state.facts(state)
      let gated =
        core_guild.is_unavailable(facts)
        || core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE")
        || core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF")
      let passive =
        table.entries(guild_state.sessions(state))
        |> list.filter(fn(found) {
          entry.is_passive(found)
          && {
            !gated || !guild_state.is_unavailable_for(state, entry.user(found))
          }
        })
      case passive {
        [] -> state
        _ -> {
          let messages = last_messages(guild_state.guild_data(state))
          let voice = guild_handler.voice_states(guild_state.voice(state))
          let #(state, _) =
            list.fold(passive, #(state, dict.new()), fn(acc, found) {
              let #(state, views) = acc
              let #(view, views) = view_of(state, entry.user(found), views)
              #(sync_session(state, found, view, messages, voice), views)
            })
          state
        }
      }
    }
  }
}

pub fn baseline(state: GuildState, user: UserId) -> Dict(Int, Int) {
  let #(view, _) = view_of(state, user, dict.new())
  dict.filter(last_messages(guild_state.guild_data(state)), fn(channel_int, _) {
    set.contains(view, channel_int)
  })
}

fn view_of(
  state: GuildState,
  user: UserId,
  views: Dict(Int, Set(Int)),
) -> #(Set(Int), Dict(Int, Set(Int))) {
  let key = id.user_id_to_int(user)
  case dict.get(views, key) {
    Ok(view) -> #(view, views)
    Error(Nil) -> {
      let view =
        perms.viewable(guild_state.perms_reader(state), user)
        |> result.map(fn(channels) {
          set.from_list(list.map(channels, id.channel_id_to_int))
        })
        |> result.unwrap(set.new())
      #(view, dict.insert(views, key, view))
    }
  }
}

pub fn last_messages(guild_data: GuildData) -> Dict(Int, Int) {
  list.fold(guild_data.channels, dict.new(), fn(acc, row) {
    case channel.last_message(row) {
      Some(message) ->
        dict.insert(acc, id.channel_id_to_int(channel.id(row)), message)
      None -> acc
    }
  })
}

fn sync_session(
  state: GuildState,
  found: Entry,
  view: Set(Int),
  messages: Dict(Int, Int),
  voice: List(VoiceState),
) -> GuildState {
  let current =
    dict.filter(messages, fn(channel_int, _) { set.contains(view, channel_int) })
    |> dict.merge(guild_threads.joined_last_messages(state, found))
  let previous = entry.seen_messages(found)
  let delta =
    dict.filter(current, fn(channel_int, message) {
      dict.get(previous, channel_int) != Ok(message)
    })
  let current_voice =
    voice
    |> list.filter(fn(item) {
      voice_state.channel(item)
      |> option.map(fn(channel_id) {
        set.contains(view, id.channel_id_to_int(channel_id))
      })
      |> option.unwrap(False)
    })
    |> list.filter_map(fn(item) {
      voice_state.key(item) |> result.map(fn(key) { #(key, item) })
    })
    |> dict.from_list
  let updates = voice_updates(state, current_voice, entry.seen_voice(found))
  let seen_messages = case dict.size(delta) > 0 || updates != [] {
    True -> {
      send(state, found, delta, updates)
      current
    }
    False ->
      dict.filter(previous, fn(channel_int, _) {
        dict.has_key(current, channel_int)
      })
  }
  store(
    state,
    entry.set_seen(found, messages: seen_messages, voice: current_voice),
  )
}

fn voice_updates(
  state: GuildState,
  current: Dict(VoiceKey, VoiceState),
  previous: Dict(VoiceKey, VoiceState),
) -> List(json.Json) {
  let voice = guild_state.voice(state)
  let updated =
    dict.to_list(current)
    |> list.filter(fn(pair) {
      case dict.get(previous, pair.0) {
        Ok(old) -> voice_state.version(old) != voice_state.version(pair.1)
        Error(Nil) -> True
      }
    })
    |> list.map(fn(pair) { guild_handler.voice_state_json(voice, pair.1) })
  let held =
    dict.keys(current) |> list.map(voice_key.connection) |> set.from_list
  let removed =
    dict.to_list(previous)
    |> list.filter(fn(pair) {
      !dict.has_key(current, pair.0)
      && !set.contains(held, voice_key.connection(pair.0))
    })
    |> list.map(fn(pair) {
      guild_handler.voice_state_json(
        voice,
        voice_state.set_channel(pair.1, None),
      )
    })
  list.append(updated, removed)
}

fn send(
  state: GuildState,
  found: Entry,
  delta: Dict(Int, Int),
  updates: List(json.Json),
) -> Nil {
  let guild = guild_state.guild_id(state)
  let channels =
    dict.to_list(delta)
    |> list.map(fn(pair) { #(id.write(pair.0), id.json(pair.1)) })
    |> json.object
  let fields = [
    #("guild_id", id.guild_json(guild)),
    #("channels", channels),
  ]
  let fields = case updates {
    [] -> fields
    _ ->
      list.append(fields, [#("voice_states", json.preprocessed_array(updates))])
  }
  relay.send(
    [entry.ref(found)],
    session.guild_dispatch(
      guild,
      "PASSIVE_UPDATES",
      raw_json.from_json(json.object(fields)),
    ),
  )
}

fn store(state: GuildState, found: Entry) -> GuildState {
  guild_state.set_sessions(state, table.put(guild_state.sessions(state), found))
}
