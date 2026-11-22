// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/guild_create.{type GuildCreate} as created
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/role
import fluxer_gateway/core/session.{type IdentifyFlags, type SessionInbox}
import fluxer_gateway/core/voice_state.{type VoiceState}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import fluxer_gateway/threads/payload as thread_payload
import fluxer_gateway/threads/value as thread_value
import fluxer_gateway/threads/view as thread_view
import fluxer_gateway/threads/viewers as thread_viewers
import fluxer_gateway/voice/guild_handler
import gleam/bit_array
import gleam/dict
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

type MemberWire {
  MemberWire(user: UserId, wire: RawJson)
}

pub fn build(
  state: GuildState,
  user: UserId,
  flags: IdentifyFlags,
  bot: Bool,
) -> #(GuildState, GuildCreate) {
  let viewer = thread_viewers.is_viewer_session(flags, bot)
  let #(state, head) = head(state, !viewer)
  let found = guild_state.member(state, user)
  let viewable = case found {
    Some(_) ->
      perms.viewable(guild_state.perms_reader(state), user)
      |> option.from_result
      |> option.unwrap([])
    None -> []
  }
  let #(state, channels) = channels(state, user, viewer, viewable)
  let visible = set.from_list(list.map(viewable, id.channel_id_to_int))
  let voice_states =
    guild_handler.voice_states(guild_state.voice(state))
    |> list.filter(fn(entry) { in_channels(entry, visible) })
  let own = case found {
    Some(owner) -> [MemberWire(member.user_id(owner), member.wire(owner))]
    None -> []
  }
  let members = merge_members(own, voice_members(state, voice_states))
  let after =
    [
      #("member_count", json.int(guild_state.member_count(state))),
      #("online_count", json.int(guild_state.online_count(state))),
      #("presences", json.preprocessed_array([])),
      #(
        "voice_states",
        json.preprocessed_array(
          list.map(voice_states, guild_handler.voice_state_json(
            guild_state.voice(state),
            _,
          )),
        ),
      ),
      #("joined_at", joined_at(found)),
      ..thread_fields(state, user, viewer, bot)
    ]
    |> list.map(fn(field) {
      raw_json.concat([
        raw_json.wrap(<<",">>),
        raw_json.from_json(json.string(field.0)),
        raw_json.wrap(<<":">>),
        raw_json.from_json(field.1),
      ])
    })
  #(
    state,
    created.available(
      guild_state.guild_id(state),
      before_members: raw_json.concat([
        raw_json.wrap(head),
        raw_json.wrap(<<",\"channels\":">>),
        raw_json.array(channels),
      ]),
      members: list.map(members, fn(item) { item.wire }),
      after_members: raw_json.concat(
        list.append(after, [raw_json.wrap(<<"}">>)]),
      ),
    ),
  )
}

pub fn unavailable(state: GuildState) -> GuildCreate {
  let facts = guild_state.facts(state)
  created.Unavailable(
    guild: core_guild.id(facts),
    hidden: core_guild.has_feature(facts, "UNAVAILABLE_HIDDEN")
      && is_unavailable_mode(facts),
  )
}

fn is_unavailable_mode(facts: core_guild.GuildFacts) -> Bool {
  core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE")
  || core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF")
}

pub fn sync_message(state: GuildState, payload: GuildCreate) -> SessionInbox {
  session.guild_dispatch(
    guild_state.guild_id(state),
    "GUILD_SYNC",
    created.to_raw(payload),
  )
}

fn head(state: GuildState, masked: Bool) -> #(GuildState, BitArray) {
  let guild_data = guild_state.guild_data(state)
  let key =
    table.HeadKey(
      properties: guild_data.properties,
      roles: guild_data.roles,
      emojis: guild_data.emojis,
      stickers: guild_data.stickers,
      masked:,
    )
  let sessions = guild_state.sessions(state)
  case table.head(sessions, key) {
    Some(bytes) -> #(state, bytes)
    None -> {
      let encoded =
        json.object([
          #("id", id.json(id.guild_id_to_int(guild_state.guild_id(state)))),
          #("properties", data.properties_json(guild_data, [])),
          #("roles", roles_json(guild_data, masked)),
          #("emojis", raw_json.to_json(guild_data.emojis)),
          #("stickers", raw_json.to_json(guild_data.stickers)),
        ])
        |> json.to_string
        |> bit_array.from_string
      let bytes = case
        bit_array.slice(encoded, 0, bit_array.byte_size(encoded) - 1)
      {
        Ok(open) -> open
        Error(Nil) -> <<"{">>
      }
      let sessions = table.set_head(sessions, key, bytes)
      #(guild_state.set_sessions(state, sessions), bytes)
    }
  }
}

fn roles_json(guild_data: data.GuildData, masked: Bool) -> Json {
  case masked {
    False -> data.roles_json(guild_data)
    True ->
      json.array(guild_data.roles, fn(row) {
        thread_value.expand(role.raw(row))
        |> thread_value.as_object
        |> thread_payload.mask_role
        |> thread_value.to_json
      })
  }
}

fn thread_fields(
  state: GuildState,
  user: UserId,
  viewer: Bool,
  bot: Bool,
) -> List(#(String, Json)) {
  case viewer {
    True -> [
      #(
        "threads",
        json.preprocessed_array(thread_view.guild_threads(
          guild_state.threads(state),
          guild_state.thread_context(state),
          user,
          bot,
        )),
      ),
    ]
    False -> []
  }
}

fn channels(
  state: GuildState,
  user: UserId,
  viewer: Bool,
  viewable: List(ChannelId),
) -> #(GuildState, List(RawJson)) {
  let guild_data = guild_state.guild_data(state)
  let by_id =
    list.fold(guild_data.channels, dict.new(), fn(acc, row) {
      dict.insert(acc, id.channel_id_to_int(channel.id(row)), row)
    })
  let rows =
    list.filter_map(viewable, fn(channel_id) {
      dict.get(by_id, id.channel_id_to_int(channel_id))
    })
  case viewer {
    True -> #(state, list.map(rows, data.channel_wire(guild_data, _)))
    False -> masked_channels(state, user, rows)
  }
}

fn masked_channels(
  state: GuildState,
  user: UserId,
  rows: List(channel.Channel),
) -> #(GuildState, List(RawJson)) {
  let guild_data = guild_state.guild_data(state)
  let sessions = guild_state.sessions(state)
  let #(wires, fresh) =
    thread_view.visible_for_non_viewer(
      guild_state.thread_context(state),
      user,
      rows,
    )
    |> list.fold(#([], []), fn(acc, row) {
      let #(wires, fresh) = acc
      let key = id.channel_id_to_int(channel.id(row))
      let source = data.channel_wire(guild_data, row)
      case table.masked_channel(sessions, key, source) {
        Some(masked) -> #([masked, ..wires], fresh)
        None -> {
          let masked = thread_view.mask_channel(source)
          #([masked, ..wires], [#(key, source, masked), ..fresh])
        }
      }
    })
  let state = case fresh {
    [] -> state
    _ ->
      guild_state.set_sessions(
        state,
        table.set_masked_channels(
          sessions,
          fresh,
          live: list.length(guild_data.channels),
        ),
      )
  }
  #(state, list.reverse(wires))
}

fn in_channels(entry: VoiceState, visible: set.Set(Int)) -> Bool {
  voice_state.channel(entry)
  |> option.map(fn(channel_id) {
    set.contains(visible, id.channel_id_to_int(channel_id))
  })
  |> option.unwrap(False)
}

fn voice_members(
  state: GuildState,
  states: List(VoiceState),
) -> List(MemberWire) {
  list.filter_map(states, fn(entry) {
    let user = voice_state.user(entry)
    voice_state.member(entry)
    |> option.lazy_or(fn() {
      guild_state.member(state, user) |> option.map(member.wire)
    })
    |> option.map(MemberWire(user, _))
    |> option.to_result(Nil)
  })
}

fn merge_members(
  primary: List(MemberWire),
  secondary: List(MemberWire),
) -> List(MemberWire) {
  let #(merged, _) =
    list.fold(list.append(primary, secondary), #([], set.new()), fn(acc, item) {
      let #(kept, seen) = acc
      let key = id.user_id_to_int(item.user)
      case set.contains(seen, key) {
        True -> acc
        False -> #([item, ..kept], set.insert(seen, key))
      }
    })
  list.reverse(merged)
}

fn joined_at(found: Option(member.Member)) -> Json {
  case found {
    Some(owner) ->
      case raw_json.field(member.wire(owner), "joined_at") {
        Ok(value) -> raw_json.to_json(value)
        Error(Nil) -> json.null()
      }
    None -> json.null()
  }
}
