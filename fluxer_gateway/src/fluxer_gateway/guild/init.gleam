// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/guild.{type GuildRef} as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/core/member
import fluxer_gateway/guild/data.{type Loaded, Loaded}
import fluxer_gateway/guild/ets_owner
import fluxer_gateway/guild/inbox.{type Control}
import fluxer_gateway/guild/members
import fluxer_gateway/guild/presence/row as presence_row
import fluxer_gateway/guild/push/dispatch as push_dispatch
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/member_list
import fluxer_gateway/member_list/config as ml_config
import fluxer_gateway/member_list/snapshot as ml_snapshot
import fluxer_gateway/perms
import fluxer_gateway/perms/cutoff
import fluxer_gateway/perms/snapshot as perm_snapshot
import fluxer_gateway/threads/store.{type Handoff}
import fluxer_gateway/voice/guild_handler.{type GuildVoice, type VoiceRequest}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option
import gleam/result

pub type InitError {
  PermsLoadFailed
  MemberListLoadFailed
}

pub fn build(
  self_ref: GuildRef,
  control: Subject(Control),
  voice_inbox: Subject(VoiceRequest),
  loaded: Loaded,
  threads: option.Option(Handoff),
  voice: option.Option(GuildVoice),
  sessions: guild_sessions.Sessions,
) -> Result(GuildState, InitError) {
  prepare(self_ref, control, voice_inbox, loaded, voice, sessions)
  |> load(loaded.members)
  |> result.map(guild_threads.install(_, threads))
}

pub fn prepare(
  self_ref: GuildRef,
  control: Subject(Control),
  voice_inbox: Subject(VoiceRequest),
  loaded: Loaded,
  voice: option.Option(GuildVoice),
  sessions: guild_sessions.Sessions,
) -> GuildState {
  let guild = self_ref.guild
  let owner = process.self()
  let table = members.new(ets_owner.heir())
  members.put_many(table, loaded.members)
  let voice =
    option.lazy_unwrap(voice, fn() { guild_handler.new(loaded.data.facts) })
  let #(handle, reader) = perms.new(guild, owner)
  let #(ml_list, counts) =
    member_list.new(guild, ml_config.default(), owner, handle)
  guild_state.new(
    self_ref:,
    control:,
    data: loaded.data,
    members: table,
    perms: handle,
    reader:,
    list: ml_list,
    counts:,
    voice:,
    voice_inbox:,
    threads: store.new(),
    thread_inbox: process.new_subject(),
    push: push_info(push_dispatch.new(guild), loaded.data),
    sessions:,
  )
}

pub fn load(
  state: GuildState,
  rows: List(member.Member),
) -> Result(GuildState, InitError) {
  let loaded = Loaded(data: guild_state.guild_data(state), members: rows)
  use _ <- result.try(
    perms.load(
      guild_state.perms_handle(state),
      permission_snapshot(loaded)
        |> with_virtual(guild_handler.virtual_grants(guild_state.voice(state))),
    )
    |> result.replace_error(PermsLoadFailed),
  )
  use _ <- result.map(
    member_list.load(
      guild_state.member_list(state),
      list_snapshot(loaded, guild_state.sessions(state)),
    )
    |> result.replace_error(MemberListLoadFailed),
  )
  state
}

pub fn push_info(
  push: push_dispatch.GuildPush,
  guild_data: data.GuildData,
) -> push_dispatch.GuildPush {
  push_dispatch.set_info(
    push,
    name: data.decoded_property(guild_data, "name", decode.string),
    default_message_notifications: data.decoded_property(
      guild_data,
      "default_message_notifications",
      decode.int,
    )
      |> option.unwrap(0),
    disabled_operations: data.decoded_property(
      guild_data,
      "disabled_operations",
      decode.int,
    )
      |> option.unwrap(0),
    channels: guild_data.channels,
  )
}

fn permission_snapshot(loaded: Loaded) -> perm_snapshot.Snapshot {
  let guild_data = loaded.data
  let nsfw = data.decoded_property(guild_data, "nsfw", decode.bool)
  let level =
    data.decoded_property(guild_data, "nsfw_level", decode.int)
    |> option.unwrap(0)
  let base =
    perm_snapshot.new(core_guild.id(guild_data.facts))
    |> perm_snapshot.set_owner(core_guild.owner(guild_data.facts))
    |> perm_snapshot.set_roles(guild_data.roles)
    |> perm_snapshot.set_nsfw(nsfw, level)
    |> perm_snapshot.set_cutoff(
      data.decoded_property(
        guild_data,
        "message_history_cutoff",
        cutoff.decoder(),
      )
      |> option.unwrap(cutoff.Missing),
    )
  let with_channels =
    list.fold(guild_data.channels, base, fn(acc, row) {
      case channel.is_thread_kind(channel.kind(row)) {
        True -> acc
        False -> perm_snapshot.add_channel(acc, row)
      }
    })
  list.fold(loaded.members, with_channels, perm_snapshot.add_member)
}

pub fn list_snapshot(
  loaded: Loaded,
  sessions: guild_sessions.Sessions,
) -> ml_snapshot.Snapshot {
  let with_members =
    list.fold(loaded.members, ml_snapshot.new(), ml_snapshot.add_member)
  let with_presences =
    list.fold(
      guild_sessions.presence_rows(sessions),
      with_members,
      fn(acc, pair) {
        case id.user_id(pair.0) {
          Ok(user) ->
            ml_snapshot.add_presence(
              acc,
              presence_row.to_presence(pair.1, user),
            )
          Error(_) -> acc
        }
      },
    )
  guild_sessions.connected_list(sessions)
  |> list.fold(with_presences, fn(acc, user) {
    case id.user_id(user) {
      Ok(found) -> ml_snapshot.add_connected(acc, found)
      Error(_) -> acc
    }
  })
  |> ml_snapshot.set_updates(data.updates_enabled(loaded.data))
}

pub fn reload_snapshot(
  guild_data: data.GuildData,
  table: members.MembersTable,
  virtual: List(#(id.UserId, List(id.ChannelId))),
) -> perm_snapshot.Snapshot {
  permission_snapshot(data.Loaded(
    data: guild_data,
    members: members.to_list(table),
  ))
  |> with_virtual(virtual)
}

fn with_virtual(
  snapshot: perm_snapshot.Snapshot,
  virtual: List(#(id.UserId, List(id.ChannelId))),
) -> perm_snapshot.Snapshot {
  list.fold(virtual, snapshot, fn(acc, pair) {
    perm_snapshot.add_virtual_access(acc, pair.0, pair.1)
  })
}
