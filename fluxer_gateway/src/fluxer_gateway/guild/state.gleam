// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildFacts, type GuildRef} as core_guild
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/member.{type Member} as core_member
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/core/user as core_user
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/inbox.{type Control}
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/guild/presence/row as presence_row
import fluxer_gateway/guild/push/dispatch as push_dispatch
import fluxer_gateway/guild/sessions.{type Sessions} as guild_sessions
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/member_list as ml
import fluxer_gateway/perms
import fluxer_gateway/perms/cutoff
import fluxer_gateway/threads/context.{type Context, Context}
import fluxer_gateway/threads/message.{type ThreadMsg}
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/voice/guild_handler.{type GuildVoice, type VoiceRequest}
import gleam/erlang/process.{type Subject, type Timer}
import gleam/list
import gleam/option.{type Option, None, Some}

const small_guild_members = 250

pub type PermsRepair {
  Intact
  Rebuilt(stale: List(perms.Perms))
  Unrepairable
}

pub opaque type GuildState {
  GuildState(
    self_ref: GuildRef,
    control: Subject(Control),
    data: GuildData,
    members: MembersTable,
    perms: perms.Perms,
    reader: perms.Reader,
    list: ml.MemberList,
    counts: ml.CountsHandle,
    voice: GuildVoice,
    voice_inbox: Subject(VoiceRequest),
    threads: ThreadStore,
    thread_inbox: Subject(ThreadMsg),
    push: push_dispatch.GuildPush,
    sessions: Sessions,
    version: Int,
    public_online: Option(Int),
    list_flush: Option(Timer),
    guild_id_json: raw_json.RawJson,
    repair: PermsRepair,
  )
}

pub fn new(
  self_ref self_ref: GuildRef,
  control control: Subject(Control),
  data data: GuildData,
  members members: MembersTable,
  perms perms: perms.Perms,
  reader reader: perms.Reader,
  list list: ml.MemberList,
  counts counts: ml.CountsHandle,
  voice voice: GuildVoice,
  voice_inbox voice_inbox: Subject(VoiceRequest),
  threads threads: ThreadStore,
  thread_inbox thread_inbox: Subject(ThreadMsg),
  push push: push_dispatch.GuildPush,
  sessions sessions: Sessions,
) -> GuildState {
  GuildState(
    self_ref:,
    control:,
    data:,
    members:,
    perms:,
    reader:,
    list:,
    counts:,
    voice:,
    voice_inbox:,
    threads:,
    thread_inbox:,
    push:,
    sessions:,
    version: 1,
    public_online: None,
    list_flush: None,
    guild_id_json: raw_json.from_json(
      id.json(id.guild_id_to_int(core_guild.id(data.facts))),
    ),
    repair: Intact,
  )
}

pub fn facts(state: GuildState) -> GuildFacts {
  state.data.facts
}

pub fn member(state: GuildState, user: UserId) -> Option(Member) {
  members.get(state.members, user)
}

pub fn members_table(state: GuildState) -> MembersTable {
  state.members
}

pub fn set_members_table(
  state: GuildState,
  members: MembersTable,
) -> GuildState {
  GuildState(..state, members:)
}

pub fn perms_reader(state: GuildState) -> perms.Reader {
  state.reader
}

pub fn member_list(state: GuildState) -> ml.MemberList {
  state.list
}

pub fn sessions(state: GuildState) -> Sessions {
  state.sessions
}

pub fn set_sessions(state: GuildState, sessions: Sessions) -> GuildState {
  GuildState(..state, sessions:)
}

pub fn self_ref(state: GuildState) -> GuildRef {
  state.self_ref
}

pub fn guild_data(state: GuildState) -> GuildData {
  state.data
}

pub fn set_guild_data(state: GuildState, next: GuildData) -> GuildState {
  GuildState(..state, data: next, version: state.version + 1)
}

pub fn replace_guild_data(state: GuildState, next: GuildData) -> GuildState {
  GuildState(..state, data: next)
}

pub fn control(state: GuildState) -> Subject(Control) {
  state.control
}

pub fn perms_handle(state: GuildState) -> perms.Perms {
  state.perms
}

pub fn set_perms(
  state: GuildState,
  perms: perms.Perms,
  reader: perms.Reader,
) -> GuildState {
  GuildState(
    ..state,
    perms:,
    reader:,
    sessions: guild_sessions.clear_groups(state.sessions),
  )
}

pub fn replace_perms(
  state: GuildState,
  perms: perms.Perms,
  reader: perms.Reader,
) -> GuildState {
  let repair = case state.repair {
    Intact -> Rebuilt([state.perms])
    Rebuilt(stale) -> Rebuilt([state.perms, ..stale])
    Unrepairable -> Unrepairable
  }
  GuildState(
    ..set_perms(state, perms, reader),
    repair:,
    version: state.version + 1,
  )
}

pub fn mark_unrepairable(state: GuildState) -> GuildState {
  GuildState(..state, repair: Unrepairable)
}

pub fn repair(state: GuildState) -> PermsRepair {
  state.repair
}

pub fn take_repair(state: GuildState) -> #(GuildState, PermsRepair) {
  case state.repair {
    Unrepairable -> #(state, Unrepairable)
    found -> #(GuildState(..state, repair: Intact), found)
  }
}

pub fn set_member_list(
  state: GuildState,
  list: ml.MemberList,
  counts: ml.CountsHandle,
) -> GuildState {
  GuildState(..state, list:, counts:)
}

pub fn list_flush(state: GuildState) -> Option(Timer) {
  state.list_flush
}

pub fn set_list_flush(state: GuildState, timer: Option(Timer)) -> GuildState {
  GuildState(..state, list_flush: timer)
}

pub fn counts_handle(state: GuildState) -> ml.CountsHandle {
  state.counts
}

pub fn member_count(state: GuildState) -> Int {
  core_guild.member_count(state.data.facts)
}

pub fn online_count(state: GuildState) -> Int {
  ml.counts(state.counts).1
}

pub fn public_online(state: GuildState) -> Int {
  case state.public_online {
    Some(count) -> count
    None -> online_count(state)
  }
}

pub fn set_public_online(state: GuildState, count: Int) -> GuildState {
  GuildState(..state, public_online: Some(count))
}

pub fn voice(state: GuildState) -> GuildVoice {
  state.voice
}

pub fn set_voice(state: GuildState, voice: GuildVoice) -> GuildState {
  GuildState(..state, voice:)
}

pub fn voice_context(
  state: GuildState,
  now_ms: Int,
) -> guild_handler.VoiceContext {
  let table = state.members
  let guild_data = state.data
  let sessions = state.sessions
  guild_handler.VoiceContext(
    facts: state.data.facts,
    reader: state.reader,
    member: fn(user) { members.get(table, user) },
    channel: fn(id) { data.find_channel(guild_data, id) },
    sessions_of: fn(user) {
      guild_sessions.entries(sessions)
      |> list.filter(fn(found) { entry.user(found) == user })
      |> list.map(fn(found) { session.session_id_to_string(entry.id(found)) })
    },
    self_voice: state.voice_inbox,
    now_ms:,
  )
}

pub fn threads(state: GuildState) -> ThreadStore {
  state.threads
}

pub fn set_threads(state: GuildState, threads: ThreadStore) -> GuildState {
  GuildState(..state, threads:)
}

pub fn voice_inbox(state: GuildState) -> Subject(VoiceRequest) {
  state.voice_inbox
}

pub fn thread_inbox(state: GuildState) -> Subject(ThreadMsg) {
  state.thread_inbox
}

pub fn thread_context(state: GuildState) -> Context {
  let table = state.members
  let guild_data = state.data
  let sessions = state.sessions
  Context(
    guild: core_guild.id(guild_data.facts),
    reader: state.reader,
    member: fn(user) { members.get(table, user) },
    channel: fn(channel_id) { data.find_channel(guild_data, channel_id) },
    channels: guild_data.channels,
    passive: fn(key) {
      guild_sessions.get_key(sessions, key)
      |> option.map(entry.is_passive)
      |> option.unwrap(False)
    },
    small_guild: is_small_guild(state),
    presence: fn(user) {
      let key = id.user_id_to_int(user)
      case guild_sessions.is_connected(sessions, key) {
        False -> None
        True ->
          guild_sessions.presence_row(sessions, key)
          |> option.then(fn(found) {
            case presence_row.is_online(found) {
              True -> Some(raw_json.from_json(presence_row.to_json(found)))
              False -> None
            }
          })
      }
    },
    unavailable_for: fn(user) { is_unavailable_for(state, user) },
    history_cutoff_ms: history_cutoff_ms(guild_data),
    self: state.thread_inbox,
  )
}

fn history_cutoff_ms(guild_data: GuildData) -> Option(Int) {
  use raw <- option.then(data.property(guild_data, "message_history_cutoff"))
  case raw_json.read(raw, cutoff.decoder()) {
    Ok(cutoff.At(ms)) -> Some(ms)
    _ -> None
  }
}

pub fn is_small_guild(state: GuildState) -> Bool {
  core_guild.member_count(state.data.facts) <= small_guild_members
}

pub fn is_unavailable_for(state: GuildState, user: UserId) -> Bool {
  let facts = state.data.facts
  case
    core_guild.is_unavailable(facts),
    core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE"),
    core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF")
  {
    True, _, _ -> True
    _, True, _ -> True
    _, _, True -> !is_staff(state, user)
    _, _, _ -> False
  }
}

pub fn is_staff(state: GuildState, user: UserId) -> Bool {
  let from_sessions =
    guild_sessions.entries(state.sessions)
    |> list.find_map(fn(found) {
      case entry.user(found) == user, entry.staff(found) {
        True, Some(staff) -> Ok(staff)
        _, _ -> Error(Nil)
      }
    })
  case from_sessions {
    Ok(staff) -> staff
    Error(Nil) ->
      case member(state, user) {
        Some(found) -> core_user.is_staff(core_member.user(found))
        None -> False
      }
  }
}

pub fn push(state: GuildState) -> push_dispatch.GuildPush {
  state.push
}

pub fn set_push(
  state: GuildState,
  push: push_dispatch.GuildPush,
) -> GuildState {
  GuildState(..state, push:)
}

pub fn push_inputs(state: GuildState) -> push_dispatch.PushInputs {
  push_dispatch.PushInputs(
    facts: state.data.facts,
    reader: state.reader,
    model: perms.model(state.reader),
    list: state.list,
    members: state.members,
    roles: state.data.roles,
    sessions: guild_sessions.session_refs(state.sessions),
    self_ref: state.self_ref,
  )
}

pub fn version(state: GuildState) -> Int {
  state.version
}

pub fn guild_id(state: GuildState) -> id.GuildId {
  core_guild.id(state.data.facts)
}

pub fn guild_id_json(state: GuildState) -> raw_json.RawJson {
  state.guild_id_json
}
