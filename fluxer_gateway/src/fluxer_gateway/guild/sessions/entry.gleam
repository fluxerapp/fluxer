// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId, user_id_to_int}
import fluxer_gateway/core/session.{
  type IdentifyFlags, type SessionId, type SessionRef,
}
import fluxer_gateway/core/voice_key.{type VoiceKey}
import fluxer_gateway/core/voice_state.{type VoiceState}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/option.{type Option, None}

pub type Mode {
  Active
  Passive
}

pub opaque type Entry {
  Entry(
    ref: SessionRef,
    pid: Pid,
    monitor: Monitor,
    flags: IdentifyFlags,
    bot: Bool,
    staff: Option(Bool),
    mode: Mode,
    synced: Bool,
    typing: Option(Bool),
    seen_messages: Dict(Int, Int),
    seen_voice: Dict(VoiceKey, VoiceState),
    dm_partners: List(Int),
    dm_eligible: List(Int),
  )
}

pub fn new(
  ref ref: SessionRef,
  pid pid: Pid,
  monitor monitor: Monitor,
  flags flags: IdentifyFlags,
  bot bot: Bool,
  staff staff: Option(Bool),
  mode mode: Mode,
) -> Entry {
  Entry(
    ref:,
    pid:,
    monitor:,
    flags:,
    bot:,
    staff:,
    mode:,
    synced: False,
    typing: None,
    seen_messages: dict.new(),
    seen_voice: dict.new(),
    dm_partners: [],
    dm_eligible: [],
  )
}

pub fn ref(entry: Entry) -> SessionRef {
  entry.ref
}

pub fn id(entry: Entry) -> SessionId {
  entry.ref.id
}

pub fn key(entry: Entry) -> String {
  session.session_id_to_string(entry.ref.id)
}

pub fn user(entry: Entry) -> UserId {
  entry.ref.user
}

pub fn user_int(entry: Entry) -> Int {
  user_id_to_int(entry.ref.user)
}

pub fn pid(entry: Entry) -> Pid {
  entry.pid
}

pub fn monitor(entry: Entry) -> Monitor {
  entry.monitor
}

pub fn staff(entry: Entry) -> Option(Bool) {
  entry.staff
}

pub fn flags(entry: Entry) -> IdentifyFlags {
  entry.flags
}

pub fn is_bot(entry: Entry) -> Bool {
  entry.bot
}

pub fn is_passive(entry: Entry) -> Bool {
  !entry.bot && entry.mode == Passive
}

pub fn is_synced(entry: Entry) -> Bool {
  entry.synced
}

pub fn typing(entry: Entry) -> Option(Bool) {
  entry.typing
}

pub fn seen_messages(entry: Entry) -> Dict(Int, Int) {
  entry.seen_messages
}

pub fn seen_voice(entry: Entry) -> Dict(VoiceKey, VoiceState) {
  entry.seen_voice
}

pub fn dm_partners(entry: Entry) -> List(Int) {
  entry.dm_partners
}

pub fn dm_eligible(entry: Entry) -> List(Int) {
  entry.dm_eligible
}

pub fn rebind(
  entry: Entry,
  ref ref: SessionRef,
  pid pid: Pid,
  monitor monitor: Monitor,
) -> Entry {
  Entry(..entry, ref:, pid:, monitor:)
}

pub fn set_mode(entry: Entry, mode: Mode) -> Entry {
  Entry(..entry, mode:)
}

pub fn set_synced(entry: Entry, synced: Bool) -> Entry {
  Entry(..entry, synced:)
}

pub fn set_typing(entry: Entry, typing: Option(Bool)) -> Entry {
  Entry(..entry, typing:)
}

pub fn set_seen(
  entry: Entry,
  messages messages: Dict(Int, Int),
  voice voice: Dict(VoiceKey, VoiceState),
) -> Entry {
  Entry(..entry, seen_messages: messages, seen_voice: voice)
}

pub fn set_dm_partners(
  entry: Entry,
  partners partners: List(Int),
  eligible eligible: List(Int),
) -> Entry {
  Entry(..entry, dm_partners: partners, dm_eligible: eligible)
}
