// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{
  type CloseCode, type GatewayError, type Unavailable,
}
import fluxer_gateway/core/guild.{type JoinReply}
import fluxer_gateway/core/guild_create.{type GuildCreate}
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type IdentifyFlags, type Outbound, type SessionId, type SessionInbox,
  type SessionRef, type SocketRef, Close, DispatchFrame, GatewayErrorFrame, Send,
}
import fluxer_gateway/core/user.{type User}
import fluxer_gateway/presence/client.{type PresenceLink}
import fluxer_gateway/session/control.{type Control}
import fluxer_gateway/session/links.{type Links}
import fluxer_gateway/session/pending_presences.{type PendingPresences}
import fluxer_gateway/session/presence_view.{type PresenceView}
import fluxer_gateway/session/reactions.{type Reactions}
import fluxer_gateway/session/ready.{type ReadyData}
import fluxer_gateway/session/relations.{type Relations}
import fluxer_gateway/session/replay_buffer.{
  type ReplayBuffer, Entry, Kept, Oversized,
}
import fluxer_gateway/session/subscriptions.{type Subscriptions}
import fluxer_gateway/session/visits.{type Visits}
import fluxer_gateway/telemetry/timings.{type Timing}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid, type Subject, type Timer}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

pub type Internal {
  ConnectGuild(guild: GuildId, attempt: Int)
  JoinExpired(guild: GuildId, token: Int)
  PresenceConnect(attempt: Int)
  PresenceReplied(result: Result(PresenceLink, Unavailable))
  PresenceTimeout(attempt: Int)
  JoinReplied(reply: JoinReply)
  ReadyDeadline
  EnablePresenceUpdates
  RepairTick
  ResumeExpired(generation: Int)
  OfflineGrace(generation: Int)
  FlushReactions
  CallReconnect(channel: Int, attempt: Int)
  ArrivalExpired
}

pub type Held {
  HeldInbox(SessionInbox)
  HeldControl(Control)
}

pub type Arrival {
  Settled
  Lapsed
  Arriving(source: Monitor, guilds: List(GuildId), held: List(Held))
}

pub type CallSlot {
  Watched(call: Pid, monitor: Monitor)
  Lost
}

pub type PresenceState {
  PresenceDown
  PresenceConnecting(attempt: Int)
  PresenceUp(link: PresenceLink, monitor: Option(Monitor))
}

pub type PresenceSent {
  PresenceSent(
    status: Status,
    afk: Bool,
    mobile: Bool,
    custom_status: Option(RawJson),
    targets: Set(Int),
  )
}

pub type Socket {
  Attached(socket: SocketRef, monitor: Monitor)
  Detached(generation: Int)
}

pub type State {
  State(
    id: SessionId,
    me: SessionRef,
    user_id: UserId,
    user: User,
    staff: Bool,
    bot: Bool,
    version: Int,
    token_hash: BitArray,
    auth_hash: BitArray,
    flags: IdentifyFlags,
    shard: Option(#(Int, Int)),
    e2ee_capable: Bool,
    country_code: Option(String),
    ignored: Set(String),
    initial_guild: Option(GuildId),
    status: Status,
    resume_status: Status,
    afk: Bool,
    mobile: Bool,
    custom_status: Option(RawJson),
    settings_invisible: Bool,
    socket: Socket,
    generation: Int,
    seq: Int,
    ack_seq: Int,
    replay_floor: Int,
    replay: ReplayBuffer,
    ready: Option(ReadyData),
    collected: List(GuildCreate),
    links: Links,
    relations: Relations,
    pending_presences: PendingPresences,
    suppress_presences: Bool,
    presence_view: PresenceView,
    reactions: Reactions,
    presence: PresenceState,
    presence_targets: Set(Int),
    presence_sent: Option(PresenceSent),
    subscriptions: Subscriptions,
    visits: Visits,
    pending_bot_joins: Set(Int),
    unannounced: Set(Int),
    dm_mutual: Dict(Int, Set(Int)),
    health: Dict(Int, Bool),
    calls: Dict(Int, CallSlot),
    call_timers: Dict(Int, Timer),
    fenced: Bool,
    arrival: Arrival,
    ready_deadline_ms: Int,
    timing: Timing,
    internal: Subject(Internal),
    join_replies: Subject(JoinReply),
    presence_replies: Subject(Result(PresenceLink, Unavailable)),
  )
}

pub fn now_ms() -> Int {
  clock.monotonic_ms()
}

pub fn user_int(state: State) -> Int {
  id.user_id_to_int(state.user_id)
}

pub fn is_ready_sent(state: State) -> Bool {
  option.is_none(state.ready)
}

pub fn schedule(state: State, delay_ms: Int, message: Internal) -> Nil {
  process.send_after(state.internal, int.max(delay_ms, 0), message)
  Nil
}

pub fn send_frames(state: State, frames: List(Outbound)) -> Nil {
  case state.socket, frames {
    _, [] -> Nil
    Attached(socket, _), _ ->
      process.send(socket.outbox, Send(state.id, frames))
    Detached(_), _ -> Nil
  }
}

pub fn close_socket(state: State, code: CloseCode, reason: String) -> Nil {
  case state.socket {
    Attached(socket, _) ->
      process.send(socket.outbox, Close(state.id, code, reason))
    Detached(_) -> Nil
  }
}

pub fn send_error(state: State, error: GatewayError) -> Nil {
  send_frames(state, [GatewayErrorFrame(error, raw_json.null())])
}

fn skips_replay(event: String) -> Bool {
  case event {
    "GUILD_MEMBERS_CHUNK"
    | "GUILD_MEMBER_LIST_UPDATE"
    | "GUILD_SYNC"
    | "THREAD_LIST_SYNC"
    | "THREAD_MEMBER_LIST_UPDATE" -> True
    _ -> False
  }
}

pub fn deliver(state: State, event: String, payload: RawJson) -> State {
  let seq = state.seq + 1
  send_frames(state, [DispatchFrame(seq, event, payload)])
  case skips_replay(event) {
    True -> State(..state, seq:)
    False -> keep(state, seq, event, payload)
  }
}

pub fn deliver_unbuffered(
  state: State,
  event: String,
  payload: RawJson,
) -> State {
  let seq = state.seq + 1
  send_frames(state, [DispatchFrame(seq, event, payload)])
  State(..state, seq:)
}

fn keep(state: State, seq: Int, event: String, payload: RawJson) -> State {
  case replay_buffer.push(state.replay, Entry(seq:, event:, payload:)) {
    Oversized -> State(..state, seq:, replay_floor: seq)
    Kept(buffer, evicted_up_to) ->
      State(
        ..state,
        seq:,
        replay: buffer,
        replay_floor: int.max(state.replay_floor, evicted_up_to),
      )
  }
}

pub fn eligible_partners(state: State) -> Set(Int) {
  state.dm_mutual
  |> dict.to_list
  |> list.fold(set.new(), fn(acc, entry) {
    let #(guild, partners) = entry
    case id.guild_id(guild) {
      Ok(guild_id) ->
        case links.linked_ref(state.links, guild_id) {
          Some(_) -> set.union(acc, partners)
          None -> acc
        }
      Error(_) -> acc
    }
  })
}

pub fn push_hold(state: State) -> Bool {
  case state.status {
    presence.Online | presence.Dnd | presence.Invisible ->
      !state.afk && !state.mobile
    _ -> False
  }
}

pub fn session_id_text(state: State) -> String {
  session.session_id_to_string(state.id)
}

pub fn guild_key(guild: GuildId) -> Int {
  id.guild_id_to_int(guild)
}
