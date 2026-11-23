// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{type DispatchFacts}
import fluxer_gateway/core/error.{type CloseCode, type GatewayError}
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{type Option}
import gleam/order.{type Order}
import gleam/string

pub opaque type SessionId {
  SessionId(String)
}

pub type SessionRef {
  SessionRef(id: SessionId, user: UserId, inbox: Subject(SessionInbox))
}

pub type SocketRef {
  SocketRef(pid: Pid, outbox: Subject(SocketMsg))
}

pub type IdentifyFlags {
  IdentifyFlags(
    debounce_message_reactions: Bool,
    channel_threads: Bool,
    guild_visits: Bool,
  )
}

pub type ClientProperties {
  ClientProperties(os: String, browser: String, device: String)
}

pub type Identify {
  Identify(
    token: String,
    properties: ClientProperties,
    shard: Option(#(Int, Int)),
    presence: Option(Dynamic),
    flags: IdentifyFlags,
    ignored_events: List(String),
    initial_guild_id: Option(GuildId),
    e2ee_capable: Bool,
    mobile: Bool,
    latitude: Option(String),
    longitude: Option(String),
  )
}

pub type Peer {
  Peer(ip: String, api_version: Int)
}

pub type StartError {
  StartAuthFailed
  StartShardingRequired
  StartRateLimited(retry_after_ms: Int)
  StartTooManySessions
  StartUnavailable(retry_after_ms: Int)
  StartUnknownError
}

pub type ClientOp {
  Heartbeat(seq: Option(Int))
  PresenceUpdate(data: Dynamic)
  VoiceStateUpdate(data: Dynamic)
  RequestGuildMembers(data: Dynamic)
  VoiceSignal(data: RawJson)
  LazyRequest(data: Dynamic)
  RequestGuildCounts(data: Dynamic)
  RequestChannelMemberCounts(data: Dynamic)
  RequestForumUnreads(data: Dynamic)
}

pub type ResumeError {
  InvalidResumeSeq
  SessionGone
  ResumeAuthFailed
  ResumeUnavailable(retry_after_ms: Int)
}

pub type TerminateReason {
  TerminatedByRpc
  TerminatedByManager
  Replaced
}

pub type SessionInbox {
  SocketOp(op: ClientOp)
  SocketDown(socket: Pid)
  Dispatch(event: String, payload: RawJson)
  GuildDispatch(
    guild: GuildId,
    event: String,
    payload: RawJson,
    facts: DispatchFacts,
  )
  PresenceFrame(user: UserId, payload: RawJson, facts: DispatchFacts)
  GuildDown(guild: GuildId, retry_after_ms: Int)
  GuildRebound(guild: GuildId, kept: Bool)
  PresenceRebound(kept: Bool)
  Terminate(reason: TerminateReason)
}

pub type Outbound {
  DispatchFrame(seq: Int, event: String, payload: RawJson)
  ReconnectFrame
  HeartbeatAckFrame
  GatewayErrorFrame(error: GatewayError, payload: RawJson)
}

pub type SocketMsg {
  Send(session: SessionId, frames: List(Outbound))
  Close(session: SessionId, code: CloseCode, reason: String)
  Moved(session: SessionId, from: SessionRef, to: SessionRef, seq: Int)
  Reattached(session: SessionId)
}

@external(erlang, "crypto", "strong_rand_bytes")
fn strong_rand_bytes(count: Int) -> BitArray

pub fn new_session_id() -> SessionId {
  SessionId(bit_array.base16_encode(strong_rand_bytes(16)))
}

pub fn session_id_from_string(text: String) -> Result(SessionId, Nil) {
  case string.byte_size(text) {
    32 -> Ok(SessionId(text))
    _ -> Error(Nil)
  }
}

pub fn session_id_to_string(id: SessionId) -> String {
  let SessionId(text) = id
  text
}

pub fn guild_dispatch(
  guild: GuildId,
  event: String,
  payload: RawJson,
) -> SessionInbox {
  GuildDispatch(
    guild:,
    event:,
    payload:,
    facts: dispatch_facts.scan(event, payload),
  )
}

pub fn session_id_before(a: SessionId, b: SessionId) -> Bool {
  let SessionId(left) = a
  let SessionId(right) = b
  less_than(left, right)
}

pub fn session_id_bits(id: SessionId) -> BitArray {
  let SessionId(text) = id
  <<text:utf8>>
}

@external(erlang, "gleam_stdlib", "less_than")
fn less_than(left: String, right: String) -> Bool

pub fn compare_session_id(a: SessionId, b: SessionId) -> Order {
  let SessionId(left) = a
  let SessionId(right) = b
  string.compare(left, right)
}

pub fn send(session: SessionRef, message: SessionInbox) -> Nil {
  process.send(session.inbox, message)
}

pub fn no_flags() -> IdentifyFlags {
  IdentifyFlags(
    debounce_message_reactions: False,
    channel_threads: False,
    guild_visits: False,
  )
}
