// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings
import fluxer_gateway/core/error.{type Unavailable, Overloaded, Unavailable}
import fluxer_gateway/core/guild.{type GuildRequest}
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/presence.{type Presence, type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type SessionId, type SessionInbox, type SessionRef,
}
import fluxer_gateway/core/user.{type User}
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/custom_status
import fluxer_gateway/presence/dispatch_id
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/manager
import fluxer_gateway/presence/pids.{type Inbox}
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/protocol.{
  ConnectMoved, ConnectReply, ConnectSession, Disconnect, RepointSession,
  StartRequest, SubscribeTargets, UnsubscribeTargets, Update,
}
import fluxer_gateway/presence/push_buffer
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None}
import gleam/result

pub opaque type PresenceLink {
  PresenceLink(
    user: UserId,
    session: SessionId,
    owner: Subject(SessionInbox),
    inbox: Inbox,
    sessions_replace: RawJson,
  )
}

pub type SessionConnect {
  SessionConnect(
    session: SessionRef,
    status: Status,
    afk: Bool,
    mobile: Bool,
    custom_status: Option(RawJson),
    settings_invisible: Bool,
    user: User,
    friends: PackedIds,
    group_dm_recipients: PackedIds,
    guilds: List(GuildId),
  )
}

fn start_timeout_ms() -> Int {
  durations.presence().client_start_timeout_ms
}

fn connect_timeout_ms() -> Int {
  durations.presence().client_connect_timeout_ms
}

fn retry_after_ms() -> Int {
  durations.presence().client_retry_after_ms
}

const max_moved_hops = 3

const stopped_retries = 3

pub fn session_connect(
  request: SessionConnect,
  reply: Subject(Result(PresenceLink, Unavailable)),
) -> Nil {
  let _ = process.spawn_unlinked(fn() { process.send(reply, connect(request)) })
  Nil
}

fn start_request(request: SessionConnect) -> protocol.StartRequest {
  let config = settings.get()
  StartRequest(
    user: id.user_id_to_int(user.id(request.user)),
    profile: profile.from_user(request.user),
    custom_status: custom_status.initial(request.custom_status),
    settings_invisible: request.settings_invisible,
    guilds: id_set.from_list(list.map(request.guilds, id.guild_id_to_int)),
    targets: targets_of(request),
    push_limits: push_buffer.Limits(
      max_entries: config.presence_push_buffer_max_entries,
      max_bytes: config.presence_push_buffer_max_bytes,
    ),
    carried: None,
  )
}

fn targets_of(request: SessionConnect) -> id_set.IdSet {
  id_set.union(
    id_set.from_packed(request.friends),
    id_set.from_packed(request.group_dm_recipients),
  )
}

fn connect(request: SessionConnect) -> Result(PresenceLink, Unavailable) {
  connect_attempt(request, start_request(request), stopped_retries)
}

fn connect_attempt(
  request: SessionConnect,
  start: protocol.StartRequest,
  retries_left: Int,
) -> Result(PresenceLink, Unavailable) {
  let reply = process.new_subject()
  use inbox <- result.try(
    manager.start_or_lookup(
      start,
      connect_input(request, start, reply),
      start_timeout_ms(),
    )
    |> result.map_error(unavailable),
  )
  let connected =
    remote.await_from_disposable(reply, inbox, connect_timeout_ms())
    |> result.try(follow_moves(_, inbox, request, start, max_moved_hops))
  case connected {
    Ok(#(inbox, sessions_replace)) ->
      Ok(PresenceLink(
        user: user.id(request.user),
        session: request.session.id,
        owner: request.session.inbox,
        inbox:,
        sessions_replace: raw_json.wrap(sessions_replace),
      ))
    Error(remote.CallUnavailable) if retries_left > 0 ->
      connect_attempt(request, start, retries_left - 1)
    Error(reason) -> Error(unavailable(reason))
  }
}

fn connect_input(
  request: SessionConnect,
  start: protocol.StartRequest,
  reply: Subject(protocol.ConnectReply),
) -> protocol.Input(process.Monitor) {
  ConnectSession(
    session: request.session,
    status: request.status,
    afk: request.afk,
    mobile: request.mobile,
    guilds: start.guilds,
    targets: start.targets,
    reply:,
  )
}

fn follow_moves(
  reply: protocol.ConnectReply,
  inbox: Inbox,
  request: SessionConnect,
  start: protocol.StartRequest,
  hops_left: Int,
) -> Result(#(Inbox, BitArray), remote.CallError) {
  case reply {
    ConnectReply(sessions_replace:) -> Ok(#(inbox, sessions_replace))
    ConnectMoved(to:) if hops_left > 0 -> {
      use moved <- result.try(
        remote.call_subject_from_disposable(to, connect_timeout_ms(), fn(reply) {
          connect_input(request, start, reply)
        }),
      )
      follow_moves(moved, to, request, start, hops_left - 1)
    }
    ConnectMoved(..) -> Error(remote.CallUnavailable)
  }
}

fn unavailable(reason: remote.CallError) -> Unavailable {
  case reason {
    remote.CallBusy -> Overloaded(retry_after_ms: retry_after_ms())
    remote.CallTimeout | remote.CallUnavailable ->
      Unavailable(retry_after_ms: retry_after_ms())
  }
}

pub fn session_update(
  link: PresenceLink,
  status: Status,
  afk: Bool,
  mobile: Bool,
  custom: Option(RawJson),
) -> Nil {
  process.send(
    link.inbox,
    Update(
      session: link.session,
      status:,
      afk:,
      mobile:,
      custom_status: custom_status.op_value(custom),
    ),
  )
}

pub fn link_pid(link: PresenceLink) -> Result(process.Pid, Nil) {
  process.subject_owner(link.inbox)
}

pub fn rebind(link: PresenceLink, to: Inbox) -> PresenceLink {
  PresenceLink(..link, inbox: to)
}

pub fn repoint(link: PresenceLink, to: SessionRef, from: SessionRef) -> Nil {
  process.send(link.inbox, RepointSession(session: to, from:))
}

pub fn reown(link: PresenceLink, owner: Subject(SessionInbox)) -> PresenceLink {
  PresenceLink(..link, owner:)
}

pub fn same_entry(a: PresenceLink, b: PresenceLink) -> Bool {
  a.inbox == b.inbox && a.session == b.session
}

pub fn session_disconnect(link: PresenceLink) -> Nil {
  process.send(link.inbox, Disconnect(link.session, link.owner))
}

pub fn subscribe_users(link: PresenceLink, users: PackedIds) -> Nil {
  process.send(link.inbox, SubscribeTargets(id_set.from_packed(users)))
}

pub fn unsubscribe_users(link: PresenceLink, users: PackedIds) -> Nil {
  process.send(link.inbox, UnsubscribeTargets(id_set.from_packed(users)))
}

pub fn register_guild_interest(
  guild: GuildId,
  members: PackedIds,
  deliver_to: Subject(GuildRequest),
) -> Nil {
  registry.register_guild(guild, members, deliver_to)
}

pub fn register_guild_interest_and_resend(
  guild: GuildId,
  members: PackedIds,
  deliver_to: Subject(GuildRequest),
) -> Nil {
  registry.register_guild_with(guild, members, deliver_to, resend: True)
}

pub fn unregister_guild_interest(guild: GuildId, members: PackedIds) -> Nil {
  registry.unregister_guild(guild, members)
}

pub fn drop_guild_interest(guild: GuildId) -> Nil {
  registry.drop_guild(guild)
}

pub fn drop_guild_interest_of(guild: GuildId, owner: Pid) -> Nil {
  registry.drop_guild_of(guild, owner)
}

pub fn lookup_many(users: PackedIds) -> List(Presence) {
  cache.lookup_many(users)
}

pub fn lookup_known(users: PackedIds) -> #(List(Presence), List(Int)) {
  cache.lookup_known(users)
}

pub fn sessions_replace(link: PresenceLink) -> RawJson {
  link.sessions_replace
}

pub fn is_link_alive(link: PresenceLink) -> Bool {
  case process.subject_owner(link.inbox) {
    Ok(pid) ->
      case remote.node_of(link.inbox) == remote.self_atom() {
        True -> process.is_alive(pid)
        False -> True
      }
    Error(Nil) -> False
  }
}

pub fn dispatch_to_user(
  user: UserId,
  event: String,
  payload: RawJson,
) -> Result(Nil, Nil) {
  use inboxes <- result.map(
    manager.lookup_all(id.user_id_to_int(user), start_timeout_ms())
    |> result.replace_error(Nil),
  )
  let id = dispatch_id.mint()
  list.each(inboxes, process.send(
    _,
    protocol.UserDispatch(
      id:,
      event:,
      data: payload,
      parsed: protocol.OtherEvent,
    ),
  ))
}
