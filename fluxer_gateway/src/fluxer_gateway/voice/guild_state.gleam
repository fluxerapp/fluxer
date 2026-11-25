// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/error.{type GatewayError, type RpcError}
import fluxer_gateway/core/guild.{type GuildFacts}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/core/voice_state
import fluxer_gateway/perms
import fluxer_gateway/voice/client_update.{type ViewerKeys}
import fluxer_gateway/voice/entry.{type Entry, type Flags}
import fluxer_gateway/voice/geo.{type Geo}
import fluxer_gateway/voice/p2p.{type Decision}
import fluxer_gateway/voice/store.{type Store}
import fluxer_gateway/voice/token.{
  type Outcome, type Permissions as VoicePermissions,
}
import fluxer_gateway/voice/virtual_access.{type VirtualAccess}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type State {
  State(
    guild: GuildId,
    states: Store(Entry),
    geo: Dict(ConnectionId, Geo),
    pending: Store(Pending),
    recent: Store(Recent),
    room_keys: Dict(ChannelId, String),
    access: VirtualAccess,
    overrides: Dict(UserId, VoiceOverride),
    ticking: Bool,
    p2p_suspects: Set(VoiceKey),
    fetches: Dict(String, Fetch),
  )
}

pub type Fetch {
  Fetch(job: Job, request: token.Request, started_at: Int)
}

pub type VoiceOverride {
  VoiceOverride(mute: Bool, deaf: Bool)
}

pub type Pending {
  Pending(
    user: UserId,
    channel: ChannelId,
    session_id: Option(String),
    entry: Entry,
    geo: Geo,
    nonce: Option(String),
    created_at: Int,
    expires_at: Int,
    moved: Bool,
  )
}

pub type Recent {
  Recent(entry: Entry, geo: Geo, disconnected_at: Int)
}

pub type Ctx {
  Ctx(
    facts: GuildFacts,
    reader: perms.Reader,
    member: fn(UserId) -> Option(Member),
    channel: fn(ChannelId) -> Option(Channel),
    sessions_of: fn(UserId) -> List(String),
    now_ms: Int,
    notify: fn(Request) -> Nil,
    notify_after: fn(Request, Int) -> Nil,
  )
}

pub type Effect {
  Broadcast(event: String, payload: RawJson, channel: Option(ChannelId))
  BroadcastMove(
    event: String,
    payload: RawJson,
    left: RawJson,
    from: ChannelId,
    to: ChannelId,
  )
  ToSession(session: String, event: String, payload: RawJson)
  ErrorTo(session: String, error: GatewayError)
}

pub type Step {
  Step(state: State, effects: List(Effect))
}

pub type Update {
  Update(
    session: SessionRef,
    channel: ChannelTarget,
    connection_id: Option(ConnectionId),
    flags: Flags,
    viewer_keys: ViewerKeys,
    geo: Geo,
    e2ee_capable: Bool,
    bot: Bool,
    p2p: Option(Bool),
    country_code: Option(String),
  )
}

pub type ChannelTarget {
  Leave
  Join(ChannelId)
}

pub type Job {
  JoinJob(
    update: Update,
    channel: ChannelId,
    member: Member,
    keys: List(String),
    perms: VoicePermissions,
    nonce: String,
    decision: Decision,
    e2ee: Bool,
  )
  MoveJob(
    update: Update,
    channel: ChannelId,
    member: Member,
    old: Entry,
    perms: VoicePermissions,
    nonce: String,
    decision: Decision,
    e2ee: Bool,
  )
  ReissueJob(
    user: UserId,
    channel: ChannelId,
    session_id: Option(String),
    base: Entry,
    geo: Geo,
    nonce: String,
    moved: Bool,
    suppress: Option(Bool),
  )
}

pub type RpcCall {
  MemberVoice(user: UserId, mute: Bool, deaf: Bool)
  DisconnectIfInChannel(
    user: UserId,
    channel: ChannelId,
    connection_id: Option(ConnectionId),
  )
  Confirm(connection_id: Option(ConnectionId), nonce: Option(String))
  ChannelStates(channel: ChannelId)
  MoveMember(
    user: UserId,
    moderator: UserId,
    channel: Option(ChannelId),
    connection_id: Option(ConnectionId),
  )
  GetVoiceState(user: UserId)
  SwitchRegion(channel: ChannelId)
}

pub type Replier {
  ReplyTo(Subject(Result(Json, RpcError)))
}

pub type Request {
  ClientUpdate(Update)
  Signal(session: SessionRef, channel: ChannelId, to: String, data: RawJson)
  TokenReady(job: Job, result: Result(Outcome, GatewayError))
  Rpc(call: RpcCall, reply: Replier)
  CollectStates(reply: Subject(List(Entry)))
  Sweep
  P2pCheck
  CleanupAccess(users: List(UserId))
}

pub fn new(guild: GuildId) -> State {
  State(
    guild:,
    states: store.new(),
    geo: dict.new(),
    pending: store.new(),
    recent: store.new(),
    room_keys: dict.new(),
    access: virtual_access.new(),
    overrides: dict.new(),
    ticking: False,
    p2p_suspects: set.new(),
    fetches: dict.new(),
  )
}

pub fn job_nonce(job: Job) -> String {
  case job {
    JoinJob(nonce:, ..) | MoveJob(nonce:, ..) | ReissueJob(nonce:, ..) -> nonce
  }
}

pub fn job_target(job: Job) -> #(UserId, ChannelId) {
  case job {
    JoinJob(update:, channel:, ..) | MoveJob(update:, channel:, ..) -> #(
      update.session.user,
      channel,
    )
    ReissueJob(user:, channel:, ..) -> #(user, channel)
  }
}

pub fn holds_transition(job: Job) -> Bool {
  case job {
    JoinJob(..) -> False
    MoveJob(..) | ReissueJob(..) -> True
  }
}

pub fn start_fetch(
  step: Step,
  ctx: Ctx,
  job: Job,
  request: token.Request,
) -> Step {
  spawn_fetch(ctx.notify, job, request)
  map_state(step, fn(state) {
    State(
      ..state,
      fetches: dict.insert(
        state.fetches,
        job_nonce(job),
        Fetch(job:, request:, started_at: ctx.now_ms),
      ),
    )
  })
}

pub fn spawn_fetch(
  notify: fn(Request) -> Nil,
  job: Job,
  request: token.Request,
) -> Nil {
  process.spawn_unlinked(fn() {
    let fetched = case job {
      ReissueJob(..) -> token.fetch_sfu(request) |> result.map(token.Granted)
      JoinJob(..) | MoveJob(..) -> token.fetch(request)
    }
    notify(TokenReady(job, fetched))
  })
  Nil
}

pub fn transition_in_use(
  state: State,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  let target = #(user, channel)
  list.any(store.values(state.pending), fn(pending) {
    #(pending.user, pending.channel) == target
  })
  || list.any(dict.values(state.fetches), fn(fetch) {
    holds_transition(fetch.job) && job_target(fetch.job) == target
  })
}

pub fn start(state: State) -> Step {
  Step(state, [])
}

pub fn emit(step: Step, effect: Effect) -> Step {
  Step(..step, effects: [effect, ..step.effects])
}

pub fn map_state(step: Step, f: fn(State) -> State) -> Step {
  Step(..step, state: f(step.state))
}

pub fn geo_of(state: State, connection_id: ConnectionId) -> Geo {
  dict.get(state.geo, connection_id) |> result.unwrap(geo.unknown())
}

pub fn place(state: State, value: Entry, at: Geo) -> State {
  case entry.key(value) {
    Error(Nil) -> state
    Ok(key) -> {
      let connection_id = voice_key.connection(key)
      let located = case geo.is_known(at) {
        True -> dict.insert(state.geo, connection_id, at)
        False -> dict.delete(state.geo, connection_id)
      }
      State(..state, states: store.put(state.states, key, value), geo: located)
    }
  }
}

pub fn put_entry(state: State, value: Entry) -> State {
  case entry.key(value) {
    Ok(key) -> State(..state, states: store.put(state.states, key, value))
    Error(Nil) -> state
  }
}

pub fn pending_key(pending: Pending) -> VoiceKey {
  voice_key.new(
    channel: pending.channel,
    user: pending.user,
    connection: entry.connection_id(pending.entry),
  )
}

pub fn connected(
  state: State,
  connection_id: ConnectionId,
) -> Result(Entry, Nil) {
  store.by_connection(state.states, connection_id)
  |> result.map(fn(pair) { pair.1 })
}

pub fn pending_of(
  state: State,
  connection_id: ConnectionId,
) -> Result(Pending, Nil) {
  store.by_connection(state.pending, connection_id)
  |> result.map(fn(pair) { pair.1 })
}

pub fn entries(state: State) -> List(Entry) {
  store.values(state.states)
}

pub fn channel_entries(state: State, channel: ChannelId) -> List(Entry) {
  store.in_channel(state.states, channel)
  |> list.map(fn(pair) { pair.1 })
}

pub fn user_entries(state: State, user: UserId) -> List(#(VoiceKey, Entry)) {
  store.of_user(state.states, user)
}

pub fn send_error(step: Step, session: String, reason: GatewayError) -> Step {
  case p2p.reports_rejection(reason) {
    True -> emit(step, ErrorTo(session, reason))
    False -> step
  }
}

pub fn broadcast(
  step: Step,
  value: Entry,
  old_channel: Option(ChannelId),
) -> Step {
  let target = option.or(entry.channel(value), old_channel)
  let payload = entry.to_raw(value, entry.Broadcast)
  let step = case target {
    Some(_) -> emit(step, Broadcast("VOICE_STATE_UPDATE", payload, target))
    None -> step
  }
  persist(step.state.guild, value, payload)
  step
}

pub fn broadcast_move(
  step: Step,
  moved: Entry,
  old_channel: Option(ChannelId),
) -> Step {
  let payload = entry.to_raw(moved, entry.Broadcast)
  let step = case old_channel, entry.channel(moved) {
    Some(from), Some(to) if from != to ->
      emit(
        step,
        BroadcastMove(
          "VOICE_STATE_UPDATE",
          payload,
          entry.to_raw(entry.disconnected(moved), entry.Broadcast),
          from,
          to,
        ),
      )
    _, Some(to) ->
      emit(step, Broadcast("VOICE_STATE_UPDATE", payload, Some(to)))
    Some(from), None ->
      emit(
        step,
        Broadcast(
          "VOICE_STATE_UPDATE",
          entry.to_raw(entry.disconnected(moved), entry.Broadcast),
          Some(from),
        ),
      )
    None, None -> step
  }
  persist(step.state.guild, moved, payload)
  step
}

pub fn broadcast_disconnects(
  step: Step,
  removed: List(#(VoiceKey, Entry)),
) -> Step {
  list.fold(removed, step, fn(acc, pair) {
    broadcast(acc, entry.disconnected(pair.1), entry.channel(pair.1))
  })
}

fn persist(guild: GuildId, value: Entry, broadcast: RawJson) -> Nil {
  let guild_id = case voice_state.guild(entry.state(value)) == Some(guild) {
    True -> entry.guild_json(value)
    False -> id.json(id.guild_id_to_int(guild))
  }
  let #(kind, fields) = case entry.channel(value) {
    None -> #("voice_state_remove", [
      #("guild_id", guild_id),
      #("connection_id", voice_key.connection_json(entry.connection_id(value))),
    ])
    Some(_) -> #("voice_state_upsert", [
      #("guild_id", guild_id),
      #("voice_state", entry.external_from(value, broadcast)),
    ])
  }
  process.spawn_unlinked(fn() { token.api_send(kind, fields) })
  Nil
}

pub fn force_disconnect(
  guild: Option(GuildId),
  channel: ChannelId,
  user: UserId,
  connection_id: ConnectionId,
) -> Result(Nil, Nil) {
  let base = [
    #("channel_id", id.channel_json(channel)),
    #("user_id", id.user_json(user)),
    #("connection_id", voice_key.connection_json(connection_id)),
  ]
  let fields = case guild {
    Some(guild) -> [#("guild_id", id.guild_json(guild)), ..base]
    None -> base
  }
  token.api_send("voice_force_disconnect_participant", fields)
  |> result.replace_error(Nil)
}

pub fn spawn_force_disconnect(guild: GuildId, value: Entry) -> Nil {
  case entry.channel(value) {
    Some(channel) -> {
      let user = entry.user(value)
      let conn = entry.connection_id(value)
      process.spawn_unlinked(fn() {
        force_disconnect(Some(guild), channel, user, conn)
      })
      Nil
    }
    None -> Nil
  }
}

pub fn permissions_of(
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
) -> Permissions {
  perms.permissions(ctx.reader, user, perms.ChannelScope(channel))
  |> result.unwrap(permissions.none())
}

pub fn can_view_and_connect(
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  let bits = permissions_of(ctx, user, channel)
  permissions.has(bits, permissions.ViewChannel)
  && permissions.has(bits, permissions.Connect)
}

pub fn has_base_access(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  !is_timed_out(ctx, user)
  && {
    virtual_access.has_voice_access(state.access, user, channel)
    || can_view_and_connect(ctx, user, channel)
  }
}

pub fn is_timed_out(ctx: Ctx, user: UserId) -> Bool {
  case ctx.member(user) {
    Some(value) -> member.is_timed_out(value, now_ms: ctx.now_ms)
    None -> False
  }
}

pub fn voice_permissions(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel: ChannelId,
) -> VoicePermissions {
  let bits = permissions_of(ctx, user, channel)
  let admin = permissions.has(bits, permissions.Administrator)
  let virtual = virtual_access.has_voice_access(state.access, user, channel)
  let can_speak = admin || permissions.has(bits, permissions.Speak) || virtual
  let can_stream = admin || permissions.has(bits, permissions.Stream) || virtual
  token.Permissions(can_speak:, can_stream:, can_video: can_stream)
}

pub fn apply_permissions(flags: Flags, granted: VoicePermissions) -> Flags {
  entry.Flags(
    ..flags,
    suppress: !granted.can_speak,
    self_stream: granted.can_stream && flags.self_stream,
    self_video: granted.can_video && flags.self_video,
  )
}

pub fn member_mute_deaf(state: State, value: Member) -> #(Bool, Bool) {
  case dict.get(state.overrides, member.user_id(value)) {
    Ok(VoiceOverride(mute:, deaf:)) -> #(mute, deaf)
    Error(Nil) -> #(
      entry.member_flag(member.wire(value), "mute"),
      entry.member_flag(member.wire(value), "deaf"),
    )
  }
}

pub fn member_raw(state: State, value: Member) -> RawJson {
  case dict.get(state.overrides, member.user_id(value)) {
    Ok(VoiceOverride(mute:, deaf:)) ->
      entry.patch_member(member.wire(value), mute, deaf)
    Error(Nil) -> member.wire(value)
  }
}

pub type ChannelFacts {
  ChannelFacts(
    user_limit: Int,
    connection_limit: Int,
    rtc_p2p: Bool,
    is_voice: Bool,
  )
}

pub fn channel_facts(value: Channel) -> ChannelFacts {
  let limits = channel.voice(value)
  ChannelFacts(
    user_limit: limits.user_limit,
    connection_limit: limits.connection_limit,
    rtc_p2p: limits.rtc_p2p,
    is_voice: channel.kind(value) == channel.Voice,
  )
}

pub fn pinned(ctx: Ctx, channel: ChannelId) -> Bool {
  case ctx.channel(channel) {
    Some(value) -> channel_facts(value).rtc_p2p
    None -> False
  }
}

pub fn join_decision(
  state: State,
  agreed: Bool,
  channel: ChannelId,
) -> Decision {
  let pending =
    state.pending
    |> store.in_channel(channel)
    |> list.is_empty
    |> bool.negate
  p2p.join_decision(agreed:, states: channel_entries(state, channel), pending:)
}

pub fn session_text(ref: SessionRef) -> String {
  session.session_id_to_string(ref.id)
}
