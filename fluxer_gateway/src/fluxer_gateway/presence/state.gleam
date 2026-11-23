// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/dispatch_facts.{type DispatchFacts}
import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type SessionId, type SessionInbox, type SessionRef, Dispatch, PresenceFrame,
  PresenceRebound, Terminate, TerminatedByRpc,
}
import fluxer_gateway/presence/custom_status.{type CustomStatus}
import fluxer_gateway/presence/dispatch_id.{type DispatchId, type Recent}
import fluxer_gateway/presence/display.{type Display}
import fluxer_gateway/presence/event.{type PresenceEvent, type Row}
import fluxer_gateway/presence/id_set.{type IdSet}
import fluxer_gateway/presence/payload
import fluxer_gateway/presence/profile.{type Profile}
import fluxer_gateway/presence/protocol.{
  type AdoptReply, type Carried, type Catchup, type ConnectReply,
  type ConnectRequest, type Doom, type Input, type Published, type StartRequest,
  type Subscriber, type TerminateOutcome, type TerminateResult, type UserEvent,
  AdoptMoved, Adopted, Carried, CarriedSession, ConnectMoved, ConnectReply,
  Published, StartRequest, TerminateEvery, TerminateMatching, TerminateMoved,
  TerminateOk, TerminateRefused,
}
import fluxer_gateway/presence/push_buffer.{
  type Limits, type Pending, type PushBuffer,
}
import fluxer_gateway/presence/sessions.{type Sessions}
import fluxer_gateway/presence/version.{type Epoch, type Seen, type Version}
import fluxer_gateway/session/control.{type Control}
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order

type Validation {
  Idle
  Pending(generation: Int, candidate: CustomStatus)
}

pub type Effect(monitor) {
  SendSessions(sessions: List(SessionRef), message: SessionInbox)
  NotifySessions(sessions: List(SessionRef), control: Control)
  SendSnapshot(session: SessionRef, events: List(PresenceEvent))
  WriteRow(user: Int, row: Row)
  DeleteRow(user: Int)
  Fanout(event: PresenceEvent)
  ResendEvent(event: PresenceEvent, to: Subscriber)
  ResendToNode(event: PresenceEvent, node: Atom)
  Admit(
    to: Subscriber,
    catchup: Catchup,
    current: Option(PresenceEvent),
    visible: Bool,
  )
  Subscribe(users: IdSet)
  Unsubscribe(users: IdSet)
  ReadSnapshot(users: IdSet, token: Int)
  StartValidation(candidate: CustomStatus, generation: Int)
  ArmExpiry(after_ms: Int)
  CancelExpiry
  ArmGrace(token: Int, after_ms: Int)
  ArmIdleCheck(after_ms: Int)
  PushFlush(messages: List(Pending))
  PushNow(message: Pending)
  PushOverflow(dropped: Int)
  NoteSessionActive
  Demonitor(monitor: monitor)
  AskTerminate(session: SessionRef, hashes: List(String), token: Int)
  KickTemporary(guilds: List(Int))
  ReplyConnect(to: Subject(ConnectReply), reply: ConnectReply)
  ReplyTerminate(to: Subject(TerminateOutcome), outcome: TerminateOutcome)
  ReplyAdopt(to: Subject(AdoptReply), reply: AdoptReply)
  StartMove(node: Atom, token: Int, request: StartRequest)
  RetryMove(after_ms: Int)
  Forward(to: Subject(Input(Monitor)), inputs: List(Input(Monitor)))
  Replay(inputs: List(Input(monitor)))
  Stop
}

type Termination {
  Termination(
    reply: Subject(TerminateOutcome),
    hashes: List(String),
    waiting: List(SessionId),
    removed: List(SessionId),
    any_removed: Bool,
    errors: List(#(SessionId, String)),
  )
}

type Move {
  Settled
  Backoff
  Moving(token: Int, node: Atom)
  Moved
}

pub opaque type State(monitor) {
  State(
    user: Int,
    profile: Profile,
    sessions: Sessions(monitor),
    settings_invisible: Bool,
    custom_status: Option(CustomStatus),
    validation: Validation,
    generation: Int,
    expiry: Option(Int),
    targets: IdSet,
    seen: Seen,
    guilds: IdSet,
    temporary_guilds: IdSet,
    last: Option(Published),
    current: Option(PresenceEvent),
    published_ms: Int,
    version: Version,
    push: PushBuffer,
    push_limits: Limits,
    token: Int,
    snapshots: Dict(Int, SessionRef),
    terminations: Dict(Int, Termination),
    doomed: Dict(SessionId, #(Doom, Int)),
    grace: Option(Int),
    move: Move,
    move_attempts: Int,
    stash: List(Input(monitor)),
    delivered: Recent,
  )
}

const json_null = <<"null":utf8>>

fn noconnection_grace_ms() -> Int {
  durations.presence().state_noconnection_grace_ms
}

fn doomed_for_ms() -> Int {
  durations.presence().state_doomed_for_ms
}

fn max_move_backoff_ms() -> Int {
  durations.presence().state_max_move_backoff_ms
}

fn idle_check_ms() -> Int {
  durations.presence().state_idle_check_ms
}

fn move_busy_retry_ms() -> Int {
  durations.presence().state_move_busy_retry_ms
}

pub fn init(
  request: StartRequest,
  epoch: Epoch,
  carried_monitors: List(monitor),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let bot = profile.is_bot(request.profile)
  let targets = case bot {
    True -> id_set.empty()
    False -> id_set.remove(request.targets, request.user)
  }
  let base =
    State(
      user: request.user,
      profile: request.profile,
      sessions: sessions.empty(),
      settings_invisible: request.settings_invisible,
      custom_status: request.custom_status,
      validation: Idle,
      generation: 0,
      expiry: None,
      targets:,
      seen: version.empty_seen(),
      guilds: request.guilds,
      temporary_guilds: id_set.empty(),
      last: None,
      current: None,
      published_ms: 0,
      version: version.first(epoch),
      push: push_buffer.new(),
      push_limits: request.push_limits,
      token: 0,
      snapshots: dict.new(),
      terminations: dict.new(),
      doomed: dict.new(),
      grace: None,
      move: Settled,
      move_attempts: 0,
      stash: [],
      delivered: dispatch_id.empty(),
    )
  let state = case request.carried {
    None -> base
    Some(carried) -> adopt(base, carried, carried_monitors)
  }
  let subscribed =
    list.append(subscribe(targets), [
      ArmIdleCheck(idle_check_ms()),
    ])
  case request.carried {
    None -> #(state, subscribed)
    Some(_) -> {
      let validation = case state.validation {
        Pending(generation:, candidate:) -> [
          StartValidation(candidate, generation),
        ]
        Idle -> []
      }
      let #(state, announced) = case state.current, last_visible(state) {
        Some(current), True -> #(State(..state, published_ms: now_ms), [
          WriteRow(state.user, event.row(current, published_ms: now_ms)),
          Fanout(current),
        ])
        _, _ -> #(state, [])
      }
      let #(state, published) = publish(state, now_ms, False, False)
      #(state, list.flatten([subscribed, validation, announced, published]))
    }
  }
}

fn adopt(
  state: State(monitor),
  carried: Carried,
  monitors: List(monitor),
) -> State(monitor) {
  let adopted =
    list.zip(carried.sessions, monitors)
    |> list.fold(sessions.empty(), fn(acc, pair) {
      let #(CarriedSession(session:, status:, afk:, mobile:), monitor) = pair
      sessions.upsert(
        acc,
        sessions.entry(session:, status:, afk:, mobile:, monitor:),
      ).0
    })
  let #(generation, validation) = case carried.validating {
    Some(candidate) -> #(1, Pending(generation: 1, candidate:))
    None -> #(0, Idle)
  }
  State(
    ..state,
    sessions: adopted,
    version: carried.version,
    last: carried.last,
    current: carried.current,
    temporary_guilds: carried.temporary_guilds,
    push: carried.push,
    generation:,
    validation:,
    delivered: carried.delivered,
    doomed: dict.from_list(
      list.map(carried.doomed, fn(row) { #(row.0, #(row.1, row.2)) }),
    ),
  )
}

pub fn has_moved(state: State(monitor)) -> Bool {
  state.move == Moved
}

pub fn is_moving(state: State(monitor)) -> Bool {
  case state.move {
    Moving(..) -> True
    Settled | Backoff | Moved -> False
  }
}

pub fn handle(
  state: State(monitor),
  input: Input(monitor),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case state.move {
    Moving(..) -> while_moving(state, input, now_ms)
    Settled | Backoff | Moved -> dispatch(state, input, now_ms)
  }
}

fn while_moving(
  state: State(monitor),
  input: Input(monitor),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case input {
    protocol.UserDispatch(id:, ..) ->
      case dispatch_id.contains(state.delivered, id) {
        True -> #(state, [])
        False -> #(defer(state, input), [])
      }
    protocol.ConnectSession(..)
    | protocol.Connect(..)
    | protocol.RepointSession(..)
    | protocol.Repoint(..)
    | protocol.SessionDown(..)
    | protocol.Disconnect(..)
    | protocol.Update(..)
    | protocol.SubscribeTargets(..)
    | protocol.UnsubscribeTargets(..)
    | protocol.Validated(..)
    | protocol.Guild(..)
    | protocol.TerminateSessions(..)
    | protocol.AdoptHandoff(..)
    | protocol.Adopt(..)
    | protocol.ExpiryTick
    | protocol.GraceExpired(..) -> #(defer(state, input), [])
    _ -> dispatch(state, input, now_ms)
  }
}

fn defer(state: State(monitor), input: Input(monitor)) -> State(monitor) {
  State(..state, stash: [input, ..state.stash])
}

fn redirect(
  input: Input(monitor),
  inbox: Subject(Input(Monitor)),
) -> Result(Effect(monitor), Nil) {
  case input {
    protocol.ConnectSession(reply:, ..) ->
      Ok(ReplyConnect(reply, ConnectMoved(inbox)))
    protocol.TerminateSessions(reply:, ..) ->
      Ok(ReplyTerminate(reply, TerminateMoved(inbox)))
    protocol.AdoptHandoff(reply:, ..) | protocol.Adopt(reply:, ..) ->
      Ok(ReplyAdopt(reply, AdoptMoved(inbox)))
    _ -> Error(Nil)
  }
}

fn portable(state: State(a), input: Input(a)) -> Result(Input(b), Nil) {
  case input {
    protocol.SessionDown(monitor, ending: protocol.Closed) ->
      case sessions.find_by_monitor(state.sessions, monitor) {
        Ok(entry) -> {
          let session = sessions.entry_session(entry)
          Ok(protocol.Disconnect(session.id, session.inbox))
        }
        Error(Nil) -> Error(Nil)
      }
    protocol.Disconnect(session, owner) ->
      Ok(protocol.Disconnect(session, owner))
    protocol.RepointSession(session, from) ->
      Ok(protocol.RepointSession(session, from))
    protocol.Repoint(session, from, _) ->
      Ok(protocol.RepointSession(session, from))
    protocol.Update(session, status, afk, mobile, custom) ->
      Ok(protocol.Update(session, status, afk, mobile, custom))
    protocol.SubscribeTargets(users) -> Ok(protocol.SubscribeTargets(users))
    protocol.UnsubscribeTargets(users) -> Ok(protocol.UnsubscribeTargets(users))
    protocol.UserDispatch(id, event, data, parsed) ->
      Ok(protocol.UserDispatch(id, event, data, parsed))
    protocol.Guild(operation) -> Ok(protocol.Guild(operation))
    _ -> Error(Nil)
  }
}

fn dispatch(
  state: State(monitor),
  input: Input(monitor),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case input {
    protocol.Connect(request, reply) -> connect(state, request, reply, now_ms)
    protocol.Repoint(session, from, monitor) ->
      repoint(state, session, from, monitor)
    protocol.RepointSession(_, from) -> #(state, [
      SendSessions([from], PresenceRebound(False)),
    ])
    protocol.SessionDown(monitor, ending) ->
      session_down(state, monitor, ending, now_ms)
    protocol.Disconnect(session, owner) ->
      disconnect(state, session, owner, now_ms)
    protocol.Update(session, status, afk, mobile, custom) ->
      update(state, session, status, afk, mobile, custom, now_ms)
    protocol.SubscribeTargets(users) -> add_targets(state, users)
    protocol.UnsubscribeTargets(users) -> remove_targets(state, users)
    protocol.Foreign(incoming) -> foreign(state, incoming)
    protocol.Resend(to) -> #(state, resend(state, to))
    protocol.ResendSince(to, since_ms) -> #(
      state,
      resend_since(state, to, since_ms),
    )
    protocol.ResendNode(node) -> #(state, resend_node(state, node))
    protocol.Joined(to, catchup) -> #(state, [
      Admit(to, catchup, state.current, last_visible(state)),
    ])
    protocol.SnapshotRead(token, events) -> snapshot_read(state, token, events)
    protocol.Validated(generation, reply) ->
      validated(state, generation, reply, now_ms)
    protocol.UserDispatch(id, name, data, parsed) ->
      user_dispatch(state, id, name, data, parsed, now_ms)
    protocol.Guild(operation) -> guild_op(state, operation)
    protocol.VisitStart(guild, operator) -> #(
      state,
      guild_notice(state, guild, fn(g) { control.VisitStart(g, operator) }),
    )
    protocol.VisitEnd(guild) -> #(
      state,
      guild_notice(state, guild, control.VisitEnd),
    )
    protocol.TerminateSessions(hashes, reply) ->
      terminate_sessions(state, hashes, reply, now_ms)
    protocol.TerminateReply(token, session, result) ->
      terminate_reply(state, token, session, result, now_ms)
    protocol.TerminateAll -> #(
      doom(state, lost_ids(state), TerminateEvery, now_ms),
      send_all(state, Terminate(TerminatedByRpc)),
    )
    protocol.Rejoin -> #(state, notify_all(state, control.PresenceRejoinCheck))
    protocol.ExpiryTick -> publish(state, now_ms, False, True)
    protocol.GraceExpired(token) -> grace_expired(state, token, now_ms)
    protocol.IdleCheck -> idle_check(state)
    protocol.MoveTo(node) -> move_to(state, node)
    protocol.MoveRetry(target) -> move_retry(state, target)
    protocol.MoveFinished(token, moved) -> move_finished(state, token, moved)
    protocol.Adopt(request, monitors, reply) ->
      adopt_handoff(state, request, monitors, reply, now_ms)
    protocol.ShedTick
    | protocol.SubscribeAfterStart(..)
    | protocol.ConnectSession(..)
    | protocol.AdoptHandoff(..) -> #(state, [])
  }
}

pub fn terminate(state: State(monitor)) -> List(Effect(monitor)) {
  let replies =
    dict.values(state.terminations)
    |> list.map(fn(pending) {
      ReplyTerminate(pending.reply, termination_outcome(pending))
    })
  case state.move {
    Moved -> replies
    Settled | Backoff | Moving(..) -> {
      let #(_, pending) = push_buffer.flush(state.push)
      let flushed = case pending {
        [] -> []
        _ -> [PushFlush(pending)]
      }
      let offline = case last_visible(state) {
        True -> [
          Fanout(event.status_changed(
            user: state.user,
            version: version.next(state.version),
            display: display.offline(),
            custom_status: json_null,
            user_wire: profile.wire(state.profile),
          )),
        ]
        False -> []
      }
      let kicked = case id_set.to_list(state.temporary_guilds) {
        [] -> []
        guilds -> [KickTemporary(guilds)]
      }
      list.flatten([
        replies,
        flushed,
        [DeleteRow(state.user)],
        offline,
        unsubscribe(state.targets),
        kicked,
      ])
    }
  }
}

fn subscribe(users: IdSet) -> List(Effect(monitor)) {
  case id_set.is_empty(users) {
    True -> []
    False -> [Subscribe(users)]
  }
}

fn unsubscribe(users: IdSet) -> List(Effect(monitor)) {
  case id_set.is_empty(users) {
    True -> []
    False -> [Unsubscribe(users)]
  }
}

fn send_to(
  refs: List(SessionRef),
  message: SessionInbox,
) -> List(Effect(monitor)) {
  case refs {
    [] -> []
    _ -> [SendSessions(refs, message)]
  }
}

fn send_all(
  state: State(monitor),
  message: SessionInbox,
) -> List(Effect(monitor)) {
  send_to(sessions.live_refs(state.sessions), message)
}

fn notify_all(
  state: State(monitor),
  message: Control,
) -> List(Effect(monitor)) {
  case sessions.live_refs(state.sessions) {
    [] -> []
    refs -> [NotifySessions(refs, message)]
  }
}

fn guild_notice(
  state: State(monitor),
  guild: Int,
  build: fn(id.GuildId) -> Control,
) -> List(Effect(monitor)) {
  case id.guild_id(guild) {
    Ok(parsed) -> notify_all(state, build(parsed))
    Error(_) -> []
  }
}

fn replace_wire(state: State(monitor)) -> RawJson {
  sessions.replace_wire(state.sessions, invisible: state.settings_invisible)
  |> raw_json.from_tree
}

fn replace_message(state: State(monitor)) -> SessionInbox {
  Dispatch("SESSIONS_REPLACE", replace_wire(state))
}

fn frame(
  user: Int,
  wire: BitArray,
  facts: DispatchFacts,
) -> Result(SessionInbox, Nil) {
  case id.user_id(user) {
    Ok(parsed) -> Ok(PresenceFrame(parsed, raw_json.wrap(wire), facts))
    Error(_) -> Error(Nil)
  }
}

fn last_visible(state: State(monitor)) -> Bool {
  case state.last {
    Some(published) -> display.is_visible(published.display)
    None -> False
  }
}

fn row_effect(
  user: Int,
  published: PresenceEvent,
  now_ms: Int,
) -> Effect(monitor) {
  case display.is_visible(published.display) {
    True -> WriteRow(user, event.row(published, published_ms: now_ms))
    False -> DeleteRow(user)
  }
}

fn visible_custom_status(
  state: State(monitor),
  shown: Display,
  now_ms: Int,
) -> #(BitArray, BitArray) {
  case
    custom_status.visible(
      state.custom_status,
      now_ms:,
      restricted: profile.restricted(state.profile),
      display: shown,
    )
  {
    Some(value) -> #(custom_status.key(value), custom_status.wire(value))
    None -> #(json_null, json_null)
  }
}

fn publish(
  state: State(monitor),
  now_ms: Int,
  force: Bool,
  fired: Bool,
) -> #(State(monitor), List(Effect(monitor))) {
  let shown =
    sessions.flatten(state.sessions, invisible: state.settings_invisible)
  let external = display.external(shown)
  let #(key, wire) = visible_custom_status(state, shown, now_ms)
  let published = Published(external, key, wire)
  let #(state, timer) = rearm(state, now_ms, fired)
  let #(state, sent) = case force || state.last != Some(published) {
    False -> #(state, [])
    True -> {
      let next = version.next(state.version)
      let outgoing =
        event.status_changed(
          user: state.user,
          version: next,
          display: external,
          custom_status: wire,
          user_wire: profile.wire(state.profile),
        )
      #(
        State(
          ..state,
          version: next,
          last: Some(published),
          current: Some(outgoing),
          published_ms: now_ms,
        ),
        [row_effect(state.user, outgoing, now_ms), Fanout(outgoing)],
      )
    }
  }
  let #(state, push) = push_eligibility(state)
  #(state, list.flatten([timer, sent, push]))
}

fn rearm(
  state: State(monitor),
  now_ms: Int,
  fired: Bool,
) -> #(State(monitor), List(Effect(monitor))) {
  let armed = case fired {
    True -> None
    False -> state.expiry
  }
  let target = case
    option.then(state.custom_status, custom_status.expires_at_ms),
    custom_status.wakeup_ms(state.custom_status, now_ms)
  {
    Some(at), Some(delay) -> Some(#(at, delay))
    _, _ -> None
  }
  case target, armed {
    None, None -> #(State(..state, expiry: None), [])
    None, Some(_) -> #(State(..state, expiry: None), [CancelExpiry])
    Some(#(at, _)), Some(current) if current == at -> #(state, [])
    Some(#(at, delay)), _ -> #(State(..state, expiry: Some(at)), [
      ArmExpiry(delay),
    ])
  }
}

fn push_eligibility(
  state: State(monitor),
) -> #(State(monitor), List(Effect(monitor))) {
  let eligible = !sessions.holds_push(state.sessions)
  let #(buffer, pending, announce) =
    push_buffer.set_eligible(state.push, eligible)
  let announced = case announce {
    True -> [NoteSessionActive]
    False -> []
  }
  let flushed = case pending {
    [] -> []
    _ -> [PushFlush(pending)]
  }
  #(State(..state, push: buffer), list.append(announced, flushed))
}

fn connect(
  state: State(monitor),
  request: ConnectRequest(monitor),
  reply: Subject(ConnectReply),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let #(cleared, _) = sessions.drop_unreachable(state.sessions)
  let grace = case sessions.has_lost(cleared) {
    True -> state.grace
    False -> None
  }
  let state = State(..state, sessions: cleared, grace:)
  let before = sessions.live_refs(state.sessions)
  let added =
    sessions.entry(
      session: request.session,
      status: request.status,
      afk: request.afk,
      mobile: request.mobile,
      monitor: request.monitor,
    )
  let #(next, outcome) = sessions.upsert(state.sessions, added)
  let state =
    State(
      ..state,
      sessions: next,
      guilds: id_set.union(state.guilds, request.guilds),
    )
  let #(state, subscribed) = add_targets(state, request.targets)
  let first = case outcome {
    sessions.Added ->
      send_to(
        list.filter(before, fn(other) { other.id != request.session.id }),
        replace_message(state),
      )
    sessions.Unchanged -> [Demonitor(request.monitor)]
    sessions.Refreshed(old_monitor) -> [Demonitor(old_monitor)]
  }
  let wire = raw_json.to_bits(replace_wire(state))
  let #(state, published) = publish(state, now_ms, False, False)
  let #(state, snapshot) = request_snapshot(state, request.session)
  let #(state, doomed) = execute_doom(state, request.session, now_ms)
  #(
    state,
    list.flatten([
      subscribed,
      first,
      [ReplyConnect(reply, ConnectReply(wire))],
      published,
      snapshot,
      doomed,
    ]),
  )
}

fn repoint(
  state: State(monitor),
  to: SessionRef,
  from: SessionRef,
  monitor: monitor,
) -> #(State(monitor), List(Effect(monitor))) {
  case sessions.repoint(state.sessions, to, from.inbox, monitor) {
    Ok(#(next, old)) -> #(State(..state, sessions: next), [
      Demonitor(old),
      SendSessions([from], PresenceRebound(True)),
    ])
    Error(Nil) -> #(state, [
      Demonitor(monitor),
      SendSessions([from], PresenceRebound(False)),
    ])
  }
}

fn request_snapshot(
  state: State(monitor),
  session: SessionRef,
) -> #(State(monitor), List(Effect(monitor))) {
  case id_set.is_empty(state.targets) {
    True -> #(state, [])
    False -> {
      let token = state.token + 1
      #(
        State(
          ..state,
          token:,
          snapshots: dict.insert(state.snapshots, token, session),
        ),
        [ReadSnapshot(state.targets, token)],
      )
    }
  }
}

fn snapshot_read(
  state: State(monitor),
  token: Int,
  events: List(PresenceEvent),
) -> #(State(monitor), List(Effect(monitor))) {
  case dict.get(state.snapshots, token) {
    Error(Nil) -> #(state, [])
    Ok(target) -> {
      let kept =
        list.filter(events, fn(e) {
          e.user != state.user
          && id_set.contains(state.targets, e.user)
          && !version.superseded(state.seen, e.user, e.version)
        })
      let known =
        list.fold(kept, state.seen, fn(acc, e) {
          version.observe(acc, e.user, e.version).0
        })
      let state =
        State(
          ..state,
          seen: known,
          snapshots: dict.delete(state.snapshots, token),
        )
      let connected =
        list.any(sessions.live_refs(state.sessions), fn(r) { r.id == target.id })
      case kept, connected {
        [_, ..], True -> #(state, [SendSnapshot(target, kept)])
        _, _ -> #(state, [])
      }
    }
  }
}

pub fn foreign(
  state: State(monitor),
  incoming: PresenceEvent,
) -> #(State(monitor), List(Effect(monitor))) {
  case
    incoming.user == state.user
    || !id_set.contains(state.targets, incoming.user)
  {
    True -> #(state, [])
    False -> {
      let #(known, fresh) =
        version.observe(state.seen, incoming.user, incoming.version)
      let state = State(..state, seen: known)
      case fresh, frame(incoming.user, incoming.wire, event.facts(incoming)) {
        True, Ok(message) -> #(state, send_all(state, message))
        _, _ -> #(state, [])
      }
    }
  }
}

fn resend(state: State(monitor), to: Subscriber) -> List(Effect(monitor)) {
  case state.current, last_visible(state) {
    Some(current), True -> [ResendEvent(current, to)]
    _, _ -> []
  }
}

fn resend_since(
  state: State(monitor),
  to: Subscriber,
  since_ms: Int,
) -> List(Effect(monitor)) {
  case state.current {
    Some(current) if state.published_ms >= since_ms -> [
      ResendEvent(current, to),
    ]
    _ -> []
  }
}

fn resend_node(state: State(monitor), node: Atom) -> List(Effect(monitor)) {
  case state.current, last_visible(state) {
    Some(current), True -> [ResendToNode(current, node)]
    _, _ -> []
  }
}

fn session_down(
  state: State(monitor),
  monitor: monitor,
  ending: protocol.SessionEnd,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case sessions.find_by_monitor(state.sessions, monitor) {
    Error(Nil) -> #(state, [])
    Ok(found) ->
      case ending {
        protocol.Unreachable | protocol.HandedOff -> {
          let token = state.token + 1
          let state =
            State(
              ..state,
              sessions: sessions.mark_lost(
                state.sessions,
                monitor,
                handed_off: ending == protocol.HandedOff,
              ),
              token:,
              grace: Some(token),
            )
          let #(state, resolved) =
            sessions_lost(state, sessions.entry_id(found), now_ms)
          #(state, [ArmGrace(token, noconnection_grace_ms()), ..resolved])
        }
        protocol.Closed ->
          case sessions.remove_by_monitor(state.sessions, monitor) {
            Ok(#(next, entry)) ->
              removed(state, next, [sessions.entry_id(entry)], now_ms)
            Error(Nil) -> #(state, [])
          }
      }
  }
}

fn grace_expired(
  state: State(monitor),
  token: Int,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case state.grace == Some(token) {
    False -> #(state, [])
    True -> {
      let #(next, dropped) = sessions.drop_lost(state.sessions)
      let gone = list.map(dropped, sessions.entry_id)
      removed(
        State(..state, grace: None, doomed: dict.drop(state.doomed, gone)),
        next,
        gone,
        now_ms,
      )
    }
  }
}

fn idle_check(
  state: State(monitor),
) -> #(State(monitor), List(Effect(monitor))) {
  case sessions.is_empty(state.sessions), state.move {
    False, _ | True, Moved -> #(state, [])
    True, Moving(..) -> #(state, [ArmIdleCheck(idle_check_ms())])
    True, Settled | True, Backoff -> #(state, [Stop])
  }
}

fn removed(
  state: State(monitor),
  next: Sessions(monitor),
  gone: List(SessionId),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let #(state, published) =
    publish(State(..state, sessions: next), now_ms, False, False)
  case sessions.is_empty(next) {
    True -> #(state, list.append(published, [Stop]))
    False -> {
      let replaced = case sessions.has_live(next) {
        True -> send_all(state, replace_message(state))
        False -> []
      }
      let #(state, resolved) = sessions_gone(state, gone, now_ms)
      #(state, list.flatten([published, replaced, resolved]))
    }
  }
}

fn sessions_lost(
  state: State(monitor),
  lost: SessionId,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  dict.to_list(state.terminations)
  |> list.fold(#(state, []), fn(acc, pair) {
    let #(token, pending) = pair
    case list.contains(pending.waiting, lost) {
      False -> acc
      True -> {
        let #(state, effects) = acc
        let state =
          doom(state, [lost], TerminateMatching(pending.hashes), now_ms)
        let pending =
          Termination(
            ..pending,
            waiting: list.filter(pending.waiting, fn(id) { id != lost }),
          )
        let #(state, more) = advance_termination(state, token, pending, now_ms)
        #(state, list.append(effects, more))
      }
    }
  })
}

fn doom(
  state: State(monitor),
  ids: List(SessionId),
  verdict: Doom,
  now_ms: Int,
) -> State(monitor) {
  let doomed =
    list.fold(ids, state.doomed, fn(doomed, id) {
      merge_doom(doomed, #(id, verdict, now_ms))
    })
  State(..state, doomed:)
}

fn merge_doom(
  doomed: Dict(SessionId, #(Doom, Int)),
  row: #(SessionId, Doom, Int),
) -> Dict(SessionId, #(Doom, Int)) {
  let #(id, verdict, at_ms) = row
  dict.upsert(doomed, id, fn(existing) {
    case existing, verdict {
      Some(#(TerminateEvery, before_ms)), _ -> #(
        TerminateEvery,
        int.max(before_ms, at_ms),
      )
      _, TerminateEvery -> #(TerminateEvery, at_ms)
      Some(#(TerminateMatching(before), before_ms)), TerminateMatching(more) -> #(
        TerminateMatching(list.unique(list.append(before, more))),
        int.max(before_ms, at_ms),
      )
      None, _ -> #(verdict, at_ms)
    }
  })
}

fn lost_ids(state: State(monitor)) -> List(SessionId) {
  let live = list.map(sessions.live_refs(state.sessions), fn(r) { r.id })
  sessions.entries(state.sessions)
  |> list.map(sessions.entry_id)
  |> list.filter(fn(id) { !list.contains(live, id) })
}

fn execute_doom(
  state: State(monitor),
  session: SessionRef,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let current =
    dict.filter(state.doomed, fn(_, entry) {
      now_ms - entry.1 <= doomed_for_ms()
    })
  case dict.get(current, session.id) {
    Error(Nil) -> #(State(..state, doomed: current), [])
    Ok(#(TerminateEvery, _)) -> #(
      State(..state, doomed: dict.delete(current, session.id)),
      [SendSessions([session], Terminate(TerminatedByRpc))],
    )
    Ok(#(TerminateMatching(hashes), _)) -> {
      let token = state.token + 1
      #(State(..state, token:, doomed: dict.delete(current, session.id)), [
        AskTerminate(session, hashes, token),
      ])
    }
  }
}

fn sessions_gone(
  state: State(monitor),
  gone: List(SessionId),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  dict.to_list(state.terminations)
  |> list.fold(#(state, []), fn(acc, pair) {
    let #(token, pending) = pair
    case list.partition(pending.waiting, list.contains(gone, _)) {
      #([], _) -> acc
      #(hit, waiting) -> {
        let #(state, effects) = acc
        let pending =
          Termination(
            ..pending,
            waiting:,
            removed: list.append(hit, pending.removed),
            any_removed: True,
          )
        let #(state, more) = advance_termination(state, token, pending, now_ms)
        #(state, list.append(effects, more))
      }
    }
  })
}

fn disconnect(
  state: State(monitor),
  session: SessionId,
  owner: Subject(SessionInbox),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case sessions.remove_owned(state.sessions, session, owner) {
    #(_, []) -> #(state, [])
    #(next, gone) -> {
      let demonitors =
        list.map(gone, fn(e) { Demonitor(sessions.entry_monitor(e)) })
      let #(state, effects) =
        removed(state, next, list.map(gone, sessions.entry_id), now_ms)
      #(state, list.append(demonitors, effects))
    }
  }
}

fn update(
  state: State(monitor),
  session: SessionId,
  status: Status,
  afk: Bool,
  mobile: Bool,
  custom: custom_status.OpValue,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let compared = case state.validation {
    Pending(candidate:, ..) -> Some(candidate)
    Idle -> state.custom_status
  }
  let #(state, validation) = case custom_status.classify(compared, custom) {
    custom_status.Keep -> #(state, [])
    custom_status.Clear -> #(
      State(..state, custom_status: None, validation: Idle),
      [],
    )
    custom_status.Validate(candidate) -> {
      let generation = state.generation + 1
      #(
        State(
          ..state,
          generation:,
          validation: Pending(generation:, candidate:),
        ),
        [StartValidation(candidate, generation)],
      )
    }
  }
  let #(state, replaced) = case
    sessions.set_state(state.sessions, session, status, afk, mobile)
  {
    Ok(#(next, True)) -> {
      let state = State(..state, sessions: next)
      #(state, send_all(state, replace_message(state)))
    }
    _ -> #(state, [])
  }
  let #(state, published) = publish(state, now_ms, False, False)
  #(state, list.flatten([validation, replaced, published]))
}

fn validated(
  state: State(monitor),
  generation: Int,
  reply: custom_status.ValidationReply,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case state.validation {
    Pending(generation: current, ..) if current == generation ->
      publish(
        State(
          ..state,
          custom_status: custom_status.from_validated(
            state.custom_status,
            reply,
          ),
          validation: Idle,
        ),
        now_ms,
        False,
        False,
      )
    _ -> #(state, [])
  }
}

fn add_targets(
  state: State(monitor),
  users: IdSet,
) -> #(State(monitor), List(Effect(monitor))) {
  case profile.is_bot(state.profile) {
    True -> #(state, [])
    False -> {
      let added =
        users
        |> id_set.subtract(state.targets)
        |> id_set.remove(state.user)
      #(
        State(..state, targets: id_set.union(state.targets, added)),
        subscribe(added),
      )
    }
  }
}

fn remove_targets(
  state: State(monitor),
  users: IdSet,
) -> #(State(monitor), List(Effect(monitor))) {
  let gone =
    id_set.subtract(state.targets, id_set.subtract(state.targets, users))
  case id_set.is_empty(gone) {
    True -> #(state, [])
    False -> {
      let state =
        State(
          ..state,
          targets: id_set.subtract(state.targets, gone),
          seen: version.forget(state.seen, id_set.to_list(gone)),
        )
      let offline =
        id_set.to_list(gone)
        |> list.flat_map(fn(user) {
          let wire = payload.forced_offline_wire(user)
          case frame(user, wire, event.scan(wire)) {
            Ok(message) -> send_all(state, message)
            Error(Nil) -> []
          }
        })
      #(state, list.append(unsubscribe(gone), offline))
    }
  }
}

fn user_dispatch(
  state: State(monitor),
  id: DispatchId,
  name: String,
  data: RawJson,
  parsed: UserEvent,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case dispatch_id.contains(state.delivered, id) {
    True -> #(state, [])
    False -> {
      let state =
        State(..state, delivered: dispatch_id.remember(state.delivered, id))
      let forwarded = send_all(state, Dispatch(name, data))
      let #(state, applied) = apply_user_event(state, parsed, now_ms)
      #(state, list.append(forwarded, applied))
    }
  }
}

fn apply_user_event(
  state: State(monitor),
  parsed: UserEvent,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case parsed {
    protocol.UserUpdate(updated) -> user_update(state, updated, now_ms)
    protocol.UserSettingsUpdate(value, invisible) -> {
      let validation = case value {
        custom_status.SettingsAbsent -> state.validation
        _ -> Idle
      }
      publish(
        State(
          ..state,
          custom_status: custom_status.from_settings(state.custom_status, value),
          settings_invisible: option.unwrap(invisible, state.settings_invisible),
          validation:,
        ),
        now_ms,
        True,
        False,
      )
    }
    protocol.MessageCreate(Some(message)) -> {
      let eligible = !sessions.holds_push(state.sessions)
      let #(buffer, action) =
        push_buffer.on_message(
          state.push,
          message,
          state.user,
          eligible,
          now_ms,
          state.push_limits,
        )
      let effects = case action {
        push_buffer.SendNow([], pending) -> [PushNow(pending)]
        push_buffer.SendNow(flushed, pending) -> [
          PushFlush(flushed),
          PushNow(pending),
        ]
        push_buffer.Buffered(dropped) if dropped > 0 -> [PushOverflow(dropped)]
        push_buffer.Buffered(_) | push_buffer.Ignored -> []
      }
      #(State(..state, push: buffer), effects)
    }
    protocol.MessageAck(Some(channel), Some(message)) -> #(
      State(..state, push: push_buffer.on_ack(state.push, channel, message)),
      [],
    )
    protocol.MessageCreate(None)
    | protocol.MessageAck(_, _)
    | protocol.OtherEvent -> #(state, [])
  }
}

fn user_update(
  state: State(monitor),
  updated: Profile,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let change = profile.apply_update(state.profile, updated)
  let #(state, sent) = case change.differs, state.last {
    True, Some(published) ->
      case display.is_visible(published.display) {
        True -> {
          let next = version.next(state.version)
          let outgoing =
            event.status_changed(
              user: state.user,
              version: next,
              display: published.display,
              custom_status: published.custom_status_wire,
              user_wire: profile.wire(change.profile),
            )
          #(
            State(
              ..state,
              version: next,
              current: Some(outgoing),
              published_ms: now_ms,
            ),
            [
              row_effect(state.user, outgoing, now_ms),
              Fanout(event.as_user_update(outgoing)),
            ],
          )
        }
        False -> #(state, [])
      }
    _, _ -> #(state, [])
  }
  let state = State(..state, profile: change.profile)
  case change.restriction_changed {
    True -> {
      let #(state, published) = publish(state, now_ms, False, False)
      #(state, list.append(sent, published))
    }
    False -> #(state, sent)
  }
}

fn join(
  state: State(monitor),
  guild: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case id_set.contains(state.guilds, guild) {
    True -> #(state, [])
    False -> {
      let state = State(..state, guilds: id_set.insert(state.guilds, guild))
      #(state, guild_notice(state, guild, control.GuildJoin))
    }
  }
}

fn guild_op(
  state: State(monitor),
  operation: protocol.GuildMembership,
) -> #(State(monitor), List(Effect(monitor))) {
  case operation {
    protocol.JoinGuild(guild) -> join(state, guild)
    protocol.LeaveGuild(guild) ->
      case id_set.contains(state.guilds, guild) {
        False -> #(state, [])
        True -> {
          let state =
            State(
              ..state,
              guilds: id_set.remove(state.guilds, guild),
              temporary_guilds: id_set.remove(state.temporary_guilds, guild),
            )
          #(state, guild_notice(state, guild, control.GuildLeave))
        }
      }
    protocol.AddTemporaryGuild(guild) -> {
      let #(state, joined) = join(state, guild)
      #(
        State(
          ..state,
          temporary_guilds: id_set.insert(state.temporary_guilds, guild),
        ),
        joined,
      )
    }
    protocol.RemoveTemporaryGuild(guild) -> #(
      State(
        ..state,
        temporary_guilds: id_set.remove(state.temporary_guilds, guild),
      ),
      [],
    )
  }
}

fn terminate_sessions(
  state: State(monitor),
  hashes: List(String),
  reply: Subject(TerminateOutcome),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case sessions.entries(state.sessions) {
    [] -> #(state, [ReplyTerminate(reply, TerminateOk)])
    _ -> {
      let live = sessions.live_refs(state.sessions)
      let state =
        doom(state, lost_ids(state), TerminateMatching(hashes), now_ms)
      let token = state.token + 1
      let pending =
        Termination(
          reply:,
          hashes:,
          waiting: list.map(live, fn(r) { r.id }),
          removed: [],
          any_removed: False,
          errors: [],
        )
      case live {
        [] -> finish_termination(State(..state, token:), pending, now_ms)
        _ -> #(
          State(
            ..state,
            token:,
            terminations: dict.insert(state.terminations, token, pending),
          ),
          list.map(live, fn(r) { AskTerminate(r, hashes, token) }),
        )
      }
    }
  }
}

fn terminate_reply(
  state: State(monitor),
  token: Int,
  session: SessionId,
  result: TerminateResult,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case dict.get(state.terminations, token) {
    Error(Nil) -> #(state, [])
    Ok(pending) ->
      case list.contains(pending.waiting, session) {
        False -> #(state, [])
        True -> {
          let pending = case result {
            protocol.Terminated | protocol.Gone ->
              Termination(
                ..pending,
                removed: [session, ..pending.removed],
                any_removed: True,
              )
            protocol.KeptBySession -> pending
            protocol.TerminateFailed(reason) ->
              Termination(..pending, errors: [
                #(session, reason),
                ..pending.errors
              ])
          }
          let pending =
            Termination(
              ..pending,
              waiting: list.filter(pending.waiting, fn(k) { k != session }),
            )
          advance_termination(state, token, pending, now_ms)
        }
      }
  }
}

fn advance_termination(
  state: State(monitor),
  token: Int,
  pending: Termination,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case pending.waiting {
    [] ->
      finish_termination(
        State(..state, terminations: dict.delete(state.terminations, token)),
        pending,
        now_ms,
      )
    _ -> #(
      State(
        ..state,
        terminations: dict.insert(state.terminations, token, pending),
      ),
      [],
    )
  }
}

fn termination_outcome(pending: Termination) -> TerminateOutcome {
  case pending.errors {
    [] -> TerminateOk
    [first, ..rest] -> TerminateRefused(highest_error(first, rest))
  }
}

fn finish_termination(
  state: State(monitor),
  done: Termination,
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  let #(next, gone) = sessions.remove_by_ids(state.sessions, done.removed)
  let demonitors =
    list.map(gone, fn(e) { Demonitor(sessions.entry_monitor(e)) })
  let state = State(..state, sessions: next)
  let #(state, published) = case done.any_removed {
    True -> publish(state, now_ms, False, False)
    False -> #(state, [])
  }
  let reply = ReplyTerminate(done.reply, termination_outcome(done))
  let tail = case done.any_removed, sessions.is_empty(next) {
    True, True -> [reply, Stop]
    True, False -> list.append(send_all(state, replace_message(state)), [reply])
    False, _ -> [reply]
  }
  #(state, list.flatten([demonitors, published, tail]))
}

fn highest_error(
  first: #(SessionId, String),
  rest: List(#(SessionId, String)),
) -> String {
  let best =
    list.fold(rest, first, fn(best, candidate) {
      case session.compare_session_id(candidate.0, best.0) {
        order.Gt -> candidate
        _ -> best
      }
    })
  best.1
}

fn move_to(
  state: State(monitor),
  node: Atom,
) -> #(State(monitor), List(Effect(monitor))) {
  case state.move {
    Backoff | Moving(..) | Moved -> #(state, [])
    Settled ->
      case dict.is_empty(state.snapshots) && dict.is_empty(state.terminations) {
        False -> #(State(..state, move: Backoff), [
          RetryMove(move_busy_retry_ms()),
        ])
        True -> start_moving(state, node)
      }
  }
}

fn move_retry(
  state: State(monitor),
  target: Option(Atom),
) -> #(State(monitor), List(Effect(monitor))) {
  case state.move, target {
    Backoff, Some(node) -> move_to(State(..state, move: Settled), node)
    Backoff, None -> #(State(..state, move: Settled), [])
    Settled, _ | Moving(..), _ | Moved, _ -> #(state, [])
  }
}

fn start_moving(
  state: State(monitor),
  node: Atom,
) -> #(State(monitor), List(Effect(monitor))) {
  let token = state.token + 1
  let state = State(..state, token:, move: Moving(token:, node:))
  #(state, [StartMove(node, token, snapshot(state))])
}

fn move_finished(
  state: State(monitor),
  token: Int,
  moved: Result(Subject(Input(Monitor)), Nil),
) -> #(State(monitor), List(Effect(monitor))) {
  case state.move, moved {
    Moving(token: current, ..), Ok(inbox) if current == token -> {
      let #(redirected, portable_inputs) =
        list.reverse(state.stash)
        |> list.fold(#([], []), fn(acc, input) {
          case redirect(input, inbox) {
            Ok(reply) -> #([reply, ..acc.0], acc.1)
            Error(Nil) ->
              case portable(state, input) {
                Ok(moved) -> #(acc.0, [moved, ..acc.1])
                Error(Nil) -> acc
              }
          }
        })
      let forwarded = case list.reverse(portable_inputs) {
        [] -> []
        inputs -> [Forward(inbox, inputs)]
      }
      #(
        State(
          ..state,
          push: push_buffer.new(),
          temporary_guilds: id_set.empty(),
          stash: [],
          move: Moved,
        ),
        list.flatten([
          list.reverse(redirected),
          forwarded,
          notify_all(state, control.PresenceMoved),
          [DeleteRow(state.user)],
          unsubscribe(state.targets),
          [Stop],
        ]),
      )
    }
    Moving(token: current, ..), Error(Nil) if current == token -> {
      let attempts = state.move_attempts + 1
      let delay =
        int.min(
          max_move_backoff_ms(),
          1000 * int.bitwise_shift_left(1, attempts - 1),
        )
      let replay = case state.stash {
        [] -> []
        stash -> [Replay(list.reverse(stash))]
      }
      #(State(..state, move: Backoff, move_attempts: attempts, stash: []), [
        RetryMove(delay),
        ..replay
      ])
    }
    _, _ -> #(state, [])
  }
}

fn adopt_handoff(
  state: State(monitor),
  request: StartRequest,
  monitors: List(monitor),
  reply: Subject(AdoptReply),
  now_ms: Int,
) -> #(State(monitor), List(Effect(monitor))) {
  case request.carried {
    None -> #(
      state,
      list.append(list.map(monitors, Demonitor), [ReplyAdopt(reply, Adopted)]),
    )
    Some(carried) -> {
      let #(next, changed, demonitors) =
        list.zip(carried.sessions, monitors)
        |> list.fold(#(state.sessions, False, []), fn(acc, pair) {
          let #(current, changed, demonitors) = acc
          let #(CarriedSession(session:, status:, afk:, mobile:), monitor) =
            pair
          let #(current, outcome) =
            sessions.upsert_carried(
              current,
              sessions.entry(session:, status:, afk:, mobile:, monitor:),
            )
          case outcome {
            sessions.Added -> #(current, True, demonitors)
            sessions.Unchanged -> #(current, changed, [
              Demonitor(monitor),
              ..demonitors
            ])
            sessions.Refreshed(old) -> #(current, True, [
              Demonitor(old),
              ..demonitors
            ])
          }
        })
      let fresh_guilds =
        id_set.subtract(carried.temporary_guilds, state.guilds)
        |> id_set.to_list
      let state =
        State(
          ..state,
          sessions: next,
          grace: case sessions.has_lost(next) {
            True -> state.grace
            False -> None
          },
          guilds: state.guilds
            |> id_set.union(request.guilds)
            |> id_set.union(carried.temporary_guilds),
          temporary_guilds: id_set.union(
            state.temporary_guilds,
            carried.temporary_guilds,
          ),
          push: push_buffer.merge(state.push, carried.push, state.push_limits),
          delivered: dispatch_id.merge(state.delivered, carried.delivered),
          doomed: list.fold(carried.doomed, state.doomed, merge_doom),
        )
      let #(state, pushed) = push_eligibility(state)
      let #(state, subscribed) = add_targets(state, request.targets)
      let joined =
        list.flat_map(fresh_guilds, fn(guild) {
          guild_notice(state, guild, control.GuildJoin)
        })
      let #(state, validation) = case state.validation, carried.validating {
        Idle, Some(candidate) -> {
          let generation = state.generation + 1
          #(
            State(
              ..state,
              generation:,
              validation: Pending(generation:, candidate:),
            ),
            [
              StartValidation(candidate, generation),
            ],
          )
        }
        _, _ -> #(state, [])
      }
      let replaced = case changed {
        True -> send_all(state, replace_message(state))
        False -> []
      }
      let #(state, published) = publish(state, now_ms, False, False)
      let #(state, doomed) =
        list.fold(carried.sessions, #(state, []), fn(acc, carried) {
          let #(state, effects) = acc
          let #(state, more) = execute_doom(state, carried.session, now_ms)
          #(state, list.append(effects, more))
        })
      #(
        state,
        list.flatten([
          list.reverse(demonitors),
          pushed,
          subscribed,
          joined,
          validation,
          replaced,
          published,
          [ReplyAdopt(reply, Adopted)],
          doomed,
        ]),
      )
    }
  }
}

fn snapshot(state: State(monitor)) -> StartRequest {
  StartRequest(
    user: state.user,
    profile: state.profile,
    custom_status: state.custom_status,
    settings_invisible: state.settings_invisible,
    guilds: state.guilds,
    targets: state.targets,
    push_limits: state.push_limits,
    carried: Some(Carried(
      version: state.version,
      last: state.last,
      current: state.current,
      temporary_guilds: state.temporary_guilds,
      push: state.push,
      validating: case state.validation {
        Pending(candidate:, ..) -> Some(candidate)
        Idle -> None
      },
      delivered: state.delivered,
      doomed: dict.to_list(state.doomed)
        |> list.map(fn(row) { #(row.0, row.1.0, row.1.1) }),
      sessions: list.map(sessions.entries(state.sessions), fn(e) {
        CarriedSession(
          session: sessions.entry_session(e),
          status: sessions.entry_status(e),
          afk: sessions.entry_afk(e),
          mobile: sessions.entry_mobile(e),
        )
      }),
    )),
  )
}

pub fn leftovers(state: State(monitor)) -> #(IdSet, PushBuffer) {
  #(state.temporary_guilds, state.push)
}

pub fn user(state: State(monitor)) -> Int {
  state.user
}

pub fn has_live_sessions(state: State(monitor)) -> Bool {
  sessions.has_live(state.sessions)
}

pub fn targets(state: State(monitor)) -> IdSet {
  state.targets
}

pub fn current_version(state: State(monitor)) -> Version {
  state.version
}

pub fn stored_custom_status(state: State(monitor)) -> Option(CustomStatus) {
  state.custom_status
}

pub fn export(state: State(monitor)) -> StartRequest {
  snapshot(state)
}

pub fn can_freeze(state: State(monitor)) -> Bool {
  case state.move {
    Settled | Backoff ->
      dict.is_empty(state.snapshots) && dict.is_empty(state.terminations)
    Moving(..) | Moved -> False
  }
}

pub fn crossing(state: State(a), input: Input(a)) -> Result(Input(b), Nil) {
  case input {
    protocol.SessionDown(..) -> portable(state, input)
    _ -> protocol.retag(input)
  }
}

pub fn session_refs(state: State(monitor)) -> List(SessionRef) {
  list.map(sessions.entries(state.sessions), sessions.entry_session)
}

pub fn hand_over(
  state: State(monitor),
) -> #(State(monitor), List(Effect(monitor))) {
  #(
    State(
      ..state,
      push: push_buffer.new(),
      temporary_guilds: id_set.empty(),
      stash: [],
      move: Moved,
    ),
    [DeleteRow(state.user), ..unsubscribe(state.targets)],
  )
}
